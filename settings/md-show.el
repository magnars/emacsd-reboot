;;; md-show.el --- Visual rendering of markdown buffers -*- lexical-binding: t -*-

;;; Commentary:
;; A minor mode that gives markdown buffers a GitHub-ish visual rendering
;; without modifying the underlying file.  All styling is done via overlays
;; and buffer-local display settings, so toggling the mode off restores the
;; raw text exactly as it was.
;;
;; Features:
;;   - Headings (#..######)        -> bold + blue, scaled by level
;;   - Blockquotes (> ...)         -> italic, subtle left bar
;;   - Fenced code blocks (```)    -> tinted background, monospace
;;     - ```clojure blocks         -> syntax-highlighted via clojure-mode
;;   - Inline code (`code`)        -> tinted background
;;   - Bold (**x**) / italic (*x*) -> rendered with their faces
;;   - Soft line wrapping at ~80 columns
;;
;; Usage:
;;   M-x md-show-mode    ; toggle in current buffer

;;; Code:

(require 'cl-lib)

;; ---- Customization ---------------------------------------------------------

(defgroup md-show nil
  "Visual rendering of markdown buffers."
  :group 'text)

(defcustom md-show-fill-column 80
  "Column at which md-show soft-wraps lines."
  :type 'integer
  :group 'md-show)

(defcustom md-show-language-modes
  '(("clojure"     . clojure-mode)
    ("clojurescript". clojurescript-mode)
    ("clj"         . clojure-mode)
    ("cljs"        . clojurescript-mode)
    ("edn"         . clojure-mode))
  "Alist mapping fenced-code-block language tags to major modes.
The mode is used in a temporary buffer to syntax-highlight the body
of a matching code block.  If the mode isn't available, the block
is left with its plain code-block styling."
  :type '(alist :key-type string :value-type symbol)
  :group 'md-show)

;; ---- Faces -----------------------------------------------------------------

(defface md-show-h1-face
  '((t :foreground "SteelBlue1" :weight bold :height 2.0 :inherit variable-pitch))
  "Face for level-1 headings.")

(defface md-show-h2-face
  '((t :foreground "SteelBlue1" :weight bold :height 1.6 :inherit variable-pitch))
  "Face for level-2 headings.")

(defface md-show-h3-face
  '((t :foreground "SteelBlue1" :weight bold :height 1.3 :inherit variable-pitch))
  "Face for level-3 headings.")

(defface md-show-h4-face
  '((t :foreground "SteelBlue1" :weight bold :height 1.1 :inherit variable-pitch))
  "Face for level-4+ headings.")

(defface md-show-quote-face
  '((t :slant italic :inherit font-lock-string-face))
  "Face for blockquote bodies.")

(defface md-show-quote-bar-face
  '((((background dark))  :foreground "gray40")
    (((background light)) :foreground "gray60"))
  "Face for the leading marker on blockquote lines.")

(defface md-show-code-block-face
  '((((background dark))  :background "gray8" :extend t :inherit fixed-pitch)
    (((background light)) :background "gray92" :extend t :inherit fixed-pitch))
  "Face for fenced code block bodies.")

(defface md-show-code-fence-face
  '((t :inherit (shadow fixed-pitch)))
  "Face for the ``` fence lines themselves.")

(defface md-show-inline-code-face
  '((((background dark))  :background "gray25" :inherit fixed-pitch)
    (((background light)) :background "gray88" :inherit fixed-pitch))
  "Face for inline `code`.")

(defface md-show-bold-face   '((t :weight bold))   "Face for **bold**.")
(defface md-show-italic-face '((t :slant italic))  "Face for *italic*.")

(defface md-show-marker-face
  '((t :inherit shadow))
  "Face for markdown punctuation that we keep visible (e.g. *, _, `).")

;; ---- Overlay bookkeeping ---------------------------------------------------

(defun md-show--make-overlay (beg end &rest props)
  "Create an overlay BEG..END tagged for md-show, with PROPS."
  (let ((ov (make-overlay beg end nil t nil)))
    (overlay-put ov 'md-show t)
    (overlay-put ov 'evaporate t)
    (cl-loop for (k v) on props by #'cddr do (overlay-put ov k v))
    ov))

(defun md-show--clear-overlays (beg end)
  "Remove all md-show overlays between BEG and END."
  (dolist (ov (overlays-in beg end))
    (when (overlay-get ov 'md-show)
      (delete-overlay ov))))

;; ---- Fontification passes --------------------------------------------------
;;
;; Order matters: we do code blocks first and remember their spans so the
;; inline passes don't re-style content inside them.

(defvar-local md-show--code-spans nil
  "List of (BEG . END) ranges covered by fenced code blocks.")

(defun md-show--in-code-block-p (pos)
  "Return non-nil if POS lies inside a known fenced code span."
  (cl-some (lambda (span) (and (>= pos (car span)) (< pos (cdr span))))
           md-show--code-spans))

;; ---- Language-aware syntax highlighting ------------------------------------
;;
;; We render BODY in a temp buffer under MODE, run font-lock, then walk the
;; resulting `face' (and `font-lock-face') text properties and project them
;; onto our buffer as overlays.  This works for any major mode without
;; coupling to clojure-mode internals.

(defun md-show--mode-for-lang (lang)
  "Return the major-mode symbol for LANG (a string), or nil if unknown/unavailable."
  (when lang
    (let* ((key (downcase (string-trim lang)))
           (mode (cdr (assoc key md-show-language-modes))))
      (when (and mode (fboundp mode))
        mode))))

(defun md-show--fontify-string (text mode)
  "Return a propertized copy of TEXT as fontified by MODE.
Returns nil if fontification fails for any reason."
  (condition-case nil
      (with-temp-buffer
        (insert text)
        (delay-mode-hooks (funcall mode))
        ;; Make sure font-lock is on and run it synchronously.
        (font-lock-mode 1)
        (font-lock-ensure (point-min) (point-max))
        (buffer-string))
    (error nil)))

(defun md-show--apply-fontified-overlays (target-beg fontified)
  "Project face properties from FONTIFIED string onto buffer at TARGET-BEG.
Each contiguous run of identical face value becomes one overlay.
Both `face' and `font-lock-face' text properties are honored, since
some major modes set one and some the other."
  (let ((len (length fontified))
        (i 0))
    (while (< i len)
      (let* ((face (or (get-text-property i 'face fontified)
                       (get-text-property i 'font-lock-face fontified)))
             ;; End of this run = first position where EITHER face property
             ;; changes.  `next-single-property-change' returns nil if the
             ;; property never changes again before LIMIT, in which case we
             ;; treat LIMIT as the boundary.
             (j-face (or (next-single-property-change i 'face fontified len)
                         len))
             (j-fl   (or (next-single-property-change i 'font-lock-face fontified len)
                         len))
             (j (min j-face j-fl)))
        ;; Defensive: never let j stall at i; that would loop forever.
        (when (<= j i) (setq j (1+ i)))
        (when face
          (md-show--make-overlay (+ target-beg i) (+ target-beg j)
                                 'face face))
        (setq i j)))))

(defun md-show--highlight-code-body (body-beg body-end lang)
  "Apply LANG-specific syntax highlighting to BODY-BEG..BODY-END if possible."
  (when-let ((mode (md-show--mode-for-lang lang)))
    (let* ((src (buffer-substring-no-properties body-beg body-end))
           (fontified (md-show--fontify-string src mode)))
      (when fontified
        (md-show--apply-fontified-overlays body-beg fontified)))))

(defun md-show--fontify-code-blocks (beg end)
  "Style fenced ``` code blocks between BEG and END.
Blocks tagged with a known language (see `md-show-language-modes')
get syntax-highlighted in addition to the base code-block face."
  (save-excursion
    (goto-char beg)
    ;; Match line-by-line.  We can't use `^...^...$' with a lazy "any chars
    ;; including newlines" group because Emacs' regex engine doesn't backtrack
    ;; the inner `^' against the lazy quantifier reliably.  Instead, we
    ;; consume whole lines: language tag on line 1, zero-or-more body lines,
    ;; closing fence line.
    (while (re-search-forward
            "^```\\(.*\\)\n\\(\\(?:.*\n\\)*?\\)```[ \t]*$"
            end t)
      (let* ((lang        (match-string 1))
             (block-beg   (match-beginning 0))
             (match-end   (match-end 0))
             (body-beg    (match-beginning 2))
             (body-end    (match-end 2))
             ;; Opening fence: positions [block-beg, block-beg+3) are the
             ;; three backticks.  The language tag (if any) follows them
             ;; and stays visible as a label.  The trailing newline of the
             ;; opening line sits just before body-beg.
             (open-bt-beg block-beg)
             (open-bt-end (+ block-beg 3))
             ;; Closing fence: from the start of its line through (and
             ;; including) its trailing newline, so the whole line vanishes.
             (close-line-beg (save-excursion
                               (goto-char match-end)
                               (line-beginning-position)))
             (close-line-end (if (and (< match-end (point-max))
                                      (eq (char-after match-end) ?\n))
                                 (1+ match-end)
                               match-end))
             ;; Block background must reach past the closing fence's newline
             ;; so `:extend t' fills the (now-hidden) line out to the edge.
             (block-end   close-line-end))
        ;; Base background for the whole block.
        (md-show--make-overlay block-beg block-end 'face 'md-show-code-block-face)
        ;; Hide the three opening backticks but leave the language tag visible.
        (md-show--make-overlay open-bt-beg open-bt-end 'invisible 'md-show)
        ;; Style whatever language tag is present (may be empty) with the
        ;; dim fence face so it reads as a label, not as content.
        (when (> (- body-beg 1 open-bt-end) 0) ; -1 for the trailing \n
          (md-show--make-overlay open-bt-end (1- body-beg)
                                 'face 'md-show-code-fence-face))
        ;; Hide the closing fence line entirely (including its newline).
        (md-show--make-overlay close-line-beg close-line-end 'invisible 'md-show)
        ;; Optional language-aware highlighting on the body only.
        (when (and lang (> (length lang) 0))
          (md-show--highlight-code-body body-beg body-end lang))
        (push (cons block-beg block-end) md-show--code-spans)))))

(defun md-show--fontify-headings (beg end)
  "Style ATX headings between BEG and END."
  (save-excursion
    (goto-char beg)
    (while (re-search-forward "^\\(#\\{1,6\\}\\) \\(.*\\)$" end t)
      (unless (md-show--in-code-block-p (match-beginning 0))
        (let* ((level (length (match-string 1)))
               (face (cond ((= level 1) 'md-show-h1-face)
                           ((= level 2) 'md-show-h2-face)
                           ((= level 3) 'md-show-h3-face)
                           (t           'md-show-h4-face))))
          (md-show--make-overlay (match-beginning 0) (match-end 0) 'face face)
          ;; Hide the leading "### " marker so headings read cleanly.
          (md-show--make-overlay (match-beginning 1) (1+ (match-end 1))
                                 'invisible 'md-show))))))

(defun md-show--fontify-quotes (beg end)
  "Style blockquote lines between BEG and END."
  (save-excursion
    (goto-char beg)
    (while (re-search-forward "^\\(>\\)\\(.*\\)$" end t)
      (unless (md-show--in-code-block-p (match-beginning 0))
        (md-show--make-overlay (match-beginning 1) (match-end 1)
                               'face 'md-show-quote-bar-face
                               'display "")
        (md-show--make-overlay (match-beginning 2) (match-end 2)
                               'face 'md-show-quote-face)))))

(defun md-show--fontify-inline-code (beg end)
  "Style `inline code` between BEG and END."
  (save-excursion
    (goto-char beg)
    (while (re-search-forward "`\\([^`\n]+\\)`" end t)
      (unless (md-show--in-code-block-p (match-beginning 0))
        (md-show--make-overlay (match-beginning 0) (match-end 0)
                               'face 'md-show-inline-code-face)
        ;; Dim the backticks themselves
        (md-show--make-overlay (match-beginning 0) (1+ (match-beginning 0))
                               'face 'md-show-marker-face
                               'display "")
        (md-show--make-overlay (1- (match-end 0)) (match-end 0)
                               'face 'md-show-marker-face
                               'display "")))))

(defun md-show--fontify-emphasis (beg end)
  "Style **bold** and *italic* spans between BEG and END."
  (save-excursion
    (goto-char beg)
    (while (re-search-forward "\\*\\*\\([^*\n]+\\)\\*\\*" end t)
      (unless (md-show--in-code-block-p (match-beginning 0))
        (md-show--make-overlay (match-beginning 1) (match-end 1)
                               'face 'md-show-bold-face)
        (md-show--make-overlay (match-beginning 0) (+ 2 (match-beginning 0))
                               'face 'md-show-marker-face)
        (md-show--make-overlay (- (match-end 0) 2) (match-end 0)
                               'face 'md-show-marker-face))))
  (save-excursion
    (goto-char beg)
    ;; *italic* but not part of ** (negative lookahead-ish via context check)
    (while (re-search-forward "\\(?:^\\|[^*]\\)\\(\\*\\([^*\n]+\\)\\*\\)\\(?:[^*]\\|$\\)" end t)
      (unless (md-show--in-code-block-p (match-beginning 1))
        (md-show--make-overlay (match-beginning 2) (match-end 2)
                               'face 'md-show-italic-face)
        (md-show--make-overlay (match-beginning 1) (1+ (match-beginning 1))
                               'face 'md-show-marker-face)
        (md-show--make-overlay (1- (match-end 1)) (match-end 1)
                               'face 'md-show-marker-face))
      ;; Step back so adjacent matches aren't skipped
      (goto-char (1- (match-end 0))))))

;; ---- Top-level fontify / clear ---------------------------------------------

(defun md-show--fontify-buffer ()
  "Apply all md-show overlays to the current buffer."
  (setq md-show--code-spans nil)
  (let ((beg (point-min)) (end (point-max))
        (inhibit-read-only t))
    (md-show--clear-overlays beg end)
    (md-show--fontify-code-blocks beg end)
    (md-show--fontify-headings    beg end)
    (md-show--fontify-quotes      beg end)
    (md-show--fontify-inline-code beg end)
    (md-show--fontify-emphasis    beg end)))

(defun md-show--unfontify-buffer ()
  "Remove every md-show overlay in the current buffer."
  (md-show--clear-overlays (point-min) (point-max))
  (setq md-show--code-spans nil))

;; ---- Wrapping --------------------------------------------------------------
;;
;; visual-line-mode soft-wraps without touching the file.  We pair it with
;; a window-local fill-column-indicator-free wrap width by setting
;; `fill-column' (used by `visual-line-fill-column-mode' if available) and
;; falling back to a window margin trick otherwise.

(defvar-local md-show--saved-visual-line-mode nil)
(defvar-local md-show--saved-truncate-lines nil)
(defvar-local md-show--saved-fill-column nil)
(defvar-local md-show--saved-buffer-invisibility-spec nil)

(defun md-show--enable-wrap ()
  "Turn on soft wrapping at `md-show-fill-column'."
  (setq md-show--saved-visual-line-mode visual-line-mode
        md-show--saved-truncate-lines truncate-lines
        md-show--saved-fill-column fill-column)
  (setq-local fill-column md-show-fill-column)
  (visual-line-mode 1)
  ;; If the user has visual-fill-column installed, use it for a hard 80-col
  ;; visual wrap; otherwise visual-line-mode wraps at the window edge, which
  ;; is usually fine.
  (when (fboundp 'visual-fill-column-mode)
    (visual-fill-column-mode 1)))

(defun md-show--disable-wrap ()
  "Restore prior wrapping settings."
  (when (fboundp 'visual-fill-column-mode)
    (visual-fill-column-mode -1))
  (visual-line-mode (if md-show--saved-visual-line-mode 1 -1))
  (setq truncate-lines md-show--saved-truncate-lines)
  (setq-local fill-column (or md-show--saved-fill-column 70)))

;; ---- Minor mode ------------------------------------------------------------

;;;###autoload
(define-minor-mode md-show-mode
  "Toggle visual rendering of the current markdown buffer.

When enabled, headings, blockquotes, code blocks, inline code, and
emphasis are styled via overlays, and lines are soft-wrapped at
`md-show-fill-column'.  The underlying file is never modified;
disabling the mode restores the raw view."
  :lighter " md-show"
  (if md-show-mode
      (progn
        (setq md-show--saved-buffer-invisibility-spec buffer-invisibility-spec)
        (add-to-invisibility-spec 'md-show)
        (md-show--enable-wrap)
        (md-show--fontify-buffer)
        (read-only-mode 1))
    (read-only-mode -1)
    (md-show--unfontify-buffer)
    (md-show--disable-wrap)
    (setq buffer-invisibility-spec md-show--saved-buffer-invisibility-spec)))

(provide 'md-show)
;;; md-show.el ends here
