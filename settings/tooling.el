;; -*- lexical-binding: t; -*-
(use-package s)
(use-package dash)
(use-package diminish)

;; Shorthand for interactive lambdas
(defmacro λ (&rest body)
  `(lambda ()
     (interactive)
     ,@body))

(global-set-key (kbd "s-l") (λ (insert "\u03bb")))

;; Set up a keybinding for the very next command invocation
(defun one-shot-keybinding (key command)
  (set-temporary-overlay-map
   (let ((map (make-sparse-keymap)))
     (define-key map (kbd key) command)
     map)
   t))

;; Instrument a `command' to store the current window configuration in
;; `register' and then going fullscreen.
(defmacro wrap-fullscreen (command)
  `(defadvice ,command (around ,(intern (concat "wrap-" (symbol-name command) "-fullscreen")) activate)
     (let ((my/prev (list (current-window-configuration) (point-marker))))
       ad-do-it
       (delete-other-windows)
       (setq-local my/previous-window-configuration my/prev))))

(defvar my/previous-window-configuration nil)

;; No need to remind me about eldoc-mode all the time
(diminish 'eldoc-mode)

;; Insert pressed keybindings
(defun read-and-insert-key-sequence ()
  (interactive)
  (let ((keys []))
    (catch 'done
      (while t
        (let ((event (read-event (format "Press keys (ESC when done): %s" keys) nil)))
          (if (eq event 'escape)
              (throw 'done nil)
            (setq keys (vconcat keys (vector event)))))))
    (when (< 0 (length keys))
      (insert (format "(kbd \"%s\")" (key-description keys))))))

(add-hook 'emacs-lisp-mode-hook
          (lambda ()
            (local-set-key (kbd "C-M-S-s-k") 'read-and-insert-key-sequence)))

(defun my/find-free-modifier-families (&optional require-all)
  "Find keys where variations across modifiers are all unbound globally.
Display as a table with one column per modifier.
When REQUIRE-ALL is non-nil, only show keys free across ALL modifier
combinations."
  (interactive "P")
  (let* ((modifiers '("C-"      ; Control
                      "M-"      ; Meta
                      "s-"      ; Super
                      "H-"      ; Hyper
                      "C-S-"    ; Control-Shift
                      "C-M-"    ; Control-Meta
                      "C-s-"    ; Control-Super
                      "M-s-"    ; Meta-Super
                      "C-M-S-"  ; Control-Meta-Shift
                      "C-M-s-"  ; Control-Meta-Super
                      "C-S-s-"  ; Control-Shift-Super
                      "M-S-s-")); Meta-Shift-Super
         (col-width 7)
         (results '()))
    (dolist (key-char (append (number-sequence ?a ?z)
                              (number-sequence ?A ?Z)
                              (number-sequence ?0 ?9)
                              '(?, ?\; ?. ?: ?- ?_ ?@ ?* ?+ ?? ?< ?>
                                   ?' ?§ ?! ?\" ?# ?$ ?% ?& ?/ ?\( ?\) ?= ?´ ?`)))
      (let ((row '())
            (free-count 0))
        (dolist (mod modifiers)
          (let* ((key-str (format "%s%c" mod key-char))
                 (binding (key-binding (kbd key-str)))
                 (free-p (or (null binding) (eq binding 'undefined))))
            (when free-p (setq free-count (1+ free-count)))
            (push (cons mod free-p) row)))
        (when (or (not require-all) (= free-count (length modifiers)))
          (push (list (char-to-string key-char)
                      free-count
                      (nreverse row))
                results))))
    (with-output-to-temp-buffer "*Free Modifier Families*"
      ;; Header row
      (princ (format "%-5s" "Key"))
      (dolist (mod modifiers)
        (princ (format (format "%%-%ds" col-width)
                       (if (string= mod "") "(none)" mod))))
      (princ (format "%s\n" "Free"))
      ;; Separator
      (princ (format "%-5s" "----"))
      (dolist (_ modifiers)
        (princ (format (format "%%-%ds" col-width) "------")))
      (princ "----\n")
      ;; Data rows
      (dolist (entry (nreverse results))
        (princ (format "%-5s" (nth 0 entry)))
        (dolist (cell (nth 2 entry))
          (princ (format (format "%%-%ds" col-width)
                         (if (cdr cell) "·" "✗"))))
        (princ (format "%d/%d\n"
                       (nth 1 entry)
                       (length modifiers)))))))

(provide 'tooling)
