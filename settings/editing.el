;; -*- lexical-binding: t; -*-
;; Manipulating the contents of a buffer

;; Killing words backwards
(global-set-key (kbd "C-w") 'kill-region-or-backward-word)
(global-set-key (kbd "M-h") 'kill-region-or-backward-word) ;; matches C-h

;; Duplicate region
(global-set-key (kbd "C-c d") 'duplicate-current-line-or-region)

;; Increase and decrease number at point
(global-set-key (kbd "C-+") 'inc-number-at-point)
(global-set-key (kbd "C-?") 'dec-number-at-point)

;; Clean up whitespace
(global-set-key (kbd "C-c n") 'cleanup-buffer)

;; Copy to end of current line if no region
(global-set-key (kbd "M-w") 'copy-region-or-current-line)

;; Completion at point
(global-set-key (kbd "C-,") 'completion-at-point)

;; Use shell-like backspace C-h, rebind help to F1
(define-key key-translation-map [?\C-h] [?\C-?])
(global-set-key (kbd "<f1>") 'help-command)

;; Revert entire buffer without any fuss
(global-set-key (kbd "M-<escape>") (λ (revert-buffer t t)))

;; Join lines with ease
(global-set-key (kbd "M-j") (λ (join-line -1)))

;; Query replace regex key binding
(global-set-key (kbd "M-&") 'query-replace-regexp)

;; Delete blank lines
(global-set-key (kbd "C-c C-<return>") 'delete-blank-lines)

;; Eval emacs-lisp expressions anywhere.
(global-set-key (kbd "C-c C-e") 'eval-and-replace)
(global-set-key (kbd "M-s-e") 'eval-and-replace)

;; Clever newlines
(global-set-key (kbd "C-o") 'open-line-and-indent)
(global-set-key (kbd "<C-return>") 'open-line-below)
(global-set-key (kbd "<C-S-return>") 'open-line-above)
(global-set-key (kbd "<M-return>") 'new-line-dwim)

;; Move whole lines
(global-set-key (kbd "<C-S-down>") 'move-text-down)
(global-set-key (kbd "<C-S-up>") 'move-text-up)

;; Sorting lines alphabetically
(global-set-key (kbd "M-s l") 'sort-lines)

;; Display and edit occurances of regexp in buffer
(global-set-key (kbd "C-c o") 'occur)

;;;; Implementations

(use-package move-text)

(defun duplicate-region (&optional num start end)
  "Duplicates the region bounded by START and END NUM times.
If no START and END is provided, the current region-beginning and
region-end is used."
  (interactive "p")
  (save-excursion
    (let* ((start (or start (region-beginning)))
           (end (or end (region-end)))
           (region (buffer-substring start end)))
      (goto-char end)
      (dotimes (i num)
        (insert region)))))

(defun paredit-duplicate-current-line ()
  (back-to-indentation)
  (let (kill-ring kill-ring-yank-pointer)
    (paredit-kill)
    (yank)
    (newline-and-indent)
    (yank)))

(defun duplicate-current-line (&optional num)
  "Duplicate the current line NUM times."
  (interactive "p")
  (if (bound-and-true-p paredit-mode)
      (paredit-duplicate-current-line)
    (save-excursion
      (when (eq (point-at-eol) (point-max))
        (goto-char (point-max))
        (newline)
        (forward-char -1))
      (duplicate-region num (point-at-bol) (1+ (point-at-eol))))))

(defun duplicate-current-line-or-region (arg)
  "Duplicates the current line or region ARG times.
If there's no region, the current line will be duplicated."
  (interactive "p")
  (if (region-active-p)
      (let ((beg (region-beginning))
            (end (region-end)))
        (duplicate-region arg beg end)
        (one-shot-keybinding "d" (λ (duplicate-region 1 beg end))))
    (duplicate-current-line arg)
    (one-shot-keybinding "d" 'duplicate-current-line)))

(defun incs (s &optional num)
  (let* ((inc (or num 1))
         (new-number (number-to-string (+ inc (string-to-number s))))
         (zero-padded? (s-starts-with? "0" s)))
    (if zero-padded?
        (s-pad-left (length s) "0" new-number)
      new-number)))

(defun goto-closest-number ()
  (interactive)
  (let ((closest-behind (save-excursion (search-backward-regexp "[0-9]" nil t)))
        (closest-ahead (save-excursion (search-forward-regexp "[0-9]" nil t))))
    (push-mark)
    (goto-char
     (cond
      ((and (not closest-ahead) (not closest-behind)) (error "No numbers in buffer"))
      ((and closest-ahead (not closest-behind)) closest-ahead)
      ((and closest-behind (not closest-ahead)) closest-behind)
      ((> (- closest-ahead (point)) (- (point) closest-behind)) closest-behind)
      ((> (- (point) closest-behind) (- closest-ahead (point))) closest-ahead)
      :else closest-ahead))))

(defun inc-number-at-point (arg)
  (interactive "p")
  (unless (or (looking-at "[0-9]")
              (looking-back "[0-9]"))
    (goto-closest-number))
  (save-excursion
    (while (looking-back "[0-9]")
      (forward-char -1))
    (re-search-forward "[0-9]+" nil)
    (replace-match (incs (match-string 0) arg) nil nil)))

(defun dec-number-at-point (arg)
  (interactive "p")
  (inc-number-at-point (- arg)))

(defun cleanup-buffer ()
  "Perform a bunch of operations on the whitespace content of a buffer.
Including indent-buffer, which should not be called automatically on save."
  (interactive)
  (untabify (point-min) (point-max))
  (delete-trailing-whitespace)
  (indent-region (point-min) (point-max)))

(defun kill-region-or-backward-word ()
  (interactive)
  (if (region-active-p)
      (kill-region (region-beginning) (region-end))
    (backward-kill-word 1)))

(defun copy-to-end-of-line ()
  (interactive)
  (kill-ring-save (point)
                  (line-end-position))
  (message "Copied to end of line"))

(defun copy-region-or-current-line ()
  (interactive)
  (if (region-active-p)
      (kill-ring-save (region-beginning) (region-end))
    (copy-to-end-of-line)))

(defun eval-and-replace ()
  "Replace the preceding sexp with its value."
  (interactive)
  (backward-kill-sexp)
  (condition-case nil
      (prin1 (eval (read (current-kill 0)))
             (current-buffer))
    (error (message "Invalid expression")
           (insert (current-kill 0)))))

(defun open-line-and-indent ()
  (interactive)
  (newline-and-indent)
  (end-of-line 0)
  (indent-for-tab-command))

(defun open-line-below ()
  (interactive)
  (end-of-line)
  (newline)
  (indent-for-tab-command))

(defun open-line-above ()
  (interactive)
  (beginning-of-line)
  (newline)
  (forward-line -1)
  (indent-for-tab-command))

(defun new-line-dwim ()
  (interactive)
  (let ((break-open-pair (or (and (looking-back "{" 1) (looking-at "}"))
                             (and (looking-back ">" 1) (looking-at "<"))
                             (and (looking-back "(" 1) (looking-at ")"))
                             (and (looking-back "\\[" 1) (looking-at "\\]")))))
    (newline)
    (when break-open-pair
      (save-excursion
        (newline)
        (indent-for-tab-command)))
    (indent-for-tab-command)))

(defun editing--is-comment? ()
  (or (eq 'comment (syntax-ppss-context (syntax-ppss)))
      (memq (get-text-property (point) 'face)
            '(font-lock-comment-face
              font-lock-comment-delimiter-face))))

(defun editing--is-string? ()
  (or (eq 'string (syntax-ppss-context (syntax-ppss)))
      (memq (get-text-property (point) 'face)
            '(font-lock-string-face
              font-lock-doc-face))))

(defun remove-all-commas ()
  "Remove all commas from the current buffer."
  (interactive)
  (save-excursion
    (goto-char (point-min))
    (while (search-forward "," nil t)
      (unless (or (editing--is-string?)
                  (editing--is-comment?))
        (replace-match "" nil t)))))

(provide 'editing)
