(defun my/fold-this-summary (beg end)
  (let* ((content (buffer-substring-no-properties beg end))
         (open (aref content 0))
         (close (pcase open
                  (?\( ?\))
                  (?\[ ?\])
                  (?\{ ?\})
                  (?\" ?\"))))
    (concat content " ,,," (string close))))

(defun my/fold-this-datastructure ()
  "Fold this, or if within datastructure, fold parent datastructure"
  (interactive)
  (save-excursion
    (let ((delimiters '(?\( ?\[ ?{ ?\")))
      (unless (memq (char-after) delimiters)
        (ignore-errors (backward-up-list 1 t nil)))
      (let ((beg (point))
            (line-end (line-end-position)))
        (forward-sexp)
        (fold-this beg (point)
                   (my/fold-this-summary beg line-end))))))

(use-package fold-this
  :bind (("C-<tab>" . my/fold-this-datastructure)
         ("C-S-<tab>" . fold-this-unfold-all))
  :config
  (define-key fold-this--overlay-keymap (kbd "C-<tab>") 'fold-this-unfold-at-point))
