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
                   (buffer-substring-no-properties beg line-end))))))

(use-package fold-this
  :bind (("C-<tab>" . my/fold-this-datastructure))
  :config
  (define-key fold-this--overlay-keymap (kbd "C-<tab>") 'fold-this-unfold-at-point))
