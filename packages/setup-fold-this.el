(defun my/fold-this-datastructure ()
  "Fold this, or if within datastructure, fold parent datastructure"
  (interactive)
  (save-excursion
    (let ((delimiters '(?\( ?\[ ?{)))
      (unless (memq (char-after) delimiters)
        (ignore-errors (backward-up-list 1)))
      (set-mark (point))
      (forward-sexp)
      (fold-this (mark) (point)))))

(use-package fold-this
  :bind (("C-<tab>" . my/fold-this-datastructure))
  :config
  (define-key fold-this--overlay-keymap (kbd "C-<tab>") 'fold-this-unfold-at-point))
