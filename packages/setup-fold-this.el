(defun my/fold-this-datastructure ()
  "Fold this, or if within datastructure, fold parent datastructure"
  (interactive)
  (save-excursion
    (let ((delimiters '(?\( ?\[ ?{)))
      (when (not (memq (char-after) delimiters))
        (condition-case nil
            (progn
              (backward-up-list 1))
          (error nil)))
      (set-mark (point))
      (forward-sexp)
      (fold-this (mark) (point)))))

(use-package fold-this
  :bind (("C-<tab>" . my/fold-this-datastructure)))
