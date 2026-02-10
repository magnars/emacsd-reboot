;; -*- lexical-binding: t; -*-
(use-package expand-region
  :straight t
  :defer t
  :bind (("C-@" . er/expand-region)
         ("C-'" . er/expand-region)))

(provide 'setup-expand-region)
