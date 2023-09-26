(use-package flycheck
  :ensure t
  :config (setq-default flycheck-disabled-checkers '(emacs-lisp-checkdoc emacs-lisp))
  :init (global-flycheck-mode))

(provide 'setup-flycheck)
