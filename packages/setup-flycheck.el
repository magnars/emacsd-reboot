(use-package flycheck
  :config (setq-default flycheck-disabled-checkers '(emacs-lisp-checkdoc emacs-lisp))
  :init (global-flycheck-mode)
  :diminish flycheck-mode)

(provide 'setup-flycheck)
