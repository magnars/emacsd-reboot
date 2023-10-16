;; Smartparens
;;
;; Smartparens is a minor mode for dealing with pairs in Emacs. Like a
;; paredit-lite for non-lisps.

(use-package smartparens
  :defer 2
  :init
  (require 'smartparens-config)
  (setq sp-autoescape-string-quote nil)
  (--each '(css-mode-hook
            js-mode-hook
            markdown-mode)
    (add-hook it 'turn-on-smartparens-mode)))

(provide 'setup-smartparens)
