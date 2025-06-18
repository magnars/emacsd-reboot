(use-package js-mode
  :ensure nil
  :defer t

  :custom
  (js-indent-level 2))

;; Add CommonJS and ES6 Module JS extensions to the js-mode list
(add-to-list 'auto-mode-alist '("\\.cjs$" . js-mode))
(add-to-list 'auto-mode-alist '("\\.mjs$" . js-mode))

(provide 'setup-js-mode)
