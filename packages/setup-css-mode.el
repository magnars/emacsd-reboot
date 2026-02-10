;; -*- lexical-binding: t; -*-
(add-hook 'css-mode-hook 'lsp)

(setq css-fontify-colors nil)

(use-package lsp-tailwindcss
  :init
  (setq lsp-tailwindcss-add-on-mode t)
  (setq lsp-css-lint-unknown-at-rules "ignore") ;; ignore @tailwind and @apply (and all other such errors, oops)
  )

(require 'lsp-tailwindcss)

(provide 'setup-css-mode)
