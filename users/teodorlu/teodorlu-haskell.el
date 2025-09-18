;; Rudimentary support for Haskell
;;
;; Requires an LSP binary:
;;
;;   brew install haskell-language-server

(use-package haskell-mode)
(use-package lsp-haskell)
(require 'lsp-mode)
(add-hook 'haskell-mode-hook #'lsp)
(add-hook 'haskell-literate-mode-hook #'lsp)
