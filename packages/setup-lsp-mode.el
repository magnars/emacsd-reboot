(use-package lsp-mode
  :hook ((clojure-mode . lsp)
         (clojurescript-mode-hook . lsp)
         (clojurec-mode-hook . lsp))

  :init
  (setq lsp-headerline-breadcrumb-enable nil)
  (setq lsp-enable-indentation nil) ;; use clojure-mode indentation
  (setq lsp-eldoc-enable-hover nil) ;; use CIDER eldoc
  ;; (setq lsp-enable-completion-at-point nil) ;; consider CIDER vs LSP?
  )

(provide 'setup-lsp-mode)
