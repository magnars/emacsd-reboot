(use-package lsp-mode
  :hook ((clojure-mode . lsp)
         (clojurescript-mode-hook . lsp)
         (clojurec-mode-hook . lsp))

  :init
  (setq lsp-headerline-breadcrumb-enable nil) ;; Don't need file path in my buffer
  (setq lsp-lens-enable nil) ;; Hide clutter (reference and test counts)
  (setq lsp-enable-indentation nil) ;; use clojure-mode indentation
  (setq lsp-eldoc-enable-hover nil) ;; use CIDER eldoc
  (setq lsp-modeline-code-actions-enable nil) ;; Don't clutter modeline
  (setq lsp-modeline-diagnostics-enable nil) ;; Don't clutter modeline, jeez
  (setq lsp-completion-provider :none) ;; Skip company-mode
  (setq lsp-enable-symbol-highlighting nil) ;; Don't highlight current symbol

  ;; To consider
  ;;
  ;; (setq lsp-enable-completion-at-point nil) ;; CIDER vs LSP?
  
  )

(provide 'setup-lsp-mode)
