(use-package lsp-mode
  :hook ((clojure-mode . lsp))

  :init
  (setq lsp-headerline-breadcrumb-enable nil))

(provide 'setup-lsp-mode)
