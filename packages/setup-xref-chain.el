(use-package xref-chain
  :straight (xref-chain :type git :host codeberg :repo "boosja/xref-chain.el")
  :config
  (defun chain-cider-lsp-backend ()
    '(chain cider xref-lsp))
  (defun my/chain-cider-lsp ()
    (add-hook 'xref-backend-functions #'chain-cider-lsp-backend -91 t))
  (add-hook 'cider-mode-hook #'my/chain-cider-lsp)
  (add-hook 'lsp-mode-hook #'my/chain-cider-lsp))
