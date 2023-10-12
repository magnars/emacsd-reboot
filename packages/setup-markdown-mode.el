(use-package markdown
  :ensure nil
  :defer 2

  :init
  (add-hook 'markdown-mode-hook 'auto-fill-mode))

(provide 'setup-markdown-mode)
