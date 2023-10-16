(use-package markdown
  :ensure nil
  :defer t

  :init
  (add-hook 'markdown-mode-hook 'auto-fill-mode))

(provide 'setup-markdown-mode)
