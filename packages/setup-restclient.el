;; -*- lexical-binding: t; -*-
(use-package restclient
  :defer t)

(add-to-list 'auto-mode-alist '("\\.restclient$" . restclient-mode))
(add-hook 'restclient-mode-hook 'turn-on-smartparens-mode)

(provide 'setup-restclient)
