;; -*- lexical-binding: t; -*-

(use-package amx
  :init
  ;; Hook amx into completing-read
  (setq amx-backend 'standard)

  ;; Don't do unnecessary polling of available commands
  (setq amx-auto-update-interval nil)

  :config
  ;; Automatically use amx to drive M-x behavior
  (amx-mode 1))

(provide 'setup-amx)
