;; -*- lexical-binding: t; -*-

;; N Λ N O theme
(use-package nano-theme
  :vc (:url "https://github.com/rougier/nano-theme")
  :init
  (setq nano-light-background "#fafafa"
        nano-light-highlight "#f5f7f8"))

;; N Λ N O modeline
(use-package nano-modeline
  :vc (:url "https://github.com/rougier/nano-modeline")
  :init
  ;; Disable the default modeline
  (setq-default mode-line-format nil)
  :config
  (defun my-default-nano-modeline (&optional default)
    "My nano modeline configuration."
    (funcall nano-modeline-position
             `((nano-modeline-buffer-status)
               (nano-modeline-buffer-name) " "
               (nano-modeline-git-info))
             `((nano-modeline-cursor-position)
               (nano-modeline-window-dedicated))
             default))
  (my-default-nano-modeline 1))

;; Font
(add-to-list 'default-frame-alist '(font . "Source Code Pro"))
