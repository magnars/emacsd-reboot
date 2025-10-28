;; theme
;; (use-package vscode-dark-plus-theme
;;   :ensure t
;;   :config
;;   (load-theme 'vscode-dark-plus t))

(set-face-attribute 'default nil :height 150)

;; Zig
(load
 (expand-file-name "users/teodorlu/zig.el" user-emacs-directory))

(global-set-key (kbd "M-<left>") 'beginning-of-buffer)
(global-set-key (kbd "M-<right>") 'end-of-buffer)
