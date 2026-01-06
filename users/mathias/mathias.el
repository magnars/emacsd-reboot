;; theme
;; (use-package vscode-dark-plus-theme
;;   :ensure t
;;   :config
;;   (load-theme 'vscode-dark-plus t))

;; Zig
(load
 (expand-file-name "users/teodorlu/zig.el" user-emacs-directory))

(global-set-key (kbd "M-<left>") 'beginning-of-buffer)
(global-set-key (kbd "M-<right>") 'end-of-buffer)

;; Always split to the right
(setq split-height-threshold nil)

(use-package helpful)

(global-set-key (kbd "<f1> f") #'helpful-callable)
(global-set-key (kbd "<f1> v") #'helpful-variable)
(global-set-key (kbd "<f1> k") #'helpful-key)
(global-set-key (kbd "<f1> x") #'helpful-command)

(provide 'setup-helpful)
