;; theme
;; (use-package vscode-dark-plus-theme
;;   :ensure t
;;   :config
;;   (load-theme 'vscode-dark-plus t))

;; Horizontal scroll with trackpad / mouse
(setq mouse-wheel-tilt-scroll t)
(setq mouse-wheel-flip-direction t)

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

;;

(global-set-key (kbd "C-M-S-s-r C-M-S-s-k") 'learn-kbs-show-keybindings)

;; TODO:
;;
;; 🔘 comp to threading
;; ✅ C-c C-M-s w/ e->map
