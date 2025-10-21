;; theme
;; (use-package vscode-dark-plus-theme
;;   :ensure t
;;   :config
;;   (load-theme 'vscode-dark-plus t))

(set-face-attribute 'default nil :height 150)

;; Zig
(load
 (expand-file-name "users/teodorlu/zig.el" user-emacs-directory))

;; Evil
(unless (package-installed-p 'evil)
  (package-install 'evil))

(require 'evil)
(defvar is-evil nil)
(defun choose-to-be-good-or-evil ()
  (interactive)
  (setq is-evil (not is-evil))
  (if is-evil
      (progn
       (evil-mode 1)
       (message "You've broken bad!"))
    (progn
     (evil-mode 0)
     (message "Oh, goody 2-shoes, you!"))))
(global-set-key (kbd "C-c e") 'choose-to-be-good-or-evil)
