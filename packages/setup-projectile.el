;; Projectile
;;
;; A project interaction library. It provides a nice set of features operating
;; on a project level without introducing external dependencies

(use-package projectile
  :diminish projectile-mode
  :commands (projectile-switch-project-by-name)

  :bind-keymap
  (("s-p" . projectile-command-map))

  :bind
  ("C-x p p" . projectile-switch-project)
  ("C-x p e" . my/projectile-switch-project-to-emacs)

  :config
  (projectile-mode +1)
  (define-key projectile-command-map (kbd "s-p") #'projectile-switch-project)

  (setq projectile-ignored-project-function 'my/ignore-project?)

  (require 'setup-perspective)
  (setq projectile-switch-project-action 'switch-perspective+find-file))

(defun current-project-name ()
  (cadr (reverse (split-string (projectile-project-root) "/"))))

(defun switch-perspective+find-file ()
  (with-perspective (current-project-name)
    (projectile-find-file)))

(defun my/ignore-project? (file-name)
  (s-contains? ".gitlibs" file-name))

(defun my/projectile-switch-project-to-emacs ()
  (interactive)
  (projectile-switch-project-by-name "~/.emacs.d/"))

(provide 'setup-projectile)
