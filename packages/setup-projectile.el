;; Projectile
;;
;; A project interaction library. It provides a nice set of features operating
;; on a project level without introducing external dependencies

(use-package projectile
  :diminish projectile-mode
  :bind-keymap
  (("s-p" . projectile-command-map))

  :config
  (projectile-mode +1)

  (require 'setup-perspective)
  (setq projectile-switch-project-action 'switch-perspective+find-file))

(defun current-project-name ()
  (cadr (reverse (split-string (projectile-project-root) "/"))))

(defun switch-perspective+find-file ()
  (with-perspective (current-project-name)
    (projectile-find-file)))

(provide 'setup-projectile)
