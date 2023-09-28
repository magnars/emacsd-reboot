;; Projectile
;;
;; A project interaction library. It provides a nice set of features operating
;; on a project level without introducing external dependencies

(use-package projectile
  :ensure t
  :diminish projectile-mode
  :bind-keymap
  (("s-p" . projectile-command-map))

  :config
  (projectile-mode +1))


(provide 'setup-projectile)
