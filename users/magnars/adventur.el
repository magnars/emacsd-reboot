;; Set up Adventur Delux editing mode

(straight-use-package
 '(adventur-mode :type git :host github :protocol ssh :repo "magnars/adventur-mode"))

(autoload 'adventur-mode "adventur-mode")
(add-to-list 'auto-mode-alist '("\\.adv$" . adventur-mode))

(global-set-key (kbd "C-x p n") (λ (projectile-switch-project-by-name "~/projects/no-adventur")))
(global-set-key (kbd "C-x p m") (λ (require 'adventur-mode)
                                   (with-perspective "eventyr" (find-file "~/projects/eventyr/master/notat.org"))))

(setq magit-bind-magit-project-status nil)
