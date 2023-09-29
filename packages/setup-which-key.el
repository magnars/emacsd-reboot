;; which-key
;;
;; a minor mode for Emacs that displays the key bindings following your
;; currently entered incomplete command (a prefix) in a popup.

(use-package which-key
  :init
  ;; Wait 3 seconds before opening normally.
  (setq which-key-idle-delay 3)

  ;; Allow F1 to trigger which-key before it is done automatically
  (setq which-key-show-early-on-C-h t)

  ;; Refresh quickly once it is open
  (setq which-key-idle-secondary-delay 0.05)

  :config
  (which-key-mode)

  :diminish which-key-mode)

(provide 'setup-which-key)
