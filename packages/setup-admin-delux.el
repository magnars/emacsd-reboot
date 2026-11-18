(use-package admin-delux
  :ensure t
  :after (cider)
  :bind (:map cider-mode-map
              ("s-a" . admin-delux-run)))
