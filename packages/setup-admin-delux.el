(dolist (pkg '(cider clojure-mode parseedn parseclj sesman spinner))
  (add-to-list 'straight-built-in-pseudo-packages pkg))

(use-package admin-delux
  :straight (admin-delux :type git :host codeberg :repo "boosja/admin-delux.el"))
