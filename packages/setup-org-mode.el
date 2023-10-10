(use-package org
  :ensure nil
  :defer 2

  :custom
  (org-todo-keyword-faces
   '(("DONE" . (:foreground "green" :weight bold))))

  :bind (:map org-mode-map
              ("M-+" . org-shiftright)
              ("C-S-<down>" . org-metadown)
              ("C-S-<up>" . org-metaup))

  :config
  (unbind-key "S-<up>" org-mode-map)
  (unbind-key "S-<down>" org-mode-map)
  (unbind-key "S-<left>" org-mode-map)
  (unbind-key "S-<right>" org-mode-map))

(provide 'setup-org-mode)
