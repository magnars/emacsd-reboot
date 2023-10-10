(use-package org-mode
  :ensure nil
  :defer 2

  :bind (:map org-mode-map
              ("M-<up>" . org-shiftright)
              ("M-<down>" . org-shiftleft)
              ("C-S-<down>" . org-metadown)
              ("C-S-<up>" . org-metaup)
              ("s-<up>" . org-shiftup)
              ("s-<down>" . org-shiftdown))

  :init
  (unbind-key "S-<up>" org-mode-map)
  (unbind-key "S-<down>" org-mode-map)
  (unbind-key "S-<left>" org-mode-map)
  (unbind-key "S-<right>" org-mode-map))
