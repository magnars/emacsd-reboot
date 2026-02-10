;; -*- lexical-binding: t; -*-
(use-package org
  :ensure nil
  :defer t

  :custom
  (org-todo-keyword-faces
   '(("DONE" . (:foreground "green" :weight bold))))

  :bind (:map org-mode-map
              ("M-+" . org-shiftright)
              ("C-S-<down>" . org-metadown)
              ("C-S-<up>" . org-metaup)
              ("s-<return>" . org-metareturn-hook))

  :config
  (unbind-key "S-<up>" org-mode-map)
  (unbind-key "S-<down>" org-mode-map)
  (unbind-key "S-<left>" org-mode-map)
  (unbind-key "S-<right>" org-mode-map)

  ;; Update [/] cookies after a selection of commands
  (--each '(org-yank
            org-kill-line
            kill-whole-line
            duplicate-current-line-or-region)
    (advice-add it :after 'my/org-update-parent-cookie)))

(defun my/org-update-parent-cookie (&rest _)
  (when (equal major-mode 'org-mode)
    (save-excursion
      (ignore-errors
        (forward-char 1)
        (org-back-to-heading)
        (org-update-parent-todo-statistics)))
    (save-excursion
      (ignore-errors
        (forward-char -1)
        (org-back-to-heading)
        (org-update-parent-todo-statistics)))))

(provide 'setup-org-mode)
