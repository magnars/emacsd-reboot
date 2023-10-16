(use-package multiple-cursors
  :defer t
  :bind (("C-S-c C-S-c" . mc/edit-lines)
         ("C-S-c C-a" . mc/edit-beginnings-of-lines)
         ("C-S-c C-e" . mc/edit-ends-of-lines)
         ("C-æ" . mc/mark-next-like-this)
         ("M-æ" . mc/mark-all-dwim)
         ("C-å" . mc/mark-previous-like-this)
         ("C-æ" . mc/mark-next-like-this)
         ("C-Æ" . mc/mark-more-like-this-extended)
         ("M-å" . mc/mark-all-in-region)
         ("H-0" . mc/insert-numbers)))

(provide 'setup-multiple-cursors)
