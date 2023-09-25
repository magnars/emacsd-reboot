(use-package deadgrep
  :ensure t
  :commands (deadgrep deadgrep--read-search-term)
  :bind (("M-s s" . deadgrep-fullscreen)
         (:map deadgrep-mode-map
               ("q" . deadgrep-quit))))

(use-package wgrep 
  :ensure t
  :bind ((:map deadgrep-mode-map
               ("e" . wgrep-change-to-wgrep-mode))))

(use-package wgrep-deadgrep 
  :ensure t
  :hook ((deadgrep-finished . wgrep-deadgrep-setup)))

(defun deadgrep-fullscreen (search-term)
  (interactive (list (deadgrep--read-search-term)))
  (window-configuration-to-register :deadgrep-fullscreen)
  (deadgrep search-term)
  (delete-other-windows))

(defun deadgrep-quit ()
  (interactive)
  (quit-window)
  (jump-to-register :deadgrep-fullscreen))

(provide 'setup-deadgrep)
