(use-package deadgrep
  :ensure t
  :bind (("M-s s" . deadgrep)
         (:map deadgrep-mode-map
               ("q" . deadgrep-quit)))
  :config
  (wrap-fullscreen deadgrep :deadgrep-fullscreen))

(use-package wgrep 
  :ensure t
  :bind ((:map deadgrep-mode-map
               ("e" . wgrep-change-to-wgrep-mode))))

(use-package wgrep-deadgrep 
  :ensure t
  :hook ((deadgrep-finished . wgrep-deadgrep-setup)))

(defun deadgrep-quit ()
  (interactive)
  (quit-window)
  (jump-to-register :deadgrep-fullscreen)
  (set-register :deadgrep-fullscreen nil))

(provide 'setup-deadgrep)
