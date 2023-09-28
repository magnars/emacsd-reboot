(use-package deadgrep
  :bind (("M-s s" . deadgrep)
         (:map deadgrep-mode-map
               ("q" . deadgrep-quit)))
  :config
  (wrap-fullscreen deadgrep :deadgrep-fullscreen))

(use-package wgrep 
  :bind ((:map deadgrep-mode-map
               ("e" . wgrep-change-to-wgrep-mode))))

(use-package wgrep-deadgrep 
  :hook ((deadgrep-finished . wgrep-deadgrep-setup)))

(defun deadgrep-quit ()
  (interactive)
  (quit-window)
  (jump-to-register :deadgrep-fullscreen))

(provide 'setup-deadgrep)
