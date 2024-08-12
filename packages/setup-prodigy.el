(use-package prodigy
  :defer t

  :bind (("C-c C-x C-p" . prodigy)
         (:map prodigy-mode-map
               ("q" . prodigy-quit)))

  :config
  (wrap-fullscreen prodigy))

(defun prodigy-quit ()
  (interactive)
  (let ((prev my/previous-window-configuration))
    (quit-window)
    (when prev (register-val-jump-to prev nil))))

(provide 'setup-prodigy)
