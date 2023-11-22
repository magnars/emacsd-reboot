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

(prodigy-define-service
  :name "www.parens-of-the-dead.com"
  :port 3334
  :command "lein"
  :args '("ring" "server-headless")
  :cwd "~/projects/www.parens-of-the-dead.com"
  :stop-signal 'sigkill
  :kill-process-buffer-on-stop t)

(provide 'setup-prodigy)
