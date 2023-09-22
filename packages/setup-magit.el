(use-package magit
  :ensure t
  :config
  (global-set-key (kbd "C-x m") 'magit-status-fullscreen)
  (define-key magit-status-mode-map (kbd "q") 'magit-quit))

(defun magit-status-fullscreen ()
  (interactive)
  (unless (get-register :magit-fullscreen)
    (window-configuration-to-register :magit-fullscreen))
  (magit-status)
  (delete-other-windows))

(defun kill-magit-buffers ()
  (let ((current (current-buffer)))
    (dolist (buf (magit-mode-get-buffers))
      (unless (eq buf current)
        (kill-buffer buf)))))

(defun magit-quit ()
  "Like magit-mode-bury-buffer, but also restores the window
configuration stored by magit-status-fullscreen"
  (interactive)
  (kill-magit-buffers)
  (funcall magit-bury-buffer-function 'kill-buffer)
  (jump-to-register :magit-fullscreen)
  (set-register :magit-fullscreen nil))

(provide 'setup-magit)
