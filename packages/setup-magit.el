(use-package magit
  :ensure t
  :defer 2

  :custom
  (magit-section-initial-visibility-alist '((untracked . show)
                                            (unstaged . show)
                                            (unpushed . show)
                                            (unpulled . show)
                                            (stashes . show)))
  (magit-push-always-verify nil)
  (magit-revert-buffers 'silent)
  (magit-no-confirm '(stage-all-changes
                      unstage-all-changes))

  :bind (("C-x m" . magit-status)
         (:map magit-status-mode-map
               ("q" . magit-quit)))

  :config
  (wrap-fullscreen magit-status :magit-fullscreen)
  (wrap-fullscreen magit-init :magit-fullscreen))

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
