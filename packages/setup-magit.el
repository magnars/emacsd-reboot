(use-package magit
  :defer 2

  :custom
  (magit-section-initial-visibility-alist '((untracked . show)
                                            (unstaged . show)
                                            (unpushed . show)
                                            (unpulled . show)
                                            (stashes . show)))
  (magit-diff-refine-hunk t)
  (magit-push-always-verify nil)
  (magit-revert-buffers 'silent)
  (magit-no-confirm '(stage-all-changes
                      unstage-all-changes))

  :bind (("C-x m" . magit-status)
         ("C-c p" . magit-toggle-pair-programming-mode)
         (:map magit-status-mode-map
               ("q" . magit-quit)))

  :config
  (wrap-fullscreen magit-status :magit-fullscreen)
  (wrap-fullscreen magit-init :magit-fullscreen)

  ;; move cursor into position when entering commit message
  (add-hook 'git-commit-mode-hook 'my/magit-cursor-fix)

  (require 'magit-header-patch))

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
  (jump-to-register :magit-fullscreen))

(defvar magit-pair-programming-partner nil)

(defun is-commit-message-buffer? ()
  (equal (buffer-file-name-body) "COMMIT_EDITMSG"))

(defun magit-insert-pair-programming-co-author ()
  (apply #'git-commit-insert-header "Co-authored-by" magit-pair-programming-partner))

(defun magit-enable-pair-programming-mode (details)
  (setq magit-pair-programming-partner details)
  (when (is-commit-message-buffer?)
    (magit-insert-pair-programming-co-author))
  (add-hook 'git-commit-setup-hook 'magit-insert-pair-programming-co-author))

(defun magit-disable-pair-programming-mode ()
  (setq magit-pair-programming-partner nil)
  (when (is-commit-message-buffer?)
    (save-excursion
      (goto-char (point-min))
      (flush-lines "Co-authored-by")))
  (remove-hook 'git-commit-setup-hook 'magit-insert-pair-programming-co-author))

(defun magit-toggle-pair-programming-mode ()
  (interactive)
  (if magit-pair-programming-partner
      (magit-disable-pair-programming-mode)
    (magit-enable-pair-programming-mode
     (git-commit-read-ident "Pair programming with"))))

(defun my/magit-cursor-fix ()
  (beginning-of-buffer)
  (when (looking-at "#")
    (while (looking-at "#")
      (forward-line))
    (forward-line)))

(provide 'setup-magit)
