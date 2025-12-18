(use-package magit
  :defer t

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
         ("C-c P" . magit-add-pair-programming-partner)
         (:map git-commit-mode-map
               ("C-c C-p" . magit-add-pair-programming-partner))
         (:map magit-status-mode-map
               ("q" . magit-quit)))

  :config
  (wrap-fullscreen magit-status)
  (wrap-fullscreen magit-init)
  (remove-hook 'magit-status-sections-hook 'magit-insert-tags-header)
  (remove-hook 'magit-status-sections-hook 'magit-insert-status-headers)
  ;; Removing these makes magit a little annoying to use
  ;; (remove-hook 'magit-status-sections-hook 'magit-insert-unpushed-to-pushremote)
  ;; (remove-hook 'magit-status-sections-hook 'magit-insert-unpulled-from-pushremote)
  ;; (remove-hook 'magit-status-sections-hook 'magit-insert-unpulled-from-upstream)
  ;; (remove-hook 'magit-status-sections-hook 'magit-insert-unpushed-to-upstream-or-recent)

  ;; move cursor into position when entering commit message
  (add-hook 'git-commit-mode-hook 'my/magit-cursor-fix)
  (add-hook 'magit-status-sections-hook 'magit-insert-unpulled-from-upstream))

(use-package git-timemachine
  :defer t
  :bind (("C-x v t" . git-timemachine)))

(defun b-a-r-k-commit ()
  (interactive)
  (browse-at-remote-kill)
  (message "Remote was killed 💀"))

(defun b-a-r-k-branch ()
  (interactive)
  (setq browse-at-remote-prefer-symbolic t)
  (browse-at-remote-kill)
  (setq browse-at-remote-prefer-symbolic nil)
  (message "Remote was killed 🪓🪵"))

(use-package browse-at-remote
  :defer t
  :custom
  (browse-at-remote-prefer-symbolic nil)
  :bind (("C-x v w" . b-a-r-k-commit)
         ("C-x v W" . b-a-r-k-branch)))

(defun kill-magit-buffers ()
  (let ((current (current-buffer)))
    (dolist (buf (magit-mode-get-buffers))
      (unless (eq buf current)
        (kill-buffer buf)))))

(defun magit-quit ()
  "Like magit-mode-bury-buffer, but also restores the window
configuration stored by magit-status-fullscreen"
  (interactive)
  (let ((prev my/previous-window-configuration))
    (kill-magit-buffers)
    (funcall magit-bury-buffer-function 'kill-buffer)
    (when prev (register-val-jump-to prev nil))))

(defvar magit-pair-programming-partners nil)

(defun is-commit-message-buffer? ()
  (when (buffer-file-name)
    (equal (buffer-file-name-body) "COMMIT_EDITMSG")))

(defun magit-insert-pair-programming-co-authors ()
  (dolist (partner magit-pair-programming-partners)
    (unless (save-excursion
              (goto-char (point-min))
              (search-forward (concat "Co-authored-by: " (car partner)) nil t))
      (apply #'git-commit-insert-header "Co-authored-by" partner))))

(defun magit-enable-pair-programming-mode (details)
  (add-to-list 'magit-pair-programming-partners details)
  (when (is-commit-message-buffer?)
    (magit-insert-pair-programming-co-authors))
  (add-hook 'git-commit-setup-hook 'magit-insert-pair-programming-co-authors)
  (message "Enabled pair programming with %s" (car details)))

(defun magit-disable-pair-programming-mode ()
  (setq magit-pair-programming-partners nil)
  (when (is-commit-message-buffer?)
    (save-excursion
      (goto-char (point-min))
      (flush-lines "Co-authored-by")))
  (remove-hook 'git-commit-setup-hook 'magit-insert-pair-programming-co-authors)
  (message "Disabled pair programming"))

(defvar my/pair-programming-usual-suspects nil)
(defvar my/pair-programming-myself nil)

(defun my/remove-same-name (a b)
  (let ((names (--map (car (s-slice-at "<" it)) b)))
    (-remove (lambda (elem)
               (--any (s-starts-with? it elem) names))
             a)))

(defun my/git-commit-read-ident-candidates ()
  (let* ((users-from-git-history (my/remove-same-name (delete-dups
                                                       (magit-git-lines "log" "-n9999" "--format=%aN <%ae>"))
                                                      my/pair-programming-myself))
         (usual-suspects-in-history (-intersection users-from-git-history
                                                   my/pair-programming-usual-suspects))
         (missing-usual-suspects (my/remove-same-name my/pair-programming-usual-suspects
                                                      usual-suspects-in-history)))
    (delete-dups
     (-concat usual-suspects-in-history
              missing-usual-suspects
              users-from-git-history))))

(defun my/git-commit-read-ident (prompt)
  (let ((str (magit-completing-read
              prompt
              (my/git-commit-read-ident-candidates)
              nil nil nil 'git-commit-read-ident-history)))
    (save-match-data
      (if (string-match "\\`\\([^<]+\\) *<\\([^>]+\\)>\\'" str)
          (list (save-match-data (string-trim (match-string 1 str)))
                (string-trim (match-string 2 str)))
        (user-error "Invalid input")))))

(defun magit-toggle-pair-programming-mode ()
  (interactive)
  (if magit-pair-programming-partners
      (magit-disable-pair-programming-mode)
    (magit-enable-pair-programming-mode
     (my/git-commit-read-ident "Pair programming with"))))

(defun magit-add-pair-programming-partner ()
  (interactive)
  (magit-enable-pair-programming-mode
   (my/git-commit-read-ident "Pair programming with")))

(defun my/magit-cursor-fix ()
  (beginning-of-buffer)
  (when (looking-at "#")
    (while (looking-at "#")
      (forward-line))
    (forward-line)))

(provide 'setup-magit)
