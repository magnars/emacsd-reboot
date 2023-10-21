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
         (:map magit-status-mode-map
               ("q" . magit-quit)))

  :config
  (wrap-fullscreen magit-status)
  (wrap-fullscreen magit-init)

  ;; move cursor into position when entering commit message
  (add-hook 'git-commit-mode-hook 'my/magit-cursor-fix))

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

(defvar magit-pair-programming-partner nil)

(defun is-commit-message-buffer? ()
  (when (buffer-file-name)
    (equal (buffer-file-name-body) "COMMIT_EDITMSG")))

(defun magit-insert-pair-programming-co-author ()
  (unless (save-excursion
            (goto-char (point-min))
            (search-forward "Co-authored-by: " nil t))
    (apply #'git-commit-insert-header "Co-authored-by" magit-pair-programming-partner)))

(defun magit-enable-pair-programming-mode (details)
  (setq magit-pair-programming-partner details)
  (when (is-commit-message-buffer?)
    (magit-insert-pair-programming-co-author))
  (add-hook 'git-commit-setup-hook 'magit-insert-pair-programming-co-author)
  (message "Enabled pair programming with %s" (car details)))

(defun magit-disable-pair-programming-mode ()
  (setq magit-pair-programming-partner nil)
  (when (is-commit-message-buffer?)
    (save-excursion
      (goto-char (point-min))
      (flush-lines "Co-authored-by")))
  (remove-hook 'git-commit-setup-hook 'magit-insert-pair-programming-co-author)
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
  (if magit-pair-programming-partner
      (magit-disable-pair-programming-mode)
    (magit-enable-pair-programming-mode
     (my/git-commit-read-ident "Pair programming with"))))

(defun my/magit-cursor-fix ()
  (beginning-of-buffer)
  (when (looking-at "#")
    (while (looking-at "#")
      (forward-line))
    (forward-line)))

(provide 'setup-magit)
