;;; setup-babashka-task-mode.el --- Run babashka tasks interactively -*- lexical-binding: t; -*-

;;; Commentary:
;; Provides an interactive menu for running babashka tasks,
;; inspired by setup-makefile-mode.el's Makefile target runner.

;;; Code:

(require 's)
(require 'dash)
(require 'projectile)

(defun babashka-find-tasks ()
  "Find all babashka tasks by running `bb tasks'."
  (let* ((default-directory (projectile-project-root))
         (output (shell-command-to-string "bb tasks 2>/dev/null")))
    (when (and output (not (string-empty-p output)))
      (->> (split-string output "\n" t)
           ;; Skip header line "The following tasks are available:"
           (--filter (not (string-match-p "^The following tasks" it)))
           ;; Parse task names (first word of each line)
           (--map (car (split-string (string-trim it) " " t)))
           ;; Filter out nil/empty
           (--filter (and it (not (string-empty-p it))))))))

(defvar babashka--previous-window-configuration nil)
(defvar babashka--previous-task nil)

(defun babashka-invoke-task (&optional repeat?)
  "Invoke a babashka task interactively.
With REPEAT? non-nil, re-run the previous task without prompting."
  (interactive)
  (let* ((project-root (projectile-project-root))
         (short-dir (shorten-path project-root))
         (default-directory project-root)
         (bb-buffer-name (concat "*Babashka " (projectile-project-name) "*"))
         (prev (if (get-buffer bb-buffer-name)
                   (with-current-buffer bb-buffer-name
                     babashka--previous-window-configuration)
                 (list (current-window-configuration) (point-marker))))
         (tasks (babashka-find-tasks))
         (task (cond
                ;; Repeat previous task
                ((and repeat? (get-buffer bb-buffer-name))
                 (with-current-buffer bb-buffer-name
                   babashka--previous-task))
                ;; No tasks found
                ((null tasks)
                 (user-error "No babashka tasks found in %s" short-dir))
                ;; Prompt user to select
                (t (completing-read (format "bb task in %s: " short-dir)
                                    (--map (concat "bb " it) tasks))))))
    (when task
      (async-shell-command task bb-buffer-name)
      (unless (s-equals? (buffer-name) bb-buffer-name)
        (switch-to-buffer-other-window bb-buffer-name))
      (setq-local babashka--previous-window-configuration prev)
      (setq-local babashka--previous-task task)
      (read-only-mode)
      (local-set-key (kbd "b") 'babashka-invoke-task)
      (local-set-key (kbd "g") (lambda () (interactive) (babashka-invoke-task t)))
      (local-set-key (kbd "q") (lambda ()
                                 (interactive)
                                 (let ((conf babashka--previous-window-configuration))
                                   (kill-buffer)
                                   (when conf (register-val-jump-to conf nil))))))))

(global-set-key (kbd "s-b") 'babashka-invoke-task)

(provide 'setup-babashka-task-mode)

;;; setup-babashka-task-mode.el ends here
