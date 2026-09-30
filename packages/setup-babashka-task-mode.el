;; -*- lexical-binding: t; -*-
;;; Run babashka tasks interactively

;;; Commentary:
;; Provides an interactive menu for running babashka tasks,
;; inspired by setup-makefile-mode.el's Makefile target runner.
;; https://github.com/magnars/emacsd-reboot/blob/0d48f748be67264cceb12bec36e129b28ecdd305/packages/setup-makefile-mode.el

;;; Code:

(require 's)
(require 'dash)
(require 'projectile)

(defun babashka--error-summary (stderr)
  "Return the interesting part of bb's STDERR.
Drops dependency progress lines like \"Cloning:\" and \"Downloading:\",
and Java stack trace frames."
  (let ((lines (--remove (string-match-p
                          "^\\(Cloning:\\|Checking out:\\|Downloading:\\|at \\)" it)
                         (split-string stderr "\n" t "[ \t]+"))))
    (if lines
        (string-join lines "\n")
      "(no error output)")))

(defun babashka-find-tasks ()
  "Find all babashka tasks by running `bb tasks'.
Signal a `user-error' with bb's error output if `bb tasks' fails,
for example when bb.edn refers to a git sha that does not exist."
  (let* ((default-directory (projectile-project-root))
         (stderr-file (make-temp-file "bb-tasks-stderr"))
         exit-code output stderr)
    (unwind-protect
        (progn
          (with-temp-buffer
            (setq exit-code (process-file "bb" nil (list t stderr-file) nil "tasks"))
            (setq output (buffer-string)))
          (with-temp-buffer
            (insert-file-contents stderr-file)
            (setq stderr (buffer-string))))
      (delete-file stderr-file))
    (unless (eql exit-code 0)
      (user-error "`bb tasks' failed in %s:\n%s"
                  (shorten-path default-directory)
                  (babashka--error-summary stderr)))
    (when (and output (not (string-empty-p output)))
      (->> (split-string output "\n" t)
           ;; Task names come after the header "The following tasks are available:".
           ;; Without a header (e.g. "No tasks found."), there are no tasks.
           (--drop-while (not (string-match-p "^The following tasks" it)))
           (cdr)
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

(global-set-key (kbd "s-B") 'babashka-invoke-task)

(provide 'setup-babashka-task-mode)
