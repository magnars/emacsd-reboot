;; -*- lexical-binding: t; -*-
(defun babashka-tasks ()
  (s-lines (s-trim (shell-command-to-string (concat "bb -e " (shell-quote-argument "(run! println (keys (:tasks (read-string (slurp \"bb.edn\")))))"))))))

(defun babashka-run-task ()
  (interactive)
  (let* ((default-directory (projectile-project-root))
         (task (completing-read "Select task > " (babashka-tasks))))
    (async-shell-command (concat "bb " (shell-quote-argument task)))
    (shell-command (concat "bb " (shell-quote-argument task)))))

(provide 'setup-babashka)
