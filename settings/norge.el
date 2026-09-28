;; -*- lexical-binding: t; -*-

(defun ukenummer ()
  (interactive)
  (insert (s-trim (shell-command-to-string "date +%V"))))

(provide 'norge)
