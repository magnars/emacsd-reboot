;; -*- lexical-binding: t; -*-

(defun ukenummer ()
  (interactive)
  (insert (format-time-string "%V")))

(provide 'norge)
