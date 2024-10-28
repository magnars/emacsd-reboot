(require 'mattilsynet)

(defun teodorlu-magnar-emacs-cheat-sheet ()
  (interactive)
  (find-file (expand-file-name "~/.emacs.d/README.md")))

(defun teodorlu-today ()
  (interactive)
  (insert (format-time-string "%Y-%m-%d")))

(defun teodorlu-insert-en-dash ()
  (interactive)
  (insert "-"))

(defun teodorlu-insert-em-dash ()
  (interactive)
  (insert "—"))
