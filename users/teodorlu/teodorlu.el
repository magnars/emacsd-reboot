(require 'mattilsynet)

;; Don't auto-wrap lines, I like one line per sentence
;; https://sive.rs/1s
(remove-hook 'markdown-mode-hook 'auto-fill-mode)

;; I can't get C-@ to work, use C-ø
(global-set-key (kbd "C-ø") 'er/expand-region)
(global-set-key (kbd "C-Ø") 'er/contract-region)

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
