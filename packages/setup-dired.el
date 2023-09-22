(with-eval-after-load 'dired
  (require 'dired-x)

  ;; Make dired less verbose, toggle with (
  (add-hook 'dired-mode-hook 'dired-hide-details-mode)

  ;; Move files between split panes
  (setq dired-dwim-target t)

  ;; C-a is nicer in dired if it moves back to start of files
  (define-key dired-mode-map (kbd "C-a") 'dired-back-to-start-of-files)

  ;; Delete files with k
  (define-key dired-mode-map (kbd "k") 'dired-do-delete))

;; Nicer navigation also in writeable dired
(with-eval-after-load 'wdired
  (define-key wdired-mode-map (kbd "C-a") 'dired-back-to-start-of-files)
  (define-key wdired-mode-map (vector 'remap 'beginning-of-buffer) 'dired-back-to-top)
  (define-key wdired-mode-map (vector 'remap 'end-of-buffer) 'dired-jump-to-bottom))

(defun dired-back-to-start-of-files ()
  (interactive)
  (backward-char (- (current-column) 2)))

(provide 'setup-dired)
