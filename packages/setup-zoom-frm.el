;; Zoom FRM
;;
;; Change text size for entire frame, not just a single buffer, lol

(straight-use-package
 '(zoom-frm :type git :host github :repo "emacsmirror/zoom-frm"))

(global-set-key (kbd "C-x C-+") 'zoom-frm-in)
(global-set-key (kbd "C-x C--") 'zoom-frm-out)
(global-set-key (kbd "C-x C-0") 'zoom-frm-unzoom)

;; DO NOT TEXT SCALE ADJUST!!!
(global-unset-key (kbd "s-+"))
(global-unset-key (kbd "s--"))
(global-unset-key (kbd "s-0"))
(global-unset-key (kbd "s-="))

(defun insert-en-dash ()
  (interactive)
  (insert "–"))

(defun insert-em-dash ()
  (interactive)
  (insert "—"))

(global-set-key (kbd "s--") 'insert-en-dash)
(global-set-key (kbd "s-_") 'insert-em-dash)

(provide 'setup-zoom-frm)
