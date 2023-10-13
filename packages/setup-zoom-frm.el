;; Zoom FRM
;;
;; Change text size for entire frame, not just a single buffer, lol

(straight-use-package
 '(zoom-frm :type git :host github :repo "emacsmirror/zoom-frm"))

(global-set-key (kbd "C-x C-+") 'zoom-frm-in)
(global-set-key (kbd "C-x C--") 'zoom-frm-out)
(global-set-key (kbd "C-x C-0") 'zoom-frm-unzoom)

(provide 'setup-zoom-frm)
