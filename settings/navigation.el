;; Moving the cursor around

;; Like isearch, but adds region (if any) to history and deactivates mark
(defun isearch-forward-use-region ()
  (interactive)
  (when (region-active-p)
    (add-to-history 'search-ring (buffer-substring (region-beginning)
                                                   (region-end)))
    (deactivate-mark))
  (call-interactively 'isearch-forward))

(defun isearch-backward-use-region ()
  (interactive)
  (when (region-active-p)
    (add-to-history 'search-ring (buffer-substring (region-beginning)
                                                   (region-end)))
    (deactivate-mark))
  (call-interactively 'isearch-backward))

(global-set-key (kbd "C-s") 'isearch-forward-use-region)
(global-set-key (kbd "C-r") 'isearch-backward-use-region)

;; 

(provide 'navigation)
