;; -*- lexical-binding: t; -*-
;; Deadgrep
;;
;; The fast, beautiful text search that your Emacs deserves.

(use-package deadgrep
  :bind (("M-s s" . deadgrep)
         (:map deadgrep-mode-map
               ("q" . deadgrep-quit)))

  :config
  (wrap-fullscreen deadgrep))

(use-package wgrep
  :bind ((:map deadgrep-mode-map
               ("e" . wgrep-change-to-wgrep-mode))))

(use-package wgrep-deadgrep
  :hook ((deadgrep-finished . wgrep-deadgrep-setup)))

;; Consider deadgrep-edit-mode as an alternative to wgrep.

(defun deadgrep-quit ()
  (interactive)
  (let ((prev my/previous-window-configuration))
    (quit-window)
    (when prev (register-val-jump-to prev nil))))

(provide 'setup-deadgrep)
