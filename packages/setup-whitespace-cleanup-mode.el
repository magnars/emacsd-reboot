;; whitespace-cleanup-mode.el
;;
;; A minor mode that will intelligently call whitespace-cleanup before buffers
;; are saved.

(use-package whitespace-cleanup-mode
  :diminish whitespace-cleanup-mode
  :init
  (global-whitespace-cleanup-mode))

(provide 'setup-whitespace-cleanup-mode)
