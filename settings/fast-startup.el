;; Startup optimizations
;;
;; https://www.reddit.com/r/emacs/comments/3kqt6e/2_easy_little_known_steps_to_speed_up_emacs_start/

;; Set garbage collection threshold

(setq gc-cons-threshold (* 1024 1024 100))

;; Set file-name-handler-alist

(setq file-name-handler-alist-original file-name-handler-alist)
(setq file-name-handler-alist nil)

;; Keep tabs on startup time

(add-hook 'emacs-startup-hook
          (lambda ()
            (message "Emacs ready in %s with %d garbage collections."
                     (format "%.2f seconds"
                             (float-time
                              (time-subtract after-init-time before-init-time)))
                     gcs-done)))

(run-with-idle-timer
   5 nil
   (lambda ()
     (setq gc-cons-threshold (* 1024 1024 20))
     (setq file-name-handler-alist file-name-handler-alist-original)
     (makunbound 'file-name-handler-alist-original)))

(provide 'fast-startup)
