(use-package s :ensure t)
(use-package dash :ensure t)
(use-package diminish :ensure t)

;; Shorthand for interactive lambdas
(defmacro λ (&rest body)
  `(lambda ()
     (interactive)
     ,@body))

(global-set-key (kbd "s-l") (λ (insert "\u03bb")))

;; Set up a keybinding for the very next command invocation
(defun one-shot-keybinding (key command)
  (set-temporary-overlay-map
   (let ((map (make-sparse-keymap)))
     (define-key map (kbd key) command)
     map) 
   t))

;; Instrument a `command' to store the current window configuration in
;; `register' and then going fullscreen.
(defmacro wrap-fullscreen (command register)
  `(defadvice ,command (around ,(intern (concat "wrap-" (symbol-name command) "-fullscreen")) activate)
     (unless (get-register ,register)
       (window-configuration-to-register ,register))
     ad-do-it
     (delete-other-windows)))

(provide 'tooling)
