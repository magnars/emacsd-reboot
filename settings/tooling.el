(use-package s :ensure t)
(use-package dash :ensure t)

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

(provide 'tooling)
