(use-package s)
(use-package dash)
(use-package diminish)

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
(defmacro wrap-fullscreen (command)
  `(defadvice ,command (around ,(intern (concat "wrap-" (symbol-name command) "-fullscreen")) activate)
     (let ((my/prev (list (current-window-configuration) (point-marker))))
       ad-do-it
       (delete-other-windows)
       (setq-local my/previous-window-configuration my/prev))))

(defvar my/previous-window-configuration nil)

;; No need to remind me about eldoc-mode all the time
(diminish 'eldoc-mode)

;; Insert pressed keybindings
(defun read-and-insert-key-sequence ()
  (interactive)
  (let ((keys []))
    (catch 'done
      (while t
        (let ((event (read-event (format "Press keys (ESC when done): %s" keys) nil)))
          (if (eq event 'escape)
              (throw 'done nil)
            (setq keys (vconcat keys (vector event)))))))
    (when (< 0 (length keys))
      (insert (format "(kbd \"%s\")" (key-description keys))))))

(add-hook 'emacs-lisp-mode-hook
          (lambda ()
            (local-set-key (kbd "C-M-S-s-k") 'read-and-insert-key-sequence)))

(provide 'tooling)
