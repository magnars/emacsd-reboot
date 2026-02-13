;; -*- lexical-binding: t; -*-
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

;; Remember keybindings easier (map this to your own keybinding in your user file)
(defun show-my-kbs-to-remember ()
  "Display custom keybindings and execute selected command."
  (interactive)
  (if (boundp 'my-kbs-to-remember)
   (let* ((choices (mapcar (lambda (entry)
                             (let ((key (car entry))
                                   (cmd (cadr entry))
                                   (desc (caddr entry)))
                               (cons (format "%-15s %-45s %s" key cmd desc)
                                     cmd)))
                           my-kbs-to-remember))
          (selection (completing-read "Keybinding: " choices nil t))
          (command (cdr (assoc selection choices))))
     (when command
       (call-interactively (intern command))))
   (message "You are missing a defvar my-kbs-to-remember with your own curated list.")))

(defun capture-my-kbs-to-remember ()
  "Prompt for a keybinding, find its command and docstring, then save to clipboard.
The output format is ready to paste into my-kbs-to-remember."
  (interactive)
  (let* ((key (read-key-sequence "Press keybinding to capture: "))
         (key-desc (key-description key))
         (command (key-binding key))
         (docstring (when command
                      (documentation command)))
         (first-line (when docstring
                       (car (split-string docstring "\n"))))
         (output (if command
                     (format "(\"%s\" \"%s\" \"%s\")"
                             key-desc
                             command
                             (or first-line "No description"))
                   (format ";; No command bound to %s" key-desc))))
    (if command
        (progn
          (kill-new output)
          (message "Copied to clipboard: %s" output))
      (message "No command bound to %s" key-desc))))

(provide 'tooling)
