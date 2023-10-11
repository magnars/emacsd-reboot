;; CIDER
;;
;; Extends Emacs with support for interactive programming in Clojure. The
;; features are centered around cider-mode, an Emacs minor-mode that complements
;; clojure-mode. While clojure-mode supports editing Clojure source files,
;; cider-mode adds support for interacting with a running Clojure process for
;; compilation, debugging, definition and documentation lookup, running tests
;; and so on.

(use-package cider
  :after (clojure-mode)

  :config
  ;; Warn about missing nREPL instead of doing stupid things
  (my/shadow-cider-keys-with-warning)

  ;; Clear CIDER repl buffer with C-c C-l
  (define-key cider-mode-map (kbd "C-c C-l") 'cider-find-and-clear-repl-buffer)
  (define-key cider-repl-mode-map (kbd "C-c C-l") 'cider-repl-clear-buffer)
  (define-key clojure-mode-map (kbd "C-c C-l") 'nrepl-warn-when-not-connected)

  ;; Keybinding to switch to repl-buffer even if it is the wrong kind
  (define-key clojure-mode-map (kbd "C-c z") 'cider-switch-to-any-repl-buffer)

  :custom
  ;; save files when evaluating them
  (cider-save-file-on-load t)

  ;; don't pop up repl when connecting
  (cider-repl-pop-to-buffer-on-connect nil)

  ;; show stacktraces for everything, until https://github.com/clojure-emacs/cider/issues/3495 is solved
  (cider-clojure-compilation-error-phases nil))

(defun nrepl-warn-when-not-connected ()
  (interactive)
  (message "Oops! You're not connected to an nREPL server. Please run M-x cider or M-x cider-jack-in to connect."))

(defun my/shadow-cider-keys-with-warning ()
  "Rebind all keys from `cider-mode-map` to `nrepl-warn-when-not-connected` in `clojure-mode-map`."
  (interactive)
  (map-keymap
   (lambda (key def)
     ;; Check if 'def' is a command or another keymap.
     (cond ((commandp def)
            ;; If 'def' is a command, rebind it in `clojure-mode-map`.
            (define-key clojure-mode-map (vector key) 'nrepl-warn-when-not-connected))
           ((keymapp def)
            ;; If 'def' is another keymap, recursively apply the same process.
            (map-keymap
             (lambda (sub-key sub-def)
               (when (commandp sub-def)
                 (define-key clojure-mode-map (vconcat (vector key) (vector sub-key))
                             'nrepl-warn-when-not-connected)))
             def))))
   cider-mode-map))

(defun cider-switch-to-any-repl-buffer (&optional set-namespace)
  "Switch to current REPL buffer, when possible in an existing window.
The type of the REPL is inferred from the mode of current buffer.  With a
prefix arg SET-NAMESPACE sets the namespace in the REPL buffer to that of
the namespace in the Clojure source buffer"
  (interactive "P")
  (or (ignore-errors
        (cider--switch-to-repl-buffer
         (cider-current-repl "any" t)
         set-namespace))
      (cider--switch-to-repl-buffer
       (concat "*cider-repl " (car (sesman-current-session 'CIDER)) "(clj)*")
       set-namespace)))

(defun cider-find-and-clear-repl-buffer ()
  (interactive)
  (cider-find-and-clear-repl-output t))

(provide 'setup-cider)
