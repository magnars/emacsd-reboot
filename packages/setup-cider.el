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

  :custom
  ;; save files when evaluating them
  (cider-save-file-on-load t)

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

(provide 'setup-cider)
