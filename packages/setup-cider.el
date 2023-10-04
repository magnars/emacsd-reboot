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

  :custom
  ;; save files when evaluating them
  (cider-save-file-on-load t)

  ;; show stacktraces for everything, until https://github.com/clojure-emacs/cider/issues/3495 is solved
  (cider-clojure-compilation-error-phases nil))

(provide 'setup-cider)
