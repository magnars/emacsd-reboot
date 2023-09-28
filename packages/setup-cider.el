;; CIDER
;;
;; Extends Emacs with support for interactive programming in Clojure. The
;; features are centered around cider-mode, an Emacs minor-mode that complements
;; clojure-mode. While clojure-mode supports editing Clojure source files,
;; cider-mode adds support for interacting with a running Clojure process for
;; compilation, debugging, definition and documentation lookup, running tests
;; and so on.

(use-package cider
  :after (clojure-mode))

(provide 'setup-cider)
