(use-package clj-refactor
  :diminish clj-refactor-mode

  :custom
  (cljr-favor-prefix-notation nil)
  (cljr-favor-private-functions nil)
  (cljr-insert-newline-after-require nil)
  (cljr-assume-language-context "clj")

  :config
  (cljr-add-keybindings-with-modifier "C-s-")
  (define-key clojure-mode-map (kbd "C-S-M-u") (λ (cljr--goto-toplevel)))
  (define-key clojure-mode-map (kbd "C->") 'cljr-thread)
  (define-key clojure-mode-map (kbd "C-<") 'cljr-unwind)

  (setq cljr-clojure-test-declaration "[clojure.test :refer [deftest is testing]]")
  (setq cljr-cljs-clojure-test-declaration cljr-clojure-test-declaration)
  (setq cljr-cljc-clojure-test-declaration cljr-clojure-test-declaration)

  (add-hook 'clojure-mode-hook 'clj-refactor-mode)

  (require 'core-async-mode)
  (add-hook 'clojure-mode-hook 'core-async-mode))

(provide 'setup-clj-refactor)
