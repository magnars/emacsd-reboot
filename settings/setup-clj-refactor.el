(use-package clj-refactor
  :config
  (setq cljr-favor-prefix-notation nil)
  (setq cljr-favor-private-functions nil)
  (setq cljr-insert-newline-after-require nil)
  (setq cljr-assume-language-context "clj")

  (cljr-add-keybindings-with-modifier "C-s-")

  (add-hook 'clojure-mode-hook 'clj-refactor-mode))

(provide 'setup-clj-refactor)
