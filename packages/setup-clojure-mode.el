(use-package clojure-mode
  :hook ((clojure-mode . setup-clojure-mode-so)
         (clojurescript-mode-hook . setup-clojure-mode-so)
         (clojurec-mode-hook . setup-clojure-mode-so))

  :config
  ;; don't steal hippie-expand-lines keybinding
  (unbind-key (kbd "C-:") clojure-mode-map)

  :bind (:map clojure-mode-map
              ([remap paredit-forward] . clojure-forward-logical-sexp)
              ([remap paredit-backward] . clojure-backward-logical-sexp)
              ("C-\"" . clojure-toggle-keyword-string)))

(require 's)
(require 'significant-other)

(defun setup-clojure-mode-so ()
  (with-significant-others file-name
    ("/portfolio/.+/components/" (s-with file-name
                                   (s-replace "/portfolio/" "/src/")
                                   (s-replace "_scenes.cljs" ".cljc")))

    ("/ui/src/.+/components/" (s-with file-name
                                (s-replace "/src/" "/portfolio/")
                                (s-replace ".cljc" "_scenes.cljs")))

    ("/src/.+\.clj" (s-with file-name
                      (s-replace "/src/" "/test/")
                      (s-replace ".clj" "_test.clj")))

    ("/test/.+\.clj" (s-with file-name
                       (s-replace "/test/" "/src/")
                       (s-replace "_test.clj" ".clj")))))

(provide 'setup-clojure-mode)
