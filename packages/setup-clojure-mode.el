(use-package clojure-mode
  :hook ((clojure-mode . setup-clojure-mode-so)
         (clojurescript-mode-hook . setup-clojure-mode-so)
         (clojurec-mode-hook . setup-clojure-mode-so))

  :config
  ;; don't steal hippie-expand-lines keybinding
  (unbind-key (kbd "C-:") clojure-mode-map)

  ;; After threading all forms, check if we should maybe unwind once
  ;; according to my tastes
  (defadvice clojure--thread-all (after possibly-unwind-once activate)
    (when (my/clojure-should-unwind-once?)
      (clojure-unwind)))

  :bind (:map clojure-mode-map
              ([remap paredit-forward] . clojure-forward-logical-sexp)
              ([remap paredit-backward] . clojure-backward-logical-sexp)
              ("C-\"" . clojure-toggle-keyword-string)))

;; Set up jumping to other file (src/test, component/scene)

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

;; Don't fully unthread always

(defun my/clojure-should-unwind-once? ()
  (save-excursion
    (ignore-errors
      (when (looking-at "(")
        (forward-char 1)
        (forward-sexp 1)))
    (let ((forms nil))
      (while (not (looking-at ")"))
        (clojure-forward-logical-sexp)
        (clojure-backward-logical-sexp)
        (setq forms (cons (buffer-substring-no-properties (point) (+ 1 (point))) forms))
        (clojure-forward-logical-sexp))
      (and (--any? (s-equals? it "(") forms)
           (< 2 (length forms))))))

(provide 'setup-clojure-mode)
