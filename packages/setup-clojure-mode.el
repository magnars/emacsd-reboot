(use-package clojure-mode
  :hook ((clojure-mode . setup-clojure-mode-so)
         (clojurescript-mode-hook . setup-clojure-mode-so)
         (clojurec-mode-hook . setup-clojure-mode-so))

  :custom
  (clojure-toplevel-inside-comment-form t)

  :config
  ;; don't steal hippie-expand-lines keybinding
  (unbind-key (kbd "C-:") clojure-mode-map)

  (require 'i18n-edn)

  ;; After threading all forms, check if we should maybe unwind once
  ;; according to my tastes
  (defadvice clojure--thread-all (after possibly-unwind-once activate)
    (when (my/clojure-should-unwind-once?)
      (clojure-unwind)))

  :bind (:map clojure-mode-map
              ([remap paredit-forward] . clojure-forward-logical-sexp)
              ([remap paredit-backward] . clojure-backward-logical-sexp)
              ("C-\"" . clojure-toggle-keyword-string)
              ("C-x M-e" . my/cider-eval-including-lets)
              ("C-c C-M-s" . my/cider-eval-def-symbol)
              ("C-." . clj-hippie-expand-no-case-fold)
              ("C-c i 1 8 n" . i18n-edn-edit-in-multifile)
              ("<f7>" . cider-eval-last-sexp)
              ("<f6>" . cider-pprint-eval-last-sexp)
              ("<f5>" . delete-other-windows)
              ("s-<return>" . clerk-show)))

(use-package zprint-mode
  :defer 2)

;; Set up jumping to other file (src/test, component/scene)

(require 's)
(require 'significant-other)

(defun setup-clojure-mode-so ()
  (with-significant-others file-name
    ("/portfolio/.+/components/" (list (s-with file-name
                                         (s-replace "/portfolio/" "/src/")
                                         (s-replace "_scenes.cljs" ".cljc"))))

    ("/ui/src/.+/components/" (list (s-with file-name
                                      (s-replace "/src/" "/portfolio/")
                                      (s-replace ".cljc" "_scenes.cljs"))
                                    (s-with file-name
                                      (s-replace "/src/" "/test/")
                                      (s-replace ".cljc" "_test.clj"))))

    ("/src/.+\.cljc" (list (s-with file-name
                             (s-replace "/src/" "/test/")
                             (s-replace ".cljc" "_test.clj"))
                           (s-with file-name
                             (s-replace "/src/" "/test/")
                             (s-replace ".cljc" "_test.cljc"))))

    ("/src/.+\.clj" (list (s-with file-name
                            (s-replace "/src/" "/test/")
                            (s-replace ".clj" "_test.clj"))))

    ("/dev/.+\.clj" (list (s-with file-name
                            (s-replace "/dev/" "/test/")
                            (s-replace ".clj" "_test.clj"))))

    ("/test/.+\.clj" (list
                      (s-with file-name
                        (s-replace "/test/" "/src/")
                        (s-replace "_test.clj" ".clj"))
                      (s-with file-name
                        (s-replace "/test/" "/src/")
                        (s-replace "_test.clj" ".cljc"))
                      (s-with file-name
                        (s-replace "/test/" "/dev/")
                        (s-replace "_test.clj" ".clj"))))))

;; Set up Clojure CSS completions

(use-package css-completions
  :after (cider projectile)
  :ensure nil
  :defer t
  :commands (cssc/enable-for-clojure)
  :init
  (add-hook 'clojure-mode-hook 'cssc/enable-for-clojure)
  (add-hook 'clojurescript-mode-hook 'cssc/enable-for-clojure)
  (add-hook 'clojurec-mode-hook 'cssc/enable-for-clojure)
  (add-hook 'mhtml-mode-hook 'cssc/enable-for-html))

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

;; eval-current-sexp while also including surrounding lets

(defun my/cider-looking-at-lets? ()
  (or (looking-at "(let ")
      (looking-at "(letfn ")
      (looking-at "(when-let ")
      (looking-at "(if-let ")))

(defun my/cider-collect-lets (&optional max-point)
  (let* ((beg-of-defun (save-excursion (beginning-of-defun) (point)))
         (lets nil))
    (save-excursion
      (while (not (= (point) beg-of-defun))
        (paredit-backward-up 1)
        (when (my/cider-looking-at-lets?)
          (save-excursion
            (let ((beg (point)))
              (paredit-forward-down 1)
              (paredit-forward 2)
              (when (and max-point (< max-point (point)))
                (goto-char max-point))
              (setq lets (cons (concat (buffer-substring-no-properties beg (point))
                                       (if max-point "]" ""))
                               lets))))))
      lets)))

(defun my/inside-let-block? ()
  (save-excursion
    (paredit-backward-up 2)
    (my/cider-looking-at-lets?)))

(defun my/cider-eval-including-lets (&optional output-to-current-buffer)
  "Evaluates the current sexp form, wrapped in all parent lets."
  (interactive "P")
  (let* ((beg-of-sexp (save-excursion (paredit-backward 1) (point)))
         (code (buffer-substring-no-properties beg-of-sexp (point)))
         (lets (my/cider-collect-lets (when (my/inside-let-block?)
                                        (save-excursion (paredit-backward 2) (point)))))
         (code (concat (s-join " " lets)
                       " " code
                       (s-repeat (length lets) ")"))))
    (cider-interactive-eval code
                            (when output-to-current-buffer
                              (cider-eval-print-handler))
                            nil
                            (cider--nrepl-pr-request-map))))

(defun my/cider-eval-def-symbol ()
  "Evaluate and pretty-print the value of the symbol of the def at point."
  (interactive)
  (save-excursion
    (beginning-of-defun)
    (paredit-forward-down)
    (when (looking-at "def\\(\\w*\\)")
      (paredit-forward 2)
      (cider-pprint-eval-last-sexp))))

(defun clj-hippie-expand-no-case-fold ()
  "Consider / as whitespace when doing hippie-expand i clojure-mode"
  (interactive)
  (let ((old-syntax (char-to-string (char-syntax ?/))))
    (modify-syntax-entry ?/ " ")
    (hippie-expand-no-case-fold)
    (modify-syntax-entry ?/ old-syntax)))

(defun clerk-show ()
  (interactive)
  (when-let
      ((filename
        (buffer-file-name)))
    (save-buffer)
    (cider-interactive-eval
     (concat "(nextjournal.clerk/show! \"" filename "\")"))))

(use-package neil :defer t) ;; M-x neil-find-clojure-package

(provide 'setup-clojure-mode)
