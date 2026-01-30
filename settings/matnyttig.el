(require 'parseedn)

;; The following code is a starting point for integrating Emacs with
;; "matnyttig", our Clojure code base.
;;
;; At first, I tried cider-interactive-eval. Then, I was sad.
;; cider-interactive-eval doesn't block, and doesn't provide an easy-to-use
;; mechanism to return control to the function that called it. For example, you
;; may get multiple responses if you're running multiple REPLs.
;;
;; The next thing I want to try is to shell out to Babashka.
;; shell-command-to-string just returns the script's stdout, so it's perfect for
;; our use.

(defun matnyttig-add-page ()
  (interactive)
  (let* ((page-id (read-string "page-id: ")))
    (cider-interactive-eval (concat "(do"
                                    " (require 'matnyttig.page-admin)"
                                    " (matnyttig.page-admin/add \"" page-id "\")"
                                    ")"))))

;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;
;; Renaming a page requires listing existing pages! This is hard with CIDER, but
;; maybe easy with Babashka. My CIDER-based starting point follows.
;;
;; (defun matnyttig-list-pages ()
;;   ;; (interactive)
;;   (let* ((pages-edn-str (cider-interactive-eval
;;                          (concat "(do"
;;                                  " (require 'matnyttig.feed-admin)"
;;                                  " (matnyttig.feed-admin/list-pages)"
;;                                  ")"))))
;;     (parseedn-read-str pages-edn-str)))

;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;
;; Helper code to develop interactive Emacs functions with a primitive UI.
;;
;; (defun demo-callback (something)
;;   (message something))
;;
;; (defun demo ()
;;   ;; (interactive)
;;   (let* ((edn-str (cider-interactive-eval "(map (partial * 10) '(1 2 3))"
;;                                           'demo-callback))
;;          (elisp-values (parseedn-read-str edn-str)))
;;     elisp-values))
;; ;; then M-x demo from a buffer with a running REPL.

;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;
;; Evaluate and print with e->map

(defun matnyttig-wrap-e->map (form)
  (format "((or (requiring-resolve 'clojure.core/e->map) identity) %s)" form))

(defun pprint-eval-last-sexp (prefixed)
  (if prefixed
      (cider--pprint-eval-form (cider-last-sexp))
    (cider--pprint-eval-form
     (matnyttig-wrap-e->map (cider-last-sexp)))))

(defun matnyttig-cider-pprint-eval-last-sexp-with-e->map (prefixed)
  (interactive "P")
  (pprint-eval-last-sexp prefixed))

(defun matnyttig-cider-pprint-eval-defun-at-point-with-e->map (prefixed)
  (interactive "P")
  (if prefixed
      (cider--pprint-eval-form (cider-defun-at-point))
    (cider--pprint-eval-form
     (matnyttig-wrap-e->map (cider-defun-at-point)))))

(defun matnyttig-cider-eval-def-symbol-with-e->map (prefixed)
  (interactive "P")
  (save-excursion
    (beginning-of-defun)
    (paredit-forward-down)
    (when (looking-at "def\\(\\w*\\)")
      (paredit-forward 2)
      (pprint-eval-last-sexp prefixed))))

(define-key cider-mode-map (kbd "C-c C-p") #'matnyttig-cider-pprint-eval-last-sexp-with-e->map)
(define-key cider-mode-map (kbd "C-c C-f") #'matnyttig-cider-pprint-eval-defun-at-point-with-e->map)
(define-key cider-mode-map (kbd "C-c C-M-s") #'matnyttig-cider-eval-def-symbol-with-e->map)

;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;
;; Find first class definitions (like feeds, etc.)

;; Patterns
(defun matnyttig-collector-pattern (thing)
  (format "(source-definition/define-collector\n    {:id %s" thing))

(defun matnyttig-refiner-pattern (thing)
  (format "(source-definition/define-refiner\n    {:id %s" thing))

(defun matnyttig-feed-pattern (thing)
  (format "(source-definition/define-feed\n    {:id %s" thing))

(defun matnyttig-page-pattern (thing)
  (format "(page-definition/define\n    {:id %s" thing))

;; Files
(defun matnyttig-src-files ()
  (directory-files-recursively
   (file-name-concat (projectile-project-root) "src")
   "\\.clj[sc]?$"))

(defun matnyttig-collector-files (files)
  (--filter (string-match-p "/collectors/" it) files))

(defun matnyttig-refiner-files (files)
  (--filter (string-match-p "/refiners/" it) files))

(defun matnyttig-feed-files (files)
  (--filter (string-match-p "/feeds/" it) files))

(defun matnyttig-page-files (files)
  (--filter (string-match-p "/sider/" it) files))

;; Effects
(defun matnyttig-find-effect-definition (thing)
  (interactive)
  (let ((effects-file (file-name-concat (projectile-project-root) "src/matnyttig/imperative_shell/effects.clj"))
        (result nil))
    (when (file-exists-p effects-file)
      (with-temp-buffer
        (insert-file-contents effects-file)
        (goto-char (point-min))
        (when (search-forward "(case (:effect/kind effect)" nil t)
          (when (search-forward thing nil t)
            (paredit-forward-down)
            (when (re-search-backward
                   (format "(defn \\(\\^{:indent 1} \\)?%s"
                           (thing-at-point 'symbol t)))
              (setq result (cons effects-file (line-number-at-pos)))))))
      result)))

;; Logic
(defun matnyttig-search-first-class-definition (files pattern)
  (let ((result nil))
    (catch 'found
      (dolist (file files)
        (with-temp-buffer
          (insert-file-contents file)
          (goto-char (point-min))
          (when (search-forward pattern nil t)
            (setq result (cons file (line-number-at-pos)))
            (throw 'found t)))))
    result))

(defun matnyttig-goto-first-class-definition (floc)
  (xref-push-marker-stack)
  (find-file (car floc))
  (goto-line (cdr floc))
  (goto-char (point-at-eol)))

(defun matnyttig-find-first-class-definition ()
  (interactive)
  (let ((thing (thing-at-point 'symbol))
        (matnyttig-src-files (matnyttig-src-files))
        (floc nil))
    (cond
     ((string-prefix-p ":feed/" thing)
      (setq floc (matnyttig-search-first-class-definition
                  (matnyttig-feed-files matnyttig-src-files)
                  (matnyttig-feed-pattern thing))))

     ((string-prefix-p ":pages/" thing)
      (setq floc (matnyttig-search-first-class-definition
                  (matnyttig-page-files matnyttig-src-files)
                  (matnyttig-page-pattern thing))))

     ((string-prefix-p ":data/" thing)
      (when-let ((result (matnyttig-search-first-class-definition
                          (matnyttig-collector-files matnyttig-src-files)
                          (matnyttig-collector-pattern thing))))
        (setq floc result))
      (when-let ((result (matnyttig-search-first-class-definition
                          (matnyttig-refiner-files matnyttig-src-files)
                          (matnyttig-refiner-pattern thing))))
        (setq floc result)))

     ((string-prefix-p ":effects." thing)
      (setq floc (matnyttig-find-effect-definition thing))))
    (if floc
        (matnyttig-goto-first-class-definition floc)
      (xref-find-definitions (xref-backend-identifier-at-point (xref-find-backend))))))

(define-key clojure-mode-map (kbd "M-.") 'matnyttig-find-first-class-definition)

;; Ignore annoyingly abundant files that hang Emacs on Vertico analyses
(with-eval-after-load 'lsp-mode
  (add-to-list 'lsp-file-watch-ignored-directories "[/\\\\].nats-cache\\'"))

(provide 'matnyttig)
