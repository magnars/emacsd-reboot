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

(defun matnyttig-cider-pprint-eval-last-sexp-with-e->map (prefixed)
  (interactive "P")
  (if prefixed
      (cider--pprint-eval-form (cider-last-sexp))
   (cider--pprint-eval-form
    (matnyttig-wrap-e->map (cider-last-sexp)))))

(defun matnyttig-cider-pprint-eval-defun-at-point-with-e->map (prefixed)
  (interactive "P")
  (if prefixed
      (cider--pprint-eval-form (cider-defun-at-point))
    (cider--pprint-eval-form
     (matnyttig-wrap-e->map (cider-defun-at-point)))))

(define-key cider-mode-map (kbd "C-c C-p") #'matnyttig-cider-pprint-eval-last-sexp-with-e->map)
(define-key cider-mode-map (kbd "C-c C-f") #'matnyttig-cider-pprint-eval-defun-at-point-with-e->map)

;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;
;; Find first class definitions (like feeds, etc.)

(defun matnyttig-find-first-class-definition (folder pattern)
  (let ((files (directory-files-recursively folder "\\.clj[sc]?$"))
        (result nil))
    (catch 'found
     (dolist (file files)
       (with-temp-buffer
         (insert-file-contents file)
         (goto-char (point-min))
         (when (search-forward pattern nil t)
           (setq result (cons file (line-number-at-pos)))
           (throw 'found t)))))
    (if result
        (progn
          (xref-push-marker-stack)
          (find-file (car result))
          (goto-line (cdr result))
          (goto-char (point-at-eol)))
      (message "Feed definition not found"))))

(defun matnyttig-feed-folder ()
  (file-name-concat (projectile-project-root) "src" "matnyttig" "feeds"))

(defun matnyttig-find-feed-definition ()
  (interactive)
  (let ((thing (thing-at-point 'symbol t)))
    (when (string-prefix-p ":feed/" thing)
      (matnyttig-find-first-class-definition
       (matnyttig-feed-folder)
       (format "(def feed\n  (source-definition/define-feed\n    {:id %s" thing)))))

(define-key clojure-mode-map (kbd "s-.") 'matnyttig-find-feed-definition)

(provide 'matnyttig)
