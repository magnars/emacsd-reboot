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

(defun matnyttig--wrap-e->map (form)
  (format "((or (requiring-resolve 'clojure.core/e->map) identity) %s)" form))

(defun matnyttig--cider-pprint-eval-last-sexp-with-e->map ()
  (interactive)
  (cider--pprint-eval-form
   (matnyttig--wrap-e->map (cider-last-sexp))))

(defun matnyttig--cider-pprint-eval-defun-at-point-with-e->map ()
  (interactive)
  (cider--pprint-eval-form
   (matnyttig--wrap-e->map (cider-defun-at-point))))

(define-key cider-mode-map (kbd "C-c C-p") #'matnyttig--cider-pprint-eval-last-sexp-with-e->map)
(define-key cider-mode-map (kbd "C-c C-f") #'matnyttig--cider-pprint-eval-defun-at-point-with-e->map)

(provide 'matnyttig)
