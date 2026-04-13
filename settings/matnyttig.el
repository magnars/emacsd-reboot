;; -*- lexical-binding: t; -*-
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
    (cider--pprint-eval-form (matnyttig-wrap-e->map (cider-last-sexp)))))

(defun matnyttig-cider-pprint-eval-last-sexp-with-e->map (prefixed)
  (interactive "P")
  (pprint-eval-last-sexp prefixed))

(defun matnyttig-cider-pprint-eval-defun-at-point-with-e->map (prefixed)
  (interactive "P")
  (if prefixed
      (cider--pprint-eval-form (cider-defun-at-point))
    (cider--pprint-eval-form (matnyttig-wrap-e->map (cider-defun-at-point)))))

(defun matnyttig-cider-eval-def-symbol-with-e->map (prefixed)
  "Evaluate and pprint symbol of top-level def (with e->map wrapped)"
  (interactive "P")
  (save-excursion
    (beginning-of-defun)
    (paredit-forward-down)
    (when (looking-at "def\\(\\w*\\)")
      (paredit-forward 2)
      (pprint-eval-last-sexp prefixed))))

(defun matnyttig-cider-pprint-eval-sexp-up-to-point-with-e->map (&optional prefixed)
  "Evaluate the current sexp form"
  (interactive "P")
  (let* ((beg-of-sexp (save-excursion (up-list) (backward-list) (point)))
         (beg-delimiter (save-excursion (up-list) (backward-list) (char-after)))
         (beg-set?  (save-excursion (up-list) (backward-list) (char-before)))
         (code (buffer-substring-no-properties beg-of-sexp (point)))
         (code (if (= beg-set? ?#) (concat (list beg-set?) code) code))
         (code (concat code (list (cider--matching-delimiter beg-delimiter)))))
    (if prefixed
        (cider--pprint-eval-form code)
      (cider--pprint-eval-form (matnyttig-wrap-e->map code)))))

(define-key cider-mode-map (kbd "C-c C-p") #'matnyttig-cider-pprint-eval-last-sexp-with-e->map)
(define-key cider-mode-map (kbd "C-c C-f") #'matnyttig-cider-pprint-eval-defun-at-point-with-e->map)
(define-key cider-mode-map (kbd "C-c C-M-s") #'matnyttig-cider-eval-def-symbol-with-e->map)
(define-key cider-mode-map (kbd "C-c C-M-e") #'matnyttig-cider-pprint-eval-sexp-up-to-point-with-e->map)

;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;
;; Find first class definitions (like feeds, etc.)

;; Patterns
(defun matnyttig-collector-pattern (thing)
  (format "(source-definition/define-collector\n    {:id %s\n" thing))

(defun matnyttig-refiner-pattern (thing)
  (format "(source-definition/define-refiner\n    {:id %s\n" thing))

(defun matnyttig-feed-pattern (thing)
  (format "(source-definition/define-feed\n    {:id %s\n" thing))

(defun matnyttig-page-pattern (thing)
  (format "(page-definition/define\n    {:id %s\n" thing))

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
              (setq result (list effects-file (line-number-at-pos) (current-column)))))))
      result)))

;; Commands
(defun matnyttig-find-command-definition (thing)
  (interactive)
  (let ((commands-file (file-name-concat (projectile-project-root) "src/matnyttig/commands.clj"))
        (result nil))
    (when (file-exists-p commands-file)
      (with-temp-buffer
        (insert-file-contents commands-file)
        (goto-char (point-min))
        (when (search-forward (format ":command/kind %s\n" thing) nil t)
          (when (search-forward ":command/plan" nil t)
            (paredit-forward)
            (setq result (list commands-file (line-number-at-pos) (current-column))))))
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
            (setq result (list file (line-number-at-pos) (current-column)))
            (throw 'found t)))))
    result))

(defun matnyttig-goto-first-class-definition (file-line-col)
  (xref-push-marker-stack)
  (find-file (nth 0 file-line-col))
  (goto-line (nth 1 file-line-col))
  (move-to-column (nth 2 file-line-col)))

(defun matnyttig-find-first-class-definition ()
  (interactive)
  (let ((thing (thing-at-point 'symbol))
        (matnyttig-src-files (matnyttig-src-files))
        (file-line-col nil))
    (cond
     ((string-prefix-p ":feed/" thing)
      (setq file-line-col (matnyttig-search-first-class-definition
                  (matnyttig-feed-files matnyttig-src-files)
                  (matnyttig-feed-pattern thing))))

     ((string-prefix-p ":pages/" thing)
      (setq file-line-col (matnyttig-search-first-class-definition
                  (matnyttig-page-files matnyttig-src-files)
                  (matnyttig-page-pattern thing))))

     ((string-prefix-p ":data/" thing)
      (when-let ((result (matnyttig-search-first-class-definition
                          (matnyttig-collector-files matnyttig-src-files)
                          (matnyttig-collector-pattern thing))))
        (setq file-line-col result))
      (when-let ((result (matnyttig-search-first-class-definition
                          (matnyttig-refiner-files matnyttig-src-files)
                          (matnyttig-refiner-pattern thing))))
        (setq file-line-col result)))

     ((string-prefix-p ":effects." thing)
      (setq file-line-col (matnyttig-find-effect-definition thing)))

     ((string-prefix-p ":commands" thing)
      (setq file-line-col (matnyttig-find-command-definition thing))))
    (when file-line-col
      (matnyttig-goto-first-class-definition file-line-col))))

;; Nexus

(defun nexus-find-pattern (pattern)
  (let* ((src-dir (concat (projectile-project-root) "src"))
         (res (shell-command-to-string
               (format "rg --no-heading --with-filename --line-number --multiline --json %s %s | jq -r 'select(.type == \"match\") | [.data.path.text, .data.line_number, .data.submatches[0].match.text] | @tsv'"
                       (shell-quote-argument pattern)
                       (shell-quote-argument src-dir)))))
    (mapcar (lambda (line)
              (let ((parts (split-string line "\t" t)))
                (list (nth 0 parts)
                      (string-to-number (nth 1 parts))
                      (-> (nth 2 parts)
                          (split-string " " t)
                          -last-item))))
            (split-string (string-trim res) "\n" t))))

(defun nexus-->lookup-map (nxr-result)
  (let ((map (make-hash-table :test 'equal)))
    (dolist (item nxr-result map)
      (puthash (car (last item)) item map))))

(defun nexus-goto-def (nexus-match)
  (xref-push-marker-stack)
  (find-file (nth 0 nexus-match))
  (goto-line (nth 1 nexus-match)))

(defun nexus-find-match (thing)
  (when-let ((nexus-match
              (->> (nexus-find-pattern "\\(nxr\\/register-(effect|action|placeholder)![ \n]?[ ]*:[^\s\n]+")
                   nexus-->lookup-map
                   (gethash thing))))
    nexus-match))

(defun nexus-goto-thing ()
  (interactive)
  (let ((thing (thing-at-point 'symbol)))
    (when-let ((nexus-match (nexus-find-match thing)))
      (nexus-goto-def nexus-match))))

(defun nexus-eldoc-nexus-match (callback &rest _)
  "Show eldoc info when point is on nexus match"
  (let ((thing (thing-at-point 'symbol)))
    (when-let ((nexus-match (nexus-find-match thing)))
      (save-excursion
        (with-current-buffer (find-file-noselect (nth 0 nexus-match))
          (goto-char (point-min))
          (forward-line (1- (nth 1 nexus-match)))
          (paredit-forward-down 3)
          (paredit-forward)
          (let ((start (point)))
            (paredit-forward-up)
            (paredit-backward-down)
            (funcall callback (format "[%s%s]" thing (buffer-substring-no-properties start (point)))
                     :thing thing
                     :face 'font-lock-keyword-face)))))))

(add-hook 'clojure-mode-hook
          (lambda ()
            (add-hook 'eldoc-documentation-functions #'nexus-eldoc-nexus-match nil t)))

;; Nexus end

(defun matnyttig-goto-fns ()
  (interactive)
  (or (matnyttig-find-first-class-definition)
      (nexus-goto-thing)
      (xref-find-definitions (xref-backend-identifier-at-point (xref-find-backend)))))

(define-key clojure-mode-map (kbd "M-.") 'matnyttig-goto-fns)

;; Ignore annoyingly abundant files that hang Emacs on Vertico analyses
(with-eval-after-load 'lsp-mode
  (add-to-list 'lsp-file-watch-ignored-directories "[/\\\\].nats-cache\\'"))

(provide 'matnyttig)
