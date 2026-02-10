;; -*- lexical-binding: t; -*-
;; Provides basic namespace sorting functionality for clojure

(require 'dash)
(require 's)

(defun clj-cn--goto-ns ()
  (goto-char (point-min))
  (if (re-search-forward clojure-namespace-name-regex nil t)
      (clj-cn--goto-toplevel)
    (error "No namespace declaration found")))

(defun clj-cn--search-forward-within-sexp (s &optional save-excursion)
  "Searches forward for S in the current sexp.

if SAVE-EXCURSION is T POINT does not move."
  (let ((bound (save-excursion (forward-list 1) (point))))
    (if save-excursion
        (save-excursion
          (re-search-forward (concat s "\\()\\| \\|$\\)") bound t))
      (when-let ((result (re-search-forward (concat s "\\()\\| \\|$\\)") bound t)))
        (when (or  (looking-back " " 1)
                   (looking-back ")" 1))
          (forward-char -1))
        result))))

(defun clj-cn--comment-line? ()
  (save-excursion
    (back-to-indentation)
    (or (looking-at ";")
        (looking-at "#_"))))

(defun clj-cn--delete-and-extract-sexp ()
  (let (sexp comment-before comment-after beg end)
    ;; start at the beginning
    (setq beg (point))

    ;; move forward to the the first non-comment sexp
    (clojure-forward-logical-sexp)
    (while (clj-cn--comment-line?)
      (clojure-forward-logical-sexp))
    (clojure-backward-logical-sexp)

    ;; extract any full-line comments before the sexp
    (setq comment-before (s-trim (buffer-substring-no-properties beg (point))))

    ;; extract the sexp itself
    (let ((here (point)))
      (clojure-forward-logical-sexp)
      (setq sexp (buffer-substring-no-properties here (point))))

    ;; extract any inline comments after the sexp
    (when (looking-at "\s*;")
      (let ((here (point)))
        (end-of-line)
        (setq comment-after (buffer-substring-no-properties here (point)))))

    (let ((contents (buffer-substring beg (point))))
      (delete-region beg (point))
      (list :sexp sexp
            :contents contents
            :comment-before (unless (string= "" comment-before)
                              comment-before)
            :comment-after comment-after))))

(defun clj-cn--end-of-statement? ()
  (not
   (ignore-errors
     (save-excursion (forward-sexp)) t)))

(defun clj-cn--extract-ns-statements (statement-type)
  (clj-cn--goto-ns)
  (when (and (clj-cn--search-forward-within-sexp (concat "(" statement-type))
             (not (clj-cn--comment-line?)))
    (let (statements)
      (while (not (clj-cn--end-of-statement?))
        (push (clj-cn--delete-and-extract-sexp)
              statements))
      statements)))

(defun clj-cn--prepare-insert-in-ns (type)
  (clj-cn--goto-ns)
  (if (clj-cn--search-forward-within-sexp (concat "(" type))
      (if (looking-at " *)")
          (progn
            (search-backward "(")
            (forward-list 1)
            (forward-char -1)
            (insert " "))
        (search-backward "(")
        (forward-list 1)
        (forward-char -1)
        (newline-and-indent))
    (forward-list 1)
    (forward-char -1)
    (newline-and-indent)
    (insert "(" type " )")
    (forward-char -1)))

(defun clj-cn--goto-toplevel ()
  (paredit-backward-up (cljr--depth-at-point))
  (when (looking-back "#")
    (backward-char)))

(defun clj-cn--extract-ns-form ()
  (save-excursion
    (clj-cn--goto-ns)
    (let ((beg (point))
          (end (progn (paredit-forward)
                      (point))))
      (buffer-substring-no-properties beg end))))

(defun clj-cn--comparator (a b)
  (string< (plist-get a :sexp)
           (plist-get b :sexp)))

(defun clj-cn--remove-blank-lines (beg end)
  (save-excursion
    (goto-char beg)
    (while (re-search-forward "^\\s-*$" end t)
      (delete-char 1)
      (setq end (- end 1)))))

(defun clj-cn--clean-up (statement-type f)
  (clj-cn--goto-ns)
  (when (and (clj-cn--search-forward-within-sexp (concat "(" statement-type))
             (not (clj-cn--comment-line?)))
    (paredit-backward-up)
    (let ((beg (point))
          (end (progn (paredit-forward)
                      (point))))
      (funcall f beg end))))

(defun clj-cn-sort-ns ()
  "Sort the `ns' form."
  (interactive)
  (let ((buf-already-modified? (buffer-modified-p))
        (ns-form-before (clj-cn--extract-ns-form)))
    (save-excursion
      (dolist (statement-type '(":require" ":use" ":import" ":require-macros"))
        (clj-cn--goto-ns)
        (when (clj-cn--search-forward-within-sexp (concat "(" statement-type))
          (let* ((own-line? (eolp))
                 (statements (clj-cn--extract-ns-statements statement-type))
                 (sorted-statement (->> statements
                                        (nreverse)
                                        (-sort 'clj-cn--comparator)
                                        (-distinct))))
            (while (not (looking-at ")"))
              (delete-char 1))
            (dolist (it sorted-statement)
              (clj-cn--prepare-insert-in-ns statement-type)
              (when own-line?
                (delete-char -1)
                (insert "\n"))
              (insert (s-trim (plist-get it :contents)))
              (when (plist-get it :comment-after)
                (insert "\n")))
            (clj-cn--clean-up statement-type #'clj-cn--remove-blank-lines)
            (clj-cn--clean-up statement-type #'indent-region)))))
    (when (and (not buf-already-modified?)
               (buffer-modified-p)
               (s-equals? ns-form-before
                          (clj-cn--extract-ns-form)))
      (not-modified))))

(provide 'clj-clean-namespace)
