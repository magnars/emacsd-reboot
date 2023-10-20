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

(defun clj-cn--point-after (&rest actions)
  "Returns POINT after performing ACTIONS.

An action is either the symbol of a function or a two element
list of (fn args) to pass to `apply''"
  (save-excursion
    (dolist (fn-and-args actions)
      (let ((f (if (listp fn-and-args) (car fn-and-args) fn-and-args))
            (args (if (listp fn-and-args) (cdr fn-and-args) nil)))
        (apply f args)))
    (point)))

(defun clj-cn--comment-line? ()
  (save-excursion
    (goto-char (point-at-bol))
    (looking-at "\\s-*;+")))

(defun clj-cn--delete-and-extract-sexp ()
  (let* ((beg (point))
         (end (clj-cn--point-after 'paredit-forward))
         (contents (buffer-substring beg end)))
    (delete-region beg end)
    contents))

(defun clj-cn--delete-and-extract-sexp-with-nested-sexps ()
  "Returns list of strings representing the nested sexps if there is any.
   In case there are no nested sexp the list will have only one element.
   Not recursive, does not drill down into nested sexps
   inside the first level nested sexps."
  (let* ((beg (point))
         (sexp-start beg)
         (end (progn (paredit-forward)
                     (point)))
         nested)
    (paredit-backward)
    (when (looking-at "\\[\\|(")
      (paredit-forward-down))
    (while (/= sexp-start end)
      (paredit-move-forward)
      (push (s-trim (buffer-substring sexp-start (point))) nested)
      (setq sexp-start (point)))
    (delete-region beg end)
    (nreverse (cons (concat (nth 1 nested) (car nested)) (or (nthcdr 2 nested) '())))))

(defun clj-cn--extract-ns-statements (statement-type with-nested)
  (clj-cn--goto-ns)
  (if (or (not (clj-cn--search-forward-within-sexp (concat "(" statement-type)))
          (clj-cn--comment-line?))
      '()
    (let (statements)
      (while (not (looking-at " *)"))
        (push (if with-nested
                  (clj-cn--delete-and-extract-sexp-with-nested-sexps)
                (clj-cn--delete-and-extract-sexp)) statements))
      statements)))

(defun clj-cn--insert-in-ns (type)
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

(defun clj-cn-sort-ns ()
  "Sort the `ns' form."
  (interactive)
  (save-excursion
    (let ((buf-already-modified? (buffer-modified-p)))
      (dolist (statement-type '(":require-macros" ":require" ":use" ":import"))
        (let* ((statement (->> (clj-cn--extract-ns-statements statement-type nil)
                               (nreverse)
                               (-map 's-trim)))
               (sorted-statement (->> statement
                                      (-sort 'string<)
                                      (-distinct))))
          (dolist (it sorted-statement)
            (clj-cn--insert-in-ns statement-type)
            (insert it))
          (when (and (not buf-already-modified?)
                     (buffer-modified-p)
                     (->> (-interleave statement sorted-statement)
                          (-partition 2)
                          (--map (apply 's-equals? it))
                          (--every? (eq it t))))
            (not-modified)))))))

(provide 'clj-clean-namespace)
