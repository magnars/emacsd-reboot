;; clj-auto-refer-mode.el
;;
;; Requires and refers well-known functions and macros for you.

(require 'dash)
(require 'cider)
(require 'clj-refactor)
(require 'clj-clean-namespace)

(defvar auto-refer-packages nil)
;; see setup-clj-refactor.el for usage (for now)

(defun auto-refer--in-comment? ()
  (nth 4 (syntax-ppss)))

(defun auto-refer--in-string? ()
  (nth 3 (syntax-ppss)))

(defun auto-refer--find-usages (package)
  (let ((symbols-re (concat (regexp-opt '("(" "["))
                            (regexp-opt (-concat (plist-get package :functions)
                                                 (plist-get package :macros))
                                        'symbols)))
        result)
    (save-excursion
      (goto-char (point-min))
      (while (re-search-forward symbols-re nil t)
        (unless (or (auto-refer--in-comment?)
                    (auto-refer--in-string?))
          (!cons (cider-symbol-at-point) result))))
    (-distinct result)))

(defun auto-refer--remove-from-ns (type s)
  (cljr--goto-ns)
  (when (cljr--search-forward-within-sexp (concat "(" type))
    (skip-syntax-forward " >")
    (while (not (looking-at ")"))
      (if (looking-at (regexp-quote (concat "[" s)))
          (cljr--delete-sexp)
        (paredit-forward))
      (skip-syntax-forward " >"))))

(defun auto-refer--update-clj-namespace (package)
  (let ((ns (plist-get package :ns)))
    (save-excursion
      (cljr--goto-ns)
      (unless (cljr--search-forward-within-sexp (concat ns " :as"))
        (let ((usages (auto-refer--find-usages package)))
          (auto-refer--remove-from-ns ":require" ns)
          (when usages
            (cljr--insert-in-ns ":require")
            (insert (concat "[" ns " :refer ["))
            (apply 'insert (->> usages
                                (-sort 'string<)
                                (-interpose " ")))
            (insert "]]"))))
      (clj-cn-sort-ns))))

(defun auto-refer--update-cljs-namespace (package)
  (let ((ns (or (plist-get package :cljs-ns)
                (plist-get package :ns)))
        (macro-ns (or (plist-get package :cljs-macro-ns)
                      ns))
        (functions (plist-get package :functions))
        (macros (plist-get package :macros)))
    (save-excursion
      (cljr--goto-ns)
      (unless (cljr--search-forward-within-sexp (concat ns " :as"))
        (let* ((usages (auto-refer--find-usages package))
               (used-fns (--filter (member it functions) usages))
               (used-macros (--filter (member it macros) usages)))
          (auto-refer--remove-from-ns ":require" ns)
          (auto-refer--remove-from-ns ":require-macros" macro-ns)
          (when used-fns
            (cljr--insert-in-ns ":require")
            (just-one-space)
            (insert (concat "[" ns " :refer ["))
            (apply 'insert (->> used-fns
                                (-sort 'string<)
                                (-interpose " ")))
            (insert "]]"))
          (when used-macros
            (cljr--insert-in-ns ":require-macros")
            (just-one-space)
            (insert (concat "[" macro-ns " :refer ["))
            (apply 'insert (->> used-macros
                                (-sort 'string<)
                                (-interpose " ")))
            (insert "]]"))
          (clj-cn-sort-ns))))))

(defun auto-refer-update-namespace ()
  (interactive)
  (cond ((s-ends-with? ".clj" (buffer-file-name))
         (--each auto-refer-packages
           (auto-refer--update-clj-namespace it)))

        ((s-ends-with? ".cljs" (buffer-file-name))
         (--each auto-refer-packages
           (auto-refer--update-cljs-namespace it)))))

(define-minor-mode auto-refer-mode
    "Auto refer mode"
  :lighter ""
  (if auto-refer-mode
      (add-hook 'before-save-hook 'auto-refer-update-namespace t t)
    (remove-hook 'before-save-hook 'auto-refer-update-namespace t)))

(provide 'clj-auto-refer-mode)
