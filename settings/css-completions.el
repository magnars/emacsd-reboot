(require 'cider)
(require 'dash)
(require 's)
(require 'projectile)

(defun cssc/find-css-files-in-project ()
  (let ((root (projectile-project-root)))
    (->> (projectile-dir-files root)
         (--filter (s-ends-with? ".css" it))
         (--map (concat root it)))))

(defun cssc/read-file-into-string (filename)
  (with-temp-buffer
    (insert-file-contents filename)
    (buffer-string)))

(defun cssc/read-project-css-file-contents ()
  (apply #'concat
         (-map #'cssc/read-file-into-string
               (cssc/find-css-files-in-project))))

(setq cssc/well-known-file-endings
      '("jpg" "jpeg" "png" "svg"))

(defun cssc/extract-css-class-names (string)
  "Extract all CSS class names from STRING."
  (let ((pattern "\\.[a-zA-Z][a-zA-Z0-9_-]+\\b")
        (pos 0)
        (hash (make-hash-table :test 'equal)))
    (while (string-match pattern string pos)
      (setq pos (match-end 0))
      (puthash (substring (match-string 0 string) 1) t hash))
    (--each cssc/well-known-file-endings
      (remhash it hash))
    (hash-table-keys hash)))

(defun cssc/inside-clojure-class-structure? ()
  (or
   ;; matches {:class :bar}
   (save-excursion (ignore-errors (paredit-backward 2)
                                  (looking-at ":class")))
   ;; matches {:class [:bar]}
   (save-excursion (ignore-errors (paredit-backward-up)
                                  (paredit-backward)
                                  (looking-at ":class")))
   ;; matches (into class [:bar])
   (save-excursion (ignore-errors (paredit-backward-up 2)
                                  (paredit-backward)
                                  (looking-at ":class")))))

(defun cssc/find-hiccup-class-name-position ()
  "Are we completing a Clojure keyword with dots in it, like :div.foo.bar ?"
  (when-let ((bounds (bounds-of-thing-at-point 'symbol)))
    (when (not (or (cider-in-string-p)
                   (cider-in-comment-p)))
      (let ((s (buffer-substring-no-properties (car bounds)
                                               (cdr bounds))))
        (when (s-starts-with? ":" s)
          (when-let ((last-dot-index (string-match-p "\\.[^\\.]+$" s)))
            (list
             (+ last-dot-index (car bounds))
             (cdr bounds)
             ".")))))))

(defun cssc/find-keyword-class-name-position ()
  "Are we completing a Clojure keyword inside some structure assigning to :class?"
  (when-let ((bounds (bounds-of-thing-at-point 'symbol)))
    (when (not (or (cider-in-string-p)
                   (cider-in-comment-p)))
      (let ((s (buffer-substring-no-properties (car bounds)
                                               (cdr bounds))))
        (when (and (s-starts-with? ":" s)
                   (cssc/inside-clojure-class-structure?))
          (list
           (car bounds)
           (cdr bounds)
           ":"))))))

(defun cssc/find-string-class-name-position ()
  "Are we completing a symbol in a string inside some structure assigning to :class?"
  (when-let ((bounds (bounds-of-thing-at-point 'symbol)))
    (when (and (cider-in-string-p)
               (cssc/inside-clojure-class-structure?))
      (list
       (car bounds)
       (cdr bounds)
       ""))))

(defun cssc/find-html-class-name-position ()
  (when-let ((bounds (bounds-of-thing-at-point 'symbol)))
    (when (and (nth 3 (syntax-ppss))
               (save-excursion
                 (goto-char (nth 8 (syntax-ppss)))
                 (looking-back "class=" (- (point) 10))))
      (list
       (car bounds)
       (cdr bounds)
       ""))))

(defvar cssc/find-class-name-position-fns ())
(make-variable-buffer-local 'cssc/find-class-name-position-fns)

(defun cssc/css-classes-completion-at-point ()
  (when-let ((res (--some (funcall it) cssc/find-class-name-position-fns)))
    (-let [(beg end prefix) res]
      (list beg end
            (--map (concat prefix it)
                   (cssc/extract-css-class-names
                    (cssc/read-project-css-file-contents)))))))

(defun cssc/enable-for-clojure ()
  "Sets up current buffer for clojure css completions. Run in a hook."
  (add-to-list 'completion-at-point-functions
               #'cssc/css-classes-completion-at-point)
  (setq cssc/find-class-name-position-fns
        (list #'cssc/find-hiccup-class-name-position
              #'cssc/find-keyword-class-name-position
              #'cssc/find-string-class-name-position)))

(defun cssc/enable-for-html ()
  (add-to-list 'completion-at-point-functions
               #'cssc/css-classes-completion-at-point)
  (setq cssc/find-class-name-position-fns
        (list #'cssc/find-html-class-name-position)))

(provide 'css-completions)
