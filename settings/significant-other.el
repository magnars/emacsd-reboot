;; -*- lexical-binding: t; -*-
;; Significant Other
;;
;; Many files come in pairs, like tests and source files, header files and
;; implementations, components and their devcards.
;;
;; This impromptu package-to-be helps set up functions to jump between
;; significant other files.
;;
;; See setup-clojure-mode for example usage.

(require 'dash)

(defvar-local significant-other-find-fns ()
  "Plist of RELATION to FIND-FN for this buffer")

(defun significant-other-get-find-fn (relation)
  (or (plist-get significant-other-find-fns (or relation 'default))
      (lambda ()
        (message "Significant other not configured for this mode.")
        nil)))

(defun significant-other-find-existing (relation)
  (-first 'file-exists-p (funcall (significant-other-get-find-fn relation))))

(defun significant-other-jump-to (relation arg)
  (if-let (file (significant-other-find-existing relation))
      (find-file file)
    (when-let (file (car (funcall (significant-other-get-find-fn relation))))
      (if arg
          (progn (find-file file) (save-buffer))
        (ido-find-file-in-dir (file-name-directory file))))))

(defmacro with-significant-others (relation binding &rest mappings)
  (declare (indent 2))
  `(setq
    significant-other-find-fns
    (plist-put significant-other-find-fns
               ,relation
               (lambda ()
                 (let ((,binding (buffer-file-name)))
                   (cond
                    ,@(--map
                       `((string-match-p ,(car it) ,binding)
                         ,(cadr it))
                       mappings)))))))

(defmacro significant-other-defun (name relation)
  "Define an interactive command that jumps to the RELATION other"
  `(defun ,name (arg)
     (interactive "P")
     (significant-other-jump-to ',relation arg)))

(significant-other-defun significant-other-jump default)
(global-set-key (kbd "s-j") 'significant-other-jump)

(significant-other-defun significant-other-jump-to-portfolio portfolio)
(global-set-key (kbd "s-i") 'significant-other-jump-to-portfolio)

(provide 'significant-other)
