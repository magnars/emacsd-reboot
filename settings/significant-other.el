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

(setq significant-other-find-fn
      (lambda ()
        (message "Significant other not configured for this mode.")
        nil))

(defun significant-other-jump (arg)
  (interactive "P")
  (when-let (file (funcall significant-other-find-fn))
    (cond
     ((file-exists-p file) (find-file file))
     (arg (find-file file)
          (save-buffer))
     (t (ido-find-file-in-dir (file-name-directory file))))))

(defmacro with-significant-others (binding &rest mappings)
  (declare (indent 1))
  `(setq-local
    significant-other-find-fn
    (lambda ()
      (let ((,binding (buffer-file-name)))
        (cond
         ,@(--map
            `((string-match-p ,(car it) ,binding)
              ,(cadr it))
            mappings))))))

(global-set-key (kbd "s-j") 'significant-other-jump)

(provide 'significant-other)
