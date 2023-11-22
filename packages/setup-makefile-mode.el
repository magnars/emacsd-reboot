(defun my/tab-indent (n)
  (back-to-indentation)
  (delete-horizontal-space)
  (--dotimes n
    (insert "\t")))

(defun my/in-process-of-adding-commands-to-rule? ()
  "This will indicate indentation of the current line if we are
   working on a rule, but only if there is a blank line below.
   Otherwise we would add a TAB to all the blank lines between rules
   when cleaning the buffer."
  (interactive)
  (save-excursion
    (let ((current-line (thing-at-point 'line t))
          (above-line nil)
          (below-line nil))
      (forward-line -1)
      (setq above-line (thing-at-point 'line t))
      (forward-line 2)
      (setq below-line (thing-at-point 'line t))
      (and (equal "\n" current-line)  ;; Current line is blank
           (or (string-match-p "^\t" above-line)  ;; Above line is indented with a tab
               (string-match-p "^[^ \t\n#]+:" above-line))  ;; or is a rule definition
           (equal "\n" below-line)))))  ;; Below line is blank

(defun my/makefile-indent-line ()
  "Indent current line as Makefile code."
  (interactive)
  (let* ((savep (point))
         (indent-col
          (save-excursion
            (back-to-indentation)
            (when (>= (point) savep) (setq savep nil))
            (beginning-of-line)
            (cond
             ((my/in-process-of-adding-commands-to-rule?) 1)

             ((or (looking-at "^[ \t]*$")       ;; Blank line
                  (looking-at "^[ \t]*#")       ;; Comment line
                  (looking-at "^[^ \t\n#]+ ?=") ;; Variable definition
                  (looking-at "^[A-Z_]+$")      ;; Variable definition in progress
                  (looking-at "^[^ \t\n#]+:"))  ;; Rule definition
              0)
             (t 1)))))
    (if (null indent-col)
        'noindent
      (if savep
          (save-excursion (my/tab-indent indent-col))
        (my/tab-indent indent-col)))))

(add-hook 'makefile-mode-hook
          (lambda ()
            (setq-local indent-line-function 'my/makefile-indent-line)))

(defun makefile-find-targets ()
  "Find all targets in a Makefile."
  (let (targets)
    (save-excursion
      (goto-char (point-min))
      (while (re-search-forward "^\\([^:#\t\n]+?\\):" nil t)
        (let ((target (match-string-no-properties 1)))
          (unless (s-equals? target ".PHONY")
            (setq targets (cons (string-trim target) targets))))))
    (reverse targets)))

(defun shorten-path (path)
  "Shortens the file PATH by replacing the home directory with ~."
  (let ((home (expand-file-name "~")))
    (if (string-prefix-p home path)
        (concat "~" (substring path (length home)))
      path)))

(defun makefile-invoke-target ()
  (interactive)
  (let ((file (concat (projectile-project-root) "Makefile"))
        (short-dir (shorten-path (projectile-project-root)))
        (default-directory (projectile-project-root)))
    (if (file-exists-p file)
        (async-shell-command
         (completing-read (format "Make in %s: " short-dir)
                          (--map
                           (concat "make " it)
                           (with-temp-buffer
                             (insert-file-contents file)
                             (makefile-find-targets)))))
      (message "No Makefile found in %s" short-dir))))

(global-set-key (kbd "s-m") 'makefile-invoke-target)

(provide 'setup-makefile-mode)
