(use-package htmlize :defer t)

(defun wte--unique-filename (stub &optional index)
  (setq index (or index 1))
  (let ((filename (concat "~/stuff/what-the-emacsd/resources/posts/"
                          stub
                          ".el"
                          (if (< index 10) "-0" "-")
                          (number-to-string index)
                          ".html")))
    (if (file-exists-p filename)
        (wte--unique-filename stub (1+ index))
      filename)))

(defun what-the-emacsd-post (beg end)
  (interactive "r")
  (let ((example (with-current-buffer (htmlize-region beg end)
                    (search-forward "<pre>")
                    (setq beg (point))
                    (search-forward "</pre>")
                    (forward-char -7)
                    (buffer-substring beg (point))))
         (filename (wte--unique-filename (buffer-file-name-body)))
         (timestamp (floor (time-to-seconds (current-time)))))
    (find-file filename)
    (insert (format "<!-- %S -->

<p></p>

<hr/>

<pre class=\"code-snippet\">%s</pre>

<hr/>

<p></p>
" timestamp example))
    (goto-char 25)
    (local-set-key (kbd "C-c C-c") 'what-the-emacsd-publish)
    (local-set-key (kbd "C-c C-q") 'what-the-emacsd-done)))

(defun what-the-emacsd-publish ()
  (interactive)
  (let ((filename (file-name-nondirectory (buffer-file-name))))
    (save-buffer)
    (compile
     (format "git add %s && git commit -m %S && git push && curl -i http://whattheemacsd.com/site/build"
             filename
             filename) t)))

(defun what-the-emacsd-done ()
  (interactive)
  (when-let ((prev my/previous-window-configuration))
    (when prev (register-val-jump-to prev nil))))

(wrap-fullscreen what-the-emacsd-post :what-the-emacsd)

(provide 'what-the-emacs-d)
