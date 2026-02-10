;; -*- lexical-binding: t; -*-
;; cider-run.el --- A package for running commands via CIDER.

(require 'kaocha-runner)
(require 'projectile)

(defvar cider-run--out-buffer "*cider-run-output*")
(defvar ns-regex "^(ns \\([^\s]+\\)")

(defun cider-run--find-comment-forms ()
  (when (re-search-forward "^(comment ;; s-:" nil t)
    (let (forms beg end)
      (while (ignore-errors
               (forward-sexp) (setq end (point))
               (backward-sexp) (setq beg (point))
               (!cons (buffer-substring-no-properties beg end) forms)
               (forward-sexp)
               t))
      forms)))

(defun cider-run--find-dev-namespace-and-comment-forms ()
  "Find and return the first Clojure namespace in dev.clj files under dev/ and its subdirectories."
  (let ((project-root (projectile-project-root)))
    (when project-root
      (let ((find-cmd (format "find %sdev -type f -name dev.clj" project-root))
            ns
            forms)
        (with-temp-buffer
          (call-process-shell-command find-cmd nil t)
          (goto-char (point-min))
          (while (and (not ns) (not (eobp)))
            (let ((file (buffer-substring (line-beginning-position) (line-end-position))))
              (with-temp-buffer
                (insert-file-contents file)
                (goto-char (point-min))
                (when (re-search-forward ns-regex nil t)
                  (setq ns (match-string 1))
                  (setq forms (cider-run--find-comment-forms)))))
            (forward-line 1)))
        (cons (s-trim ns)
              forms)))))

(defun cider-run--run-in-repl (ns invocation)
  (interactive)
  (kaocha-runner--clear-buffer cider-run--out-buffer)
  (let ((buffer (cider-current-repl 'clj 'ensure)))
    (cider-nrepl-request:eval
     invocation
     (let ((original-buffer (current-buffer))
           (any-errors? nil)
           (done? nil))
       (lambda (response)
         (let ((showing? (get-buffer cider-run--out-buffer)))
           (nrepl-dbind-response response (value out err status)
             (unless done?
               (when (and (or out err) showing?)
                 (with-current-buffer cider-run--out-buffer
                   (insert (propertize (or out err) 'face
                                       (if out
                                           'cider-repl-stdout-face
                                         'cider-repl-stderr-face))))
                 (kaocha-runner--with-window cider-run--out-buffer original-buffer
                   (window-resize nil (- (max 6
                                              (min 15 (1+ (line-number-at-pos (point-max)))))
                                         (window-height)))
                   (goto-char (point-max))
                   (recenter (- -1 (min (max 0 scroll-margin)
                                        (truncate (/ (window-body-height) 4.0)))) t)))
               (when out
                 (ignore-errors
                   (cider-repl-emit-stdout buffer out)))
               (when err
                 (setq any-errors? t)
                 (ignore-errors
                   (cider-repl-emit-stderr buffer err)))
               (when value
                 (message "%s" value))
               (when (and status (member "done" status))
                 (setq done? t)
                 (when (and (not any-errors?) showing?)
                   (run-with-timer 1 nil 'cider-run--kill-out-buffer))))))))
     ns nil nil nil buffer)))

(defun cider-run--kill-out-buffer ()
  (kaocha-runner--hide-window cider-run--out-buffer)
  (kill-buffer cider-run--out-buffer))

(defun cider-run-in-dev-namespace ()
  (interactive)
  (if-let ((ns+forms (cider-run--find-dev-namespace-and-comment-forms)))
      (cider-run--run-in-repl (car ns+forms)
                              (completing-read (format "Eval in %s: " (car ns+forms))
                                               (or (cdr ns+forms)
                                                   cider-run-in-dev-namespace-default-suggestions)))
    (message "No dev namespace found")))

(defvar cider-run-in-dev-namespace-default-suggestions
  '("(start)" "(reset)"))

(provide 'cider-run)
