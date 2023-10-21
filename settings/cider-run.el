;;; cider-run.el --- A package for running commands via CIDER. -*- lexical-binding: t; -*-

(require 'kaocha-runner)
(require 'projectile)

(defvar cider-run--out-buffer "*cider-run-output*")

(defun cider-run--find-dev-namespace ()
  "Find and return the first Clojure namespace in dev.clj files under dev/ and its subdirectories."
  (let ((project-root (projectile-project-root))
        (ns-regex "^(ns \\([^\s]+\\)"))
    (when project-root
      (let ((find-cmd (format "find %sdev -type f -name dev.clj" project-root))
            ns)
        (with-temp-buffer
          (call-process-shell-command find-cmd nil t)
          (goto-char (point-min))
          (while (and (not ns) (not (eobp)))
            (let ((file (buffer-substring-no-properties (line-beginning-position) (line-end-position))))
              (with-temp-buffer
                (insert-file-contents file)
                (goto-char (point-min))
                (when (re-search-forward ns-regex nil t)
                  (setq ns (match-string 1)))))
            (forward-line 1)))
        (s-trim ns)))))

(defun cider-run--run-in-repl (ns invocation)
  (interactive)
  (kaocha-runner--clear-buffer cider-run--out-buffer)
  (cider-nrepl-request:eval
   invocation
   (let ((original-buffer (current-buffer))
         (any-errors? nil))
     (lambda (response)
       (nrepl-dbind-response response (value out err status)
         (when (or out err)
           (kaocha-runner--insert cider-run--out-buffer (or out err))
           (kaocha-runner--with-window cider-run--out-buffer original-buffer
             (window-resize nil (- (max 6
                                        (min 15 (line-number-at-pos (point-max))))
                                   (window-height)))
             (goto-char (point-max))
             (recenter (- -1 (min (max 0 scroll-margin)
                                  (truncate (/ (window-body-height) 4.0)))) t)))
         (when err
           (setq any-errors? t))
         (when value
           (message "%s" value))
         (when (and status (member "done" status) (not any-errors?))
           (run-with-timer 1 nil 'kaocha-runner--hide-window cider-run--out-buffer)))))
   ns nil nil nil
   (cider-current-repl 'clj 'ensure)))

(defun cider-run-in-dev-namespace ()
  (interactive)
  (if-let ((dev-ns (cider-run--find-dev-namespace)))
      (cider-run--run-in-repl dev-ns
                              (completing-read (format "Eval in %s: " dev-ns)
                                               cider-run-in-dev-namespace-default-suggestions))
    (message "No dev namespace found")))

(defvar cider-run-in-dev-namespace-default-suggestions
  '("(start)" "(reset)"))

(provide 'cider-run)
