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
  (let ((buffer (cider-current-repl 'clj 'ensure)))
    (cider-nrepl-request:eval
     invocation
     (let ((original-buffer (current-buffer))
           (any-errors? nil))
       (lambda (response)
         (let ((showing? (get-buffer cider-run--out-buffer)))
           (nrepl-dbind-response response (value out err status)
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
             (when (and status (member "done" status) (not any-errors?) showing?)
               (run-with-timer 1 nil 'cider-run--kill-out-buffer))))))
     ns nil nil nil buffer)))

(defun cider-run--kill-out-buffer ()
  (kaocha-runner--hide-window cider-run--out-buffer)
  (kill-buffer cider-run--out-buffer))

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
