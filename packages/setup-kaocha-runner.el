;; Kaocha Runner
;;
;; An emacs package for running Kaocha tests via CIDER.

(use-package kaocha-runner
  :after (cider-mode)
  :commands (kaocha-runner--run-tests)
  :bind (:map clojure-mode-map
              ("C-c k t" . kaocha-runner-run-test-at-point)
              ("C-c k r" . kaocha-runner-run-tests)
              ("C-c k a" . kaocha-runner-run-all-tests)
              ("C-c k w" . kaocha-runner-show-warnings)
              ("C-c k h" . kaocha-runner-hide-windows)))

(defun kaocha-runner--is-test? ()
  (string-match-p "/test/.+\.clj" (buffer-file-name)))

(defun kaocha-runner-run-relevant-tests ()
  (interactive)
  (when (cljr--project-depends-on-p "kaocha")
    (if (kaocha-runner--is-test?)
        (kaocha-runner--run-tests
         (kaocha-runner--testable-sym (cider-current-ns) nil nil)
         nil t)
      (let ((original-buffer (current-buffer)))
        (save-window-excursion
          (let* ((file (funcall significant-other-find-fn)))
            (when (file-exists-p file)
              (find-file file)))
          (when (kaocha-runner--is-test?)
            (kaocha-runner--run-tests
             (kaocha-runner--testable-sym (cider-current-ns) nil nil)
             nil t original-buffer)))))))

(add-hook 'cider-file-loaded-hook #'kaocha-runner-run-relevant-tests)

(provide 'setup-kaocha-runner)
