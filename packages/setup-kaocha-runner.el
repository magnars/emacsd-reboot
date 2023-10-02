;; Kaocha Runner
;;
;; An emacs package for running Kaocha tests via CIDER.

(use-package kaocha-runner
  :after (cider-mode)
  :bind (:map clojure-mode-map
              ("C-c k t" . kaocha-runner-run-test-at-point)
              ("C-c k r" . kaocha-runner-run-tests)
              ("C-c k a" . kaocha-runner-run-all-tests)
              ("C-c k w" . kaocha-runner-show-warnings)
              ("C-c k h" . kaocha-runner-hide-windows)))

(provide 'setup-kaocha-runner)
