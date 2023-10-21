(use-package clj-refactor
  :diminish clj-refactor-mode

  :custom
  (cljr-favor-prefix-notation nil)
  (cljr-favor-private-functions nil)
  (cljr-insert-newline-after-require nil)
  (cljr-assume-language-context "clj")

  :config
  (cljr-add-keybindings-with-modifier "C-s-")
  (define-key clojure-mode-map (kbd "C-S-M-u") (λ (cljr--goto-toplevel)))
  (define-key clojure-mode-map (kbd "C->") 'cljr-thread)
  (define-key clojure-mode-map (kbd "C-<") 'cljr-unwind)

  (setq cljr-clojure-test-declaration "[clojure.test :refer [deftest is testing]]")
  (setq cljr-cljs-clojure-test-declaration cljr-clojure-test-declaration)
  (setq cljr-cljc-clojure-test-declaration cljr-clojure-test-declaration)

  (add-hook 'clojure-mode-hook 'clj-refactor-mode)

  (require 'core-async-mode)
  (add-hook 'clojure-mode-hook 'core-async-mode)

  (advice-add 'cljr-update-project-dependency :around #'my/cljr-handle-git-sha-deps))

(require 'json)

(defun my/fetch-latest-github-commits (user repo num)
  (let* ((url (format "https://api.github.com/repos/%s/%s/commits?per_page=%s" user repo num))
         (buffer (url-retrieve-synchronously url))
         (json-array-type 'list))
    (with-current-buffer buffer
      (goto-char (point-min))
      (re-search-forward "^$")
      (delete-region (point) (point-min))
      (let ((data (json-read)))
        (kill-buffer)
        (mapcar (lambda (commit)
                  (let ((sha (cdr (assoc 'sha commit)))
                        (message (cdr (assoc 'message (assoc 'commit commit))))
                        (timestamp (cdr (assoc 'date (assoc 'committer (assoc 'commit commit))))))
                    (list :sha sha :message message :timestamp timestamp)))
                data)))))

(defun my/clj-refactor-upgrade-git-sha-dep ()
  (interactive)
  (when (cljr--looking-at-dependency-p)
    (save-excursion
      (let ((kind (match-string-no-properties 2))
            (url (match-string-no-properties 4)))
        (when (and (s-equals? ":git" kind)
                   (s-contains? "github.com" url))
          (-let [(_ user repo) (s-match "github.com/\\([^/]+\\)/\\([^/.]+\\).git" url)]
            (let ((commit (car (my/fetch-latest-github-commits user repo 1))))
              (when (search-forward ":sha \"")
                (let ((old-sha (progn (looking-at "[a-z0-9]+")
                                      (match-string-no-properties 0))))
                  (if (s-equals? old-sha (plist-get commit :sha))
                      (message "Already at newest revision")
                    (progn
                      (delete-char (length old-sha))
                      (insert (plist-get commit :sha))
                      (message "Bumped to revision from %s\n%s"
                               (plist-get commit :timestamp)
                               (plist-get commit :message)))))))))))))

(defun my/cljr-handle-git-sha-deps (orig-fn &rest args)
  (unless (my/clj-refactor-upgrade-git-sha-dep)
    (apply orig-fn args)))

(provide 'setup-clj-refactor)
