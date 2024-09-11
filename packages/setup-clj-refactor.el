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

  (require 'clj-auto-refer-mode)
  (setq auto-refer-packages
        '((:ns "clojure.core.async"
           :functions ("<!" "<!!" ">!" ">!!" "admix" "alt!" "alt!!" "alts!" "alts!!" "buffer"
                       "chan" "close!" "do-alts" "dropping-buffer" "mix" "mult" "offer!"
                       "onto-chan" "pipe" "pipeline" "pipeline-async" "pipeline-blocking"
                       "poll!" "pub" "put!" "sliding-buffer" "solo-mode" "sub" "take!"
                       "tap" "thread" "thread-call" "timeout" "to-chan" "unblocking-buffer?"
                       "unmix" "unmix-all" "unsub" "unsub-all" "untap" "untap-all")
           :macros ("go" "go-loop")
           :cljs-ns "cljs.core.async"
           :cljs-macro-ns "cljs.core.async.macros")

          (:ns "clojure.test"
           :macros ("deftest" "testing" "is"))))
  (add-hook 'clojure-mode-hook 'auto-refer-mode)

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

(defun cljr--add-test-declarations ()
  (save-excursion
    (let* ((ns (clojure-find-ns))
           (source-ns (cljr--find-source-ns-of-test-ns ns (buffer-file-name))))
      (cljr--insert-in-ns ":require")
      (when source-ns
        (insert "[" source-ns " :as "
                (car (last (s-split "\\." source-ns))) "]"))
      (cljr--insert-in-ns ":require")
      (insert (cond
               ((cljr--project-depends-on-p "midje")
                cljr-midje-test-declaration)
               ((cljr--project-depends-on-p "expectations")
                cljr-expectations-test-declaration)
               ((cljr--cljs-file-p)
                cljr-cljs-clojure-test-declaration)
               ((cljr--cljc-file-p)
                cljr-cljc-clojure-test-declaration)
               (t cljr-clojure-test-declaration))))
    (indent-region (point-min) (point-max))))

(provide 'setup-clj-refactor)
