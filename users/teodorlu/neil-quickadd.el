;; Copied from https://github.com/teodorlu/neil-quickadd/blob/8c6c4a1b6a17acc52f1a26616358a2952246cb3e/neil-quickadd.el#L22

(require 'projectile)
(require 's)

(defun neil-quickadd--binary ()
  "Which neil binary name to use"
  "neil-quickadd")

(defun neil-quickadd ()
  "Add a deps.edn dependency."
  (interactive)
  (let* ((libs (s-lines (s-trim (shell-command-to-string (s-concat (neil-quickadd--binary) " libs")))))
         (selected (completing-read "add lib > " libs)))
    (projectile-run-shell-command-in-root (s-concat "neil dep add " selected))))

(defun neil-quickadd-blacklist ()
  "Blacklist a deps.edn dependency."
  (interactive)
  (let* ((libs (s-lines (s-trim (shell-command-to-string (s-concat (neil-quickadd--binary) " libs")))))
         (selected (completing-read "blacklist lib > " libs)))
    (shell-command-to-string (s-concat (neil-quickadd--binary) " blacklist-lib " selected))))

(defun neil-quickadd-upgrade ()
  "Upgrade deps.edn dependencies."
  (interactive)
  (projectile-run-shell-command-in-root "neil dep upgrade"))

(provide 'neil-quickadd)
