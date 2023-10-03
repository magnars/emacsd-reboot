(require 'package)
(add-to-list 'package-archives '("melpa" . "http://melpa.org/packages/") t)
(package-initialize)

(setq packages-dir (expand-file-name "packages" user-emacs-directory))
(add-to-list 'load-path packages-dir)

(require 'use-package-ensure)
(setq use-package-always-ensure t)

;; Load all packages
(dolist (file (directory-files packages-dir t "\\w+"))
  (when (file-regular-p file)
    (load file)))

(provide 'packages)
