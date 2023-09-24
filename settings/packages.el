(require 'package)
(add-to-list 'package-archives '("melpa" . "http://melpa.org/packages/") t)
(package-initialize)

(add-to-list 'load-path (expand-file-name "packages" user-emacs-directory))

(require 'setup-clojure-mode)
(require 'setup-dired)
(require 'setup-expand-region)
(require 'setup-hippie)
(require 'setup-lsp-mode)
(require 'setup-magit)
(require 'setup-multiple-cursors)
(require 'setup-paredit)

(provide 'packages)
