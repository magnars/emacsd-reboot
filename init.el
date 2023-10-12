;; Add settings to load-path
(add-to-list 'load-path (expand-file-name "settings" user-emacs-directory))

;; Optimize startup of Emacs
(require 'fast-startup)

;; Keep emacs Custom-settings in separate file, not appended to init.el
(setq custom-file (expand-file-name "custom.el" user-emacs-directory))
(load custom-file)

;; Set up appearance early
(require 'appearance)

;; Configure the package manager
(require 'packages)

;; Configure Emacs for Norwegian OSX, lol
(require 'norwegian-mac)

;; Lets start with a smattering of sanity
(require 'sane-defaults)

;; Set up tooling for the rest of the configuration
(require 'tooling)

;; Add utilities
(require 'navigation)
(require 'editing)
(require 'buffers)
(require 'extra-keybindings)
(require 'indented-yank)

;; Set up Straight (for packages on github)
(require 'setup-straight)

;; Load all packages
(dolist (file (directory-files packages-dir t "^[^#].*el$"))
  (when (file-regular-p file)
    (load file)))

;; Conclude init by setting up specifics for the current user
(require 'user-specific-settings)
