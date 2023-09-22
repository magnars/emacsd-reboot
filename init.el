;; Add settings to load-path
(add-to-list 'load-path (expand-file-name "settings" user-emacs-directory))

;; Keep emacs Custom-settings in separate file, not appended to init.el
(setq custom-file (expand-file-name "custom.el" user-emacs-directory))
(load custom-file)

;; Set up appearance early
(require 'appearance)

;; Configure Emacs for Norwegian OSX, lol
(require 'norwegian-mac)

;; Lets start with a smattering of sanity
(require 'sane-defaults)

;; Set up tooling for the rest of the configuration
(require 'tooling)

;; Set up global keybindings
(require 'global-keybindings)

;; Add utilities
(require 'navigation)
(require 'editing)
(require 'buffers)

;; Load packages
(require 'packages)
