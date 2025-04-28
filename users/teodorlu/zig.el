;; Zig is still not released as a stable release. Certain Internet guides
;; recommending following the latest unstable Zig release. I choose to follow
;; the latest tagged release instead. For that, we can install with Homebrew.
;;
;; Prerequisites:
;;
;;    brew install zig
;;    brew install zls

(use-package zig-mode)
(require 'lsp-mode)
(add-hook 'zig-mode-hook #'lsp-deferred)
