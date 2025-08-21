;; Prerequisites: rustup and rust-analyzer.x
;;
;; 1. Install Rust toochain with rustup
;;
;;      curl --proto '=https' --tlsv1.2 -sSf https://sh.rustup.rs | sh
;;
;;    Note: if you just press enter, .zprofile and .profile is automatically edited.
;;
;; 2. Install language server for Rust
;;
;;      brew install rust-analyzer

(use-package rust-mode)
(add-hook 'rust-mode-hook #'lsp-deferred)
