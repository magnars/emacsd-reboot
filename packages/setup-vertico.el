;; Vertico
;;
;; Provides a performant and minimalistic vertical completion UI based on the
;; default completion system.

(use-package vertico
  :bind (:map vertico-map
              ("M-<return>" . vertico-exit-input))

  :init
  (vertico-mode))

(use-package vertico-directory
  :after vertico
  :ensure nil ;; comes with vertico
  ;; More convenient directory navigation commands
  :bind (:map vertico-map
              ("RET" . vertico-directory-enter)
              ("C-d" . vertico-directory-enter)
              ("DEL" . vertico-directory-delete-char)
              ("M-DEL" . vertico-directory-delete-word))

  ;; Cleans up path when moving directories with shadowed paths syntax, e.g.
  ;; cleans ~/foo/bar/// to /, and ~/foo/bar/~/ to ~/.
  :hook (rfn-eshadow-update-overlay . vertico-directory-tidy))

;; Use the `orderless' completion style.
(use-package orderless
  :init
  ;; Configure a custom style dispatcher (see the Consult wiki)
  ;; (setq orderless-style-dispatchers '(+orderless-consult-dispatch orderless-affix-dispatch)
  ;;       orderless-component-separator #'orderless-escapable-split-on-space)
  (setq completion-styles '(substring orderless basic)
        completion-category-defaults nil
        completion-category-overrides '((file (styles partial-completion)))))

;; Persist minibuffer history over Emacs restarts. Vertico sorts from history.
(use-package savehist
  :init
  (savehist-mode))

;; Enable rich annotations using the Marginalia package
(use-package marginalia
  :bind (:map minibuffer-local-map
              ("M-A" . marginalia-cycle))

  :init (marginalia-mode))

(provide 'setup-vertico)
