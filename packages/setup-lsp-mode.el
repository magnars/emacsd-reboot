;; -*- lexical-binding: t; -*-
(use-package lsp-mode
  :hook ((clojure-mode . lsp)
         (clojurescript-mode . lsp)
         (clojurec-mode . lsp)
         (lsp-mode . lsp-enable-which-key-integration))
  :diminish " lsp"

  :bind ((:map lsp-mode-map
               ("s-l w l" . lsp-workspace-show-log)))

  :init
  (setq lsp-headerline-breadcrumb-enable nil) ;; Don't need file path in my buffer
  (setq lsp-lens-enable nil) ;; Hide clutter (reference and test counts)
  (setq lsp-enable-indentation nil) ;; use clojure-mode indentation
  (setq lsp-eldoc-enable-hover nil) ;; use CIDER eldoc
  (setq lsp-modeline-code-actions-enable nil) ;; Don't clutter modeline
  (setq lsp-modeline-diagnostics-enable nil) ;; Don't clutter modeline, jeez
  (setq lsp-completion-provider :none) ;; Skip company-mode
  (setq lsp-enable-symbol-highlighting nil) ;; Don't highlight current symbol

  (setq lsp-apply-edits-after-file-operations nil) ;; Disable broken lsp feature: https://github.com/clojure-lsp/clojure-lsp/issues/1813

  ;; To consider
  ;;
  ;; (setq lsp-enable-completion-at-point nil) ;; CIDER vs LSP?
  ;; (remove-hook 'completion-at-point-functions #'cider-complete-at-point t)

  :config
  (advice-add 'lsp--info :around #'my/silence-some-lsp-info-messages)
  (add-hook 'lsp-completion-mode-hook 'my/use-lsp-completion-only-as-fallback))

(defun my/use-lsp-completion-only-as-fallback ()
  (when (-contains? completion-at-point-functions #'lsp-completion-at-point)
    (remove-hook 'completion-at-point-functions #'tags-completion-at-point-function t)
    (remove-hook 'completion-at-point-functions #'lsp-completion-at-point t)
    (remove-hook 'completion-at-point-functions t t)
    (add-to-list 'completion-at-point-functions #'lsp-completion-at-point t)
    (add-to-list 'completion-at-point-functions t t)))

(defun my/silence-some-lsp-info-messages (orig-fn &rest args)
  (unless (or (string-equal (car args) "Connected to %s.")
              (string-equal (car args) "Disconnected"))
    (apply orig-fn args)))

(provide 'setup-lsp-mode)
