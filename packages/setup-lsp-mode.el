;; -*- lexical-binding: t; -*-
(when (not (string= "android" system-type))
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
    (add-hook 'lsp-completion-mode-hook 'my/use-lsp-completion-only-as-fallback)))

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

;; Try CIDER, then lsp-mode
(defun my/cider-lsp-xref-backend ()
  "Union xref backend: try CIDER, then lsp-mode"
  'cider-lsp)

(cl-defmethod xref-backend-identifier-at-point ((_backend (eql cider-lsp)))
  ;; Only used by xref for prompting/defaults - never reused below.
  (or (ignore-errors (xref-backend-identifier-at-point 'cider))
      (ignore-errors (xref-backend-identifier-at-point 'xref-lsp))))

(cl-defmethod xref-backend-definitions ((_backend (eql cider-lsp)) _identifier)
  (or (and (fboundp 'cider--xref-backend) (cider--xref-backend)
           (ignore-errors
             (xref-backend-definitions 'cider (xref-backend-identifier-at-point 'cider))))
      (and (bound-and-true-p lsp-mode)
           (ignore-errors
             (xref-backend-definitions 'xref-lsp (xref-backend-identifier-at-point 'xref-lsp))))))

(cl-defmethod xref-backend-references ((_backend (eql cider-lsp)) _identifier)
  (or (and (fboundp 'cider--xref-backend) (cider--xref-backend)
           (ignore-errors
             (xref-backend-references 'cider (xref-backend-identifier-at-point 'cider))))
      (and (bound-and-true-p lsp-mode)
           (ignore-errors
             (xref-backend-references 'xref-lsp (xref-backend-identifier-at-point 'xref-lsp))))))

(defun my/setup-cider-lsp-xref ()
  (remove-hook 'xref-backend-functions #'cider--xref-backend t)
  (remove-hook 'xref-backend-functions #'lsp--xref-backend t)
  (add-hook 'xref-backend-functions #'my/cider-lsp-xref-backend nil t))

(add-hook 'cider-mode-hook #'my/setup-cider-lsp-xref)
(add-hook 'lsp-mode-hook #'my/setup-cider-lsp-xref)

(provide 'setup-lsp-mode)
