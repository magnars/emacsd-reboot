;; Undo Fu
;;
;; Simple, stable linear undo with redo for Emacs. Unlike undo-tree, does not
;; mess with Emacs internals. We still get visualisation of the tree structure
;; via vundo. In addition, undo-fu-session stores undo history across Emacs sessions.

(use-package undo-fu
  :ensure t

  :config
  ;; Increase undo history limits to reduce likelihood of data loss
  (setq undo-limit 400000           ; 400kb (default is 160kb)
        undo-strong-limit 3000000   ; 3mb   (default is 240kb)
        undo-outer-limit 48000000)  ; 48mb  (default is 24mb)

  :bind (([remap undo] . undo-fu-only-undo)
         ([remap redo] . undo-fu-only-redo)
         ("C-_" . undo-fu-only-undo)
         ("M-_" . undo-fu-only-redo)
         ("C-M-_" . undo-fu-only-redo-all)))

(use-package undo-fu-session
  :ensure t
  :custom (undo-fu-session-directory
           (expand-file-name "cache/undo-fu-session" user-emacs-directory))
  :init (undo-fu-session-global-mode)
  :config
  (setq undo-fu-session-incompatible-files '("\\.gpg$"
                                             "/COMMIT_EDITMSG\\'"
                                             "/git-rebase-todo\\'"))

  (when (executable-find "zstd")
    (setq undo-fu-session-compression 'zst)))

(use-package vundo
  :ensure t
  :config
  (setq vundo-glyph-alist vundo-unicode-symbols
        vundo-compact-display t)

  :bind (("C-x u" . vundo)))

(provide 'setup-undo-fu)
