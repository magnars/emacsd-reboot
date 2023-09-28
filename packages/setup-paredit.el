(use-package paredit
  :hook ((clojure-mode . paredit-mode)
         (cider-repl-mode . paredit-mode)
         (emacs-lisp-mode . paredit-mode))

  :config
  ;; Rebind nasty paredit keys
  (--each '(("M-s"         "s-s"         paredit-splice-sexp)
            ("M-<up>"      "s-<up>"      paredit-splice-sexp-killing-backward)
            ("M-<down>"    "s-<down>"    paredit-splice-sexp-killing-forward)
            ("C-<right>"   "s-<right>"   paredit-forward-slurp-sexp)
            ("C-<left>"    "s-<left>"    paredit-forward-barf-sexp)
            ("C-M-<left>"  "s-S-<left>"  paredit-backward-slurp-sexp)
            ("C-M-<right>" "s-S-<right>" paredit-backward-barf-sexp))
    (-let [(original replacement command) it]
      (define-key paredit-mode-map (read-kbd-macro original) nil)
      (define-key paredit-mode-map (read-kbd-macro replacement) command)))

  ;; Make paredit work with delete-selection-mode
  (put 'paredit-forward-delete 'delete-selection 'supersede)
  (put 'paredit-backward-delete 'delete-selection 'supersede)
  (put 'paredit-newline 'delete-selection t)

  :bind ((:map paredit-mode-map
               ("C-w" . paredit-kill-region-or-backward-word)
               ("M-C-<backspace>" . backward-kill-sexp)
               ("C-d" . paredit-forward-delete)
               ("M-(" . paredit-wrap-round)
               ("M-)" . paredit-wrap-round-from-behind)
               ("M-s-8" . paredit-wrap-square)
               ("M-s-9" . paredit-wrap-square-from-behind)
               ("M-s-(" . paredit-wrap-curly)
               ("M-s-)" . paredit-wrap-curly-from-behind))))

(defun paredit-wrap-round-from-behind ()
  (interactive)
  (forward-sexp -1)
  (paredit-wrap-round)
  (insert " ")
  (forward-char -1))

(defun paredit-wrap-square-from-behind ()
  (interactive)
  (forward-sexp -1)
  (paredit-wrap-square))

(defun paredit-wrap-curly-from-behind ()
  (interactive)
  (forward-sexp -1)
  (paredit-wrap-curly))

(defun paredit-kill-region-or-backward-word ()
  (interactive)
  (if (region-active-p)
      (kill-region (region-beginning) (region-end))
    (paredit-backward-kill-word)))

(provide 'setup-paredit)
