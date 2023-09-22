;; I don't need to kill emacs that easily
;; the mnemonic is C-x REALLY QUIT
(global-set-key (kbd "C-x r q") 'save-buffers-kill-terminal)
(global-set-key (kbd "C-x C-c") 'delete-frame)

;; Navigate between windows using shift + arrow key
(require 'windmove)
(global-set-key (kbd "S-<right>") 'windmove-right)
(global-set-key (kbd "S-<left>") 'windmove-left)
(global-set-key (kbd "S-<up>") 'windmove-up)
(global-set-key (kbd "S-<down>") 'windmove-down)

;; Completion at point
(global-set-key (kbd "C-,") 'completion-at-point)

;; Move to window after splitting
(global-set-key (kbd "C-x 3") (λ (split-window-right)
                                 (windmove-right)))

;; M-i for back-to-indentation
(global-set-key (kbd "M-i") 'back-to-indentation)

;; Use shell-like backspace C-h, rebind help to F1
(define-key key-translation-map [?\C-h] [?\C-?])
(global-set-key (kbd "<f1>") 'help-command)

;; Create new frame
(define-key global-map (kbd "C-x C-n") 'make-frame-command)

;; Bury buffer
(global-set-key (kbd "s-y") 'bury-buffer)

;; Toggle two most recent buffers
(fset 'quick-switch-buffer [?\C-x ?b return])
(global-set-key (kbd "s-b") 'quick-switch-buffer)

(global-set-key (kbd "C-x C-b") 'ibuffer)

;; Revert without any fuss
(global-set-key (kbd "M-<escape>") (λ (revert-buffer t t)))

;; Join lines with ease
(global-set-key (kbd "M-j") (λ (join-line -1)))

(global-set-key (kbd "M-p") 'backward-paragraph)
(global-set-key (kbd "M-n") 'forward-paragraph)

;; Move more quickly
(global-set-key (kbd "C-S-n") (λ (ignore-errors (forward-line 5))))
(global-set-key (kbd "C-S-p") (λ (ignore-errors (forward-line -5))))
(global-set-key (kbd "C-S-f") (λ (ignore-errors (forward-char 5))))
(global-set-key (kbd "C-S-b") (λ (ignore-errors (backward-char 5))))

;; Query replace regex key binding
(global-set-key (kbd "M-&") 'query-replace-regexp)

;; Eval buffer
(global-set-key (kbd "C-c C-k") 'eval-buffer)

;; Create scratch buffer
(global-set-key (kbd "C-c b") 'create-scratch-buffer)

(provide 'global-keybindings)
