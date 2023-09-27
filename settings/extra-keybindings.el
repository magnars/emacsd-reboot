;; No need to kill emacs that easily.
;; The mnemonic is C-x REALLY QUIT
(global-set-key (kbd "C-x r q") 'save-buffers-kill-terminal)
(global-set-key (kbd "C-x C-c") 'delete-frame)

;; Search for more than just commands
(global-set-key (kbd "<f1> a") 'apropos)

;; Open url at point in a browser
(global-set-key (kbd "C-x M-g") 'browse-url-at-point)

(provide 'extra-keybindings)
