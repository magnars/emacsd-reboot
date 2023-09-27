(use-package yasnippet
  :ensure t
  :diminish yas-minor-mode

  :bind ((:map yas-keymap
               ("<return>" . yas-exit-all-snippets)
               ("C-e" . yas/goto-end-of-active-field)
               ("C-a" . yas/goto-start-of-active-field)))

  :config
  ;; Use only own snippets, do not use bundled ones
  (setq yas-snippet-dirs '("~/.emacs.d/snippets"))

  ;; No dropdowns please, yas
  (setq yas-prompt-functions '(yas-ido-prompt yas-completing-prompt))

  ;; No need to be so verbose
  (setq yas-verbosity 1)

  ;; Wrap around region
  (setq yas-wrap-around-region t)

  ;; Use yasnippet everywhere
  (yas-global-mode 1))

(use-package datomic-snippets :ensure t)

;; Inter-field navigation
(defun yas/goto-end-of-active-field ()
  (interactive)
  (let* ((snippet (car (yas--snippets-at-point)))
         (position (yas--field-end (yas--snippet-active-field snippet))))
    (if (= (point) position)
        (move-end-of-line 1)
      (goto-char position))))

(defun yas/goto-start-of-active-field ()
  (interactive)
  (let* ((snippet (car (yas--snippets-at-point)))
         (position (yas--field-start (yas--snippet-active-field snippet))))
    (if (= (point) position)
        (move-beginning-of-line 1)
      (goto-char position))))

;; Snippet helpers
(defun buffer-file-name-body ()
  "Buffer file name stripped of directory and extension"
  (if (buffer-file-name)
      (file-name-nondirectory (file-name-sans-extension (buffer-file-name)))
    (cadr (reverse (split-string (dired-current-directory) "/")))))

(provide 'setup-yasnippet)
