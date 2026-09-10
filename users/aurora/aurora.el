;; -*- lexical-binding: t; -*-

;; N Λ N O theme
(use-package nano-theme
  :vc (:url "https://github.com/rougier/nano-theme")
  :init
  (setq nano-light-background "#fafafa"
        nano-light-highlight "#f5f7f8"))

;; N Λ N O modeline
(use-package nano-modeline
  :vc (:url "https://github.com/rougier/nano-modeline")
  :init
  ;; Disable the default modeline
  (setq-default mode-line-format nil)
  :config
  (defun my-default-nano-modeline (&optional default)
    "My nano modeline configuration."
    (funcall nano-modeline-position
             `((nano-modeline-buffer-status)
               (nano-modeline-buffer-name) " "
               (nano-modeline-git-info))
             `((nano-modeline-cursor-position)
               (nano-modeline-window-dedicated))
             default))
  (my-default-nano-modeline 1))

;; Font
(add-to-list 'default-frame-alist '(font . "Source Code Pro"))


;;;; Org-Mode for taking notes
(use-package org
  :defer t
  :hook (org-mode . (lambda ()
                      (setq fill-column 80) ;; break lines at 80 chrs
                      (auto-fill-mode 1)))
  :config
  (setq org-pretty-entities t
        org-hide-leading-stars t
        org-hide-emphasis-markers t)
  :bind (:map org-mode-map
              ("C-c C-k" . 'save-buffer)
              ("M-<return>" . org-meta-return)
              ("C-c e" . org-latex-export-to-pdf)
              ("C-c s" . (lambda ()
                           (interactive)
                           (org-insert-structure-template "src")
                           (forward-line -1)
                           (end-of-line)))
              ("C-c v" . (lambda ()
                           (interactive)
                           (org-insert-structure-template "verse")
                           (insert "\n")
                           (forward-line -1)))
              ("C-c x" . (lambda ()
                           (interactive)
                           (org-insert-structure-template "example")
                           (forward-line -1)
                           (end-of-line)
                           (insert " -i\n")))))

;;;; LaTeX
(use-package auctex
  :defer t)

;; LaTeX Back-End for Org Export Engine
(use-package ox-latex
  :ensure nil
  :after org
  :config
  (setenv "PATH" (concat "/Library/TeX/texbin:" (getenv "PATH")))
  (add-to-list 'exec-path "/Library/TeX/texbin")
  (setq org-latex-pdf-process
        '("latexmk -xelatex -synctex=1 -shell-escape -interaction=nonstopmode -f %f"))
  (setq org-latex-listings 'minted))
