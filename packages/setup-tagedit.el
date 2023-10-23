(use-package tagedit
  :straight t
  :defer t)

(add-hook 'html-mode-hook 'tagedit-mode)

(with-eval-after-load 'mhtml-mode
  ;; Paredit lookalikes
  (define-key html-mode-map (kbd "s-<right>") 'tagedit-forward-slurp-tag)
  (define-key html-mode-map (kbd "C-)") 'tagedit-forward-slurp-tag)
  (define-key html-mode-map (kbd "s-<left>") 'tagedit-forward-barf-tag)
  (define-key html-mode-map (kbd "C-}") 'tagedit-forward-barf-tag)
  (define-key html-mode-map (kbd "M-r") 'tagedit-raise-tag)
  (define-key html-mode-map (kbd "s-s") 'tagedit-splice-tag)
  (define-key html-mode-map (kbd "M-S") 'tagedit-split-tag)
  (define-key html-mode-map (kbd "M-J") 'tagedit-join-tags)
  (define-key html-mode-map (kbd "M-?") 'tagedit-convolute-tags)

  ;; No paredit equivalents
  (define-key html-mode-map (kbd "M-k") 'tagedit-kill-attribute)
  (define-key html-mode-map (kbd "s-<return>") 'tagedit-toggle-multiline-tag)

  ;; Automatic insertion of <></> while typing
  (tagedit-add-experimental-features))

(provide 'setup-tagedit)
