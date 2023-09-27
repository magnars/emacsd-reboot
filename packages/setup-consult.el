;; Consult
;;
;; Provides search and navigation commands based on the Emacs completion
;; function completing-read. Completion allows you to quickly select an item
;; from a list of candidates.

(use-package consult
  :ensure t
  :bind (("C-x f" . consult-recent-file)
         ("C-x C-i" . consult-imenu)
         ("M-g g" . consult-goto-line)
         ("M-g M-g" . consult-goto-line)))

(use-package consult-flycheck
  :ensure t
  :bind (("M-g f" . consult-flycheck)))

(provide 'setup-consult)
