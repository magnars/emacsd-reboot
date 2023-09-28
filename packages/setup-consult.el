;; Consult
;;
;; Provides search and navigation commands based on the Emacs completion
;; function completing-read. Completion allows you to quickly select an item
;; from a list of candidates.

(use-package consult
  :ensure t
  :bind (("C-x f" . consult-recent-file)
         ("C-x C-i" . consult-imenu)
         ("C-x C-y" . consult-yank-from-kill-ring)
         ("M-g g" . consult-goto-line)
         ("M-g M-g" . consult-goto-line))

  :after (perspective)

  :config
  ;; Show only perspective-buffers with consult-buffer
  (consult-customize consult--source-buffer :hidden t :default nil)
  (add-to-list 'consult-buffer-sources persp-consult-source))

(use-package consult-flycheck
  :ensure t
  :bind (("M-g f" . consult-flycheck)))

(setq completion-in-region-function
      (lambda (&rest args)
        (apply (if vertico-mode
                   #'consult-completion-in-region
                 #'completion--in-region)
               args)))

(provide 'setup-consult)
