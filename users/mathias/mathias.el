;; theme
;; (use-package vscode-dark-plus-theme
;;   :ensure t
;;   :config
;;   (load-theme 'vscode-dark-plus t))

;; Zig
(load
 (expand-file-name "users/teodorlu/zig.el" user-emacs-directory))

(global-set-key (kbd "M-<left>") 'beginning-of-buffer)
(global-set-key (kbd "M-<right>") 'end-of-buffer)

;; Always split to the right
(setq split-height-threshold nil)

(use-package helpful)
(global-set-key (kbd "<f1> f") #'helpful-callable)
(global-set-key (kbd "<f1> v") #'helpful-variable)
(global-set-key (kbd "<f1> k") #'helpful-key)
(global-set-key (kbd "<f1> x") #'helpful-command)

;;

(defvar my-kbs-to-remember
  '(("C-c M-w" "my/cider-eval-to-clipboard" "Evaluate the Clojure form at point and put the result on the clipboard.")
    ("C-c C-M-w" "my/cider-eval-defun-to-clipboard" "Evaluate the current top-level form and copy the result to the clipboard.")
    ("C-c C-M-s" "matnyttig-cider-eval-def-symbol-with-e->map" "Evaluate and pprint symbol of top-level def (with e->map wrapped)")
    ("C-x v w" "b-a-r-k-commit" "Creates a link to Github for current line or selection using current commit")
    ("C-x v W" "b-a-r-k-branch" "Creates a link to Github for current line or selection using current branch")
    ("C-s-i C-s-m" "my/cider-add-indent-metadata-to-function" "Add ^{:indent 1} metadata to the function definition at point.")
    ("C-s-i C-s-l" "cljr-introduce-let" "Create a let form, binding the form at point.")
    ("C-s-e C-s-l" "cljr-expand-let" "Expand the let form above point by one level.")
    ("C-s-m C-s-l" "cljr-move-to-let" "Move the form at point to a binding in the nearest let."))
  "List of custom keybindings to display.
Each entry is (KEYBINDING . COMMAND . DESCRIPTION).")

(global-set-key (kbd "C-M-S-s-r C-M-S-s-k") 'show-my-kbs-to-remember)

;; TODO:
;;
;; 🔘 comp to threading
;; ✅ C-c C-M-s w/ e->map
