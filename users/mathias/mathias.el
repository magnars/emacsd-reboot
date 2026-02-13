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

(defun show-my-kbs-to-remember ()
  "Display custom keybindings and execute selected command."
  (interactive)
  (let* ((choices (mapcar (lambda (entry)
                            (let ((key (car entry))
                                  (cmd (cadr entry))
                                  (desc (caddr entry)))
                              (cons (format "%-15s %-45s %s" key cmd desc)
                                    cmd)))
                          my-kbs-to-remember))
         (selection (completing-read "Keybinding: " choices nil t))
         (command (cdr (assoc selection choices))))
    (when command
      (call-interactively (intern command)))))

(global-set-key (kbd "C-M-S-s-r C-M-S-s-k") 'show-my-kbs-to-remember)

(defun capture-my-kbs-to-remember ()
  "Prompt for a keybinding, find its command and docstring, then save to clipboard.
The output format is ready to paste into my-kbs-to-remember."
  (interactive)
  (let* ((key (read-key-sequence "Press keybinding to capture: "))
         (key-desc (key-description key))
         (command (key-binding key))
         (docstring (when command
                      (documentation command)))
         (first-line (when docstring
                       (car (split-string docstring "\n"))))
         (output (if command
                     (format "(\"%s\" \"%s\" \"%s\")"
                             key-desc
                             command
                             (or first-line "No description"))
                   (format ";; No command bound to %s" key-desc))))
    (if command
        (progn
          (kill-new output)
          (message "Copied to clipboard: %s" output))
      (message "No command bound to %s" key-desc))))

;; TODO:
;;
;; 🔘 comp to threading
;; ✅ C-c C-M-s w/ e->map
