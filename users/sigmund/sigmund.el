;; Custom own settings

;;Configure pair-programming-mode
(setq my/pair-programming-myself '("Sigmund Hansen"))

;; PlantUML
(use-package plantuml-mode)
(use-package flycheck-plantuml)

(setq plantuml-jar-path
      (concat (expand-file-name "~/bin/")
              "plantuml.jar"))
(setq plantuml-default-exec-mode 'jar)
(add-to-list 'auto-mode-alist '("\\.plantuml\\'" . plantuml-mode))

(with-eval-after-load 'org
  (add-to-list 'org-src-lang-modes '("plantuml" . plantuml)))

(with-eval-after-load 'flycheck
  (require 'flycheck-plantuml)
  (flycheck-plantuml-setup))

;; Side by side ediff
(setq ediff-split-window-function 'split-window-vertically)

;; Disable active themes when loading a different one
(defun disable-all-themes ()
  "disable all active themes."
  (dolist (theme custom-enabled-themes)
    (disable-theme theme)))

(defadvice load-theme (before disable-themes-first activate)
  (disable-all-themes))
