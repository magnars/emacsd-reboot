(require 'mattilsynet)

;; Don't auto-wrap lines, I like one line per sentence
;; https://sive.rs/1s
(remove-hook 'markdown-mode-hook 'auto-fill-mode)

;; A quicker save is helpful for non-interactive languages that require saving files to reload code
(global-set-key (kbd "C-ø") 'save-buffer)

(defun teodorlu-magnar-emacs-cheat-sheet ()
  (interactive)
  (find-file (expand-file-name "~/.emacs.d/README.md")))

(defun teodorlu-today ()
  (interactive)
  (insert (format-time-string "%Y-%m-%d")))

(defun teodorlu-current-time ()
  (shell-command-to-string "echo -n `date \"+%H:%M\"`"))

(defun teodorlu-now ()
  (interactive)
  (insert (shell-command-to-string "echo -n `date \"+%H:%M\"`")))

(defun teodorlu-insert-en-dash ()
  (interactive)
  (insert "-"))

(defun teodorlu-insert-em-dash ()
  (interactive)
  (insert "—"))

(defun teodorlu-add-clerk ()
  (interactive)
  (cider-interactive-eval "(clojure.repl.deps/add-lib 'io.github.nextjournal/clerk)"))

(defun teodorlu-add-kaocha ()
  (interactive)
  (cider-interactive-eval "(clojure.repl.deps/add-lib 'lambdaisland/kaocha)"))

(defun clerk-tap-viewer ()
  (interactive)
  (cider-interactive-eval "(nextjournal.clerk/show! 'nextjournal.clerk.tap)"))

(defun clerk-serve-browse ()
  (interactive)
  (cider-interactive-eval "((requiring-resolve 'nextjournal.clerk/serve!) {:browse true :port 7799})"))

(defun clerk-halt ()
  (interactive)
  (cider-interactive-eval "(nextjournal.clerk/halt!)"))

(defun teodorlu-clojure-remove-namespace ()
  (interactive)
  (cider-interactive-eval "(remove-ns (symbol (str *ns*)))"))

(defun teodorlu-garden-deploy ()
  (interactive)
  (projectile-run-shell-command-in-root "garden deploy"))

(use-package clay)
(use-package go-mode)
;; Husk å installere gopls
;; go install golang.org/x/tools/gopls@latest
(add-hook 'go-mode-hook 'lsp-deferred)

