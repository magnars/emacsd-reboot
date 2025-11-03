(require 'matnyttig)

;; Auto-installed clojure-lsp has given me pain, so I use Homebrew.
;; This means `which clojure-lsp` finds the clojure-lsp that Emacs calls to.
(setq lsp-clojure-custom-server-command '("clojure-lsp"))

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

(defun clerk-serve ()
  (interactive)
  (cider-interactive-eval "((requiring-resolve 'nextjournal.clerk/serve!) {:port 7799})"))

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

;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;
;; English/Norwegian spell check on Mac with Homebrew and Hunspell
;;
;; 1. Install hunspell
;;
;;    brew install hunspell
;;
;; 2. Clone the hunspell dictionary repo to a temporary location
;;
;;    dir="${HOME}/tmp/temp-$(date "+%Y-%m-%d")"
;;    mkdir -p $dir
;;    cd $dir
;;    git clone git://anongit.freedesktop.org/libreoffice/dictionaries --depth=1
;;
;; 3. Install hunspell dictionaries for your languages of choice.
;;
;;    cp -r dictionaries/en/* ~/Library/Spelling/
;;    cp -r dictionaries/no/* ~/Library/Spelling/
;;
;; 4. Set Emacs to use hunspell for spelling
;;
(setenv "DICPATH" (concat (getenv "HOME") "/Library/Spelling"))
(setq ispell-program-name "/opt/homebrew/bin/hunspell")
;;
;; 5. Validate dictionary installation
;;
;;    hunspell -D
;;
;;    You should see your installed dictionaries under AVAILABLE DICTIONARIES.
;;
;; 6. Activate flyspell-mode when you want spell checking
;;
(add-hook 'org-mode-hook (lambda () (flyspell-mode 1)))
(add-hook 'markdown-mode-hook (lambda () (flyspell-mode 1)))
;;
;; 7. Finally, when you want to change the spell checking language,
;;
;;    M-x ispell-change-dictionary

(setq cider-repl-display-help-banner nil)
(pixel-scroll-precision-mode 1)

(defun teodorlu-mblog-create-olorm ()
  "Write a new post at https://mikrobloggeriet.no/olorm/ with minimal friction!"
  (interactive)
  (projectile-run-shell-command-in-root "./mblog.sh create text/olorm"))

(defun teodorlu-dir-locals ()
  (interactive)
  (insert (prin1-to-string
           '((nil
              (cider-clojure-cli-aliases . "-A:dev")
              (cider-preferred-build-tool . clojure-cli))))))

(defun teodorlu-make ()
  (interactive)
  (projectile-run-shell-command-in-root "make"))

;; Teach projectile where to find projects
(setq projectile-project-search-path '(("~/repo" . 2) ; github projects
                                       ("~/p" . 2) ; learn stuff
                                       ))
;; Find more repos :: M-x projectile-discover-projects-in-search-path
;; Cleanup         :: M-x projectile-cleanup-known-projects

(defun teodorlu-insert-bb-edn-refer-deps-edn ()
  (interactive)
  (let* ((ancestors (thread-last (f-this-file) f-dirname f-split nreverse))
         (project (car ancestors))
         (org (cadr ancestors))
         (bb-edn-string (s-concat "{:deps {io.github."
                                  org "/" project
                                  " {:local/root \".\"}}}")))
    (insert bb-edn-string)))

(setq browse-at-remote-prefer-symbolic nil)
(global-set-key (kbd "C-x v w") 'browse-at-remote-kill)
