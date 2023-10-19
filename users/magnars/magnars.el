;; Custom own settings

(global-set-key (kbd "C-<f6>") (λ (with-perspective "org" (find-file "~/Dropbox/org/adventur/adventur-clj.org"))))

(require 'mattilsynet)

;; Configure pair-programming-mode
;;
;; Suggest these first:
(setq my/pair-programming-usual-suspects '("Christian Johansen <christian.johansen@mattilsynet.no>"
                                           "Christian Johansen <christian@cjohansen.no>"))
;;
;; Don't suggest these:
(setq my/pair-programming-myself '("Magnar Sveen"))
