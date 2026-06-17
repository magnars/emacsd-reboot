(require 'whitespace)

;; Adds a soft demarcation when the line exceeds 92 characters on the same line. It is not a
;; hard limit, but rather an indication that you should maybe consider splitting the line in
;; two.

(setq whitespace-style '(face lines-tail))
(setq whitespace-line-column 92)

(set-face-attribute 'whitespace-line nil
                    :background "orange4"
                    :foreground 'unspecified)

(add-hook 'clojure-mode-hook       #'whitespace-mode)
(add-hook 'clojurescript-mode-hook #'whitespace-mode)
(add-hook 'clojurec-mode-hook      #'whitespace-mode)

(provide 'too-long)
