(deftheme daylight)

;; A greyscale theme for e-paper displays
(custom-theme-set-faces
 'daylight
 '(default ((t (:inherit nil :stipple nil :background "white" :foreground "black" :inverse-video nil :box nil :strike-t*hrough nil :overline nil :underline nil :slant normal :weight normal :width normal :height 150))))

 '(highlight ((nil (:background "#cccccc"))))
 '(hl-line ((nil (:background "#dddddd"))))
 '(isearch ((nil (:background "#dddddd"))))
 '(js2-function-param-face ((t (:foreground "303030"))))
 '(match ((nil (:background "#cccccc"))))
 '(region ((nil (:background "#505050"))))
 '(tooltip ((nil (:background "#cccccc"))))
 '(yas-field-highlight-face ((nil (:background "#aaaaaa"))))

 '(font-lock-builtin-face ((nil (:foreground "#606060" :weight bold))))
 '(font-lock-comment-face ((nil (:foreground "#606060" :slant italic))))
 '(font-lock-constant-face ((nil (:foreground "#303030" :weight bold))))
 '(font-lock-function-name-face ((nil (:foreground "#303030" :weight bold))))
 '(font-lock-keyword-face ((nil (:foreground "#606060" :weight bold))))
 '(font-lock-regexp-face ((nil (:foreground "#303030" :slant italic))))
 '(font-lock-string-face ((nil (:foreground "#303030"))))
 '(font-lock-type-face ((nil (:foreground "#303030"))))
 '(font-lock-warning-face ((nil (:foreground "#303030" :weight bold))))

 '(clojure-character-face ((nil (:foreground "#303030"))))
 '(clojure-keyword-face ((nil (:foreground "#303030"))))
 '(outline-1 ((nil (:foreground "#101010"))))
 '(outline-2 ((nil (:foreground "#202020"))))
 '(outline-3 ((nil (:foreground "#303030"))))
 '(outline-4 ((nil (:foreground "#404040"))))
 '(outline-5 ((nil (:foreground "#505050"))))
 '(outline-6 ((nil (:foreground "#606060"))))
 '(outline-7 ((nil (:foreground "#707070"))))
 '(outline-8 ((nil (:foreground "#808080"))))
 '(show-paren-match ((nil (:background "#999999"))))
 '(show-paren-mismatch ((nil (:foreground "white" :background "black"))))
 '(success ((nil (:foreground "#505050"))))
 )

(provide-theme 'daylight)
