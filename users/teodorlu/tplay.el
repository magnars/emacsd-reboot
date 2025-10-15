;; A light Emacs interface for https://github.com/teodorlu/play.teod.eu

;; This code is based on older, messier code.
;;
;; Approach this time around: move in smaller steps. Don't copy the whole thing,
;; just the parts that are needed.

(require 's)

(defun tplay-clean ()
  (interactive)
  (let ((default-directory "~/repo/teodorlu/play.teod.eu"))
    (shell-command-to-string "./play.clj makefile && make clean && make")
    (shell-command-to-string "./play.clj reindex")))

(defun tplay-create* (page-slug title form lang body)
  "Low-level function for creating pages.

PAGE-SLUG: eg \"rudyard-kipling\"
TITLE: eg \"Rudyard Kipling\"
FORM: eg \":remote-reference\" or \"nil\"
LANG: eg \":en\" or \":no\"
BODY: nil, or the content of the document to create"
  (let ((default-directory "~/repo/teodorlu/play.teod.eu"))
    (shell-command-to-string (s-concat "./play.clj create-page"
                                       " :slug " page-slug
                                       " :title " (shell-quote-argument title)
                                       " :form " (shell-quote-argument form)
                                       " :lang " (shell-quote-argument lang)
                                       (when body
                                         (s-concat " :body " (shell-quote-argument body)))))))

(defmacro comment (&rest body) nil)

(comment
 (teod-play-create* "the-lollercoasters" "The Lollercoasters" "nil" ":en" "hello there")
 )

(defun tplay-create ()
  (interactive)
  (let* ((page-slug (read-string "Page slug: "))
         (title (read-string "Page title: "))
         (form (completing-read ":form? > " '(":remote-reference" "nil")))
         (lang (completing-read ":lang? > " '(":en" ":no")))
         (default-directory "~/repo/teodorlu/play.teod.eu"))
    (tplay-create* page-slug title form lang nil)
    (tplay-clean)
    (switch-to-buffer (find-file-noselect page-slug))))

(defun tplay-youtube-embed ()
  "Embed a youtube-video from its ID."
  (interactive)
  (let* ((youtube-video-id (read-string "Youtube video ID (like SxdOUGdseq4): ")))
    (cond ((eq major-mode 'org-mode) (insert (s-concat
                                              "#+begin_export html"
                                              "\n"
                                              "<iframe class=\"youtube-video\" src=\"https://www.youtube.com/embed/"
                                              youtube-video-id
                                              "\" title=\"YouTube video player\" frameborder=\"0\" allow=\"accelerometer; autoplay; clipboard-write; encrypted-media; gyroscope; picture-in-picture; web-share\" allowfullscreen></iframe>"
                                              "\n"
                                              "#+end_export"
                                              "\n")))
          (t (message (s-concat "sorry: youtube links for " (symbol-name major-mode) " is not yet supported."))))))

(defun tplay-link ()
  "Insert page link"
  (interactive)
  (let* ((default-directory "~/repo/teodorlu/play.teod.eu")
         (pages (s-with "bb -x tplay.linkui/page-titles"
                  shell-command-to-string
                  s-trim
                  s-lines))
         (page-title (completing-read "Select link> " pages))
         (link (s-with (concat "bb -x tplay.linkui/title-to-link" " :title " (shell-quote-argument page-title))
                 shell-command-to-string
                 s-trim)))
    (insert link)))

(require 'parseedn)

(parseedn-read-str ":select")
(parseedn-read-str "{:select 1}")
