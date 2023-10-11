;; git-commit: Ignore leading comment when inserting headers
;;
;; It is suggested several places on the internet to use a git commit template. These often include a header comment:
;;
;;    # Title: Summary, imperative, start upper case, don't end with a period
;;    # No more than 50 chars. #### 50 chars is here:  #
;;
;; This change ensures that headers are inserted after this leading comment, instead of above it.

;; This patch is made redundant when this PR is merged:
;;
;; https://github.com/magit/magit/pull/5027

(defun git-commit-insert-header (header name email)
  (setq header (format "%s: %s <%s>" header name email))
  (save-excursion
    (let ((leading-comment-end nil))
      ;; Make sure we skip forward past any leading comments
      (goto-char (point-min))
      (while (looking-at comment-start)
        (forward-line))
      (setq leading-comment-end (point))
      (goto-char (point-max))
      (cond
       ;; Look backwards for existing headers
       ((re-search-backward "^[-a-zA-Z]+: [^<\n]+? <[^>\n]+>" nil t)
        (end-of-line)
        (insert ?\n header)
        (unless (= (char-after) ?\n)
          (insert ?\n)))
       ;; Or place the new header right before the first non-leading comments
       (t
        (while (re-search-backward (concat "^" comment-start) leading-comment-end t))
        (unless (looking-back "\n\n" nil)
          (insert ?\n))
        (insert header ?\n))))
    (unless (or (eobp) (= (char-after) ?\n))
      (insert ?\n))))

(provide 'magit-header-patch)
