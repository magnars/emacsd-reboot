;;; admin-delux.el --- Do administrative work in clojure through Emacs -*- lexical-binding: t; -*-
;;
;; The Clojure fn should return a list of strings, keywords, nils or lists:
;;   '(:type :select  :prompt "Action: "   :choices ("add" "rename"))
;;   '(:type :text    :prompt "New name: " :initial "foo")
;;   '(:type :message :message "Creating...")
;;   '(:type :error   :message "Name already taken")
;;   '(:type :submit  :exec-fn "some.namespace/function")

;;; Code:

(require 'cider)
(require 'subr-x)

(defvar admin-delux-step-fn nil
  "Fully qualified Clojure fn, called as (fn history).")

(defun admin-delux--string->keyword (s)
  (if (and (stringp s) (string-match-p "\\`:[^[:space:]]+\\'" s))
      (intern s)
    s))

(defun admin-delux--call (fn history)
  "Call Clojure FN with HISTORY, a list of strings. Return its plist."
  (let* ((form  (format "(%s '%S)" fn history))
         (resp  (cider-nrepl-sync-request:eval form (cider-current-repl 'clj 'ensure)))
         (value (nrepl-dict-get resp "value")))
    (when (or (nrepl-dict-get resp "ex") (null value))
      (user-error "admin-delux: %s"
                  (string-trim (or (nrepl-dict-get resp "err") "no value returned"))))
    value))

(defun admin-delux--select (resp)
  "Prompt with `completing-read' as described by RESP."
  (admin-delux--string->keyword
   (substring-no-properties
    (completing-read (or (plist-get resp :prompt) "Choose: ")
                     (mapcar (lambda (c) (cider-font-lock-as-clojure (format "%s" c)))
                             (plist-get resp :choices))
                     nil
                     (plist-get resp :require-match)))))

(defun admin-delux--input (resp)
  "Prompt for free text as described by RESP."
  (admin-delux--string->keyword
   (read-string (or (plist-get resp :prompt) "Input: ")
                (plist-get resp :initial))))

;;;###autoload
(defun admin-delux-run (&optional step-fn)
  "Run the interactive loop against STEP-FN (default `admin-delux-step-fn')."
  (interactive)
  (let ((fn (or step-fn admin-delux-step-fn))
        (history '())                   ; newest first
        (done nil))
    (while (not done)
      (let ((resp (thread-first (admin-delux--call fn (reverse history))
                                read-from-string
                                car)))
        (pcase (plist-get resp :type)
          (:select  (push (admin-delux--select resp) history))
          (:text    (push (admin-delux--input resp) history))
          (:message (message "%s" (plist-get resp :message))
                    (push :message-posted history))
          (:error   (message "%s" (plist-get resp :message))
                    (sit-for 1.5)
                    (pop history))       ; let the user re-answer
          (:submit  (setq done t)
                    (if-let ((exec (plist-get resp :exec-fn)))
                        (message "%s" (cider-font-lock-as-clojure
                                       (admin-delux--call exec (reverse history))))
                      (message "Finished with no exec-fn")))
          (_ (user-error "admin-delux: unexpected response %S" resp)))))))

(provide 'admin-delux)
;;; admin-delux.el ends here
