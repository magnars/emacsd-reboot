;;; admin-delux.el --- Do administrative work in clojure through Emacs -*- lexical-binding: t; -*-
;;
;; The Clojure fn should return a list of strings, keywords, nils or lists:
;;   '(:type :select  :prompt "Action: "   :choices ("add" "rename"))
;;   '(:type :text    :prompt "New name: " :initial "foo")
;;   '(:type :message :message "Creating...")
;;   '(:type :error   :message "Name already taken")
;;   '(:type :done    :message "Renamed foo -> bar")

;;; Code:

(require 'cider)
(require 'subr-x)

(defvar admin-delux-step-fn nil
  "Fully qualified Clojure fn, called as (fn history).")

(defun admin-delux--call (step-fn history)
  "Call Clojure STEP-FN with HISTORY, a list of strings. Return its plist."
  (let* ((form  (format "(%s '%S)" step-fn history))
         (resp  (cider-nrepl-sync-request:eval form (cider-current-repl 'clj 'ensure)))
         (value (nrepl-dict-get resp "value")))
    (when (or (nrepl-dict-get resp "ex") (null value))
      (user-error "admin-delux: %s"
                  (string-trim (or (nrepl-dict-get resp "err") "no value returned"))))
    (car (read-from-string value))))

(defun admin-delux--choose (resp)
  "Prompt with `completing-read' as described by RESP."
  (completing-read (or (plist-get resp :prompt) "Choose: ")
                   (plist-get resp :choices)
                   nil
                   (plist-get resp :require-match)))

(defun admin-delux--input (resp)
  "Prompt for free text as described by RESP."
  (read-string (or (plist-get resp :prompt) "Input: ")
               (plist-get resp :initial)))

;;;###autoload
(defun admin-delux-run (&optional step-fn)
  "Run the interactive loop against STEP-FN (default `admin-delux-step-fn')."
  (interactive)
  (let ((fn (or step-fn admin-delux-step-fn))
        (history '())                   ; newest first
        (done nil))
    (while (not done)
      (let ((resp (admin-delux--call fn (reverse history))))
        (pcase (plist-get resp :type)
          (:select  (push (admin-delux--choose resp) history))
          (:text    (push (admin-delux--input resp) history))
          (:message (message "%s" (plist-get resp :message))
                    (push :message-posted history))
          (:error   (message "%s" (plist-get resp :message))
                    (sit-for 1.5)
                    (pop history))       ; let the user re-answer
          (:done    (setq done t)
                    (message "%s" (or (plist-get resp :message) "Done")))
          (_ (user-error "admin-delux: unexpected response %S" resp)))))))

(defmacro admin-delux-define-command (name step-fn &optional doc)
  "Define interactive command NAME that runs the loop against STEP-FN."
  `(defun ,name ()
     ,(or doc (format "Run admin-delux against %s." step-fn))
     (interactive)
     (admin-delux-run ,step-fn)))

(provide 'admin-delux)
;;; admin-delux.el ends here
