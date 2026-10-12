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
(require 'parseedn)
(require 'subr-x)

(defvar admin-delux-step-fn nil
  "Fully qualified Clojure fn that takes a single argument")

(defun admin-delux--string->keyword (s)
  (if (and (stringp s) (string-match-p "\\`:[^[:space:]]+\\'" s))
      (intern s)
    s))

(defun admin-delux--unicode-symbol-char-p (orig char &optional alpha-only)
  (or (funcall orig char alpha-only)
      (and char
           (> char 127)
           (memq (get-char-code-property char 'general-category)
                 '(Lu Ll Lt Lm Lo Mn Mc)))))

(defun admin-delux--read-edn (str)
  (advice-add 'parseclj-lex-symbol-start-p :around #'admin-delux--unicode-symbol-char-p)
  (unwind-protect (parseedn-read-str str)
    (advice-remove 'parseclj-lex-symbol-start-p #'admin-delux--unicode-symbol-char-p)))

(defun admin-delux--call (fn history)
  (let* ((form  (format "(do (require '[clojure.edn])
                             (binding [*print-namespace-maps* false
                                       *print-level* nil
                                       *print-length* nil]
                               (pr-str (%s (clojure.edn/read-string %s)))))"
                        fn
                        (parseedn-print-str
                         (parseedn-print-str (list :history (vconcat history))))))
         (resp  (cider-nrepl-sync-request:eval form (cider-current-repl 'clj 'ensure)))
         (value (nrepl-dict-get resp "value")))
    (when (or (nrepl-dict-get resp "ex") (null value))
      (user-error "admin-delux: %s"
                  (string-trim (or (nrepl-dict-get resp "err") "no value returned"))))
    (admin-delux--read-edn
     (parseedn-read-str value))))

(defun admin-delux--select (resp)
  (admin-delux--string->keyword
   (substring-no-properties
    (completing-read (gethash :prompt resp "Choose: ")
                     (mapcar (lambda (c) (cider-font-lock-as-clojure (format "%s" c)))
                             (gethash :choices resp))
                     nil
                     (gethash :require-match resp)))))

(defun admin-delux--input (resp)
  (admin-delux--string->keyword
   (read-string (gethash :prompt resp "Input: ")
                (gethash :initial resp))))

;;;###autoload
(defun admin-delux-run (&optional step-fn)
  "Run the interactive loop against STEP-FN (default `admin-delux-step-fn')."
  (interactive)
  (let ((fn (or step-fn admin-delux-step-fn))
        (history '()) ; newest first
        (done nil))
    (while (not done)
      (let ((resp (admin-delux--call fn (reverse history))))
        (pcase (gethash :type resp)
          (:select  (push (admin-delux--select resp) history))
          (:text    (push (admin-delux--input resp) history))
          (:message (message "%s" (gethash :message resp))
                    (push :message-posted history))
          (:error   (message "%s" (gethash :message resp))
                    (sit-for 1.5)
                    (pop history)) ; let the user re-answer
          (:submit  (setq done t)
                    (if-let ((exec (gethash :exec-fn resp)))
                        (message "%s" (cider-font-lock-as-clojure
                                       (parseedn-print-str
                                        (admin-delux--call exec (reverse history)))))
                      (message "Finished with no exec-fn")))
          (_ (user-error "admin-delux: unexpected response %S" resp)))))))

(provide 'admin-delux)
;;; admin-delux.el ends here
