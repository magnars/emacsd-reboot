;;; admin-delux.el --- Do administrative work in clojure through Emacs -*- lexical-binding: t; -*-

;; Copyright (C) 2026 Mathias Iversen

;; Author: Mathias Iversen <mathias.iversen487@gmail.com>
;; Maintainer: Mathias Iversen <mathias.iversen487@gmail.com>
;; URL: https://codeberg.org/boosja/admin-delux.el
;; Version: 1.0.0
;; Package-Requires: ((emacs "27.1") (cider "1.0") (parseedn "1.2"))
;; Keywords: tools, convenience, clojure, cider

;; This program is free software; you can redistribute it and/or modify
;; it under the terms of the GNU General Public License as published by
;; the Free Software Foundation, either version 3 of the License, or
;; (at your option) any later version.

;; This program is distributed in the hope that it will be useful,
;; but WITHOUT ANY WARRANTY; without even the implied warranty of
;; MERCHANTABILITY or FITNESS FOR A PARTICULAR PURPOSE.  See the
;; GNU General Public License for more details.

;; You should have received a copy of the GNU General Public License
;; along with this program.  If not, see <http://www.gnu.org/licenses/>.

;;; Commentary:

;; The Clojure fn should return a map of strings, keywords, nils or lists:
;;   '{:type :select  :prompt "Action: "   :options ["add" "rename"] :require-match true}
;;   '{:type :text    :prompt "New name: " :initial "foo"}
;;   '{:type :message :message "Creating..."}
;;   '{:type :error   :message "Name already taken"}
;;   '{:type :submit  :exec-fn "some.namespace/function"}

;;; Code:

(require 'cider)
(require 'parseedn)
(require 'subr-x)

(defvar admin-delux-step-fn nil
  "Fully qualified Clojure fn that takes a single argument.
Set per project in .dir-locals.el.")

(defun admin-delux--string->keyword (s)
  "Parse string keywords back to keywords"
  (if (and (stringp s) (string-match-p "\\`:[^[:space:]]+\\'" s))
      (intern s)
    s))

(defun admin-delux--unicode-symbol-char-p (orig char &optional alpha-only)
  "Check for unicode char"
  (or (funcall orig char alpha-only)
      (and char
           (> char 127)
           (memq (get-char-code-property char 'general-category)
                 '(Lu Ll Lt Lm Lo Mn Mc)))))

(defun admin-delux--read-edn (str)
  "Read STR as edn and parse it"
  (advice-add 'parseclj-lex-symbol-start-p :around #'admin-delux--unicode-symbol-char-p)
  (unwind-protect (parseedn-read-str str)
    (advice-remove 'parseclj-lex-symbol-start-p #'admin-delux--unicode-symbol-char-p)))

(defun admin-delux--call (fn history)
  "Call FN in Clojure with HISTORY as argument"
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
  "Prompt for a selection of one of options"
  (admin-delux--string->keyword
   (substring-no-properties
    (completing-read (gethash :prompt resp "Choose: ")
                     (mapcar (lambda (c) (cider-font-lock-as-clojure (format "%s" c)))
                             (gethash :options resp))
                     nil
                     (gethash :require-match resp)))))

(defun admin-delux--input (resp)
  "Prompt for text input"
  (admin-delux--string->keyword
   (read-string (gethash :prompt resp "Input: ")
                (gethash :initial resp))))

;;;###autoload
(defun admin-delux-run (&optional step-fn)
  "Run the interactive loop against STEP-FN (default `admin-delux-step-fn')."
  (interactive)
  (let ((fn (or step-fn admin-delux-step-fn (user-error "admin-delux: no step-fn configured")))
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
                    (if-let* ((exec (gethash :exec-fn resp)))
                        (message "%s" (cider-font-lock-as-clojure
                                       (parseedn-print-str
                                        (admin-delux--call exec (reverse history)))))
                      (message "Finished with no exec-fn")))
          (_ (user-error "admin-delux: unexpected response %S" resp)))))))

(provide 'admin-delux)
;;; admin-delux.el ends here
