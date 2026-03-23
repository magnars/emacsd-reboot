(defun learn-kbs-filename ()
  "Return the path to the user's keybinding remember file."
  (expand-file-name "learn-kbs-keybindings.el"
                    (expand-file-name (user-login-name)
                                      (expand-file-name "users" user-emacs-directory))))

(defun learn-kbs-ensure-file ()
  "Create the keybinding file and its directory if they don't exist."
  (let ((file (learn-kbs-filename)))
    (unless (file-exists-p (file-name-directory file))
      (make-directory (file-name-directory file) t))
    (unless (file-exists-p file)
      (with-temp-file file
        (insert ";; Auto-generated — keybindings to learn\n\n"
                "(defvar learn-kbs-keybindings\n"
                "  '()\n"
                "  \"List of custom keybindings to display.\n"
                "Each entry is (KEYBINDING COMMAND DESCRIPTION).\")\n")))
    file))

(defun learn-kbs-read-entries ()
  "Read the current entries from the file."
  (let ((file (learn-kbs-ensure-file)))
    (with-temp-buffer
      (insert-file-contents file)
      (goto-char (point-min))
      (when (re-search-forward "(defvar learn-kbs-keybindings\n\\s-*'(" nil t)
        (backward-char 1)
        (let ((list-start (point)))
          (forward-sexp 1)
          (car (read-from-string
                (buffer-substring-no-properties list-start (point)))))))))

(defun learn-kbs-write-entries (entries)
  "Write ENTRIES to the keybinding file, replacing the defvar form."
  (let ((file (learn-kbs-ensure-file)))
    (with-temp-buffer
      (insert-file-contents file)
      (goto-char (point-min))
      (when (re-search-forward "(defvar learn-kbs-keybindings" nil t)
        (beginning-of-line)
        (let ((start (point)))
          (forward-sexp 1)
          (delete-region start (point))
          (insert "(defvar learn-kbs-keybindings\n"
                  "  '(\n"
                  (mapconcat (lambda (entry)
                               (format "    %S\n" entry))
                             entries
                             "")
                  "    )\n"
                  "  \"List of custom keybindings to display.\n"
                  "Each entry is (KEYBINDING COMMAND DESCRIPTION).\")\n")))
      (write-region (point-min) (point-max) file nil 'quiet))))

(defun learn-kbs-add (keybinding command description)
  "Add a keybinding entry to the persistent file.
Does nothing if an entry with the same KEYBINDING already exists."
  (let* ((entries (learn-kbs-read-entries))
         (existing (assoc keybinding entries)))
    (if existing
        (message "Entry for %s already exists, skipping." keybinding)
      (setq entries (append entries (list (list keybinding command description))))
      (learn-kbs-write-entries entries)
      ;; Keep the in-memory variable in sync
      (setq learn-kbs-keybindings entries)
      (message "Added: %s → %s (%s)" keybinding command description))))

(defun learn-kbs-show-keybindings ()
  "Display custom keybindings and execute selected command."
  (interactive)
  (if (file-exists-p (learn-kbs-filename))
      (let* ((choices (mapcar (lambda (entry)
                                (let ((key (car entry))
                                      (cmd (cadr entry))
                                      (desc (caddr entry)))
                                  (cons (format "%-15s %-45s %s" key cmd desc)
                                        cmd)))
                              (learn-kbs-read-entries)))
             (selection (completing-read "Keybinding: " choices nil t))
             (command (cdr (assoc selection choices))))
        (when command
          (call-interactively (intern command))))
    (message "No kbs found. Add keybindings with learn-kbs-capture.")))

(defun learn-kbs-capture ()
  "Prompt for a keybinding, find its command and docstring, then save to `learn-kbs-filename'."
  (interactive)
  (let* ((key (read-key-sequence "Press keybinding to capture: "))
         (key-desc (key-description key))
         (command (key-binding key)))
    (if command
        (let* ((docstring (documentation command))
               (first-line (if docstring
                               (car (split-string docstring "\n"))
                             "No description")))
          (learn-kbs-add key-desc (symbol-name command) first-line))
      (message "No command bound to %s" key-desc))))

(provide 'learn-kbs)
