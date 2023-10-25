;; Edit multiple i18n.edn files at once with multifile.el

(require 'multifiles)
(require 'tooling)
(require 's)

(defun i18n-edn--find-current-i18n-header ()
  (save-excursion
    (unless (looking-at "#:")
      (unless (search-backward "#:" nil t)
        (when (search-forward "#:" nil t)
          (forward-char -2))))
    (when (looking-at "#:")
      (let* ((beg (point))
             (end (progn (paredit-forward) (point))))
        (buffer-substring-no-properties beg end)))))

(defun i18n-edn--find-files-with-same-extension ()
  "Find all files in the current directory with the same extension as the current file."
  (let* ((current-file (buffer-file-name))
         (current-dir (file-name-directory current-file))
         (extension (file-name-extension current-file))
         (search-pattern (concat "\\." extension "$")))
    (directory-files current-dir nil search-pattern)))

(defun i18n-edn--cleanup ()
  (when (get-buffer "*i18n-multifile*")
    (with-current-buffer "*i18n-multifile*"
      (delete-region (point-min) (point-max)))
    (kill-buffer "*i18n-multifile*")))

(defvar i18n-heading
  ";; Editing files in %s
;;
;; C-x C-s to save all buffers, but keep this open.
;; C-c C-c to save all buffers and close this.
;; C-c C-q to just close this (changes will not be saved or undone)

")

(defun i18n-edn-edit-in-multifile ()
  (interactive)
  (if-let ((header (i18n-edn--find-current-i18n-header)))
      (progn
        (i18n-edn--cleanup)
        (let ((root-dir (s-chop-prefix
                         (projectile-project-root)
                         (file-name-directory (buffer-file-name)))))
          (save-excursion
            (switch-to-buffer-other-window "*i18n-multifile*")
            (insert (format i18n-heading root-dir))))
        (--each (i18n-edn--find-files-with-same-extension)
          (with-current-buffer (find-file-noselect it)
            (save-excursion
              (goto-char (point-min))
              (when (search-forward header nil t)
                (paredit-backward)
                (let* ((beg (point))
                       (end (progn (paredit-forward 2) (point))))
                  (with-current-buffer "*i18n-multifile*"
                    (insert (format ";; %s\n" it)))
                  (mf/mirror-region-in-multifile beg end "*i18n-multifile*"))))))
        (switch-to-buffer-other-window "*i18n-multifile*")
        (goto-line 6)
        (define-key multifiles-minor-mode-map (kbd "C-c C-c") 'i18n-edn-save-and-close)
        (define-key multifiles-minor-mode-map (kbd "C-c C-q") 'i18n-edn-close))
    (message "No i18n header found. i18n-edn expects to find namespaced maps.")))

(wrap-fullscreen i18n-edn-edit-in-multifile)

(defun i18n-edn-close ()
  (interactive)
  (let ((prev my/previous-window-configuration))
    (i18n-edn--cleanup)
    (when prev (register-val-jump-to prev nil))))

(defun i18n-edn-save-and-close ()
  (interactive)
  (mf/save-original-buffers)
  (i18n-edn-close))

(provide 'i18n-edn)
