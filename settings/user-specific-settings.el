;; To add your own settings, add them to .el-files in the users directory.

;; Evaluate user-settings-dir below if you're unsure of where to put them.

(setq user-settings-dir
      (concat user-emacs-directory "users/" user-login-name))

(add-to-list 'load-path user-settings-dir)

(when (file-exists-p user-settings-dir)
  (dolist (file (directory-files user-settings-dir t "^[^#].*el$"))
    (when (file-regular-p file)
      (load file))))

(provide 'user-specific-settings)
