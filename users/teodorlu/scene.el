;;; scene.el --- Utilities for working with scenes -*- lexical-binding: t; -*-

;;; Commentary:

;; A scene is a directory containing one or more project roots.

;;; Code:

(require 'cl-lib)
(require 'projectile)

(defun scene-directory (dir)
  "Return the scene directory containing DIR, or nil.

The scene directory is the parent directory of Projectile's project root."
  (let* ((default-directory (file-name-as-directory (expand-file-name dir)))
         (project-root (projectile-project-root)))
    (when project-root
      (file-name-directory (directory-file-name project-root)))))

;;;###autoload
(defun scene-yank-path ()
  "Yank the current file path relative to the scene directory."
  (interactive)
  (unless buffer-file-name
    (user-error "Current buffer is not visiting a file"))
  (let ((scene-dir (scene-directory (file-name-directory buffer-file-name))))
    (unless scene-dir
      (user-error "Could not find scene directory for %s" buffer-file-name))
    (kill-new (file-relative-name buffer-file-name scene-dir))))

(provide 'scene)

;;; scene.el ends here
