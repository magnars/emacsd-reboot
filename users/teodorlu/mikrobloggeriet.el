(defun mikrobloggeriet-deploy ()
  (interactive)
  (let ((default-directory "~/dev/iterate/mikrobloggeriet"))
    (shell-command "garden deploy")))
