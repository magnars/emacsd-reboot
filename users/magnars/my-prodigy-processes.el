(when (file-exists-p "~/projects/www.parens-of-the-dead.com")
  (prodigy-define-service
    :name "www.parens-of-the-dead.com"
    :port 3334
    :command "lein"
    :args '("ring" "server-headless")
    :cwd "~/projects/www.parens-of-the-dead.com"
    :stop-signal 'sigkill
    :kill-process-buffer-on-stop t))

(provide 'my-prodigy-processes)
