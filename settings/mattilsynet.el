(with-eval-after-load 'lsp-mode
  (add-to-list 'lsp-file-watch-ignored-directories "[/\\\\]docker/build"))

(prodigy-define-service
  :name "Datomic transactor"
  :tags '(matnyttig datomic)
  :command "make"
  :args '("start-transactor")
  :cwd "~/work/matnyttig"
  :stop-signal 'sigkill
  :kill-process-buffer-on-stop t)

(prodigy-define-service
  :name "NATS server"
  :tags '(matnyttig nats)
  :command "nats-server"
  :args '("--jetstream" "-sd=data/matnyttig-nats")
  :cwd "~/"
  :stop-signal 'sigkill
  :kill-process-buffer-on-stop t)

(prodigy-define-service
  :name "Jaeger"
  :tags '(matnyttig otel)
  :command "docker"

  :args '("run" "--rm"
          "-p" "16686:16686"
          "-p" "4318:4318"
          "-p" "4317:4317"
          "jaegertracing/all-in-one"
          "--collector.otlp.enabled=true")
  :cwd "~/"
  :stop-signal 'sigkill
  :kill-process-buffer-on-stop t)

(provide 'mattilsynet)
