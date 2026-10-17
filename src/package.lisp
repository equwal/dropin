(defpackage :dropin
  (:use :cl)
  (:export #:log-line #:run #:native
           ;; inotify
           #:watch-dirs #:wait-for-change
           ;; stages
           #:defstage #:*stages* #:stage-names #:retry-later #:retry-later-until
           ;; jobs and the queue
           #:job #:job-name #:job-dir #:job-source #:job-path #:job-get #:job-put
           #:queue #:make-queue #:queue-root #:queue-path #:queue-status #:drop-file #:retry
           #:process-one #:serve
           ;; CLI
           #:defcommand #:main #:option #:int-option #:flag #:positional #:*valued-options* #:*program*))
