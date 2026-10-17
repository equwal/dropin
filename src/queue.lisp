(in-package :dropin)
;;; The queue is a folder, and the folders are its state, so a restart loses nothing:
;;;   inbox/          files dropped in (names starting with "." are ignored: writers use them for parts)
;;;   work/NAME/      a claimed job: the file plus job.sexp, the record the stages read and write
;;;   done/NAME/      every stage ran
;;;   failed/NAME/    a stage signalled an error; job.sexp has it, `retry NAME` sends the job back
;;; A job parked by RETRY-LATER stays in work/ with :hold-until in its record.

(defstruct (queue (:constructor %make-queue))
  (root "" :type string)                  ; native path, no trailing slash
  (lock (bt:make-lock "queue"))
  (active '()))                           ; names of jobs a worker holds now

(defun make-queue (root)
  (let ((q (%make-queue :root (string-right-trim "/" (native (uiop:ensure-directory-pathname root))))))
    (dolist (d '("inbox" "work" "done" "failed") q)
      (ensure-directories-exist (queue-path q d "")))))

(defun queue-path (queue &rest parts)
  "Native path under the queue root. Built from strings so names with [ * ? stay literal."
  (format nil "~a~{/~a~}" (queue-root queue) parts))

(defun entries (dir &key directories)
  "Names in native DIR, files or directories, dotfiles left out, sorted."
  (let ((p (uiop:parse-native-namestring (uiop:strcat dir "/") :ensure-directory t)))
    (sort (loop for e in (if directories (uiop:subdirectories p) (uiop:directory-files p))
                for path = (string-right-trim "/" (native e))     ; native: file-namestring escapes [ and *
                for n = (subseq path (1+ (position #\/ path :from-end t)))
                unless (char= (char n 0) #\.) collect n)
          #'string<)))

(defun rename (from to)
  "Atomic rename of a native file or directory path on one filesystem."
  #+sbcl (multiple-value-bind (ok errno) (sb-unix:unix-rename from to)
           (unless ok (error "rename ~a -> ~a: ~a" from to (sb-int:strerror errno))))
  #-sbcl (run (list "mv" from to)))

;;; Jobs

(defstruct (job (:constructor %make-job))
  name dir (data '()))

(defun record-path (job) (uiop:strcat (job-dir job) "/job.sexp"))

(defun load-job (dir name)
  (let ((job (%make-job :name name :dir dir))
        (rec (uiop:strcat dir "/job.sexp")))
    (when (exists-p rec)
      (with-open-file (s (uiop:parse-native-namestring rec) :external-format :utf-8)
        (setf (job-data job) (let ((*read-eval* nil)) (read s nil '())))))
    job))

(defun save-job (job)
  (let ((tmp (uiop:strcat (record-path job) ".tmp")))
    (with-open-file (s (uiop:parse-native-namestring tmp) :direction :output :if-exists :supersede
                                                          :external-format :utf-8)
      (with-standard-io-syntax (prin1 (job-data job) s)))
    (rename tmp (record-path job))))

(defun job-get (job key &optional default) (getf (job-data job) key default))

(defun job-put (job &rest plist)
  "Set KEY VALUE pairs in the job record and write it to disk at once."
  (loop for (k v) on plist by #'cddr do (setf (getf (job-data job) k) v))
  (save-job job)
  job)

(defun job-source (job)
  "Pathname of the dropped file inside the job folder; safe for OPEN and RUN whatever its name holds."
  (job-path job (job-get job :file)))

(defun job-path (job name)
  "Pathname of a file NAME the stages keep in the job folder."
  (uiop:parse-native-namestring (uiop:strcat (job-dir job) "/" name)))

;;; Claiming and running

(defun exists-p (path)
  "True when native PATH names a file or directory."
  #+sbcl (and (sb-unix:unix-stat path) t)
  #-sbcl (and (probe-file (uiop:parse-native-namestring path)) t))

(defun taken-p (queue name)
  (some (lambda (d) (exists-p (queue-path queue d name))) '("work" "done" "failed")))

(defun numbered (name i)
  "NAME with -I before its extension: book.mp3, book-2.mp3."
  (let ((dot (position #\. name :from-end t)))
    (if (and dot (plusp dot))
        (format nil "~a-~d~a" (subseq name 0 dot) i (subseq name dot))
        (format nil "~a-~d" name i))))

(defun unique-name (queue name &key inbox)
  "NAME, or NAME-2, NAME-3 … when a job of that name exists already (with INBOX, also a file in inbox/)."
  (loop for i from 1
        for n = (if (= i 1) name (numbered name i))
        unless (or (taken-p queue n) (and inbox (exists-p (queue-path queue "inbox" n))))
          return n))

(defun mkdir (path)
  "Make the native directory PATH (its parent exists). ensure-directories-exist would escape [ in it."
  #+sbcl (multiple-value-bind (ok errno) (sb-unix:unix-mkdir path #o755)
           (unless ok (error "mkdir ~a: ~a" path (sb-int:strerror errno))))
  #-sbcl (run (list "mkdir" path)))

(defun claim (queue file)
  "Move inbox/FILE into a new job folder work/NAME/. Nil when another worker took it first."
  (let* ((name (unique-name queue file))
         (dir (queue-path queue "work" name)))
    (mkdir dir)
    (handler-case (rename (queue-path queue "inbox" file) (uiop:strcat dir "/" file))
      (error () (uiop:delete-empty-directory (uiop:parse-native-namestring dir :ensure-directory t))
        (return-from claim nil)))
    (let ((job (%make-job :name name :dir dir)))
      (job-put job :file file :dropped (get-universal-time) :done '()))))

(defun runnable-p (job now)
  (let ((hold (job-get job :hold-until)))
    (or (null hold) (>= now hold))))

(defun next-job (queue)
  "A job to run, marked active: a work/ job past its hold first (resumes after a restart), else a new file."
  (bt:with-lock-held ((queue-lock queue))
    (let ((now (get-universal-time)))
      (flet ((take (job) (push (job-name job) (queue-active queue)) (return-from next-job job)))
        (dolist (name (entries (queue-path queue "work") :directories t))
          (unless (member name (queue-active queue) :test #'string=)
            (let ((job (load-job (queue-path queue "work" name) name)))
              (when (runnable-p job now) (take job)))))
        (dolist (file (entries (queue-path queue "inbox")))
          (let ((job (claim queue file)))
            (when job (take job))))))))

(defun finish (queue job where)
  (let ((to (queue-path queue where (job-name job))))
    (rename (job-dir job) to)
    (setf (job-dir job) to)))

(defun run-stages (queue job)
  "Each stage the job has not done, in order. Ends in done/, failed/, or parked in work/."
  (handler-case
      (progn
        (dolist (s *stages*)
          (let ((stage (string-downcase (first s))))
            (unless (member stage (job-get job :done) :test #'string=)
              (job-put job :stage stage :hold-until nil :error nil)
              (log-line "~a: ~a" (job-name job) stage)
              (funcall (third s) job)
              (job-put job :done (append (job-get job :done) (list stage))))))
        (job-put job :stage nil :finished (get-universal-time))
        (finish queue job "done")
        (log-line "~a: done" (job-name job)))
    (retry-later (c)
      (job-put job :hold-until (retry-later-until c) :error (princ-to-string c))
      (log-line "~a: ~a" (job-name job) c))
    (error (e)
      (job-put job :error (princ-to-string e) :finished (get-universal-time))
      (finish queue job "failed")
      (log-line "~a: FAILED in ~a: ~a" (job-name job) (job-get job :stage) e))))

(defun process-one (queue)
  "Run one job if one is ready. True when a job ran."
  (let ((job (next-job queue)))
    (when job
      (unwind-protect (run-stages queue job)
        (bt:with-lock-held ((queue-lock queue))
          (setf (queue-active queue) (remove (job-name job) (queue-active queue) :test #'string=))))
      t)))

;;; Feeding and reading the queue

(defun drop-file (queue path &optional (name (file-namestring path)))
  "Put the file at native PATH in the inbox as NAME, moving it when it is on the queue's filesystem.
It appears under a dot name first, so a watcher never sees half a file. Returns the inbox name."
  (let* ((final (unique-name queue name :inbox t))
         (part (queue-path queue "inbox" (uiop:strcat "." final ".part"))))
    (handler-case (rename (native path) part)
      (error () (uiop:copy-file (uiop:parse-native-namestring (native path)) (uiop:parse-native-namestring part))))
    (rename part (queue-path queue "inbox" final))
    final))

(defun retry (queue name)
  "Send failed/NAME back to work/; the stages it finished stay finished."
  (let ((from (queue-path queue "failed" name)))
    (rename from (queue-path queue "work" name))
    (job-put (load-job (queue-path queue "work" name) name) :error nil :hold-until nil)))

(defun queue-status (queue)
  "A plist per job, newest folder state first: :name :state (inbox work held done failed) and the record."
  (let ((now (get-universal-time)) out)
    (dolist (f (entries (queue-path queue "inbox")))
      (push (list :name f :state "inbox") out))
    (dolist (where '("work" "done" "failed"))
      (dolist (n (entries (queue-path queue where) :directories t))
        (let ((job (load-job (queue-path queue where n) n)))
          (push (list* :name n
                       :state (cond ((string/= where "work") where)
                                    ((member n (queue-active queue) :test #'string=) "running")
                                    ((not (runnable-p job now)) "held")
                                    (t "work"))
                       (job-data job))
                out))))
    (nreverse out)))

(defun next-wake (queue every)
  "Seconds until the earliest parked job may run, at most EVERY."
  (let ((now (get-universal-time)))
    (reduce #'min (loop for j in (queue-status queue)
                        for h = (getf j :hold-until)
                        when (and h (string= (getf j :state) "held")) collect (max 1 (- h now)))
            :initial-value every)))

(defun serve (queue &key (jobs 1) (every 600))
  "The daemon: JOBS workers run jobs while any are ready; a file landing in inbox/ wakes them at once,
and they look again every EVERY seconds or when a parked job's hold ends. Never returns."
  (let* ((fd (watch-dirs (list (uiop:parse-native-namestring (queue-path queue "inbox") :ensure-directory t))))
         (lock (bt:make-lock "wake"))
         (cv (bt:make-condition-variable))
         (generation 0))
    (flet ((worker ()
             (loop
               (let ((seen (bt:with-lock-held (lock) generation)))
                 (unless (handler-case (process-one queue)
                           (error (e) (log-line "worker: ~a" e) (sleep 5) nil))
                   ;; Sleep only if nothing landed while this worker looked: GENERATION moves on each wake.
                   (bt:with-lock-held (lock)
                     (when (= seen generation)
                       (bt:condition-wait cv lock :timeout (next-wake queue every)))))))))
      (dotimes (i jobs)
        (bt:make-thread #'worker :name (format nil "dropin worker ~d" i)))
      (log-line "watching ~a/inbox with ~d worker~:p" (queue-root queue) jobs)
      (loop
        (wait-for-change fd every)
        (bt:with-lock-held (lock)
          (incf generation)
          (dotimes (i jobs) (bt:condition-notify cv)))))))
