;;; sbcl --non-interactive --load tests/self-check.lisp   (needs quicklisp; run from the repo root)
(require :asdf)
(push (uiop:getcwd) asdf:*central-registry*)
(ql:quickload :dropin :silent t)
(defpackage :dropin-test (:use :cl :dropin))
(in-package :dropin-test)

(defvar *fails* 0)
(defmacro check (name form)
  `(if ,form (format t "ok   ~a~%" ,name) (progn (incf *fails*) (format t "FAIL ~a~%" ,name))))

(defvar *roots* '())
(defun fresh-root ()
  (let ((d (uiop:ensure-directory-pathname
            (format nil "/tmp/dropin-test-~d-~d/" (get-universal-time) (random 1000000 (make-random-state t))))))
    (ensure-directories-exist d)
    (push d *roots*)
    d))

(defun touch (path text)
  (with-open-file (s (uiop:parse-native-namestring path) :direction :output :if-exists :supersede)
    (write-string text s)))

(defun state-of (q name) (getf (find name (queue-status q) :key (lambda (j) (getf j :name)) :test #'string=) :state))

;;; Stages run in order, see the record, and a name with [brackets] stays literal.
(setf *stages* '())
(defvar *trace* '())
(defstage first-stage (job) "records the file size"
  (push :first *trace*)
  (job-put job :size (with-open-file (s (job-source job)) (file-length s))))
(defstage second-stage (job) "fails on bad, parks on later"
  (push :second *trace*)
  (cond ((search "bad" (job-name job)) (error "bad input"))
        ((and (search "later" (job-name job)) (not (job-get job :parked)))
         (job-put job :parked t)
         (retry-later (+ (get-universal-time) 3600) "quota"))))

(let* ((root (fresh-root)) (q (make-queue root)))
  (touch (queue-path q "inbox" "a [1].txt") "hello")
  (check "process-one runs a job" (process-one q))
  (check "stages ran in order" (equal *trace* '(:second :first)))
  (check "job reached done/" (equal (state-of q "a [1].txt") "done"))
  (check "record kept the stage's data"
         (= 5 (getf (find "a [1].txt" (queue-status q) :key (lambda (j) (getf j :name)) :test #'string=) :size)))
  (check "nothing left to run" (null (process-one q)))

  (touch (queue-path q "inbox" "bad.txt") "x")
  (process-one q)
  (check "an error fails the job" (equal (state-of q "bad.txt") "failed"))
  (setf *trace* '())
  (retry q "bad.txt")
  (process-one q)
  (check "retry resumes after the finished stages" (equal *trace* '(:second)))

  (touch (queue-path q "inbox" "later.txt") "x")
  (process-one q)
  (check "retry-later parks the job" (equal (state-of q "later.txt") "held"))
  (check "a parked job does not run before its hold" (null (process-one q)))

  (let ((tmp (uiop:strcat (native root) "x.txt")))
    (touch tmp "again")
    (check "drop-file renames a clash" (string= (drop-file q tmp "a [1].txt") "a [1]-2.txt"))))

;;; The watcher wakes when a file lands, and times out when none does.
#+(and sbcl linux)
(let* ((root (fresh-root)) (q (make-queue root))
       (fd (watch-dirs (list (uiop:parse-native-namestring (queue-path q "inbox") :ensure-directory t)))))
  (check "no file, no wake" (null (wait-for-change fd 1)))
  (bt:make-thread (lambda () (sleep 0.5) (touch (queue-path q "inbox" "new.txt") "x")))
  (check "a new file wakes the watcher" (wait-for-change fd 10)))

;;; serve picks up a dropped file without being polled.
(let* ((root (fresh-root)) (q (make-queue root)))
  (setf *stages* '())
  (defstage only (job) "nothing" nil)
  (bt:make-thread (lambda () (serve q :every 3600)) :name "serve")
  (sleep 1)
  (touch (queue-path q "inbox" "served.txt") "x")
  (loop repeat 50 until (equal (state-of q "served.txt") "done") do (sleep 0.2))
  (check "serve runs a dropped file" (equal (state-of q "served.txt") "done")))

(dolist (d *roots*) (uiop:delete-directory-tree d :validate (lambda (p) (search "/tmp/dropin-test-" (namestring p)))))
(format t "~:[all checks passed~;~:*~d check~:p FAILED~]~%" (and (plusp *fails*) *fails*))
(uiop:quit (if (plusp *fails*) 1 0))
