(in-package :dropin)
;;; Stages: what happens to a dropped file, in order. A stage that returns has done its part; its name
;;; goes in the job record so a restarted daemon skips it. An error fails the job; RETRY-LATER parks it.

(defvar *stages* '() "(name doc fn) in run order; fn takes the job.")

(defmacro defstage (name (job) doc &body body)
  "Register stage NAME at the end of *stages*; a redefinition keeps its place."
  `(let ((entry (list ',name ,doc (lambda (,job) (declare (ignorable ,job)) ,@body)))
         (old (assoc ',name *stages*)))
     (if old
         (setf (cdr old) (cdr entry))
         (setf *stages* (append *stages* (list entry))))))

(defun stage-names () (mapcar #'first *stages*))

(define-condition retry-later (error)
  ((until :initarg :until :reader retry-later-until
          :documentation "Universal time after which the job may run again.")
   (reason :initarg :reason :initform "" :reader retry-later-reason))
  (:report (lambda (c s) (format s "retry after ~a: ~a" (timestamp (retry-later-until c)) (retry-later-reason c))))
  (:documentation "Signal from a stage when the work cannot happen now (a quota, a sign-in) but will later."))

(defun retry-later (until &optional (reason ""))
  (error 'retry-later :until until :reason reason))

(defun timestamp (ut)
  (multiple-value-bind (s m h d mo y) (decode-universal-time ut 0)
    (format nil "~d-~2,'0d-~2,'0d ~2,'0d:~2,'0d:~2,'0d UTC" y mo d h m s)))
