(in-package :dropin)

(defun native (path) (if (pathnamep path) (uiop:native-namestring path) path))

(defvar *log-lock* (bt:make-lock "log"))

(defun log-line (fmt &rest args)
  "One time-stamped line on standard output; whole lines even when threads log at once."
  (multiple-value-bind (s m h) (get-decoded-time)
    (bt:with-lock-held (*log-lock*)
      (format t "~2,'0d:~2,'0d:~2,'0d ~?~%" h m s fmt args)
      (finish-output))))

(defun run (args &key directory (ignore-error nil))
  "Run ARGS (a list, no shell; pathnames allowed). Return stdout, then the exit code."
  (multiple-value-bind (out err code)
      (uiop:run-program (mapcar #'native args) :directory directory :ignore-error-status t
                        :output :string :error-output :string :external-format :utf-8)
    (unless (or ignore-error (zerop code))
      (error "command failed (~a): ~{~a ~}~%~a" code (mapcar #'native args) (or err "")))
    (values (or out "") code)))
