(in-package :dropin)
;;; The CLI: `PROGRAM COMMAND [args] [--option value]`. Commands are a registry like the stages, so a
;;; program built on dropin adds its own (`defcommand`) next to the built-in queue commands.

(defvar *commands* '() "(name usage doc fn) in help order; fn takes the queue and the positional words.")
(defvar *valued-options* '("--root" "--jobs" "--every") "Options that take a value; a program adds its own.")
(defvar *args* '() "The command line of this run, for OPTION and FLAG inside a command.")

(defmacro defcommand (name (queue words) usage doc &body body)
  "Register command NAME. USAGE is its argument summary for `help`."
  `(let ((entry (list ,(string-downcase name) ,usage ,doc
                      (lambda (,queue ,words) (declare (ignorable ,queue ,words)) ,@body)))
         (old (assoc ,(string-downcase name) *commands* :test #'string=)))
     (if old
         (setf (cdr old) (cdr entry))
         (setf *commands* (append *commands* (list entry))))))

(defun option (name &optional default)
  "The value after --NAME on the command line, else DEFAULT."
  (let ((pos (position name *args* :test #'string=)))
    (if (and pos (< (1+ pos) (length *args*))) (nth (1+ pos) *args*) default)))

(defun flag (name) (and (member name *args* :test #'string=) t))

(defun positional (args)
  (loop with skip = nil
        for a in args
        if skip do (setf skip nil)
        else if (member a *valued-options* :test #'string=) do (setf skip t)
        else unless (uiop:string-prefix-p "--" a) collect a))

(defun int-option (name default) (parse-integer (option name (princ-to-string default))))

(defvar *program* "dropin")

(defun help ()
  (format t "usage: ~a COMMAND [args] [options]~2%commands~%" *program*)
  (dolist (c *commands*)
    (format t "  ~a~@[ ~a~]~24t~a~%" (first c) (and (string/= (second c) "") (second c)) (third c)))
  (format t "~%options~%  --root DIR~20tqueue folder (default $DROPIN_ROOT, else .)~%  --jobs N~20tworkers for watch (default 1)~%  --every S~20tlook again every S seconds even without a new file (default 600)~%~%stages, in order~%")
  (dolist (s *stages*) (format t "  ~(~a~)~20t~a~%" (first s) (second s))))

(defcommand help (q words) "" "this text" (help))

(defcommand stages (q words) "" "list the stages in run order"
  (dolist (s *stages*) (format t "~(~a~)~20t~a~%" (first s) (second s))))

(defcommand watch (q words) "" "the daemon: run jobs as files land in inbox/"
  (serve q :jobs (int-option "--jobs" 1) :every (int-option "--every" 600)))

(defcommand once (q words) "" "run every ready job, then exit"
  (loop while (process-one q)))

(defcommand drop (q words) "FILE..." "copy FILEs into inbox/"
  (dolist (f words) (format t "~a~%" (drop-file q (native (uiop:parse-native-namestring f))))))

(defcommand status (q words) "" "one line per job: state, stage, error"
  (dolist (j (queue-status q))
    (format t "~8a ~40a ~@[~a~]~@[  ~a~]~%" (getf j :state) (getf j :name) (getf j :stage) (getf j :error))))

(defcommand retry (q words) "NAME..." "send failed jobs back to work/"
  (dolist (n words) (retry q n) (format t "~a~%" n)))

(defun main (args &key (program *program*))
  "Parse ARGS, open the queue at --root, run the command. Exit status 2 on an unknown command."
  (let* ((*args* args)
         (*program* program)
         (words (positional args))
         (cmd (assoc (or (first words) "help") *commands* :test #'string=)))
    (if (null cmd)
        (progn (help) (uiop:quit 2))
        (funcall (fourth cmd)
                 (make-queue (option "--root" (or (uiop:getenv "DROPIN_ROOT") ".")))
                 (rest words)))))
