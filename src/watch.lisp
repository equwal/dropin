(in-package :dropin)
;;; Wake when a file lands in a folder. Linux SBCL sleeps on inotify; elsewhere a timed poll stands in.

#+(and sbcl linux)
(progn
  (sb-alien:define-alien-routine ("inotify_init1" inotify-init1) sb-alien:int (flags sb-alien:int))
  (sb-alien:define-alien-routine ("inotify_add_watch" inotify-add-watch) sb-alien:int
    (fd sb-alien:int) (path sb-alien:c-string) (mask sb-alien:unsigned-int))

  (defun watch-dirs (dirs)
    "An inotify descriptor that becomes readable when a file is closed after writing or moved into DIRS."
    (let ((fd (inotify-init1 #o4000)))                                   ; IN_NONBLOCK
      (when (minusp fd) (error "inotify_init1 failed"))
      (dolist (d dirs fd)
        (ensure-directories-exist d)
        (when (minusp (inotify-add-watch fd (native d) #x88))           ; IN_CLOSE_WRITE | IN_MOVED_TO
          (error "inotify_add_watch ~a failed" (native d))))))

  (defun drain (fd)
    "Throw the queued events away: the queue rescans its folders, so which file changed does not matter."
    (sb-alien:with-alien ((buf (sb-alien:array sb-alien:unsigned-char 4096)))
      (loop while (plusp (or (sb-unix:unix-read fd (sb-alien:alien-sap buf) 4096) 0)))))

  (defun wait-for-change (fd seconds)
    "T when a watched folder changed within SECONDS, nil on the timeout."
    (when (sb-sys:wait-until-fd-usable fd :input seconds)
      ;; ponytail: a fixed settle for writers that close and reopen; per-file quiet time if one shows up
      (sleep 2)
      (drain fd)
      t)))

#-(and sbcl linux)
(progn
  (defun watch-dirs (dirs) (mapc #'ensure-directories-exist dirs) nil)
  ;; ponytail: polls every 5 s off Linux; ReadDirectoryChangesW / kqueue if that ever matters
  (defun wait-for-change (fd seconds) (declare (ignore fd)) (sleep (min seconds 5)) nil))
