(asdf:defsystem "dropin"
  :description "Drop a file in a folder, and a chain of stages runs on it: an event-driven queue with a CLI."
  :license "CCAI-1.0"
  :depends-on ("uiop" "bordeaux-threads")
  :serial t
  :components ((:module "src"
                :components ((:file "package")
                             (:file "util")
                             (:file "watch")
                             (:file "stages")
                             (:file "queue")
                             (:file "cli")))))
