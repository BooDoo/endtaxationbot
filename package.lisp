; -*- mode: lisp -*-

(cl:defpackage :end-taxation-bot
  (:nicknames #:etb)
  (:use :cl :anaphora)
  (:import-from :ccl
                #:quit)
  (:export #:start-swank
           #:reload
           #:total-replies
           #:start-bot-thread
           ))

(in-package :etb)


;;;
;;; Utilities
;;;

(defun start-swank (&optional port)
  (when port
    (when (eq port t)
      (setf port 4005))
    (ql:quickload "swank")
    (funcall (find-symbol "CREATE-SERVER" :swank)
             :port port :dont-close t)))

(defun reload ()
  (load (asdf:system-definition-pathname :end-taxation-bot))
  (ql:quickload :end-taxation-bot :verbose t))
