; -*- mode: lisp -*-

(cl:defpackage :twitter-bot
  (:use :cl :anaphora)
  (:import-from :ccl
                #:quit)
  (:export #:start-swank
           #:reload
           #:authenticate
           #:tweet
           #:@reply-to
           #:avoiding-duplicate-users
           #:time-tails
           #:bot
           #:step-function
           #:make-bot
           #:delete-bot
           #:time-zone
           #:time-string
           #:start-bot-thread
           ))

(cl:defpackage :end-taxation-bot
  (:nicknames :etb)
  (:use :cl :anaphora)
  (:import-from :ccl
                #:quit)
  (:export #:start-etb
           #:total-replies))

(in-package :twitter-bot)

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
  (load (asdf:system-definition-pathname :twitter-bot))
  (ql:quickload :twitter-bot :verbose t))
