; -*- mode: lisp -*-

(cl:defpackage :endtaxationbot
  (:nicknames #:etb)
  (:use :cl :anaphora)
  (:import-from :ccl
                #:quit)
  (:export #:start-swank
           #:reload
           #:total-replies
           #:start-bot-thread
           ))
