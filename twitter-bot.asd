; -*- mode: lisp -*-
(in-package #:cl-user)

(asdf:defsystem :twitter-bot
  :description "A simple Twitter bot framework."
  :author "Bill St. Clair <bill@billstclair.com>"
  :version "0.1"
  :license "BSD 2-Clause License"
  :depends-on (:cl-twitter
               :cl-twit-repl
               :bordeaux-threads
               :anaphora)
  :components
  ((:file "package")
   (:file "twitter-patches")
   (:file "twitter-bot" :depends-on ("package"))
   (:file "end-taxation-bot" :depends-on ("package" "twitter-bot"))))
