; -*- mode: lisp -*-
(in-package #:cl-user)

(asdf:defsystem :etwof-quote-bot
  :description "A bot to post quotes from the End the War on Freedom blog."
  :author "Bill St. Clair <bill@billstclair.com>"
  :version "0.1"
  :license "BSD 2-Clause License"
  :depends-on (:twitter-bot
               :fsdb
               :lisplog)
  :components
  ((:file "etwof-quote-bot")))
