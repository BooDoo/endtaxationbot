; -*- mode: lisp -*-
(in-package #:cl-user)

(asdf:defsystem :endtaxationbot
  :description "A simple Twitter bot that equates taxation with extortion."
  :author "Bill St. Clair <bill@billstclair.com>"
  :version "0.1"
  :license "BSD 2-Clause License"
  :depends-on (:cl-twitter
               :cl-twit-repl
               :anaphora)
  :components
  ((:file "package")
   (:file "twitter-patches")
   (:file "bot" :depends-on ("package"))))
