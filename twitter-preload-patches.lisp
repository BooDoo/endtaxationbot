;;;
;;; Patch cl-twitter to make it use SSL in its Twitter URLs.
;;;
;;; These are wired into the commands it creates at load time,
;;; so we have to set the variables before it loads.
;;; Fortunately, they're defined with DEFVAR instead of DEFPARAMETER,
;;; so this works.
;;;

(in-package :cl-user)

(unless (find-package :twitter)
  (defpackage :twitter
    (:use cl)
    (:nicknames :cl-twitter :twit)))

(defun set-twitter-var (var val)
  (let ((sym (intern (string var) :twitter)))
    (set sym val)))

(defun set-twitter-uri-vars ()
  (loop for (var val)
     in '((#:*twitter-app-uri*  "https://api.twitter.com/1.1/")
          (#:*twitter-search-uri*    "https://api.twitter.com/1.1/")
          (#:*twitter-oauth-uri*  "https://api.twitter.com/oauth/"))
     do (set-twitter-var var val)))

(set-twitter-uri-vars)
