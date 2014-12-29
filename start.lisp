;;;;
;;;; Load the :

(in-package :cl-user)

(unless (find-package "QUICKLISP")
  (cond ((probe-file "~/quicklisp/setup.lisp")
         (load "~/quicklisp/setup"))
        (t (format t "Installing Quicklisp...")
           (load "http://beta.quicklisp.org/quicklisp.lisp")
           (funcall (find-symbol "INSTALL" :quicklisp-quickstart)))))

(defvar *twitter-bot-directory*
  (directory-namestring *loading-file-source-file*))

(defun twitter-bot-relative-path (path)
  (merge-pathnames path *twitter-bot-directory*))

(defun load-twitter-bot-relative (path)
  (load (twitter-bot-relative-path path)))

;; Change "http://" to "https://", as Twitter now requires
(load-twitter-bot-relative "twitter-preload-patches")

(defun reload-twitter-bot ()
  (load-twitter-bot-relative "twitter-bot.asd")
  (ql:quickload :twitter-bot :verbose t))

(reload-twitter-bot)

(defun :twitter-bot ()
  (in-package :twitter-bot))

(defun :etb ()
  (in-package :etb))
