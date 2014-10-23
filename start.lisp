(in-package :cl-user)

(unless (find-package "QUICKLISP")
  (cond ((probe-file "~/quicklisp/setup.lisp")
         (load "~/quicklisp/setup"))
        (t (format t "Installing Quicklisp...")
           (load "http://beta.quicklisp.org/quicklisp.lisp")
           (funcall (find-symbol "INSTALL" :quicklisp-quickstart)))))

;; Change "http://" to "https://", as Twitter now requires
(load "twitter-preload-patches")

(load "endtaxationbot.asd")
(ql:quickload :endtaxationbot)

(defun etb ()
  (in-package :endtaxationbot))

(etb)

(defun start-swank (&optional port)
  (when port
    (ql:quickload "swank")
    (funcall (find-symbol "CREATE-SERVER" :swank)
             :port port :dont-close t)))

(defun reload ()
  (ql:quickload :endtaxationbot :verbose t))
