(in-package :cl-user)

(unless (find-package "QUICKLISP")
  (cond ((probe-file "~/quicklisp/setup.lisp")
         (load "~/quicklisp/setup"))
        (t (format t "Installing Quicklisp...")
           (load "http://beta.quicklisp.org/quicklisp.lisp")
           (funcall (find-symbol "INSTALL" :quicklisp-quickstart)))))

;; Change "http://" to "https://", as Twitter now requires
(load "twitter-preload-patches")

(defun reload ()
  (load "endtaxationbot.asd")
  (ql:quickload :endtaxationbot :verbose t))

(reload)

(defun etb ()
  (in-package :endtaxationbot))

(etb)

(defun start-swank (&optional port)
  (when port
    (when (eq port t)
      (setf port 4005))
    (ql:quickload "swank")
    (funcall (find-symbol "CREATE-SERVER" :swank)
             :port port :dont-close t)))

(defun reload ()
  (cl-user::reload))
