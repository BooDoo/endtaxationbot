(in-package :cl-user)

(unless (find-package "QUICKLISP")
  (cond ((probe-file "~/quicklisp/setup.lisp")
         (load "~/quicklisp/setup"))
        (t (format t "Installing Quicklisp...")
           (load "http://beta.quicklisp.org/quicklisp.lisp")
           (funcall (find-symbol "INSTALL" :quicklisp-quickstart)))))

(defvar *etb-directory*
  (directory-namestring *loading-file-source-file*))

(defun etb-relative-path (path)
  (merge-pathnames path *etb-directory*))

(defun load-etb-relative (path)
  (load (etb-relative-path path)))

;; Change "http://" to "https://", as Twitter now requires
(load-etb-relative "twitter-preload-patches")

(defun reload-etb ()
  (load-etb-relative "end-taxation-bot.asd")
  (ql:quickload :end-taxation-bot :verbose t))

(reload-etb)

(defun etb ()
  (in-package :end-taxation-bot))
