;;;;
;;;; Bot to post etwof.com quotes
;;;;

(cl:defpackage :etwof-quote-bot
  (:nicknames :eqb)
  (:use :cl)
  (:import-from :ccl
                #:quit)
  (:export #:reload
           #:authenticate
           #:start-bot))

(in-package :eqb)

(defun reload ()
  (ql:quickload :etwof-quote-bot :verbose t))

(defparameter *twitter-name* "EtWoFQuoteBot")

;; Before running this bot to actually post tweets on Twitter, you need
;; to get an authentication token for the *twitter-name* account.
;; Do this by calling (eqb:authenticate), and following the printed 
;; instructions.
;; OAuth parameters are saved in ~/.cl-twitter/access/access.ht

(defun authenticate ()
  (twitter-bot:authenticate *twitter-name*))

(defun scan-to-strings (regex string)
  (let ((scanner (if (stringp regex)
                     (cl-ppcre:create-scanner regex :single-line-mode t)
                     regex)))
    (multiple-value-bind (b e bs es)
        (cl-ppcre:scan scanner string)
      (when b
        (let ((matches (make-array (length bs))))
          (loop for b across bs
             for e across es
             for i from 0 do
               (setf (elt matches i) (and b e (subseq string b e))))
          (values (subseq string b e)
                  matches))))))

(defparameter *scanners*
  (list
   (cl-ppcre:create-scanner "^<a href=[^\\n]*(\\n|\\r)+(.*)" :single-line-mode t)
   (cl-ppcre:create-scanner "^(F|f)rom[^\\n]*(\\n|\\r)+(.*)" :single-line-mode t)
   (cl-ppcre:create-scanner "^(R|r)eceived[^\\n]*(\\n|\\r)+(.*)" :single-line-mode t)
   (cl-ppcre:create-scanner "^(T|t)weeted[^\\n]*(\\n|\\r)+(.*)" :single-line-mode t)
   (cl-ppcre:create-scanner "^(I|i)in response[^\\n]*(\\n|\\r)+(.*)" :single-line-mode t)
   ))

(defun body-to-text (body)
  (let (w m)
    (dolist (scanner *scanners*)
      (multiple-value-setq (w m) (scan-to-strings scanner body))
      (when w (return)))
    (or (and w (elt m (1- (length m))))
        body)))

(defparameter *quote-scanner*
  (cl-ppcre:create-scanner "^quote_(.*).html$"))

(defun alias-to-quote-number (alias)
  (if (equal alias "quote.html")
      t
      (multiple-value-bind (w m) (scan-to-strings *quote-scanner* alias)
        (and w (elt m (1- (length m)))))))

(defun alias-to-url (alias)
  (let ((qn (alias-to-quote-number alias)))
    (cond ((eq qn t) "http://etwof.com/q/")
          (qn (format nil "http://etwof.com/q/~a" qn))
          (t (format nil "http://etwof.com/blog/~a" alias)))))

(defparameter *quote-category* 18)

(defparameter *etwof-data-dir*
  (asdf:system-relative-pathname :lisplog "etwof-data"))

(unless (probe-file *etwof-data-dir*)
  (error "Directory does not exist: ~s" *etwof-data-dir*))

(defvar *db* nil)

(defun db ()
  (or *db*
      (setf *db* (fsdb:make-fsdb *etwof-data-dir*))))

(defun quote-alist ()
  (ll::read-catnodes *quote-category* (db)))

(defun random-quote-node ()
  (let* ((alist (quote-alist)))
    (car (elt alist (random (length alist))))))

(defun get-node-text-and-url (node-number)
  (let* ((node (ll::read-node node-number (db)))
         (body (getf node :body))
         (alias (car (getf node :aliases))))
    (values (body-to-text body)
            (alias-to-url alias))))

(defun random-quote-and-url ()
  (get-node-text-and-url (random-quote-node)))

(defparameter *ellipsis-char*
  #\20046)

(defun random-quote-tweet ()
  (multiple-value-bind (quote url) (random-quote-and-url)
    (setf quote (delete #\" quote))
    (let ((len (length quote)))
      (if (<= len 140)
          quote
          (format nil "~a~c ~a"
                  (subseq quote 0 (- 140 (length url) 2))
                  *ellipsis-char*
                  url)))))

(defvar *last-tweet* nil)

(defun bot-step (bot idx)
  (declare (ignore bot))
  (let ((tweet (random-quote-tweet)))
    (setf *last-tweet*
          `(:time ,(twitter-bot:last-time)
            :idx ,idx
            :tweet ,tweet))
    (twitter-bot:tweet tweet)))

(defvar *bot* nil)

(defparameter *times* '("00:00" "04:00" "08:00" "12:00" "16:00" "20:00"))

(defun start-bot ()
  (setf *bot*
        (twitter-bot:make-bot
         :user-name *twitter-name*
         :time-tails *times*
         :step-function 'bot-step)))
