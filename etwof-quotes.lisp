;;;;
;;;; Bot to post etwof.com quotes
;;;;

(cl:defpackage :etwof-quote-bot
  (:nicknames :eqb)
  (:use :cl)
  (:export #:start-bot))

(in-package :eqb)

(defparameter *twitter-name* "EtWoFQuoteBot")

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

(defun get-node-text-and-url (node-number)
  (let* ((node (ll::read-node node-number (db)))
         (body (getf node :body))
         (alias (car (getf node :aliases))))
    (values (body-to-text body)
            (alias-to-url alias))))

(defparameter *quote-category* 18)

(defparameter *etwof-data-dir* "etwof-data")

(defvar *db* nil)

(defun db ()
  (or *db*
      (setf *db* (fsdb:make-fsdb *etwof-data-dir*))))

(defun quote-alist ()
  (ll::read-catnodes *quote-category* (db)))

(defun random-quote-node ()
  (let* ((alist (quote-alist)))
    (car (elt alist (random (length alist))))))

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

(defparameter *times* '("00:00" "04:00" "08:00" "12:00" "16:00" "20:00"))
(defvar *times-length* 6)

(defun bot-step (bot idx)
  (declare (ignore idx))
  ;; Remove initial times
  (let* ((tails (twitter-bot:time-tails bot))
         (tails-len (length tails)))
    (when (> tails-len *times-length*)
      (setf (cdr (nthcdr (1- *times-length*) tails)) nil)))
  (twitter-bot:tweet (random-quote-tweet)))

(defvar *bot* nil)

(defun start-bot ()
  (let* ((time (get-universal-time))
         (now (twitter-bot:time-string time))
         (now+1 (twitter-bot:time-string (+ time 60)))
         (times `(,@*times* ,now ,now+1)))
    (setf *times-length* (length *times*))
    (setf *bot*
          (twitter-bot:make-bot
           :user-name *twitter-name*
           :time-tails times
           :step-function 'bot-step))))
