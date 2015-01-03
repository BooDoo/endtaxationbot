(in-package :twitter-bot)

;; This isn't used anymore. We store a twit:twitter-user instance in the
;; twitter-bot instance and bind twit:*twitter-user* to it while
;; calling the bot's step-function.
(defun authenticate (name)
  (twit:get-authenticated-user name))

(defvar *disable-tweets* nil)

(defun tweet (text)
  (if *disable-tweets*
      (format t "~&(tweet ~s)~%" text)
      (twit:tweet text)))

(defun @reply-to (tweet msg)
  (if *disable-tweets*
      (format t "~&(@reply-to ~s ~s)~%"
              (twit:twitter-user-screen-name (twit:tweet-user tweet))
              msg)
      (twit:@reply-to tweet msg)))

(defmacro avoiding-duplicate-users ((tweet tweets &optional user-name)
                                    &body body)
  (let ((thunk (gensym "THUNK-")))
    `(flet ((,thunk (,tweet)
              ,@body))
       (declare (dynamic-extent #',thunk))
       (call-avoiding-duplicate-users
        ,tweets #',thunk
        ,@(and user-name `(,user-name))))))

(defvar *bot-user-name* nil
  "Bound to the user name of the current bot.")

(defun call-avoiding-duplicate-users (tweets thunk
                                      &optional (user-name *bot-user-name*))
  (loop for tweet in tweets
     for user = (twit:tweet-user tweet)
     with already-seen = nil
     do
       (unless (equal user-name (twit:twitter-user-screen-name user))
         (loop with ids = (cons (twit:twitter-user-id
                                 (twit:tweet-user tweet))
                                (mapcar #'twit:user-mention-id
                                        (twit:twitter-entities-user-mentions
                                         (twit:tweet-entities tweet))))
            for seen in ids
            do (when (member seen already-seen)
                 (return nil))
            finally
              (setf already-seen (nconc ids already-seen))
              (funcall thunk tweet)))))

(defclass bot ()
  ((user-name :initarg :user-name
              :reader user-name
              :documentation "Twitter user name of this bot.")
   (time-tails
    :initarg :time-tails
    :accessor time-tails
    :documentation "List of strings giving yyyymmdd,hh:mm tails of when to run.")
   (step-function
    :initarg :step-function
    :accessor step-function
    :documentation "Called with two args: BOT instance & TIME-TAILS index.")
   (twitter-user :accessor twitter-user
                 :documentation "A twit:twitter-user intance.")))

(defmethod print-object ((bot bot) stream)
  (print-unreadable-object (bot stream :type t)
    (format stream "~s" (user-name bot))))

(defun authenticate-bot (bot)
  (check-type bot bot)
  (let* ((user-name (user-name bot))
         (access-token (twit-repl::get-access-token user-name))
         (user (let ((twit:*twitter-user*
                      (make-instance 'twit:twitter-user :access-token access-token)))
                 (twit:get-user user-name))))
    (setf (twit:twitter-user-access-token user) access-token
          (twitter-user bot) user)))

(defvar *bot-alist* nil
  "Map user-name to BOT instance.")

(defvar *bot-alist-lock* (bt:make-recursive-lock "*bot-alist*"))

(defmacro with-bot-alist-lock-held (() &body body)
  `(bt:with-recursive-lock-held (*bot-alist-lock*)
     ,@body))

(defun make-bot (&key user-name time-tails step-function)
  (let* ((bot (make-instance 'bot
                             :user-name user-name
                             :time-tails time-tails
                             :step-function step-function)))
    (authenticate-bot bot)
    (with-bot-alist-lock-held ()
      (let ((cell (assoc user-name *bot-alist* :test #'string=)))
        (if cell
            (setf (cdr cell) bot)
            (push (cons user-name bot) *bot-alist*))))
    (start-bot-thread)
    bot))

(defun delete-bot (user-name)
  (with-bot-alist-lock-held ()
    (let ((found nil))
      (setf *bot-alist*
            (delete-if (lambda (cell)
                         (when (string= user-name (car cell))
                           (setf found t)))
                         *bot-alist*))
      found)))

(defvar *time-zone* 5
  "US East Coast")

(defun time-zone ()
  *time-zone*)

(defun (setf time-zone) (tz)
  (check-type tz integer)
  (setf *time-zone* tz))

(defun time-string (&optional (time (get-universal-time)))
  (multiple-value-bind (s m h d mo y)
      (decode-universal-time time (time-zone))
    (declare (ignore s))
    (format nil "~4,'0d~2,'0D~2,'0D,~2,'0d:~2,'0d" y mo d h m)))

(defun string-tail-p (string tail)
  (check-type string string)
  (check-type tail string)
  (let ((tail-len (length tail))
        (str-len (length string)))
    (and (<= tail-len str-len)
         (string= string tail :start1 (- str-len tail-len)))))

(defvar *error-log* nil)
(defparameter *error-log-length* 10)

(defvar *error-log-lock* (bt:make-recursive-lock "*error-log*"))

(defmacro with-error-log-locked (() &body body)
  `(bt:with-recursive-lock-held (*error-log-lock*) ,@body))

(defun log-error (c)
  (let ((info (trivial-backtrace:print-backtrace c :output nil)))
    (with-error-log-locked ()
      (let ((diff (- (length *error-log*) *error-log-length*)))
        (when (>= diff 0)
          (setf *error-log* (nbutlast *error-log* (1+ diff)))))
      (push info *error-log*))))

(defun do-bot (bot idx)
  (check-type bot bot)
  (check-type idx integer)
  (let ((*bot-user-name* (user-name bot))
        (twit:*twitter-user* (twitter-user bot)))
    (handler-bind
        ((error (lambda (c)
                  (log-error c)
                  (return-from do-bot nil))))
      (funcall (step-function bot) bot idx))))

(defvar *last-time* nil)
(defvar *last-time-bots* nil)

(defun last-time ()
  *last-time*)

(defun get-do-bot-list (time-string)
  (let ((bots nil))
    (dolist (cell *bot-alist*)
      (let* ((bot (cdr cell))
             (tails (time-tails bot)))
        (loop for tail in tails
           for idx from 0
           do
             (when (string-tail-p time-string tail)
               (push (list bot idx) bots)
               (return)))))
    bots))

(defun bot-thread-step ()
  (bot-thread-step-internal (time-string)))

(defun bot-thread-step-internal (time-string)
  (let ((bots nil))
    (unless (equal time-string *last-time*)
      (setf *last-time* time-string
            *last-time-bots* nil))
    (setf bots
          (delete-if (lambda (bot)
                       (member bot *last-time-bots* :test #'eq))
                     (get-do-bot-list time-string)
                     :key #'car))
    (setf *last-time-bots*
          (nconc (mapcar #'car bots) *last-time-bots*))
    (loop for (bot idx) in bots do
         (ignore-errors (do-bot bot idx)))))

(defun bot-thread-loop-body ()
  (bot-thread-step)
  (sleep 10))

(defun bot-thread-loop ()
  (loop do
       (restart-case (bot-thread-loop-body)
         (continue ()
           :report "Continue Twitter Bot thread."))))

(defvar *bot-thread* nil)

(defun start-bot-thread ()
  (unless (and *bot-thread*
               (bt:thread-alive-p *bot-thread*))
    (setf *bot-thread*
          (bt:make-thread 'bot-thread-loop
                          :name "Twitter Bot"))))
