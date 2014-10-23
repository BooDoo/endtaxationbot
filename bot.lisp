(in-package :etb)

(defparameter *bot-user-name* "endtaxationbot")

;; The first time you run this bot in a new environment, evaluate
;; (twitter:repl-authenticate-user), enter the printed URL in your web browser,
;; when logged in to Twitter as *bot-user-name*, and paste the code into the repl.
;; OAuth parameters will be saved in ~/.cl-twitter/access/access.ht

(defun authenticate ()
  (twit:get-authenticated-user *bot-user-name*))

(defparameter *reply-messages*
  '("Taxation is extortion. End it."
    "Taxation is theft. End it."))

(defun get-reply-message ()
  (nth (random (length *reply-messages*)) *reply-messages*))

(defvar *total-replies* 0)

(defun total-replies ()
  *total-replies*)

(defun @reply-to (tweet &optional (msg (get-reply-message)))
  (twit:@reply-to tweet msg)
  (incf *total-replies*))

(defvar *since-id* nil)

(defparameter *search-result-count* 10)

(defun find-taxation-tweets ()
  (let ((res (twit:search-result-statuses
              (twit:search-twitter "taxation"
                                   :lang "en"
                                   :count *search-result-count*
                                   :since-id *since-id*))))
    (setf *since-id*
          (reduce #'max res
                  :initial-value (or *since-id* 0)
                  :key #'twit:tweet-id))
    (values res *since-id*)))

(defmacro avoiding-duplicate-users ((tweet tweets) &body body)
  (let ((thunk (gensym "THUNK-")))
    `(flet ((,thunk (,tweet)
              ,@body))
       (declare (dynamic-extent #',thunk))
       (call-avoiding-duplicate-users ,tweets #',thunk))))

(defun call-avoiding-duplicate-users (tweets thunk)
  (loop for tweet in tweets
     with already-seen = nil
     do
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
            (funcall thunk tweet))))

(defparameter *pause-between-tweets* 1)

(defun reply-to-search-results (tweets)
  (let ((first t))
    (avoiding-duplicate-users (tweet tweets)
      (cond (first (setf first nil))
            (t (sleep *pause-between-tweets*)))
      (ignore-errors (@reply-to tweet)))))

(defvar *bot-thread-period* 3600
  "Number of seconds between searches by the bot.")

(defun bot-thread-loop ()
  (authenticate)
  (loop for tweets = (ignore-errors (find-taxation-tweets))
     do
       (ignore-errors (reply-to-search-results tweets))
       (sleep *bot-thread-period*)))

(defvar *bot-thread* nil)

(defun start-bot-thread ()
  (unless (and *bot-thread*
               (bt:thread-alive-p *bot-thread*))
    (setf *bot-thread*
          (bt:make-thread 'bot-thread-loop :name "End Taxation Bot"))))
