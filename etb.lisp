;;;;
;;;; End Taxation Bot
;;;; An example of using the Twitter Bot framework that just happens
;;;; to be live as @endtaxationbot
;;;;

(in-package :etb)

(defparameter *etb-user-name* "endtaxationbot")

;; The first time you run this bot in a new environment, evaluate
;; (twitter:repl-authenticate-user), enter the printed URL in your web browser,
;; when logged in to Twitter as *etb-user-name*, and paste the code into the repl.
;; OAuth parameters will be saved in ~/.cl-twitter/access/access.ht

(defun authenticate ()
  (twitter-bot:authenticate *etb-user-name*))

(defparameter *reply-messages*
  '("Taxation is extortion. End it."
    "Taxation is theft. End it."))

(defun get-reply-message ()
  (nth (random (length *reply-messages*)) *reply-messages*))

(defvar *total-replies* 0)

(defun total-replies ()
  *total-replies*)

(defun @reply-to (tweet &optional (msg (get-reply-message)))
  (twitter-bot:@reply-to tweet msg)
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

(defparameter *pause-between-tweets* 0.5)

(defun reply-to-search-results (tweets)
  (let ((first t))
    (twitter-bot:avoiding-duplicate-users (tweet tweets)
      (cond (first (setf first nil))
            (t (sleep *pause-between-tweets*)))
      (ignore-errors (@reply-to tweet)))))

(defun etb-step (bot idx)
  (declare (ignore bot idx))
  (let ((tweets (ignore-errors (find-taxation-tweets))))
    (reply-to-search-results tweets)))

(defvar *etb-bot* nil)

(defun start-etb ()
  (setf *etb-bot*
        (twitter-bot:make-bot
         :user-name *etb-user-name*
         :time-tails '("00")            ;tweet on the hour
         :step-function 'etb-step)))
