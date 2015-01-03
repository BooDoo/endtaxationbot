;;;;
;;;; Patch cl-twitter's JSON decoder to replace \uxxxx character codes that
;;;; are unknown to CL with a period.
;;;;

(in-package :twitter)

(export 'twitter-configuration)

(defun safe-decode-json (response-stream)
  (let ((json:*json-identifier-name-to-lisp* 'convert-from-twitter))
    (declare (special json:*json-identifier-name-to-lisp*
                      json:*lisp-identifier-name-to-json*))
    (handler-bind
        ;; This works around getting a \uxxxx code in a JSON string
        ;; that is unknown to CL. Twitter returns these sometimes.
        ((json:no-char-for-code (lambda (c)
                                  (declare (ignore c))
                                  (invoke-restart 'json:substitute-char #\.))))
      (json:decode-json response-stream))))

(define-command help/configuration (:get :identity)
    (twitter-app-uri "help/configuration.json")
    "Returns a json object representing Twitter's configuration.")

(defun twitter-configuration ()
  (apply 'twitter-op :help/configuration nil))
