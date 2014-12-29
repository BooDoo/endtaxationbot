;;;;
;;;; Load and start the etwof-quotes-bot
;;;;

(in-package :cl-user)

(unless (fboundp 'twitter-bot-relative-path)
  (load (merge-pathnames "start.lisp" *loading-file-source-file*)))

(load (twitter-bot-relative-path "etwof-quote-bot.asd"))
(ql:quickload :etwof-quote-bot :verbose t)

(eqb:start-bot)
