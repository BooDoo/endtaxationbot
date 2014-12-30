Introduction
============

A simple Twitter bot framework with two examples.

Written in Common Lisp. Depends on QuickLisp. I have only tested it in
CCL (Clozure Common Lisp), http://ccl.clozure.com/

The scripts below expect that you have installed CCL, and that it is
in your path as the `ccl` command.

The :twitter-bot system includes the framework plus an "End Taxation
Bot". It will run stand-alone.

The :etwof-quote-bot system includes a bot that tweets random quotes
from my End the War on Freedom blog. It requires LispLog.

Loading
-------

To load the :twitter-bot system:

    cd ~/endtaxationbot
    ccl -l start

Then, to start the End Taxation bot:

    (etb:start-etb)

To load the :etwof-quote-bot system, start LispLog, then execute:

   (load "~/endtaxationbot/start-eqb")

End Taxation Bot
----------------

The End Taxation Bot tweets one of two sentences:

* Taxation is theft. End it.
* Taxation is extortion. End it.

It searches for "taxation" once per hour, requesting ten replies since
the last reply it received. It replies to each search result, unless
that result is from or mentions a user in a tweet already replied to
from this search. The replies are spaced by one second.

License
-------

Released under a BSD 2-Clause License. See the LICENSE file.

Bill St. Clair<br/>
Created: 23 October 2014<br/>
Updated: 29 December 2014