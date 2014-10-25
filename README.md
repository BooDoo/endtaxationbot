A simple Twitter bot that replies to any mentions of the word
"taxation" with one of two messages:

* Taxation is theft. End it.
* Taxation is extortion. End it.

It searches for "taxation" once per hour, requesting ten replies since
the last reply it received. It replies to each search result, unless
that result is from or mentions a user in a tweet already replied to
from this search. The replies are spaced by one second.

Released under a BSD 2-Clause License. See the LICENSE file.

Known to work in Clozure Common Lisp, http://ccl.clozure.com/

To start, if "ccl" runs the lisp and the code is in ~/endtaxationbot/:

```
ccl -l "~/endtaxationbot/start" -e "(etb:start-bot-thread)"
```

Bill St. Clair
Created: 23 October 2014<br/>
Updated: 24 October 2014