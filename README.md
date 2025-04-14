# MBTA-Ticker
Informational ticker for an MBTA stop 


**Requirements**
---
`sudo apt install sbcl buildapp cl-yason`

`curl -O https://beta.quicklisp.org/quicklisp.lisp`

`sbcl --load quicklisp.lisp`

`(quicklisp-quickstart:install)`

This installs Quicklisp at the default location ~/quicklisp/setup.lisp


**Run as a script**
---

`sbcl --script main.lisp place-pktrm`

Takes a stop as an argument. In the above example place-pktrm is Park Street.
Find more stop names at https://www.mbta.com/

**Build an executable**
---
Use buildapp (https://www.xach.com/lisp/buildapp/):

`buildapp --load main.lisp --entry main --output MBTA-Ticker`

`chmod +x MBTA-Ticker`

Run

`./MBTA-ticker place-pktrm`