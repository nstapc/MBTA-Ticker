# MBTA-Ticker
Informational ticker for an MBTA stop 


**Requirements**
---
`sudo apt install sbcl buildapp cl-yason`

`curl -O https://beta.quicklisp.org/quicklisp.lisp`

`sbcl --load quicklisp.lisp`

`(quicklisp-quickstart:install)`


**Run as a script**
---

`sbcl --script main.lisp`



**Build an executable**
---
Use buildapp (https://www.xach.com/lisp/buildapp/):

`buildapp --load main.lisp --entry main --output MBTA-Ticker`

`chmod +x MBTA-Ticker`

Run

`./MBTA-ticker`