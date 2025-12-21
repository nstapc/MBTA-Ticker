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

**Download Pre-built Executable**
---
Pre-built executables are available in [GitHub Releases](https://github.com/NikoStapczynski/MBTA-Ticker/releases).

Download the latest `MBTA-Ticker` executable, make it executable, and run:

```bash
chmod +x MBTA-Ticker
./MBTA-Ticker place-pktrm
```

**Build from Source**
---
Use buildapp (https://www.xach.com/lisp/buildapp/):

`buildapp --load main.lisp --entry main --output MBTA-Ticker`

`chmod +x MBTA-Ticker`

Run

`./MBTA-ticker place-pktrm`
