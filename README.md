# MBTA-Ticker
Shows the ETA at an MBTA stop 


**Run**
---
`wget https://github.com/nstapczynski/MBTA-Ticker/raw/main/MBTA-Ticker`

`chmod +x MBTA-Ticker`

`./MBTA-ticker`






**Evaluate**
---
Test in the sbcl

`sbcl`

`(load "main.lsp")`

`(main 0)`



**Build**
---
To Build an executable use buildapp (https://www.xach.com/lisp/buildapp/):

`buildapp --load main.lsp --entry main --output MBTA-Ticker`

Run

`./MBTA-ticker`
