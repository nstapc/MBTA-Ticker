# MBTA-Ticker
Shows the ETA at an MBTA stop 


**To Run**
`wget ...`
`./MBTA-ticker`




**To Build**

To run lisp you have quite a few options (infinte?), this is just one of them

*On Debian*

Install sbcl or clisp or both
`sudo apt install sbcl clisp`

Install lish too (https://github.com/nibbula/lish)

Evaluate main.lsp
`clisp main.lsp`

Test in the REPL that dislikes you the least
`(main 0)`

To export to an executable use buildapp (https://www.xach.com/lisp/buildapp/):
`buildapp --eval '(load "main.lsp")' --entry main --output MBTA-ticker`

Run
`./MBTA-ticker`
