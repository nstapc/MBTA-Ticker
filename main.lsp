(load "quicklisp.lisp")
(load "/home/mary/quicklisp/setup.lisp")
;(ql:quickload :drakma)
(ql:quickload :dexador)



;; (defpackage #:basic-test
;;  (:use :cl))

;; (in-package #:basic-test)

;; (defvar x 10)

;; (print x)

;; (drakma:http-request
;; "https://api-v3.mbta.com/vehicles?filter%5Bdirection_id%5D=0%"
;; )

;(defun main (argv) (declare (ignore argv)) 
(setq res
(dex:get
"https://api-v3.mbta.com/predictions?sort=arrival_time&filter%5Bdirection_id%5D=1&filter%5Broute_type%5D=3&filter%5Bstop%5D=634&filter%5Broute%5D=34"
)
)

;)

;(dex:head "http://www.sbcl.org/" :verbose t)
