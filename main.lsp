
(load "quicklisp.lisp")
(load "~/quicklisp/setup.lisp")
(ql:quickload :dexador)

(defun main (argv) (declare (ignore argv))
 (write-string
  (dex:get
   "https://api-v3.mbta.com/predictions?sort=arrival_time&filter%5Bdirection_id%5D=1&filter%5Broute_type%5D=3&filter%5Bstop%5D=634&filter%5Broute%5D=34"
  )
 )
)