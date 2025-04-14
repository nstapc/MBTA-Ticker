(load "~/quicklisp/setup.lisp")
(ql:quickload '(:dexador :yason :local-time))

(defun format-time (iso-time)
  "Convert ISO 8601 time to minutes until arrival."
  (let* ((parsed-time (local-time:parse-timestring iso-time))
         (now (local-time:now))
         (diff (local-time:timestamp-difference parsed-time now)))
    (round (/ diff 60))))

(defun get-destination (prediction)
  "Extract trip headsign (destination) from prediction relationships."
  (let* ((trip-id (gethash "id" (gethash "trip" (gethash "relationships" prediction))))
         ;; Mock trip headsign; replace with actual data if available
         (headsign (concatenate 'string "Destination " trip-id)))
    headsign))

(defun format-ticker (json-data)
  "Format predictions into a ticker string."
  (let ((predictions (gethash "data" json-data))
        (ticker-items '()))
    (loop for pred in predictions
          for attrs = (gethash "attributes" pred)
          for arrival = (gethash "arrival_time" attrs)
          when arrival
          do (let* ((minutes (format-time arrival))
                    (route "34")
                    (destination (get-destination pred))
                    (ticker-str (format nil "Route ~a to ~a arriving in ~a min"
                                        route destination minutes)))
               (push ticker-str ticker-items)))
    (format nil "~{~a | ~}" (nreverse ticker-items))))

(defun main (argv)
  (declare (ignore argv))
  (let* ((url "https://api-v3.mbta.com/predictions?sort=arrival_time&filter%5Bdirection_id%5D=1&filter%5Broute_type%5D=3&filter%5Bstop%5D=634&filter%5Broute%5D=34")
         (response (dex:get url))
         (json-data (yason:parse response)))
    (write-string (format-ticker json-data))
    (terpri)))

(main 0)