(load "~/quicklisp/setup.lisp")
(ql:quickload '(:dexador :yason :local-time))

(defun format-time (iso-time)
  "Minutes and seconds until arrival in MM:SS format."
  (let* ((diff-sec (local-time:timestamp-difference
                    (local-time:parse-timestring iso-time)
                    (local-time:now)))
         (minutes (floor diff-sec 60))
         (seconds (mod (round diff-sec) 60)))
    (format nil "~d:~2,'0d" minutes seconds)))

(defun get-destination (pred json)
  "Get trip headsign from included trips or fallback."
  (let* ((trip-rel (gethash "trip" (gethash "relationships" pred)))
         (trip-id (when trip-rel (gethash "id" (gethash "data" trip-rel))))
         (included (gethash "included" json)))
    (if (and trip-id included)
        (loop for item in included
              when (and (string= (gethash "type" item) "trip")
                        (string= (gethash "id" item) trip-id))
              return (or (gethash "headsign" (gethash "attributes" item))
                         "No Headsign")
              finally (return "No Matching Trip"))
        "No Trip Data")))

(defun get-route (pred json)
  "Get route ID from included routes or fallback."
  (let* ((route-rel (gethash "route" (gethash "relationships" pred)))
         (route-id (when route-rel (gethash "id" (gethash "data" route-rel))))
         (included (gethash "included" json)))
    (if (and route-id included)
        (loop for item in included
              when (and (string= (gethash "type" item) "route")
                        (string= (gethash "id" item) route-id))
              return (or (gethash "short_name" (gethash "attributes" item))
                         route-id)
              finally (return route-id))
        "Unknown Route")))

(defun get-stop-name (json stop-id)
  "Get stop name from included stops or fallback to 'Stop <stop-id>'."
  (let ((included (gethash "included" json)))
    (if included
        (loop for item in included
              when (and (string= (gethash "type" item) "stop")
                        (string= (gethash "id" item) stop-id))
              return (or (gethash "name" (gethash "attributes" item))
                         (format nil "Stop ~a" stop-id))
              finally (return (format nil "Stop ~a" stop-id)))
        (format nil "Stop ~a" stop-id))))


(defun format-ticker (json stop-id)
  "Format predictions as ticker with stop name and all routes/directions."
  (let ((stop-name (get-stop-name json stop-id))
        (predictions (gethash "data" json)))
    (if predictions
        (format nil "~a~%~{~a~%~}"
                stop-name
                (loop for pred in predictions
                      for arrival = (gethash "arrival_time" (gethash "attributes" pred))
                      for count from 1
                      when (and arrival (<= count 60))
                      collect (format nil "  Route ~a to ~a arrives in ~a"
                                      (get-route pred json)
                                      (get-destination pred json)
                                      (format-time arrival))))
        (format nil "~a~%No predictions available" stop-name))))

(defun main (stop-id)
  (let* ((url (format nil "https://api-v3.mbta.com/predictions?sort=arrival_time&filter%5Broute_type%5D=3&filter%5Bstop%5D=~a&include=trip,stop,route" stop-id))
         (json (yason:parse (dex:get url))))
    (write-string (format-ticker json stop-id))
    (terpri)))

;; Get command-line argument for stop ID
(let ((args (cdr sb-ext:*posix-argv*))) ;; skip script name
  (if args
      (main (first args))
      (format t "Usage: sbcl --script main.lisp <stop-id>~%")))