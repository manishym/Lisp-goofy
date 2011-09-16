


(defun american-representation (day month year)
  (if (and (between day 1 12)
           (between month 1 31)
           (<= year (current-year)))
      t))
(defun indian-representation (day month year)
  (if (and (between month 1 12)
           (between day 1 31)
           (<= year (current-year)))
      t))
(defun between (num low high)
  (if (and (> high low)
           (<= num high)
           (>= num low)) 
      num))
(defun current-year ()
  (multiple-value-bind (s min h d m y) 
      (get-decoded-time)
    y))
(defun read-date-from-string (str)
  (let ((date (cl-ppcre:split "\\.|-| |_|/" str)))
    (let ((day (read-from-string (first date)))
          (month (read-from-string (second date)))
          (year (read-from-string (third date))))
      (if (and (numberp day) (numberp month) (numberp year))
          (cond ((american-representation day month year)
                 (encode-universal-time 0 0 0 month day year))
                ( (indian-representation day month year)
                 (encode-universal-time 0 0 0 day month year))
                (t nil))))))