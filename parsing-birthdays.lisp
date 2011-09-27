


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


(defun send-email (to subject body)
  (cl-smtp:send-email "smtp.gmail.com"
                      "mani.emsys@gmail.com"
                      to
                      subject
                      body
                      :ssl :tls
                      :authentication '(:login "mani.emsys@gmail.com" "real-password")))




(defun friend-equal (x y)
  (and (string= (friend-name x) (friend-name y))
       (string= (friend-email x) (friend-email y))))

;;DONE: need to fix this fun such that it will not push duplicates
(defun make-user-friends-list-from-file (user-obj file) 
  (dolist (x (rest (fare-csv:read-csv-file file)))                    
    (destructuring-bind (name email birthday) x
      (pushnew  
       (make-friend 
        :name name 
        :email email 
        :birthday (read-date-from-string birthday)) 
       (user-friends-list user-obj) :test #'friend-equal)))
  'done)