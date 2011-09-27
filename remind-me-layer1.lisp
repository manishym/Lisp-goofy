;;; This is the file with all low level function. I am doing it because
;      I want a better way to be organized. Basically, I want to be able to hold the
;      top level of the application in my head.
(defpackage :remindme
  (:use :cl :hunchentoot :cl-who :cl-fad :parenscript :elephant :cl-ppcre))
(defmacro intopack (pack)
  `(in-package ,pack))
(intopack :remindme)

(defvar *log-lisp-errors-p* t)          ;hunchentoot variable to log
(defvar *log-lisp-warnings-p* t)        ;same for warnings
(defvar *lisp-warnings-log-level* :warning) 
(defvar *lisp-errors-log-level* :warning)
(defvar *show-lisp-errors-p* t)         ;shows error on browser
(defvar *tmp-test-directory*
  #+win32 #P"C:/hunchentoot-temp/"
  #-win32 #P"/tmp/hunchentoot-temp/")
(defvar *message-log-pathname*
  #+win32 #P"C:/hunchentoot-temp/upload-message.txt"
  #-win32 #P"/tmp/hunchentoot-temp/upload-message.txt")

;; Function to parse and handle dates
(defun american-representation (day month year)
  "this function parses data which is in american representation. Needs refactor."
  (if (and (between day 1 12)
           (between month 1 31)
           (<= year (current-year)))
      t))
(defun indian-representation (day month year)
  "Parses date if it is in indian representaion."
  (if (and (between month 1 12)
           (between day 1 31)
           (<= year (current-year)))
      t))
(defun between (num low high)
  "Returns if number is between low and high"
  (if (and (> high low)
           (<= num high)
           (>= num low)) 
      num))
(defun current-year ()
  "returns current year"
  (multiple-value-bind (s min h d m y) 
      (get-decoded-time)
    y))
(defun read-date-from-string (str)
  "reads date from strings like '26.12.1981' or like '12-26-1981'
parses any similar format as long as year is at the end. Needs refactor
Assumes by default it is american representation. An indian date like 2-6-1981 will
be misread as 6th feb 1981 instead of 2nd june 1981"
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

;;;End

(defun send-email (to subject body)
  "sends email from my gmail account Needs refactor, since it keeps my password in plain
text in the program itself."

  (cl-smtp:send-email "smtp.gmail.com"
                      "mani.emsys@gmail.com"
                      to
                      subject
                      body
                      :ssl :tls
                      :authentication '(:login "mani.emsys@gmail.com" "password")))
;; handles uploads
(defun handle-file (post-parameter)
  "Given post parameter in the form (path name content), it creates a file in
*tmp-test-directory*"
  (when (and post-parameter
             (listp post-parameter))
    (destructuring-bind (path file-name content-type) post-parameter       
      (let ((new-path (make-pathname :name file-name
                                     :type nil
                                     :defaults *tmp-test-directory*)))
        (log-message "New-path ~A" (pathname-name new-path))
        (copy-file path (ensure-directories-exist new-path) :overwrite t )
        new-path))))


;; Date time functions
(defun today ()
  "returns todays date in encoded format. Ignores s m and h"
  (multiple-value-bind (s m h day month year) (get-decoded-time)
    (encode-universal-time 0 0 0 day month year)))



(defun add-days (date no-of-days)
  (+ (* no-of-days (seconds-in-a-day))
     date))

(defun seconds-in-a-day ()
  (* 24 60 60))

(defun ignore-year (date)
  (multiple-value-bind (h m s day month year) (decode-universal-time date)
    (encode-universal-time 0 0 0 day month 0)))

(defun closest-date (date1 date2)
  (date< (ignore-year date1) (ignore-year date2)))


(defun encode-birthday-from-time (time)
  (multiple-value-bind (s m h day mon year) (decode-universal-time time)
    (format nil "~A.~A.~A" mon day year)))

(defun date< (date1 date2 &optional(ignore-year t))
  (let ((today (if ignore-year (ignore-year (today)) (today))))
    (let ((one (if ignore-year (- (ignore-year date1) today) (- date1  today)))
          (two (if ignore-year (- (ignore-year date2) today) (- date2 today))))
      (if (or (and (<= one 0) (<= two 0)) (and (>= one 0) (>= two 0)))
          (< one two)
          (if (< one 0)
              nil
              t
              )))))

(defun within-n-days (from date n)
  (let ((day (ignore-year date))
        (frm (ignore-year from)))
    (let ((diff (- frm day)))
      (between diff 0 (* n (seconds-in-a-day))))))
