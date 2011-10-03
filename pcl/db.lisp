;; from book practicle common lisp
;;

(defvar *db* nil)
(defun make-cd (title artist rating ripped)
  (list :title title :artist artist :rating rating :ripped ripped))
(defun add-record (cd)
  (push cd *db*))

(defun dump-db ()
  (dolist (cd *db*)
    (format t "~{~A:~10t~A~%~}~%" cd))) ; ~{~} directive for format tells that the
                                        ; args must be list, which will be cared
                                        ; to get the actual args
                                        ; we can write the whole function
                                        ; in format like
                                        ; (format t "~{~{~A:~10t~A~%~}~%~}" *db*)

(defun prompt-read (prompt)
  (format *query-io* "~A: " prompt)
  (force-output *query-io*)
  (read-line *query-io*))               ; *query-io* is a global variable
                                        ; containing the input stream connected
                                        ; to the terminal
(defun prompt-for-cd ()
  (make-cd
   (prompt-read "Title")
   (prompt-read "Artist")
   (or (parse-integer (prompt-read "Rating") :junk-allowed t) 0)
   (y-or-n-p  "Ripped? [y/n]")))

(defun add-cds ()
  (loop (add-record (prompt-for-cd))
     (if (not (y-or-n-p "Add another? [y/n]: ")) (return))))


(defun save-db (filename)
  (with-open-file (out filename
                       :direction :output
                       :if-exists :supersede)
    (with-standard-io-syntax
      (print *db* out))))

(defun load-db (filename)
  (with-open-file (in filename)
    (with-standard-io-syntax
      (setf *db* (read in)))))

(defun select (selector-fn)
  (remove-if-not selector-fn *db*))

(defun update ( selector-fn &key title artist rating (ripped nil ripped-p))
  (setf *db*
        (mapcar 
         #'(lambda (row)
             (when (funcall selector-fn row)
               (if title (setf (getf row :title) title))
               (if artist (setf (getf row :artist) artist))
               (if rating (setf (getf row :rating) rating))
               (if ripped-p (setf (getf row :ripped) ripped)))
             row) *db*)))

(defun make-comparision-expr (field value)
  `(equal (getf cd ,field) ,value))

(defun make-comparision-list (fields)
  (loop while fields
       collecting (make-comparision-expr (pop fields) (pop fields))))

(defmacro where (&rest clauses)
  `#'(lambda (cd) (and ,@(make-comparision-list clauses))))