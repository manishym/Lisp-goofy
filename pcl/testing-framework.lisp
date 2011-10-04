(defun report-result (result form)
  (format t "~:[FAIL~;pass~] ...~A: ~10t~A~%" result *test-name* form)
  result)

;; (check (= (+ 1 2) 3)) should expand to
;; (report-result (that 'that))

(defmacro check (&BODY forms)
  `(combine-results
     ,@(loop for f in forms collect `(report-result ,f ',f))))

(defmacro with-gensyms ((&rest bindings) &body body)
  `(let ,(loop for b in bindings collecting `(,b (gensym)))
     ,@body))

(defmacro combine-results (&body forms)
  (with-gensyms (result)
    `(let ((,result t))
       ,@(loop for f in forms collect `(unless ,f (setf ,result nil)))
       ,result)))
(defvar *test-name* nil)

(defmacro deftest (name parameters &body body)
  `(defun ,name ,parameters
     (let ((*test-name* (append *test-name* (list ',name))))
         ,@body)))
(deftest test- ()
    (check
      (= (- 3 2) 1)
      (= (- -3 -2) -1)))
(deftest test+ ()
  (check
    (= (+ 2 3) 5)
    (= (+ 100 567) 667)))

(deftest airthmetic ()
  (combine-results
    (test-)
    (test+)))