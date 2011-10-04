;;;;; looping constructs practice
;;;
(do ((x 0 (1+ x))                       ; variable initialization form
                                        ; 
     (y 3 (+ y 3))                      ; can initialize multiple variables at a time
     (z 2 (+ z 2)))                     ; 
    ((or
      
      (> y 30)                          ; test form also has return value
      (> x 11)
      (> z 40)) x)                      ; x is return value
  (format t "~A:~10t~A - ~10T~A~%" x y z)) ; body

(defun nth-fib (num)
  (do ((n 0 (1+ n))
       (cur 0 next)
       (next 1 (+ cur next)))
      ((= num n) cur)))

(defun print-fibs (num)
  (do ((n 0 (1+ n))
       (cur 0 next)
       (next 1 (+ cur next)))
      ((= num n) cur)
    (format t "~A~%" cur)))

;;; Final format of do
;; (do (variable-form)
;;    (test-and-result-form)
;;   body-form)
;; 
;; some loops
(loop for x across "The quick brown fox jumped over the lazy dog"
   counting (find x "aeiou"))

(loop for i below 10
              and a = 0 then b
              and b = 1 then (+ a b)
              finally (return a))


(defun prime-p (num)
  (when (> num 1)
    (loop for fac from 2 to (isqrt num) never (zerop (mod num fac)))))

(defun next-prime (num)
  (loop for n from num when (prime-p n) return n))

(defmacro doprimes ((var init final) &body body)
  (with-gensyms (ending-value-name)
    `(do ((,var (next-prime ,init) (next-prime (1+ ,var)))
          (,ending-value-name ,final))
         ((> ,var ,ending-value-name) ,ending-value-name)
       ,@body)))

(defmacro with-gensyms ((&rest bindings) &body body)
  `(let ,(loop for b in bindings collecting `(,b (gensym)))
     ,@body))

(defmacro once-only ((&rest names) &body body)
  (let ((gensyms (loop for n in names collect (gensym))))
    `(let (,@(loop for g in gensyms collect `(,g (gensym))))
       `(let (,,@(loop for g in gensyms for n in names collect ``(,,g ,,n)))
          ,(let (,@(loop for n in names for g in gensyms collect `(,n ,g)))
                ,@body)))))

