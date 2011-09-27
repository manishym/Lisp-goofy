;; defining member which uses eq instead of eql
;;  (member x choices :test #'eq)
;;  A macro     (memq x choices) should
;;  expand to (member x choices :test eq)
;;

(defmacro memq (obj lst) 
  `(member ,obj ,lst :test #'eq))

;;
;;(defmacro while (test &body body))
;;(do ()
;;((not hungry))
;;(body)

(defmacro while (test &body body)
  `(do ()
       ((not ,test))
     ,@body))

(defmacro mac (expr)
  `(pprint (macroexpand-1 ',expr)))

(defmacro when-bind ((var expr) &body body)
  `(let ((,var ,expr))
     (when ,var
       ,@body)))