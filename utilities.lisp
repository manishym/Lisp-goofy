;;;; Utilities as in paul graham's on lisp.

(proclaim '(inline last1 single append1 conc1 mklist))
;;; last1
;;         Gives  the last element of a list, not the last cdr
;;; single
;;         Tests if a list is single. It is more efficient than length list 1
;;; append1
;;         Appends a symbol to a list. like append, but for atoms
;;; conc1
;;         Same as nconc, but for atom
;;; mklist
;;         if arg is list, leaves as it is, else encapulates it in a list
(defun last1 (lst)
  (car (last lst)))

(defun single (lst)
  (and (consp lst) (not (cdr lst))))

(defun mklist (obj)
  (if (listp obj)
      obj
      (list obj)))

(defun append1 (lst obj)
  (append lst (mklist obj)))

(defun conc1 (lst obj)
  (nconc lst (mklist obj)))

(defun longer (x y)
  "Checks if x is longer than y, but is more efficient than comparing lengths"
  (labels ((compare (x y)
             (and (consp x)
                  (or (null y)
                      (compare (cdr x) (cdr y))))))
    (if (and (listp x) (listp y))
        (compare x y)
        (> (length x) (length y)))))

(defun filter (fn lst)
  (let ((acc nil))
    (dolist (x lst)
      (let ((val (funcall fn x)))
        (if val
            (push val acc))))
    (nreverse acc)))

(defun group (source n)
  (if (zerop n) (error "Zero length"))
  (labels ((rec (source acc)
             (let ((rest (nthcdr n source)))
               (if (consp rest)
                   (rec rest (cons (subseq source 0 n) acc))
                   (nreverse (cons source acc))))))
    (if source (rec source nil) nil)))

(defun flatten (tree)
  "Flattens a tree into a list"
  (labels ((rec (tree acc)
             (cond ((null tree) acc)
                   ((atom tree) (cons tree acc))
                   (t (rec (car tree) (rec (cdr tree) acc))))))
    (rec tree nil)))

(defun prune (test tree)
  "This is remove-if-not for trees"
  (labels ((rec (tree acc)
             (cond ((null tree) (nreverse acc))
                   ((consp (car tree))
                    (rec (cdr tree)
                         (cons (rec (car tree) nil) acc)))
                   (t (rec (cdr tree)
                           (if (funcall test (car tree))
                               acc
                               (cons (car tree) acc)))))))
    (rec tree nil)))

(defun find2 (fn lst)
  (if (null lst)
      nil
      (let ((val (funcall fn (car lst))))
        (if val
            (values (car lst) val)
            (find2 fn (cdr lst))))))

(defun before (x y lst &key (test #'eql))
  (and lst
       (let ((first (first lst)))
         (cond ((funcall test y first) nil)
               ((funcall test x first) lst)
               (t (before x y (cdr lst) :test test))))))

(defun after (x y lst &key (test #'eql))
  (let ((rest (before y x lst :test test)))
    (and rest (member x rest :test test))))

(defun duplicate (obj lst &key (test #'eql))
  (member obj (cdr (member obj lst :test test))
          :test test))

(defun split-if (fn lst)
  (let ((acc nil))
    (do ((src lst (cdr src)))
        ((or (null src) (funcall fn (car src)))
         (values (nreverse acc) src))
      (push (car src) acc))))


(defun most (fn lst)
  (if (null lst)
      (values nil nil)
      (let* ((wins (car lst))
             (max (funcall fn wins)))
        (dolist (obj (cdr lst))
          (let ((score (funcall fn obj)))
            (when (> score max)
              (setq wins obj
                    max score))))
        (values wins max))))

(defun best (fn lst)
  (if (null lst)
      nil
      (let ((wins (car lst)))
        (dolist (obj (cdr lst))
          (if (funcall fn obj wins)
              (setq wins obj)))
        wins)))


(defun mostn (fn lst)
  (if (null lst)
      (values nil nil)
      (let ((result (list (car lst)))
            (max (funcall fn (car lst))))
        (dolist (obj (cdr lst))
          (let ((score (funcall fn obj)))
            (cond ((> score max)
                   (setq max score
                         result (list obj)))
                  ((= score max) 
                   (push obj result)))))
        (values (nreverse result) max))))