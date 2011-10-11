;;;; pcl-all.lisp

(in-package #:pcl-all)

;;; "pcl-all" goes here. Hacks and glory await!

;; A tutorial on clos. Before reading the book.
(defclass point ()
  (x
   y
   z))

(defun set-point-value (point x y z)
  (setf
   (slot-value point 'x) x
   (slot-value point 'y) y
   (slot-value point 'z) z
  point)