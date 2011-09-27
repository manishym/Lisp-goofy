;;;; --------- On lisp: Chapter 6.----------
;;; closures as data representation tools.

;; 

(defun run-node (name)
  (let ((n (gethash name *nodes*)))
    (cond ((node-yes n)
           (format t "~A~%>> " (node-contents n))
           (case (read)
             (yes (run-node (node-yes n)))
             (t (run-node (node-no n)))))
          (t (node-contents n)))))
(defnode 'penny 'lincoln)
(defnode 'coin "is the coin a penny?" 'penny 'coins)
(defnode 'usa "Is he on a coin" 'coin 'cidence)
(defnode 'deadman "Was he American?" 'usa 'non-usa)
(defnode 'male "is he living" 'liveman 'deadman)
(defnode 'people "Is the person a man?" 'male 'female)
(defun defnode (name conts &optional yes no)
  (setf (gethash name *nodes*)
        (make-node :contents conts
                   :yes yes
                   :no no)))
(defvar *nodes* (make-hash-table))
(defstruct node contents yes no)

;;; did not write  compile-net, since it is not colsure and it does not make sense to
;   compile the network if you want to add one more node.