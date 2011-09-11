;;;;; The General problem solver was a AI program that was written to solve most of the
;;  problems using a computer. Although it did not live upto the expctation, it
;;  helped AI research further

;;; Problem Description
;;; Problem Specification
;;; The implementation
;;; Testing
;;; Debugging and Analysis


;;; Description
;;    Means End Analysis 
;;    We can solve a problem if we can find some way to eliminate
;;    the difference between what we have and we want.
;;    Means end analysis is a choice, we can also start from current situation and
;;    search forward to the goal.
;;    preconditions as sub problems.

;;; Specification
;;    * Current state as a list
;;    * Goal as a list
;;    * Allowable operator as a list. This list is unchanged over course of the
;;       problem, but may change when solving a new problem
;;    * An operator is represented as a structure composed of an action, a list of
;;      preconditions and a list of effects.
;;    * A complete problem:
;;      1. start state
;;      2. End state
;;      3. known operators.
;;    * (GPS '(unknown poor) '(rich famous) list-of-ops
;;    * A single goal can be achieved in two ways.
;;      1. If it is already in current state, its achieved.
;;      2. Find some operator and try to apply it.
;;    * An operator is appropriate if the goal is in operator's add-list
;;    * We can apply an operator if we can meet all the preconditions, which
;;      are sub goals.
;;

;;; Implementation

;;; Utils
(defun find-all (item sequence &rest keyword-args
                 &key (test #'eql) test-not &allow-other-keys)
  "Find all the elements of the sequence that match the test."
  (if test-not
      (apply #'remove item sequence
             :test-not (complement test-not) keyword-args)
      (apply #'remove item sequence
             :test (complement test) keyword-args)))

;;; GPS

(defvar *state* nil "Current state. A list of conditions")
(defvar *ops* nil "A list of allowable operators")
(defstruct op
  "An operation"
  (action nil) (preconditions nil) (add-list nil) (del-list nil))
(defun GPS (*state* goals *ops*)
  "General problem solver: achieve all goals using *ops*"
  (if (every #'achieve goals) 'solved))
(defun achieve (goal)
  "A goal is achieved if it already holds,
   or if threre is an appropriate op for it that is applicable."
  (or (member goal *state*)
      (some #'apply-op
            (find-all goal *ops* :test #'appropriate-p))))
(defun appropriate-p (goal op)
  "An op is appropriate if to a goal if it is in its add-to list"
  (member goal (op-add-list op)))
(defun apply-op (op)
  "Print a message and update *state* it op is applicable."
  (when (every #'achieve (op-preconditions op))
    (print (list 'executing (op-action op)))
    (setf *state* (set-difference *state* (op-del-list op)))
    (setf *state* (union *state* (op-add-list op)))
    t))
