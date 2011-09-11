(defpackage :retro-games
  (:use :cl :cl-who :hunchentoot :parenscript :elephant))
(in-package :retro-games)

(defclass game ()
  ((name :reader name
         :initarg :name)
   (votes :accessor votes
          :initform 0)))
(defmethod vote-for (user-selected-game)
  (incf (votes user-selected-game)))
(defvar *games* nil)

(defun game-stored? (name)
  (game-from-name name))


(defmacro standard-page ((&key title) &body body)
  `(with-html-output-to-string (*standard-output* nil :prologue t :indent t)
     (:html :xmlns "http://www.w3.org/1999/xhtml"
            :xml\:lang "en"
            :lang "en"
            (:head
             (:meta :http-equiv "Content-Type"
                    :content "text/html;charset=utf-8")
             (:title ,title)
             (:link :type "text/css"
                    :rel "stylesheet"
                    :href "/retro.css"))
            (:body
             (:div :id "header"
                   (:img :src "/logo.jpg"
                         :alt "Some image"
                         :class "logo")
                   (:span :class "strapline"
                          "Vote on your favourite Retro Game"))
             ,@body))))

(defmacro define-url-function ((name) &body body)
  `(progn
     (defun ,name()
       ,@body)
     (push (create-prefix-dispatcher ,(format nil "/~(~A~)" name) ',name) *dispatch-table*)))

(define-url-function (retro-games)
  (standard-page (:title "Top Retro Games")
    (:h1 "Vote on your favourite retro games!")
    (:p "Missing a game? Make it available for votes " (:a :href "new-game" "here"))
    (:h2 "Current stand")
    (:div :id "chart"
          (:ol
           (dolist (game (games))
             (htm
              (:li
               (:a :href (format nil "vote?name=~A" (name game)) "Vote!")
               (fmt "~A with ~d votes" (name game) (votes game)))))))))
(define-url-function (vote)
  (let ((game (game-from-name (parameter "name"))))
    (if game
        (vote-for game))
    (redirect "/retro-games")))

(define-url-function (new-game)
  (standard-page (:title "Add a new game")
    (:h1 "Add a new game to the chart")
    (:form :action "/game-added" :method "post"
                                        ;:onsubmit
                                        ;ps is not working right now, it is giving
                                        ;compilation error. Need to sort out after going through
                                        ;parenscript tutorial
                                        ;(str (ps
                                        ;(when (= name.value "")
                                        ;  (alert "Please enter a name")
                                        ; (return false))))
           (:p "What is the name of the game?" (:br)
               (:input :type "text"
                       :name "name"
                       :class "txt"))
           (:p (:input :type "submit"
                       :value "Add"
                       :class "btn")))))
(define-url-function (game-added)
  (let ((name (parameter "name")))
    (unless (or (null name) (zerop (length name)))
      (add-game name))
    (redirect "/retro-games")))

(defun games()
  (nreverse (get-instances-by-range 'persistant-game 'votes nil nil)))
(defun add-game (name)
  (with-transaction ()
    (unless (game-stored? name)
      (make-instance 'persistant-game :name name))))
(defun game-from-name (name)
  (get-instance-by-value 'persistant-game 'name name))
(defpclass persistant-game ()
  ((name :reader name
         :initarg :name
         :index t)
   (votes :accessor votes
          :initarg :votes
          :initform 0
          :index t)))

;(open-store '(:clsql (:sqlite3 "/home/manish/projects/lisp/web/learn/lfw")))