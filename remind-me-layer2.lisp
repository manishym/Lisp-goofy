(defstruct user
  "A structure to hold users"
  name
  password
  friends-list)

(defvar *REMAINDER-DAYS* 3)

(defvar *database-of-users*
  (make-hash-table :test #'equalp))

(defstruct friend
  "A structure to hold friends of a user"
  name
  email
  birthday)

(defun add-user (name password &optional (friends-list nil))
  "Add user given a name, password and a list of friends.
List can be left empty and added later."
  (let ((user (make-user 
               :name name
               :password password
               :friends-list friends-list)))
    (add-user-obj user)))

(defun remove-user (name)
  "Removes a user from database given the user's name."
  (remhash name *database-of-users*))

(defun get-user (name)
  "Returns a user object associated with the name. "
  (gethash name *database-of-users*))

(defun find-friend (name friend-name)
  "Given a user name and a friend name, returns the friend's user object if the
friend exists in the database"
  (find-if #'(lambda (y) (let ((x (friend-name y))) (string= x friend-name)))
           (user-friends-list (get-user name))))



(defun add-user-obj (user-obj &optional (database *database-of-users*))
  "Adds user to the database of users. This takes a user-object as opposed user-name taken by add-user function."
  (setf (gethash (user-name user-obj) database) user-obj))

(defun friend-equal (x y)
  "Tests if friends are same in two structures. A friend is same, if he has same name and same email according to this logic. May need to change based on feedback"
  (and (string= (friend-name x) (friend-name y))
       (string= (friend-email x) (friend-email y))))

;;DONE: need to fix this fun such that it will not push duplicates
(defun make-user-friends-list-from-file (user-obj file)
  "Creates a friend list and adds it to user. May need refactor, since adding the list
to user and creating the list are two different things.
Assumes that first row is useless since it represents name,email,birthday. May need to
fix that to see if first row is valid data.
Also need to validate the emails provided."
  (let ((acc nil))
    (dolist (x (rest (fare-csv:read-csv-file file)))                    
      (destructuring-bind (name email birthday) x
        (pushnew  
         (make-friend 
          :name name 
          :email email 
          :birthday (read-date-from-string birthday)) 
         acc :test #'friend-equal)))
    (setf (user-friends-list user-obj) (sort acc #'closest-date :key #'friend-birthday))))




(defun get-user-and-birthday-friend-if-birthday-is-near (user)
  (if (within-n-days (friend-birthday (first (user-friends-list user))) (ignore-year (today)) *remainder-days*)
      user
      nil))





(defun get-email-body (name f-name bdate)
  (format nil "Hi ~A, ~% Your friend ~A's birthday is coming up on ~A. Do not forget to wish!!~%
Cheers,~%
Remind me~%" name f-name bdate))
(defun get-email-subject (f-name bdate)
  (format nil "Your friend ~A's birthday coming up on ~A" f-name bdate))
(defun send-birthday-email-to-friend-list (birthday-boy friend-list)
  (dolist (friend  friend-list)
    (let ((name (friend-name friend))
          (email (friend-email friend))
          (fn (friend-name birthday-boy))
          (bdate (encode-birthday-from-time (friend-birthday birthday-boy))))
      (send-email email (get-email-subject fn bdate) (get-email-body name fn bdate)))))
(defun make-birthday-mailer-function (user friend)
  (let ((f-list (user-friends-list user)))
              
    #'(lambda () (send-birthday-email-to-friend-list friend (remove-if 
                                                             #'(lambda (x) 
                                                                 (friend-equal x friend)) f-list)))))

(defun get-day-from-bday (friend)
  (fourth (multiple-value-list (decode-universal-time (friend-birthday friend)))))
(defun get-month-from-bday (friend)
  (fifth (multiple-value-list (decode-universal-time (friend-birthday friend)))))

(defun schedule-birthday-mailer (user friend)
  (clon:schedule-function 
   (make-birthday-mailer-function user friend) 
   (clon:make-scheduler (clon:make-typed-cron-schedule :second 0 :minute 0 :hour 0
                                                       :day-of-month (get-day-from-bday friend)
                                                       :month (get-month-from-bday friend))) :name (friend-name friend) :thread t))

(defun schedule-birthdays (user)
  (dolist (friend (user-friends-list user))
    (schedule-birthday-mailer user friend)))



(defun create-user-from-form (name password friend-file)
  "This function is to create user and add it to database from html form.
Arguments are user name, password and path to friend file. This path is returned from
handle-file function."
  (let ((user (make-user :name name :password password)))
    (make-user-friends-list-from-file user friend-file)
    (add-user-obj user)
    (schedule-birthdays user)
    user))

(defun add-friend-from-form (name email birthday-string)
  (let ((frnd (make-friend :name name :email email
                           :birthday (read-from-string birthday-string)))
        (user (session-value 'current-user)))
    (push frnd (user-friends-list user))
    (schedule-birthday-mailer user frnd)))