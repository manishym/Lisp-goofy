;;; remind me app
;;   Layer three of the application
(defvar *site* (make-instance 'acceptor :port 8000))

(define-easy-handler (my-uploader :uri "/upload") ()
  "this is a temporary handler for uploaded files. Needs refactoring.
It is verbatim copy of the tutorial post for uploading files"
  (let ((file (post-parameter "myfile")))
    (log-message :info "POST ~A" (post-parameter "file1"))
    (log-message :info "FILE ~A" file)
    (if file
        (let ((path (handle-file file)))
          (log-message :info "Saved file to ~A" path)))
    (with-html 
      (:html 
       (:head (:title "Upload a file"))
       (:body 
        (:form :action "/upload" :method :post :enctype "multipart/form-data"
               (:input :type :file :name "file1")
               (:input :type :submit)))))))


(define-easy-handler (remindme-stylesheet :uri "/remindme.css") ()
  (setf (hunchentoot:content-type*) "text/css")
  (css-lite:css
    (("body") 
     (:width "70%"
             :align "center"
             :background-color "#dededc"
             :padding "20px"
             :border "2px solid #000")))
  )




(define-easy-handler (login-user :uri "/login-user") ()
  (let ((name (post-parameter "name"))
        (password (post-parameter "password")))
    (let ((user (get-user name)))
      (if user
          (let ((real-password (user-password user)))
            (if (string= password real-password)
                (progn (setf (session-value 'current-user) user)
                       (redirect "/new-user-home"))
                (redirect "/create-user")))
          (redirect "/create-user")))))



(define-easy-handler (new-user :uri "/new-user") ()
  "To create users from the form submitted by user"
  (let ((file (post-parameter "file1"))
        (name (post-parameter "name"))
        (password (post-parameter "password")))
    (log-message :info "POST ~A" (post-parameter "file1"))
    (log-message :info "POST:name ~A" name) 
    (log-message :info "FILE ~A" file)
    (when file
      (let ((path (handle-file file)))
        (log-message :info "Saved file to ~A" path)
        (setf (session-value 'current-user)  (create-user-from-form name password path))
        (redirect "/new-user-home" )))))



(defun create-css-dispatcher (file-name)
  (labels ((uri ()
             (format nil "/public/css/blueprint/~A" file-name))
           (path ()
             (format nil "/home/manish/projects/lisp/public/css/blueprint/~A" file-name)))
                
    (push (create-static-file-dispatcher-and-handler (uri) (path) "text/css")
          *dispatch-table*)))







(define-easy-handler (create-user :uri "/create-user") ()
  (standard-page (:title "Create a new assistant")
    (:h3 "Create a new assistant to remind you and your friends about upcoming birthdays in a few seconds.")
    (:div :class "span-8" (:form :class "register" :id "new-user" :action "/new-user" :method :post :enctype "multipart/form-data"
                                 (:p "Name: " (:br)  (:input :type :string :name "name" :label "Name"))
                                 (:p "Password: " (:br) (:input :type :password :name "password") (:br))
                                 (:p "Confirm Password: " (:br) (:input :type :password :name "confirmpassword"))
                                 (:p "File"  (:input :type :file :name "file1"))
                                 (:input :type :submit :value "Create user"))
          (:form :class "login" :id "login" :action "/login-user" :method :post
                 (:p "Name: " (:br) (:input :type :string :name "name" :label "Name"))
                 (:p "Password: " (:br) (:input :type :password :name "password"))
                 (:input :type :submit :value "Login")))))

(define-easy-handler (logout :uri "/logout") ()
  (when-logged-in 
    (delete-session-value 'user)
    (redirect "/create-user")))

(defmacro when-logged-in (&body body)
  `(if (logged-in?)
       (progn 
         ,@body)
       (redirect "/create-user")))

(defun logged-in? ()
  (session-value 'user))



(defun logout-link ()
  (with-html-output-to-string (s)
    (:a :href "/logout" "Logout"))
  "")






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
                    :href "/remindme.css")
             (:link :type "text/css"
                    :rel "stylesheet"
                    :href "/public/css/blueprint/screen.css"
                    :media "screen, projection")
             (:link :type "text/css"
                    :rel "stylesheet"
                    :href "/public/css/blueprint/print.css"
                    :media "print")
             (:link :type "text/css"
                    :rel "stylesheet"
                    :href "/public/css/blueprint/remindme.css"
                    )
             (:script :type "text/javascript" 
                      :src "http://ajax.googleapis.com/ajax/libs/jquery/1.6.4/jquery.min.js"))
            (:script :type "text/javascript"
                     :src "/public/jquery.validate.js")
            (:script :type "text/javascript"
                     :src "/public/remind-me.js")
            (:body
             (:div :class "container" (:div :id "header" :class "span-24"
                                            (:img :src "/logo.jpg"
                                                  :alt "Some image"
                                                  :class "logo")
                                            (:span :class "alt strapline"
                                                   "Your friend's birthday in your pocket"))
                   ,@body)))))

(define-easy-handler (new-user-home :uri "/new-user-home") ()
  (let ((user (session-value 'current-user)))
    (if user
        (standard-page (:title (format nil "Welcome ~A" (user-name user)))
          (:h3 (format nil "Listing friends email and birthdays for ~A"
                       (user-name user)))
          (:nav :class "navigation" (:a :href "/logout" "Sign out"))
          (:ul 
           (dolist (x (user-friends-list user))
             (htm (:li (:span  :class "name" (str(friend-name x)))
                       (:span :class "email" (str (friend-email x)))
                       (:span :class "birthday" (str (encode-birthday-from-time 
                                                      (friend-birthday x))))))))
          (:form :class "add-friend" :action "/add-friend-to-user" :method :post
                 (:p "Name: " (:input :type :string :name "name" :label "Name")
                     "Email: " (:input :type :string :name "email" :label "Email")
                     "Birthday: " (:input :type :string :name "birthday" :label "birthday")
                     (:input :type :submit :value "Add Friend"))))
        
        (redirect "/create-user"))))