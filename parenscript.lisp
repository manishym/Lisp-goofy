(defpackage :ps-tutorial
  (:use :cl :cl-who :hunchentoot :parenscript :cl-fad))
(setf *js-string-delimiter* #\")

;; (start (make-instance 'acceptor :port 8080))
(define-easy-handler (tutorial1 :uri "/tutorial1") ()
  (with-html-output-to-string (s)
    (:html
     (:head (:title "Parenscript tutorial: 1st example"))
     (:body
      (:h2 "Parenscript tutorial")
      "please click on the link below." :br
      (:a :href "#" :onclick (ps (alert "Hello World"))
          "Hello World")))))


(defvar *slideshows* (make-hash-table :test 'equalp))
(defparameter *slideshow-root* "/slideshow/")

(defun add-slideshow (slideshow-name image-folder)
  ;; We do not want to add duplicate slideshows.
  (unless (gethash slideshow-name *slideshows*)
    (push (create-folder-dispatcher-and-handler
           (format nil "/slideshow-images/~A/" slideshow-name)
           image-folder)
          *dispatch-table*))
  (setf (gethash slideshow-name *slideshows*)
        ;; This lists all the files int the image-folder and encodes then into url as
        ;; specified in the the variable
        ;; *hunchentoot-default-external-format*
        (mapcar (lambda (pathname)
                  (url-encode (format nil "~A.~A"
                                      (pathname-name pathname)
                                      (pathname-type pathname))))
                (list-directory image-folder))))

(add-slideshow "reception" "/home/manish/photos/Reception/")
(add-slideshow "wedding" "/home/manish/photos/Wedding/")

(defmacro/ps slideshow-image-uri (slideshow-name image-file)
  `(concatenate 'string "/slideshow-images/" ,slideshow-name "/" ,image-file))


(defun slideshow-handler ()
  (cl-ppcre:register-groups-bind (slideshow-name) ("/slideshows/(.*)" (script-name*))
    (let* ((images (gethash slideshow-name *slideshows*))
           (current-image-index (or (position (get-parameter "image") images :test #'equalp)
                                    0))
           (previous-image-index (max 0 (1- current-image-index)))
           (next-image-index (min (1- (length images)) (1+ current-image-index))))
      (with-html-output-to-string (s)
        (:html
         (:head
          (:title "Moments of my life")
          (:script :type "text/javascript"
                   (str (ps* `(progn
                                (var *slideshow-name* ,slideshow-name)
                                (var *images* (array ,@images))
                                (var *current-image-index* ,current-image-index)))))
          (:script :type "text/javascript" :src "/slideshow.js"))
         (:body
          (:div :id "slideshow-container"
                :style "width:100%;text-align:center"
                (:img :id "slideshow-img-object"
                      :src (slideshow-image-uri
                            slideshow-name
                            (elt images  current-image-index))) 
                :br
                (:a :href (format nil "/slideshows/~A?image=~A" slideshow-name  (elt images previous-image-index))
                    :onclick (ps (previous-image) (return false))
                    "Previous")
                (:a :href "#" :onclick (ps (play-slideshow)) (:i " Play "))
                (:a :href (format nil "/slideshows/~A?image=~A"
                                  slideshow-name
                                  (elt images next-image-index))
                    :onclick (ps (next-image) (return false))
                    "Next"))))))))

(push (create-prefix-dispatcher "/slideshows/" 'slideshow-handler) 
      *dispatch-table*)


(define-easy-handler (js-slideshow :uri "/slideshow.js") ()
  (setf (content-type*) "text/javascript")
  (ps
    (define-symbol-macro fragment-identifier (@ window location hash))
    
    (defun show-image-number (image-index)
      (let ((image-name (aref *images* (setf *current-image-index image-index))))
            
        (setf (chain document (get-element-by-id "slideshow-img-object") src)
              (slideshow-image-uri *slideshow-name* image-name)
              fragment-identifier
              image-name)))
    (defun previous-image ()
      (when (> *current-image-index* 0)
        (show-image-number (decf *current-image-index*))))
    (defun next-image ()
      (when (< *current-image-index* (1- (getprop *images* 'length)))
        (show-image-number (incf *current-image-index*))))
    (defun play-slideshow ()
      (progn
        (set-timeout #'play-slideshow 2000)
        (next-image)))
    (setf (getprop window 'onload)
          (lambda ()
            (when fragment-identifier
              (let ((image-name (chain fragment-identifier (slice 1))))
                (dotimes (i (length *images* ))
                  (when (string= image-name (aref *images* i))
                    (show-image-number i)))))))))





