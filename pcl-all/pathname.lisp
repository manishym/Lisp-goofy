(in-package :pcl-all)

(defun component-present-p (value)
  (and value (not (eql value :unspecific))))

(defun directory-pathname-p (p)
  (and (not (pathname-name p))
       (not (pathname-type p))
       p))

(defun pathname-as-directory (name)
  (let ((pathname (pathname name)))
    (when (wild-pathname-p pathname)
      (error "Cannot reliably convert wild pathname"))
    (if (not (directory-pathname-p pathname))
        (make-pathname :directory (append (or (pathname-directory pathname) (list :relative))
                                          (list (file-namestring pathname)))
                       :name nil
                       :type nil
                       :defaults pathname)
        pathname)))

(defun directory-wildcard (dirname)
  (make-pathname
   :name :wild
   :type #-clisp :wild #+clisp nil
   :defaults (pathname-as-directory dirname)));     PATHANME

(defun list-directory (dirname)
  (if (wild-pathname-p dirname)
      (error "Can only list concrete directory names")
      (directory (directory-wildcard dirname))))

(defun list-directory (dirname)
  (if (wild-pathname-p dirname)
      (error "Can only list concrete directory names")
      (let ((wildcard (directory-wildcard dirname)))

        #+(or sbcl lispworks cmu)
        (directory wildcard)
        ;; added ccl to openmcl case, since both have same options
        #+ (or ccl openmcl) (directory wildcard :directories t)
        #+allegro (directory wildcard :directories-are-files nil)
        #+clisp (nconc
                 (directory wildcard)
                 (directory (clisp-subdirectories-wildcard wildcard)))
        #-(or sbcl cmu lispworks openmcl allegro clisp ccl)
        (error "Directory listing not implemented"))))

(defun file-exists-p (filename)
  #+ (or sbcl lispworks openmcl ccl)
  (probe-file filename)
  #+ (or allegro cmu)
  (or (probe-file (pathname-as-directory filename))
      (probe-file filename))
  #+clisp
  (or (ignore-errors
        (probe-file (pathname-as-file pathname)))
      (ignore-errors
        (let ((directory-form (pathname-as-directory pathname)))

          (when (ext:probe-directory directory-form)
            directory-form))))
  #- (or sbcl lispworks openmcl allegro ccl cmu clisp)
  (error "file-exists-p not implemented"))

(defun pathname-as-file (pathname)
  (let ((pathname (pathname pathname)))
    (when (wild-pathname-p pathname)
        (error "Cannot reliably convert wild pathnames")
        )
    (if (directory-pathname-p pathname)
        (let* ((directory (pathname-directory pathname))
               (name-and-type (pathname (first (last directory)))))
          (make-pathname
           :directory (butlast directory)
           :name (pathname-name name-and-type)
           :type (pathname-type name-and-type)
           :defaults pathname)) pathname)))

(defun walk-directory (dirname fn &key directories (test (constantly t)))
  (labels
      ((walk (name)
         (cond
           ((directory-pathname-p name)
            (when (and directories (funcall test name))
              (funcall fn name))
            (dolist (x (list-directory name)) (walk x)))
           ((funcall test name) (funcall fn name)))))
    (walk (pathname-as-directory dirname))))