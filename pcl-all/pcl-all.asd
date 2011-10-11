;;;; pcl-all.asd

(asdf:defsystem #:pcl-all
  :serial t
  :components ((:file "package")
               (:file "pcl-all")
               (:file "pathname")))

