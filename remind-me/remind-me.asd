;;;; remind-me.asd

(asdf:defsystem #:remind-me
  :serial t
  :components ((:file "package")
               (:file "utilities")
               (:file "remind-me-layer1")
               (:file "remind-me")))

