;;;; cl-ring.asd

(asdf:defsystem #:cl-ring
  :description "Describe cl-ring here"
  :author "Your Name <your.name@example.com>"
  :license "Specify license here"
  :serial t
  :depends-on (#:alexandria #:ironclad #:house #:cl-json #:cl-who #:bordeaux-threads #:trivial-shell)
  :components ((:file "package")
               (:file "cl-ring")))

