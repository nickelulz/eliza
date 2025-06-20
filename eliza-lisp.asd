(asdf:defsystem #:eliza-lisp
  :description "A Common Lisp implementation of Dr. J. Weizenbaum's ELIZA."
  :author "M. Joseph Machaya <mufaro2@student.ubc.ca>"
  :license "MIT"
  :version "0.1.0"
  :serial t
  :components ((:file "package")
               (:file "main")
               (:module "src")))
