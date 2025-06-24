(defpackage #:eliza
  (:use #:cl #:cl-ppcre #:alexandria)
  (:export #:main
	   ;; debugging
	   #:decomp->regex
	   #:apply-decomposition
	   #:apply-reconstruction
	   #:generate-response
	   #:view-rules))
