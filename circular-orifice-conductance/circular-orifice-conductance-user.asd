;;;; circular-orifice-conductance-user.asd

(asdf:defsystem #:circular-orifice-conductance-user
  :serial t
  :depends-on (#:alexandria
	       #:gsll
               #:foreign-array
	       #:grid
	       #:mv-gnuplot
               #:lisp-unit
	       #:mv-grid-utils
	       #:circular-orifice-conductance)
  :components ((:file "circular-orifice-conductance-user-package-def")
	       (:file "example-plots")))