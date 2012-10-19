;;;; package.lisp

(defpackage #:circular-orifice-conductance-user
  (:nicknames #:coc-user)
  (:use #:cl #:lisp-unit #:mv-gnuplot #:mv-grid
	#:circular-orifice-conductance)
  (:documentation 
"User package for CIRCULAR-ORIFICE-CONDUCTANCE package.

It servers as use documentation and template for packages that build
on top of CIRCULAR-ORIFICE-CONDUCTANCE"))

