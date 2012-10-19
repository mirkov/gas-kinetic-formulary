;;;; package.lisp

(defpackage #:circular-orifice-conductance
  (:nicknames #:coc)
  (:use #:cl #:lisp-unit)
  (:import-from #:gsll
		#:make-one-dimensional-root-solver-f #:+brent-fsolver+
		#:solution
		#:fsolver-lower #:fsolver-upper #:root-test-interval
		#:iterate
		#:erf)
  (:export
   #:reduced-conductance
   #:drift-maxw #:+drift-maxwellian+ #:+truncated-drift-maxwellian+
   #:+default-phi-method+
   #:phi #:ud/phi/ #:phi%
   #:trace-gas-reduced-conductance
   #:+ud-max+)
  (:documentation 
"Implementation of formulas for circular orifice conductance into vacuum

This inludes 
- formula of Fujimoto & Usami
- my formula for gas drift velocity through the orifice
- my procedure for calculation of trace gas conductance"))

