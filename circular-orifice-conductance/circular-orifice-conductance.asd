;;;; circular-orifice-conductance.asd

(asdf:defsystem #:circular-orifice-conductance
  :serial t
  :depends-on (#:alexandria
               #:gsll
               #:lisp-unit)
  :components ((:file "circular-orifice-conductance-package-def")
	       (:file "setup")
	       (:file "fujimoto+usami")
	       (:file "orifice-drift-velocity")
               (:file "trace-gas-conductance")))

