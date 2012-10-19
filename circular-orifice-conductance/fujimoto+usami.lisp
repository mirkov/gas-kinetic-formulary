(in-package #:coc)

(defun reduced-conductance (delta &optional (invalid-arg-warn
						     nil))
  "Empirical reduced conductance formula for an orifice of Fujimoto &
Usami, ASME Trans. J. Fluids Eng. V.106,p.367, (1984)

Valid for 0<delta<20 (Sharipov,2004)

- `delta' is the rarefaction parameter

- if `invalid-arg-warn' is set to t, issue warning for delta>20.
  Negative delta are not permitted, and will be trapped by sqrt
  operation.

Quoted by Rainer, JVST-A 011002-6, 2011, Sharipov, J.Fluid.Mech.,
V518, p35, 2004"
  (when (and invalid-arg-warn
	     (> delta 20))
       (warn "Valid delta range: 0<delta<20, delta=~a" delta))
  (if (zerop delta) 1.0
      (+ 1.0
	 (/ (+ 0.4733 (/ 0.6005 (sqrt delta)))
	    (+ 1 (/ 4.559 delta) (/ 3.094 (expt delta 2)))))))

