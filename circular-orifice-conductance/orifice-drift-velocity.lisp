(in-package #:coc)

(defun drift-maxw (v vd &optional (2m/KT 1d0))
  "Value of a drift maxwellian at V, drifting at speed VD

Optionally specify 2M/KT"
  (* (sqrt (/ 2m/kT
	      +pi+))
     (exp (- (* (expt (- v vd) 2)
		2m/kT)))))

(defconstant +drift-maxwellian+ 1
  "Selector for ")

(defconstant +truncated-drift-maxwellian+ 2
  "Selector for flux based on the truncated drift maxwellian")

(defgeneric phi% (model ud)
  (:documentation "Flux through the orifice as function of normalized
drift velocity UD

UD is normalized to the thermal velocity 2kT/m
MODEL can be one of +DRIFT-MAXWELLIAN+ or +TRUNCATED-DRIFT-MAXWELLIAN+")
  (:method ((model (eql +drift-maxwellian+)) ud)
    "Flux of a drift maxwellian"
    (+ (exp (- (expt ud 2)))
       (* ud (sqrt +pi+)
	  (+ (erf ud)
	     1d0))))
  (:method ((model (eql +truncated-drift-maxwellian+)) ud)
    "Flux of a truncated drift maxwellian"
    (+ (exp (- (expt ud 2)))
	(* 2 (sqrt +pi+)
	   ud
	   (erf ud)))))

(define-test phi%
  "Test limiting forms of phi%"
  (assert-numerical-equal 1d0 (phi% +drift-maxwellian+ 0d0))
  (assert-numerical-equal 1d0 (phi% +truncated-drift-maxwellian+ 0d0))
  (let ((ud 50d0))
    (assert-numerical-equal (* 2 (sqrt +pi+) 50d0) (phi% +drift-maxwellian+ ud))
    (assert-numerical-equal (* 2 (sqrt +pi+) 50d0) (phi% +truncated-drift-maxwellian+ ud))))

(defparameter +default-phi-method+
  +truncated-drift-maxwellian+
"Signals default method for calculating the orifice flux.  It can be one of:
+drift-maxwellian+
+truncated-drift-maxwellian+")

(defun phi (ud)
  "Calculate the orifice flux as function of normalized drift velocity
UD using the default method

The default method is specified with the variable
+default-phi-method+.  See its documentation for more info"
  (phi% +default-phi-method+ ud))

(let (mr/mc% phi%)
  ;; Environment for calculating the relative drift velocity, given the
  ;; flux and the mass ratio
  ;; 
  ;; It defines internal variables `mr/mc%' and `phi%' and uses the
  ;; root function `drift-at-flux%'
  (defun delta-phi-phi% (ud)
	     "Returns the difference between the phi calculated from `ud' and
`mr/mc%' and `phi%'"
	     (- (phi (* ud (sqrt mr/mc%)))
		phi%))

    (defun ud/phi/ (phi &optional (mr/mc 1d0))
      "Root finding routine that finds the normalized drift velocity as
function of the `phi' and mass ratio `mr/mc'"
      (setf phi% phi
	    mr/mc% mr/mc)
      (let ((max-iter 50)
	    (print-steps nil)
	    (solver
	     (gsll:make-one-dimensional-root-solver-f gsll:+brent-fsolver+
						      'delta-phi-phi%
						      0.0d0 5.0d0)))
	(when print-steps
	  (format t "iter ~6t   [lower ~24tupper] ~36troot ~44terr ~54terr(est)~&"))
	(loop for iter from 0
	   for root = (gsll:solution solver)
	   for lower = (gsll:fsolver-lower solver)
	   for upper = (gsll:fsolver-upper solver)
	   do (gsll:iterate solver)
	   while  (and (< iter max-iter)
		       (not (gsll:root-test-interval lower upper 0.0d0 0.001d0)))
	   do
	   (when print-steps
	     (format t "~d~6t~10,6f~18t~10,6f~28t~12,9f ~44t~10,4g ~10,4g~&"
		     iter lower upper
		     root (- root (sqrt 5.0d0))
		     (- upper lower)))
	   finally (return root)))))

(define-test ud/phi/
    (let* ((ud0 0.3d0)
	   (phi (phi ud0))
	   (ud1 (ud/phi/ phi)))
      (let ((lisp-unit:*epsilon* 1e-6))
	(assert-numerical-equal ud0 ud1))))

(defparameter +ud-max+
  (ud/phi/ 1.4733d0)
  "Maximum realistic value of the normalized drift velocity.

According to the experimental data of Fujimoto & Usami, the maximum
flux is 1.4733 times the thermal flux.  +ud-max+ is the corresponding
drift velocity (normalized to (sqrt 2kT/m))")