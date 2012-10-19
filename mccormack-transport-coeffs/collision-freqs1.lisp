(in-package :gkf)

(export '(nu1 nu2
	  nu3 nu4
	  nu5 nu6
	  phi psi))


(defun&vars&eval-calls (1 1 2 2) (1 2 1 2)
    nu1 (m*-alpha-beta m-alpha n-beta Omega11-alpha-beta)
  "Sharipov&Kalempa, 2002 (63)"
  (* (/ 16.0 3.0)
     (/ m*-alpha-beta m-alpha)
     n-beta Omega11-alpha-beta))

(define-test nu1
  (let ((lisp-unit:*epsilon* 1e-4)
	(c (/ 16 3)))
    (assert-numerical-equal
     c
     (nu1 1 1 1 1) "1")
    (assert-numerical-equal
     (* 2 c)
     (nu1 2 1 1 1) "2")
    (assert-numerical-equal
     (/ c 2)
     (nu1 1 2 1 1) "3")
    (assert-numerical-equal
     (* 2 c)
     (nu1 1 1 2 1) "4")
    (assert-numerical-equal
     (* 2 c)
     (nu1 1 1 1 2) "5")))
     

(defun&vars&eval-calls (1 1 2 2) (1 2 1 2)
    nu2 (m*-alpha-beta m-alpha 
				  n-beta Omega11-alpha-beta Omega12-alpha-beta)
  "Sharipov&Kalempa, 2002 (64)"
  (* (/ 64.0 15.0)
     (^2 (/ m*-alpha-beta m-alpha))
     n-beta
     (- Omega12-alpha-beta
	(* 2.5 Omega11-alpha-beta))))

(define-test nu2
  (let ((lisp-unit:*epsilon* 1e-4)
	(c (/ 64 15)))
    (assert-numerical-equal
     c
     (nu2 1 1 1 0 1) "1")
    (assert-numerical-equal
     (* 4 c)
     (nu2 2 1 1 0 1) "2")
    (assert-numerical-equal
     (/ c 4)
     (nu2 1 2 1 0 1) "3")
        (assert-numerical-equal
     (* 2 c)
     (nu2 1 1 2 0 1) "4")
        (assert-numerical-equal
     (* 2 c)
     (nu2 1 1 1 0 2) "5")
        (assert-numerical-equal
     (* -5 c)
     (nu2 1 1 1 2 0) "6")
        (assert-numerical-equal
     0
     (nu2 1 1 1 2 5) "7")))



(defun&vars&eval-calls (1 1 2 2) (1 2 1 2)
    nu3 (m*-alpha-beta m-alpha m-beta
				  n-beta Omega11-alpha-beta Omega22-alpha-beta)
  "Sharipov&Kalempa, 2002 (65)"
  (* (/ 16.0 5.0)
     (* (/ m*-alpha-beta m-alpha)
	(/ m*-alpha-beta m-beta))
     n-beta
     (+ (* (/ 10.0 3.0)
	   Omega11-alpha-beta)
	(* (/ m-beta m-alpha)
	   Omega22-alpha-beta))))

(define-test nu3
  (let ((lisp-unit:*epsilon* 1e-4)
	(c (/ 16 5)))
    (assert-numerical-equal
     c
     (nu3 1 1 1 1 0 1) "C")
    (assert-numerical-equal
     (* 4 c)
     (nu3 2 1 1 1 0 1) "m*")
    (assert-numerical-equal
     (/ c 4)
     (nu3 1 2 1 1 0 1) "m-alpha")
    (assert-numerical-equal
     c
     (nu3 1 1 2 1 0 1) "m-beta")
    (assert-numerical-equal
     (* 2 c)
     (nu3 1 1 1 2 0 1) "n-beta")
    (assert-numerical-equal
     (* 10 c)
     (nu3 1 1 1 1 3 0) "omega-11")
    (assert-numerical-equal
     (* 2 c)
     (nu3 1 1 1 1 0 2) "omega-22")))

(defun&vars&eval-calls (1 1 2 2) (1 2 1 2)
    nu4 (m*-alpha-beta m-alpha m-beta
				  n-beta Omega11-alpha-beta Omega22-alpha-beta)
  "Sharipov&Kalempa, 2002 (66)"
  (* (/ 16.0 5.0)
     (* (/ m*-alpha-beta m-alpha)
	(/ m*-alpha-beta m-beta))
     n-beta
     (- (* (/ 10.0 3.0)
	   Omega11-alpha-beta)
	Omega22-alpha-beta)))

(define-test nu4
  (let ((lisp-unit:*epsilon* 1e-4)
	(c (/ 16 5)))
    (assert-numerical-equal
     c
     (nu4 1 1 1 1 0 -1) "C")
    (assert-numerical-equal
     (* 4 c)
     (nu4 2 1 1 1 0 -1) "m*")
    (assert-numerical-equal
     (/ c 2)
     (nu4 1 2 1 1 0 -1) "m-alpha")
    (assert-numerical-equal
     (/ c 2)
     (nu4 1 1 2 1 0 -1) "m-beta")
    (assert-numerical-equal
     (* 2 c)
     (nu4 1 1 1 2 0 -1) "n-beta")
    (assert-numerical-equal
     (* 10 c)
     (nu4 1 1 1 1 3 0) "omega-11")
    (assert-numerical-equal
     (* -2 c)
     (nu4 1 1 1 1 0 2) "omega-22")))

#|(define-test nu-3/4
  "Use simple values of Omega, masses and density"
  (bind-simple-equal-species
    (let ((n-1 1.0)
	  (n-2 1.0)
	  (lisp-unit:*epsilon* 1e-4))
      (calc-m*-alpha-beta)
      (calc-omega11-alpha-beta)
      (calc-omega12-alpha-beta)
      (calc-omega13-alpha-beta)
      (calc-omega22-alpha-beta)
      (calc-nu3-alpha-beta)
      (format t "Testing value of nu3-1-1= ~A~%" nu3-1-1)
      (assert-number-equal (* (/ 16.0 5.0)
		       0.25
		       n-1
		       (+ (/ 10.0 3.0)
			  2.0))
		    nu3-1-1)
      (calc-nu4-alpha-beta)
      (format t "Testing value of nu4-1-1= ~A~%" nu4-1-1)
      (assert-number-equal (* (/ 16.0 5.0)
		       0.25
		       n-1
		       (- (/ 10.0 3.0)
			  2.0))
		    nu4-1-1))))|#

(defun&vars&eval-calls (1 1 2 2) (1 2 1 2)
    nu5 (m*-alpha-beta m-alpha m-beta
				  n-beta Omega11-alpha-beta Omega22-alpha-beta
					      Omega12-alpha-beta Omega13-alpha-beta)
  "Sharipov&Kalempa, 2002 (67)"
  (* (/ 64.0 15.0)
     (^3 (/ m*-alpha-beta m-alpha))
     (/ m-alpha m-beta)
     n-beta
     (+ Omega22-alpha-beta
	(* (+ (/ (* 15.0 m-alpha)
		 (* 4.0 m-beta))
	      (/ (* 25.0 m-beta)
		 (* 8.0 m-alpha)))
	   Omega11-alpha-beta)
	(* -0.5
	   (/ m-beta m-alpha)
	   (- (* 5 Omega12-alpha-beta)
	      Omega13-alpha-beta)))))

(define-test nu5
  (let ((lisp-unit:*epsilon* 1e-4)
	(c (/ 64 15)))
    (assert-numerical-equal
     c
     (nu5 1 1 1 1 0 1 0 0) "C")
    (assert-numerical-equal
     (* 8 c)
     (nu5 2 1 1 1 0 1 0 0) "m*")
    (assert-numerical-equal
     (/ c 4)
     (nu5 1 2 1 1 0 1 0 0) "m-alpha")
    (assert-numerical-equal
     (/ c 2)
     (nu5 1 1 2 1 0 1 0 0) "m-beta")
    (assert-numerical-equal
     (* 2 c)
     (nu5 1 1 1 2 0 1 0 0) "n-beta")
    (assert-numerical-equal
     (* 2 c)
     (nu5 1 1 1 1 0 2 0 0) "omega-22")
    (assert-numerical-equal
     (* c (+ 15/4 25/8))
     (nu5 1 1 1 1 1 0 0 0) "omega-11")
    (assert-numerical-equal
     (* (/ c 4) (+ (* 2 15/4) (/ 25/8 2)))
     (nu5 1 2 1 1 1 0 0 0) "omega-11-m-alpha")
    (assert-numerical-equal
     (* (/ c 2) (+ (/ 15/4 2) (* 2 25/8)))
     (nu5 1 1 2 1 1 0 0 0) "omega-11-m-beta")
    (assert-numerical-equal
     (* c -5/2)
     (nu5 1 1 1 1 0 0 1 0) "omega-12")
    (assert-numerical-equal
     (* (/ c 4) (/ -5/2 2))
     (nu5 1 2 1 1 0 0 1 0) "omega-12-m-alpha")
    (assert-numerical-equal
     (* (/ c 2) (* -5/2 2))
     (nu5 1 1 2 1 0 0 1 0) "omega-12-m-beta")
    (assert-numerical-equal
     (* c 1/2)
     (nu5 1 1 1 1 0 0 0 1) "omega-13")))


(defun&vars&eval-calls (1 1 2 2) (1 2 1 2)
    nu6 (m*-alpha-beta m-alpha m-beta
				  n-beta Omega11-alpha-beta Omega22-alpha-beta
				  Omega12-alpha-beta Omega13-alpha-beta)
  "Sharipov&Kalempa, 2002 (68)"
  (* (/ 64.0 15.0)
     (^3 (/ m*-alpha-beta m-alpha))
     (expt (/ m-alpha m-beta) 1.5)
     n-beta
     (+ (- Omega22-alpha-beta)
	(* (/ 55.0 8.0) Omega11-alpha-beta)
	(* -2.5 Omega12-alpha-beta)
	(* 0.5 Omega13-alpha-beta))))


(define-test nu6
  (let ((lisp-unit:*epsilon* 1e-4)
	(c (/ 64 15)))
    (assert-numerical-equal
     c
     (nu6 1 1 1 1 0 -1 0 0) "C")
    (assert-numerical-equal
     (* 8 c)
     (nu6 2 1 1 1 0 -1 0 0) "m*")
    (assert-numerical-equal
     (/ c (expt 2 3/2))
     (nu6 1 2 1 1 0 -1 0 0) "m-alpha")
    (assert-numerical-equal
     (/ c (expt 2 3/2))
     (nu6 1 1 2 1 0 -1 0 0) "m-beta")
    (assert-numerical-equal
     (* 2 c)
     (nu6 1 1 1 2 0 -1 0 0) "n-beta")
    (assert-numerical-equal
     (* c 55/8)
     (nu6 1 1 1 1 1 0 0 0) "omega-11")
    (assert-numerical-equal
     (* 2 c)
     (nu6 1 1 1 1 0 -2 0 0) "omega-22")
    (assert-numerical-equal
     (* c -5/2)
     (nu6 1 1 1 1 0 0 1 0) "omega-12")
    (assert-numerical-equal
     (* c 1/2)
     (nu6 1 1 1 1 0 0 0 1) "omega-13")))


#|(define-test nu-same-species-50/50
  (bind-equal-species-50/50
      (calc-m*-alpha-beta)
      (calc-omega11-alpha-beta)
      (calc-omega12-alpha-beta)
      (calc-omega13-alpha-beta)
      (calc-omega22-alpha-beta)
    (let ((lisp-unit:*epsilon* 1e-4))
      (calc-nu1-alpha-beta)
      (assert-number-equal (* (/ 8 3)
			      n-1
			      Omega11-1-1)
			   nu1-1-1 "nu(1)-11")
      (calc-nu2-alpha-beta)
      (assert-number-equal (* (/ 8 15)
			      n-1
			      Omega11-1-1)
			   nu2-1-1 "nu(2)-11")
      (calc-nu3-alpha-beta)
      (assert-number-equal (* (/ 64 15)
			      n-1
			      Omega11-1-1)
			   nu3-1-1 "nu(3)-11")
      (calc-nu4-alpha-beta)
      (assert-number-equal (* (/ 16 15)
			      n-1
			      Omega11-1-1)
			   nu4-1-1 "nu(4)-11")
      (calc-nu5-alpha-beta)
      (assert-number-equal (* (/ 59 15)
			      n-1
			      Omega11-1-1)
			   nu5-1-1 "nu(5)-11")
      (calc-nu6-alpha-beta)
      (assert-number-equal (* (/ 9 5)
			      n-1
			      Omega11-1-1)
			   nu6-1-1 "nu(6)-11"))))|#


;; A couple of macros to simplify Phi and Psi coding
(defmacro nu-combinations (i-aa j-aa i-ab)
  "Expands into iaa - jaa + iab

Used in equations Sharipov&Kalempa, 2002 (78,9)"
  `(+ (- ,i-aa  ,j-aa) ,i-ab))




(defun&vars&eval-calls (1 2) (2 1)
    Psi (nu3-alpha-alpha nu4-alpha-alpha
			       nu3-alpha-beta)
  "Sharipov&Kalempa, 2002 (78)"
  (nu-combinations nu3-alpha-alpha nu4-alpha-alpha
		  nu3-alpha-beta))

(define-test psi
  (assert-numerical-equal
   1 (psi 1 0 0) "nu-3-alpha-alpha")
  (assert-numerical-equal
   -1 (psi 0 1 0) "nu-4-alpha-alpha")
  (assert-numerical-equal
   1 (psi 0 0 1) "nu-3-alpha-beta"))


(defun&vars&eval-calls (1 2) (2 1)
    Phi (nu5-alpha-alpha nu6-alpha-alpha
			       nu5-alpha-beta)
  "Sharipov&Kalempa, 2002 (79)"
    (nu-combinations nu5-alpha-alpha nu6-alpha-alpha
		  nu5-alpha-beta))

(define-test phi
  (assert-numerical-equal
   1 (phi 1 0 0) "nu-5-alpha-alpha")
  (assert-numerical-equal
   -1 (phi 0 1 0) "nu-6-alpha-alpha")
  (assert-numerical-equal
   1 (phi 0 0 1) "nu-5-alpha-beta"))

#|(define-test Psi&Phi-single-gas
  (bind-equal-species-50/50
    (calc-m*-alpha-beta)
    (calc-omega11-alpha-beta)
    (calc-omega22-alpha-beta)
    (calc-nu3-alpha-beta)
    (calc-nu4-alpha-beta)
    (calc-psi-alpha)
    (calc-phi-alpha)
    (let ((lisp-unit:*epsilon* 1e-4))
      (assert-number-equal (* (/ 112 15)
			      n-1
			      Omega11-1-1)
			   Psi-1 "Psi1(1)")
      (assert-number-equal (* (/ 91 15)
			      n-1
			      Omega11-1-1)
			   Phi-1 "Phi1(1)"))))|#


