;; Mirko Vukovic
;; Time-stamp: <2012-05-27 10:49:57 diffusivity.lisp>
;; 
;; Copyright 2011 Mirko Vukovic
;; Distributed under the terms of the GNU General Public License
;; 
;; This program is free software: you can redistribute it and/or modify
;; it under the terms of the GNU General Public License as published by
;; the Free Software Foundation, either version 3 of the License, or
;; (at your option) any later version.
;; 
;; This program is distributed in the hope that it will be useful,
;; but WITHOUT ANY WARRANTY; without even the implied warranty of
;; MERCHANTABILITY or FITNESS FOR A PARTICULAR PURPOSE.  See the
;; GNU General Public License for more details.
;; 
;; You should have received a copy of the GNU General Public License
;; along with this program.  If not, see <http://www.gnu.org/licenses/>.

(in-package :gkf)

;;; Diffusion
(defun&vars&eval-calls (1 2) (2 1)
    D-alpha (m-alpha m-beta
		     Phi-alpha Phi-beta
		     nu2-alpha-beta nu2-beta-alpha
		     nu6-alpha-beta nu6-beta-alpha)
  "Sharipov&Kalempa, 2002 (76)"
  (/ (- (* nu2-alpha-beta Phi-beta)
	(* nu2-beta-alpha nu6-alpha-beta
	   (sqrt (/ m-beta m-alpha))))
     (mcc-tc-coeff-denom Phi-alpha Phi-beta nu6-alpha-beta nu6-beta-alpha)))

(define-test d-alpha
  (let ((lisp-unit:*epsilon* 1e-4))
    (assert-numerical-equal
     1
     (d-alpha 1 1 1 1 1 0 0 0) "simple-nu2-beta-alpha/nu6=0")
    (assert-numerical-equal
     2
     (d-alpha 1 1 1 1 2 0 0 0) "nu2-alpha-beta")
    (assert-numerical-equal
     2
     (d-alpha 1 1 1/2 2 1 0 0 0) "Phi-beta")
    (assert-numerical-equal
     1
     (d-alpha 1 1 1 2 1 0 0 0) "Phi-alpha")
    (assert-numerical-equal
     0
     (d-alpha 1 1 1 1 1 1 1 0) "Phi-alpha")
    (assert-numerical-equal
     -1
     (d-alpha 1 1 1 1 1 2 1 0) "nu2-beta-alpha")
    (assert-numerical-equal
     -3
     (d-alpha 1 1 1 1 1 2 2 0) "nu2-beta-alpha")
    (assert-numerical-equal
     -7
     (d-alpha 1 4 1 1 1 2 2 0) "m-beta")
    (assert-numerical-equal
     -3
     (d-alpha 4 4 1 1 1 2 2 0) "m-alpha")
    (assert-numerical-equal
     3
     (d-alpha 4 4 1 1 1 2 2 1) "nu6-beta-alpha")))

(export '(DD-alpha-beta D12 D12-cc))
(defun&vars&eval-calls (1 2) (2 1)
    DD-alpha-beta (P-beta n-alpha n-beta
			  m-alpha m-beta
			  D-alpha D-beta
			  nu1-alpha-beta nu2-alpha-beta)
  "Sharipov&Kalempa, 2002 (72), generalized to alpha-beta"
  (let ((n (+ n-alpha n-beta)))
    (/ (/ P-beta (* n m-alpha nu1-alpha-beta))
       (- 1 (* (/ 5.0 8.0)
	       (/ nu2-alpha-beta nu1-alpha-beta)
	       (+ D-alpha
		  (* (/ m-alpha m-beta)
		     D-beta)))))))

(define-test DD-alpha-beta
  (let ((lisp-unit:*epsilon* 1e-4))
    (assert-numerical-equal
     1
     (dd-alpha-beta 1 1 0 1 1 1 1 1 0) "simple")
    (assert-numerical-equal
     2
     (dd-alpha-beta 2 1 0 1 1 1 1 1 0) "P-beta")
    (assert-numerical-equal
     1
     (dd-alpha-beta 2 2 0 1 1 1 1 1 0) "n-alpha")
    (assert-numerical-equal
     1
     (dd-alpha-beta 2 0 2 1 1 1 1 1 0) "n-beta")
    (assert-numerical-equal
     1
     (dd-alpha-beta 2 0 1 2 1 1 1 1 0) "m-alpha-numerator")
    (assert-numerical-equal
     1
     (dd-alpha-beta 2 0 1 1 1 1 1 2 0) "nu1-alpha-beta-numerator")
    (assert-numerical-equal
     (/ (- 1 5/8))
     (dd-alpha-beta 1 1 0 1 1 0 1 1 1) "denominator-simple")
    (assert-numerical-equal
     (/ (- 1 (* 5/8 2)))
     (dd-alpha-beta 1 1 0 1 1 0 1 1 2) "nu2-alpha-beta/denominator")
    (assert-numerical-equal
     (/ 1/2 (- 1 5/8))
     (dd-alpha-beta 1 1 0 1 1 0 1 2 2) "nu1-alpha-beta/denominator")
    (assert-numerical-equal
     (/ 1/2 (- 1 (* 5/8 (+ 2 1))))
     (dd-alpha-beta 1 1 0 1 1 2 1 2 2) "D-alpha/denominator")
    (assert-numerical-equal
     (/ 1/2 (- 1 (* 5/8 (+ 1 2))))
     (dd-alpha-beta 1 1 0 1 1 1 2 2 2) "D-beta/denominator")
    (assert-numerical-equal
     (/ 1/4 (- 1 (* 5/8 (+ 1 (* (/ 2 1)
				2)))))
     (dd-alpha-beta 1 1 0 2 1 1 2 2 2) "m-alpha/denominator")
    (assert-numerical-equal
     (/ 1/2 (- 1 (* 5/8 (+ 1 (* (/ 1 2)
				2)))))
     (dd-alpha-beta 1 1 0 1 2 1 2 2 2) "m-alpha/denominator")))
    
    
     
#|(define-test D12-single-gas
  (bind-equal-species-50/50
    (calc-omega11-alpha-beta)
    (calc-omega12-alpha-beta)
    (calc-omega13-alpha-beta)
    (calc-omega22-alpha-beta)
    (calc-nu1-alpha-beta)
    (calc-nu2-alpha-beta)
    (calc-nu5-alpha-beta)
    (calc-nu6-alpha-beta)
    (calc-phi-alpha)
    (calc-p-alpha)
    
    (let ((lisp-unit:*epsilon* 1e-4))
      (assert-number-equal (* (/ 177 464)
		     (/ P-2
			(* (+ n-1 n-2)
			   m-1 n-1 Omega11-1-1)))
		  (D12)))))|#
  
#|
(define-test D12
  (let ((m-1 1e-26)
	(m-2 1e-26)
	(Temp 300d0)
	(sigma-1 1e-10)
	(sigma-2 1e-10)
	(n-1 1e22)
	(n-2 1e22)
	(species-1 (make-species-lennard-jones-6/12-potential :Ar))
	(species-2 (make-species-lennard-jones-6/12-potential :He))
	(lisp-unit:*epsilon* 1e-4))
    (bind-m*-alpha-beta
      (bind-omega11-alpha-beta
	(bind-omega12-alpha-beta
	  (bind-omega13-alpha-beta
	    (bind-omega22-alpha-beta
	      (bind-p-alpha
		(bind-nu1-alpha-beta
		  (bind-nu2-alpha-beta
		    (bind-nu5-alpha-beta
		      (bind-nu6-alpha-beta
			(bind-phi-alpha
			  (bind-d-alpha
			    (bind-dd-alpha-beta
			      (assert-number-equal
			       (* (/ 177 464)
				  (/ 8 3)
				  (gkf:self-diffusion-coefficient-hs (* +NA+ m-1) temp sigma-1 (+ P-1 P-2)))
			       dd-1-2))))))))))))))))|#

(defgeneric D12 (species-1 species-2 n-1 n-2 temp)
  (:documentation 
   "Calculate D12 for `species-1' and `species-2' with respective
densities of `n-1' and `n-2' at temperature `temp'

")
  (:method ((species-1 lennard-jones-6/12-potential)
	    (species-2 lennard-jones-6/12-potential) n-1 n-2 temp)
    (let ((m-1 (* (mass species-1) +amu+))
	  (m-2 (* (mass  species-2) +amu+)))
      (bind-m*-alpha-beta
	(bind-omega11-alpha-beta
	  (bind-omega12-alpha-beta
	    (bind-omega13-alpha-beta
	      (bind-omega22-alpha-beta
		(bind-p-alpha
		  (bind-nu1-alpha-beta
		    (declare (ignore nu1-1-1 nu1-2-2))
		    (bind-nu2-alpha-beta
		    (declare (ignore nu2-1-1 nu2-2-2))
		      (bind-nu5-alpha-beta
			(bind-nu6-alpha-beta
			  (bind-phi-alpha
			    (bind-d-alpha
			      (bind-dd-alpha-beta
				(values dd-1-2 dd-2-1))))))))))))))))
    (:method ((species-1 hard-sphere-potential)
	      (species-2 hard-sphere-potential) n-1 n-2 temp)
    (let ((m-1 (* (mass species-1) +amu+))
	  (m-2 (* (mass species-2) +amu+)))
      (bind-m*-alpha-beta
	(bind-omega11-alpha-beta
	  (bind-omega12-alpha-beta
	    (bind-omega13-alpha-beta
	      (bind-omega22-alpha-beta
		(bind-p-alpha
		  (bind-nu1-alpha-beta
		    (declare (ignore nu1-1-1 nu1-2-2))
		    (bind-nu2-alpha-beta
		    (declare (ignore nu2-1-1 nu2-2-2))
		      (bind-nu5-alpha-beta
			(bind-nu6-alpha-beta
			  (bind-phi-alpha
			    (bind-d-alpha
			      (bind-dd-alpha-beta
				(values dd-1-2 dd-2-1)))))))))))))))))

(defgeneric D12-cc (s1 s2 n1 n2 temp)
  (:documentation
"Calculate the binary diffusion coefficient for species S1 and S2 at
respective densities N1 and N2 and gas temperature TEMP")
  (:method ((s1 lennard-jones-6/12-potential)
	    (s2 lennard-jones-6/12-potential) n1 n2 temp)
    (let* ((coll (make-collision-parameters s1 s2))
	   (m12 (* (mass coll) +amu+))
	   (sigma12 (sigma coll))
	   (T* (/ temp (epsilon/k coll)))
	   (Omega11*_12 (omega*-11 T*)))
      (* (/ 3
	    (* 16 (+ n1 n2) m12))
	 (/ (sqrt (* 2 +pi+ (* m12 1e20) (* +kb+ 1e20) temp))
	    (* +pi+ (expt sigma12 2) omega11*_12)))))
  (:method ((s1 hard-sphere-potential)
	    (s2 hard-sphere-potential) n1 n2 temp)
    (let* ((coll (make-collision-parameters s1 s2))
	   (m12 (* (mass coll) +amu+))
	   (sigma12 (sigma coll)))
      (* (/ 3
	    (* 16 (+ n1 n2) m12))
	 (/ (sqrt (* 2 +pi+ (* m12 1e20) (* +kb+ 1e20) temp))
	    (* +pi+ (expt sigma12 2)))))))

(define-test D12-cc
  ;; Use Ferziger&Kaper 10.4-4 to doublecheck the D12-cc calculation
  (let ((n1 1e22)
	(n2 1e22)
	(s1 (make-species-lennard-jones-6/12-potential :Ar))
	(temp 300d0))
    (let* ((s2 (make-species-lennard-jones-6/12-potential :He))
	   (coll (make-collision-parameters s1 s2))
	   (lisp-unit:*epsilon* 1e-2))
      (assert-number-equal (* (+ n1 n2) (expt (* (sigma coll) 1e-10) 2)
			      (d12-cc s1 s2 n1 n2 temp)
			      (sqrt (/ (* 2 (* (mass coll) +amu+))
				       (* +kb+ temp))))
			   (/ (/ 3 (* 8 (sqrt +pi+)))
			      (omega*-11 (/ temp (epsilon/k coll))))))))

(define-test D12
  (let ((n1 0.9e22)
	(n2 0.1e22)
	(s1 (make-species-lennard-jones-6/12-potential :Ar))
	(temp 300d0))
    (let ((s2 (make-species-lennard-jones-6/12-potential :Ar))
	  (lisp-unit:*epsilon* 1e-2))
      (assert-number-equal (d12-cc s1 s2 n1 n2 temp) (d12 s1 s2 n1 n2 temp)))
    (let ((s2 (make-species-lennard-jones-6/12-potential :He))
	  (lisp-unit:*epsilon* 1e-2))
      (assert-number-equal (d12-cc s1 s2 n1 n2 temp) (d12 s1 s2 n1 n2 temp)))))

(define-test D12-species-swap
  "Is D12 consistent when we swap the species"
  (let ((n-Ar 1e22)
	(n-He 1e20)
	(Ar (make-species-lennard-jones-6/12-potential :Ar))
	(temp 300d0)
	(He (make-species-lennard-jones-6/12-potential :He))
	(lisp-unit:*epsilon* 1e-12))
    (multiple-value-bind (D12/Ar-He/0 D12/Ar-He/1)
	(D12 Ar He n-Ar n-He temp)
      (multiple-value-bind (D12/He-Ar/0 D12/He-Ar/1)
			 (D12 He Ar n-He n-Ar temp)
	(assert-number-equal D12/Ar-He/0 D12/He-Ar/1)
	(assert-number-equal D12/Ar-He/1 D12/He-Ar/0)))))