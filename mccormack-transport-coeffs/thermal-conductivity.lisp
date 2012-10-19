;; Mirko Vukovic
;; Time-stamp: <2012-04-19 18:18:59 thermal-conductivity.lisp>
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


(export '(k-binary k-alpha1))

(defun&vars&eval-calls (1 2) (2 1)
    k-alpha (P-alpha m-alpha m-beta
		     Phi-alpha Phi-beta
		     nu6-alpha-beta nu6-beta-alpha)
  "Thermal conductivity of species alpha

Since we use molar mass in kg, instead of +k+, I use +R+, and also
multiply the molar mass by 1e-3.  See marked lines `***'

Sharipov&Kalempa, 2002 (75)"
  (/ (* 2.5 (/ (* P-alpha +R+) ; ***
	       (* 1e-3 m-alpha)) ; ***
	(+ Phi-beta (* (sqrt (/ m-alpha m-beta))
		       nu6-alpha-beta)))
     (mcc-tc-coeff-denom Phi-alpha Phi-beta
	  nu6-alpha-beta nu6-beta-alpha)))

(define-test k-alpha
  (let ((c (* 5/2 +kb+))
	(lisp-unit:*epsilon* 1e-4))
    (assert-numerical-equal
     (- c)
     (k-alpha 1 1 1 0 0 1 1) "simple-PHIs=0")
    (assert-numerical-equal
     c
     (k-alpha 1 1 1 1 1 0 0) "simple=nus=0")
    (assert-numerical-equal
     (- (* 2 c))
     (k-alpha 2 1 1 0 0 1 1) "P-alpha")
    (assert-numerical-equal
     (/ c -2)
     (k-alpha 1 4 1 0 0 1 1) "m-alpha")
    (assert-numerical-equal
     (/ c -2)
     (k-alpha 1 4 1 0 0 1 1) "m-beta")
    (assert-numerical-equal
     (* 2 c)
     (k-alpha 1 1 1 1/2 2 0 0) "PHI-beta")
    (assert-numerical-equal
     c
     (k-alpha 1 1 1 1 2 0 0) "PHI-alpha")))

;;; Thermal conductivity
(defun k-binary (n-1 n-2 m-1 m-2 temp
	  omega11-1-1 omega11-1-2 omega11-2-1 omega11-2-2
	  omega12-1-1 omega12-1-2 omega12-2-1 omega12-2-2
	  omega13-1-1 omega13-1-2 omega13-2-1 omega13-2-2
	  omega22-1-1 omega22-1-2 omega22-2-1 omega22-2-2)
  "Total thermal conductivity

Sharipov&Kalempa, 2002 (71)"
  (bind-p-alpha
    (bind-m*-alpha-beta
      (bind-nu5-alpha-beta
	(bind-nu6-alpha-beta
	  (bind-phi-alpha
	    (bind-k-alpha
	      (+ k-1 k-2))))))))

(define-test k-binary
  (let ((m-1 1e-26)
	(m-2 1e-26)
	(Temp 300)
	(sigma-1 1e-10)
	(sigma-2 1e-10)
	(n-1 1e22)
	(n-2 1e22)
	(lisp-unit:*epsilon* 1e-4))
    (bind-m*-alpha-beta
      (bind-omega11-alpha-beta
	(bind-omega12-alpha-beta
	  (bind-omega13-alpha-beta
	    (bind-omega22-alpha-beta
	      (let ((lisp-unit:*epsilon* 1e-4))
		(assert-number-equal (gkf:k-hs (* +NA+ m-1) temp sigma-1)
				     (k-binary n-1 n-2 m-1 m-2 temp
					omega11-1-1 omega11-1-2 omega11-2-1 omega11-2-2
					omega12-1-1 omega12-1-2 omega12-2-1 omega12-2-2
					omega13-1-1 omega13-1-2 omega13-2-1 omega13-2-2
					omega22-1-1 omega22-1-2 omega22-2-1 omega22-2-2))))))))))

(defun k-alpha1 (species-1 species-2 n-1 n-2 temp)
  (bind-p-alpha
    (let ((m-1 (mass species-1))
	  (m-2 (mass species-2)))
    (bind-m*-alpha-beta
      (bind-omega11-alpha-beta
	(bind-omega12-alpha-beta
	  (bind-omega13-alpha-beta
	    (bind-omega22-alpha-beta
	      (bind-nu5-alpha-beta
		(bind-nu6-alpha-beta
		  (bind-phi-alpha
		    (bind-k-alpha
		      (values k-1 k-2)))))))))))))