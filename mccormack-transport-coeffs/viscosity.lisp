;; Mirko Vukovic
;; Time-stamp: <2012-04-25 15:44:06 viscosity.lisp>
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

(export '(#|mu-alpha1-hs|# mu-alpha1 delta-alpha-beta mu-binary))
;; Hard sphere calculations are in conflict with the rest of the code


(defun&vars&eval-calls (1 2) (2 1)
    mu-alpha (P-alpha Psi-alpha Psi-beta
		      nu4-alpha-beta nu4-beta-alpha)
  "Viscosity of species alpha
Sharipov&Kalempa, 2002 (74)"
  (/ (* P-alpha (+ Psi-beta nu4-alpha-beta))
     (mcc-tc-coeff-denom Psi-alpha Psi-beta nu4-alpha-beta nu4-beta-alpha)))


(define-test mu-alpha
  (assert-numerical-equal
   1 (mu-alpha 1 1 1 0 0))
  (assert-numerical-equal
   2 (mu-alpha 2 1 1 0 0) "P-alpha")
  ;; PSI-beta and nu4-alpha-beta are related through the numerator.
  ;; Tests of their interaction
  (assert-numerical-equal
   1
   (mu-alpha 1 1 2 0 1) "PSI-beta")
  (assert-numerical-equal
   -1
   (mu-alpha 1 1 0 2 1) "nu4-alpha-beta")
  (assert-numerical-equal
   (/ (+ 1 2)
      (- 1 2))
   (mu-alpha 1 1 1 2 1) "PSI-beta/nu4-alpha-beta")
  (assert-numerical-equal
   (/ (+ 2 1)
      (- 2 1))
   (mu-alpha 1 1 2 1 1) "PSI-beta/nu4-alpha-beta")
  (assert-numerical-equal
   1/2 (mu-alpha 1 2 1 0 0) "PSI-alpha")
  (assert-numerical-equal
   -1/2 (mu-alpha 1 0 0 1 2) "nu4-beta-alpha"))


#|(defun mu-alpha1-hs (n-1 n-2 m-1 m-2 sigma-1 sigma-2 temp)
  "mu-alpha

Sharipov&Kalempa, 2002 (74)"
  (bind-p-alpha
    (bind-m*-alpha-beta
      (bind-omega11-hs-alpha-beta
	(bind-omega22-hs-alpha-beta
	  (bind-nu3-alpha-beta
	    (bind-nu4-alpha-beta
	      (bind-psi-alpha
		(bind-mu-alpha
		  (values mu-1 mu-2 ))))))))))|#


(defgeneric mu-alpha1 (species-1 species-2 n-1 n-2 temp)
  (:documentation 
   "Calculate mu-alpha1,2 for `species-1' and `species-2' with respective
densities of `n-1' and `n-2' at temperature `temp'

It returns mu-alpha1,2 as a list

As a second value, the quantities (/ P-1,2 mu-1,2) are also returned as a list")
  (:method ((species-1 lennard-jones-6/12-potential)
	    (species-2 lennard-jones-6/12-potential) n-1 n-2 temp)
    (let ((m-1 (mass species-1))
	  (m-2 (mass species-2))
#|	  (sigma-1 (sigma species-1))
	  (sigma-2 (sigma species-2))|#)
      (bind-p-alpha
	(bind-m*-alpha-beta
	  (bind-omega11-alpha-beta
	    (bind-omega22-alpha-beta
	      (bind-nu3-alpha-beta
		(bind-nu4-alpha-beta
		  (bind-psi-alpha
		    (bind-mu-alpha
		      (values mu-1 mu-2
			      (list (/ p-1 mu-1) (/ p-2 mu-2)))))))))))))
  (:method ((species-1 hard-sphere-potential)
	    (species-2 hard-sphere-potential) n-1 n-2 temp)
    (let ((m-1 (mass species-1))
	  (m-2 (mass species-2))
#|	  (sigma-1 (sigma species-1))
	  (sigma-2 (sigma species-2))|#)
      (bind-p-alpha
	(bind-m*-alpha-beta
	  (bind-omega11-alpha-beta
	    (bind-omega22-alpha-beta
	      (bind-nu3-alpha-beta
		(bind-nu4-alpha-beta
		  (bind-psi-alpha
		    (bind-mu-alpha
		      (values mu-1 mu-2
			      (list (/ p-1 mu-1) (/ p-2 mu-2))))))))))))))


#|(defun&vars&eval-calls (1 2) (2 1) delta-alpha (R spec-alpha spec-beta n-alpha n-beta temp)
  (multiple-value-bind (mu-s gamma-s)
      (mu-alpha1 spec-alpha spec-beta n-alpha n-beta temp)
    (destructuring-bind (mu-alpha mu-beta) mu-s
      (declare (ignore mu-alpha mu-beta))
      (destructuring-bind (gamma-alpha gamma-beta) gamma-s
	(declare (ignore gamma-beta))
	(delta1 R gamma-alpha (mass spec-alpha) temp)))))

|#

(defun delta-alpha-beta (R spec-alpha spec-beta n-alpha n-beta temp)
  (multiple-value-bind (mu-s gamma-s)
      (mu-alpha1 spec-alpha spec-beta n-alpha n-beta temp)
    (destructuring-bind (mu-alpha mu-beta) mu-s
      (declare (ignore mu-alpha mu-beta))
      (destructuring-bind (gamma-alpha gamma-beta) gamma-s
	(list (delta1 R gamma-alpha (mass spec-alpha) temp)
	      (delta1 R gamma-beta (mass spec-beta) temp))))))
#|

(defun mu-binary% (n-1 n-2 m-1 m-2 temp
	   omega11-1-1 omega11-1-2 omega11-2-1 omega11-2-2
	   omega22-1-1 omega22-1-2 omega22-2-1 omega22-2-2)
  "Total viscosity
Sharipov&Kalempa, 2002 (70)"
  (bind-p-alpha
    (bind-m*-alpha-beta
      (bind-nu3-alpha-beta
	(bind-nu4-alpha-beta
	  (bind-psi-alpha
	    (bind-mu-alpha
		(+ mu-1 mu-2))))))))

(define-test mu-binary%
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
	(bind-omega22-alpha-beta
	  (assert-number-equal
	   (gkf:mu-hs (* +NA+ m-1)
		      temp sigma-1)
	   (mu-binary% n-1 n-2 m-1 m-2 temp omega11-1-1 omega11-1-2 omega11-2-1 omega11-2-2
	       omega22-1-1 omega22-1-2 omega22-2-1 omega22-2-2)
	   "mu"))))))

|#