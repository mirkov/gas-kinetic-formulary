;; Mirko Vukovic
;; Time-stamp: <2012-05-02 22:49:46 thermal-diffusivity.lisp>
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

(defun&vars&eval-calls (1 2) (2 1)
    at-alpha (Phi-alpha Phi-beta
			m-alpha m-beta
			nu6-alpha-beta nu6-beta-alpha)
  "Sharipov&Kalempa, 2002 (77)

Used in thermal diffusion ratio (73)

I replaced the dependence on nu6-1-2 with nu6-alpha-beta.  That seems
to make more sense"
  (/ (+ Phi-beta (* (sqrt (/ m-alpha m-beta))
		    nu6-alpha-beta))
     (mcc-tc-coeff-denom Phi-alpha Phi-beta nu6-alpha-beta nu6-beta-alpha)))


(define-test at-alpha
  (let ((lisp-unit:*epsilon* 1e-4))
    ;; First batch focuses on PHI-beta in the numerator and
    ;; PHI-alpha/beta in the denumerator by setting all the collision
    ;; frequences to zero.  m-beta =1 to prevent /0 in numerator
    (assert-numerical-equal
     1
     (at-alpha 1 1 0 1 0 0) "simple")
    (assert-numerical-equal
     1
     (at-alpha 1 2 0 1 0 0) "PHI-beta-cancellation/simple")
    (assert-numerical-equal
     1/2
     (at-alpha 2 2 0 1 0 0) "PHI-alpha/simple")
    ;; Second batch adds the mass-fraction terms in the numerator
    (assert-numerical-equal
     2
     (at-alpha 1 1 1 1 1 0) "m-alpha/m-beta * nu6")
    (assert-numerical-equal
     3
     (at-alpha 1 1 4 1 1 0) "m-alpha")
    (assert-numerical-equal
     2
     (at-alpha 1 1 4 4 1 0) "m-beta")
    (assert-numerical-equal
     3
     (at-alpha 1 1 4 4 2 0) "nu6 in numerator")
    ;; Third batch focuses on the nu6 terms in the denominator
    (assert-numerical-equal
     1
     (at-alpha 2 1 0 1 1 1) "nu6 terms")
    (assert-numerical-equal
     -1
     (at-alpha 2 1 0 1 3 1) "nu6-alpha-beta")
    (assert-numerical-equal
     -1
     (at-alpha 2 1 0 1 1 3) "nu6-alpha-beta")))

(export '(alpha-T))
#|(defun alpha-T (n-1 n-2 m-1 m-2
		omega11-1-1 omega11-1-2 omega11-2-1 omega11-2-2
		omega12-1-1 omega12-1-2 omega12-2-1 omega12-2-2
		omega13-1-1 omega13-1-2 omega13-2-1 omega13-2-2
		omega22-1-1 omega22-1-2 omega22-2-1 omega22-2-2)
  "Sharipov&Kalempa, 2002 (73)"
  (bind-m*-alpha-beta
    (bind-nu2-alpha-beta
      (bind-nu5-alpha-beta
	(bind-nu6-alpha-beta
	  (bind-phi-alpha
	    (bind-at-alpha
	      (let ((n (+ n-1 n-2)))
		(* -1.25 (/ (* n nu2-1-2)
			    n-2)
		   (- at-1 (* (^2 (/ m-1 m-2))
			      at-2)))))))))))|#

(defgeneric alpha-T (species-1 species-2 n-1 n-2 temp)
  (:documentation 
   "Calculate the thermal diffusivity ration alpha-T for `species-1'
and `species-2' with respective densities of `n-1' and `n-2' at
temperature `temp'")
  (:method ((species-1 lennard-jones-6/12-potential)
	    (species-2 lennard-jones-6/12-potential) n-1 n-2 temp)
    (let ((m-1 (* (mass species-1) +amu+))
	  (m-2 (* (mass  species-2) +amu+)))
      (bind-m*-alpha-beta
	(bind-omega11-alpha-beta
	  (bind-omega12-alpha-beta
	    (bind-omega13-alpha-beta
	      (bind-omega22-alpha-beta
		(bind-nu2-alpha-beta
		  (declare (ignore nu2-1-1 nu2-2-1 nu2-2-2))
		  (bind-nu5-alpha-beta
		    (bind-nu6-alpha-beta
		      (bind-phi-alpha
			(bind-at-alpha
			  (let ((n (+ n-1 n-2)))
			    (* -1.25 (/ (* n nu2-1-2)
					n-2)
			       (- at-1 (* (^2 (/ m-1 m-2))
					  at-2))))))))))))))))
  (:method ((species-1 hard-sphere-potential)
	    (species-2 hard-sphere-potential) n-1 n-2 temp)
    (let ((m-1 (* (mass species-1) +amu+))
	  (m-2 (* (mass species-2) +amu+)))
      (bind-m*-alpha-beta
	(bind-omega11-alpha-beta
	  (bind-omega12-alpha-beta
	    (bind-omega13-alpha-beta
	      (bind-omega22-alpha-beta
		(bind-nu2-alpha-beta
		  (declare (ignore nu2-1-1 nu2-2-1 nu2-2-2))
!		  (bind-nu5-alpha-beta
		    (bind-nu6-alpha-beta
		      (bind-phi-alpha
			(bind-at-alpha
			  (let ((n (+ n-1 n-2)))
			    (* -1.25 (/ (* n nu2-1-2)
					n-2)
			       (- at-1 (* (^2 (/ m-1 m-2))
					  at-2)))))))))))))))))


