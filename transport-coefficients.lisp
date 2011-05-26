;; Mirko Vukovic
;; Time-stamp: <2011-02-22 09:45:27 transport-coefficients.lisp>
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

(export '(mu-hs k-hs self-diffusion-coefficient-hs binary-diffusion-coefficient-hs))

;;Chapman&Enskog (6.2,1)
(defun mu-hs (mass temp dia)
  "Low pressure viscosity using the hard sphere model"
  ;; I perform the calculation in double-precision.  For CLISP I have
  ;; to enable floating point contagion.
  #+clisp
  (let ((CUSTOM:*FLOATING-POINT-CONTAGION-ANSI* t))
    (coerce (/ (* (/ 5/16 (sqrt pi)) (sqrt (* 1d0 +R+ mass temp)))
	       (^2 dia))
	    'single-float))
  #-clisp
  (coerce (/ (* (/ 5/16 (sqrt pi)) (sqrt (* 1d0 +R+ mass temp)))
		      (^2 dia))
		   'single-float))

(defun k-hs (m T-k d &optional
				(specific-heat-fact 1.5))
  "Low pressure has conductivity using the hard-sphere model"
  ;;Kee et al, Eq. 3.138
    (* 2.5
       (mu-hs m T-k d)
       (/ (* specific-heat-fact +R+)
	  m)))


(define-test hs-diff-coeff
  (let ((press 1.0)
	(dia 1.0e-10)
	(mass 1.0e-26)
	(temp 300.0))
    (assert-number-equal (self-diffusion-coefficient-hs mass temp dia press)
		  (binary-diffusion-coefficient-hs (* 0.5 mass)
			  temp dia press))))


(defun self-diffusion-coefficient-hs (m T-K d P)
  "hard-sphere diffusion coefficient"
  ;; Kee et al, Eq. 12.110
;;  (let ((CUSTOM:*FLOATING-POINT-CONTAGION-ANSI* t))
    (coerce (* (/ 3d0 (* 8d0 (sqrt pi)))
	       (sqrt (/ (^3 (* 1d0 +R+ T-K))
			m))
	       (1/ (* (^2 d)
		      P)))
	    'single-float))


(defun binary-diffusion-coefficient-hs (m-* T-K sigma-r P)
  "hard-sphere diffusion coefficient"
  ;; Kee et al, Eq. 12.113
  (let (
	#+clisp(CUSTOM:*FLOATING-POINT-CONTAGION-ANSI* t)
	       )
    (coerce (* 3/16
	       (sqrt (/ (* 2 pi (^3 (* 1d0 +R+ T-K)))
			m-*))
	       (1/ (* P pi 
		      (^2 sigma-r))))
	    'single-float)))

