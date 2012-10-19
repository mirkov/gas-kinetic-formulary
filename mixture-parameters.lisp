;; Mirko Vukovic
;; Time-stamp: <2012-07-12 14:12:48 mixture-parameters.lisp>
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

(in-package :gas-kinetic-formulary)

(export '(mixture-mass-density mixture-molecular-mass
	  mixture-mass-coeff1))

;;; The following definitions are from Sharipov & Kalempa, 2002
(defun mass-density-mixture (rho1 rho2)
  "Mixture mass density"
  (+ rho1 rho2))

(defun mixture-molecular-mass (m1 m2 C)
  "Mixture mean molecular mass"
  (+ (* C m1) (*  (- 1.0 C) m2)))

(defun mixture-mass-coeff1 (m1 m2 C)
  "Mass fraction coefficient defined as

C (m1/m)^1/2 + (1 - C) (m2/m)^1/2

This coefficient appears as mixture correction factor to a tube free
molecular conductivity.

See Sharipov & Kalempa, JVSTA 20 (814) 2002, Eq. 53, 
"
  (let ((m (mixture-molecular-mass m1 m2 C)))
    (+ (* C (sqrt (/ m1 m)))
       (* (- 1 C) (sqrt (/ m2 m))))))