;; Mirko Vukovic
;; Time-stamp: <2011-08-08 09:11:41 helper-macros.lisp>
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


;;(export '(bind-p-alpha n-1 n-2 temp p-1 p-1))
(defmacro bind-p-alpha (&body body)
  "Establish binding for partial pressures p-1 and p-2 as function of
densities n-1, n-2, and temperature temp

Used in DD-alpha-bta, mu-alpha, k-alpha"
  `(let ((p-1 (p n-1 temp))
	 (p-2 (p n-2 temp)))
     ,@body))


(defmacro mcc-tc-coeff-denom (A B nu-ab nu-ba)
  "Expands into (A * B) - (nu-ab * nu-ba)

This is the denominator in mu-alpha, k-alpha, D-alpha, alpha-T-alpha
equations for transport coefficients Sharipov&Kalempa, 2002 (74-77)"
  `(- (* ,A ,B)
      (* ,nu-ab ,nu-ba)))

#|(defun&vars&eval-calls (1 2) (2 1) P-alpha (n-alpha temp)
  "Partial pressure
Sharipov&Kalempa, 2002 (??)"
  (* n-alpha +k+ temp))|#

