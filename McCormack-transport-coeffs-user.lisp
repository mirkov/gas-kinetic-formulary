;; Mirko Vukovic
;; Time-stamp: <2012-05-02 23:09:21 McCormack-transport-coeffs-user.lisp>
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

(in-package :gkf-user)


(defun  viscosity (temp species1 &optional species2 n-1 n-2)
  "Demonstrate calculation of Argon viscosity"
  (unless species2
    (setf species2 species1
	  n-1 1d20
	  n-2 1d20))
  (let* ((coeffs1 (make-species-lennard-jones-6/12-potential species1))
	 (coeffs2 (make-species-lennard-jones-6/12-potential species2)))
  (setf (default-omega*-lj-calc-method :Omega*-11) *omega*-table*
	(default-omega*-lj-calc-method :Omega*-22) *omega*-table*)
  (multiple-value-bind (mu1 mu2)
      (mu-alpha1 coeffs1 coeffs2 n-1 n-2 temp)
    (+ mu1 mu2))))

#|
 (viscosity 300.0d0 :Ar)
 (viscosity 300.0d0 :Ar :Ar 1d20 1d18)
 (viscosity 300.0d0 :Ar :He 1d14 1d20)

|#




(defun delta-example ()
  (delta-alpha-beta
   5e-3
   (make-species-lennard-jones-6/12-potential :He)
   (make-species-lennard-jones-6/12-potential :O2)
   1e19 1e21 300.0d0))



(defun alpha-t-vs-C (C &key (species1 :He) (species2 :Ar)
		     (n 1d20) (temp 300d0)
		     (x1x2-norm nil))
"Calculate alpha-T using the McCormack collision integral as function
of C for species1 and species2

C -- ratio of density of species1 to total density
SPECIES1 -- species 1 (default :He)
SPECIES2 -- species 2 (default :Ar)
N -- density (default 1d20)
TEMP -- temperature (default 300d0)
X1X2-NORM -- flag (default nil).  If set, divide calculated value by C(1-C)

This flag was used to test whether alpha-T is the thermal diffusion
coefficient or factor (see Kobayashi & Yamamoto, 1997 for the
distinction).  The latter depends much less on composition.

Based on the plots of alpha-T vs C with and without the flag, I
conclude that alpha-T is what Kobayashi & Yamamoto call the thermal
diffusion factor.
"
  (let* ((x-He C)
       (x-Ar (- 1 C)))
    (/ (alpha-t (make-species-lennard-jones-6/12-potential species1)
	   (make-species-lennard-jones-6/12-potential species2)
	   (* x-He n) (* x-Ar n) temp)
     (if x1x2-norm
	 (* x-He x-Ar)
	 1d0))))

(defun plot-alpha-T-vs-c (&optional (x1x2-norm nil))
  "Function plots alpha-T as function of C.  

X1X2-NORM -- flag (default nil).  If set, divide calculated value by C(1-C)

This flag was used to test whether alpha-T is the thermal diffusion
coefficient or factor (see Kobayashi & Yamamoto, 1997 for the
distinction).  The latter depends much less on composition.

The plots show that alpha-T as calculated depends relatively weakly on
composition (by about a factor of 2)

With the X1X2-NORM set, the calculated profile varies from 0 at C=0 or
1 to some non-zero value in the intermediate regime.
"
  (apply #'plot-xy 
       (loop for C from 0.01 upto 0.991 by 0.01
	  collect C into Cs
	  collect (alpha-t-vs-c C :x1x2-norm x1x2-norm) into alphas
	  finally (return (list Cs alphas)))))
  
#|(defun k-example ()
  (k-alpha1 1e8 1e20
	    (make-species-lennard-jones-6/12-potential :Ar)
	    (make-species-lennard-jones-6/12-potential :He)
	    300d0)

  (let ((temp 300.0))
    (* 2.682e-3 (expt temp 0.71)))


  |#