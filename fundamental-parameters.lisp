;; Mirko Vukovic
;; Time-stamp: <2012-07-27 14:22:53 fundamental-parameters.lisp>
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

(export '(lambda-mfp lambda-mfp-ilt-mu lambda-mfp-ilt-mu
	  delta delta-1 delta-2 delta-3 Kn Kn-1))


;;; Rarefaction parameters

(defun lambda-mfp (mu P T-K M)
  "Mean free path, as function of viscosity, pressure, temperature and
  mass

Sharipov&Kalempa, 2002, (11)"
  (/ (/ (* (sqrt +pi+) mu)
	(* 2.0 P))
     (beta T-K M)))


(defun lambda-mfp-ilt-mu (mu P T-K M)
  "Mean free path as function of viscosity

Ivchenko, Loyalka & Tompson, ZAMP, 58-72 (2002), top of p. 59"
    (/ (/ (* 8 mu)
	(* 5.0 (sqrt +pi+) P))
     (beta T-K M)))

(defun lambda-mfp-ilt-kappa (kappa P T-K M)
  "Mean free path as function of thermal conductivity

Ivchenko, Loyalka & Tompson, ZAMP, 58-72 (2002), top of p. 59"
    (/ (/ (* 64.0 kappa)
	  (* 75.0 (sqrt +pi+) P))
     (beta T-K M)))

(defun delta (R P mu m-amu T-K)
  "Rarefaction parameter as function of the characteristic dimension
  `R', pressure `P', viscosity `mu', molecular-mass `m' (in amu's) and
  temperature 'T-K'

`delta' is defined in terms of bulk quantities to calculate the
rarefaction regime.  Therefore, I do not use values such as molecular
mass or gas density.  Instead, I use values such as mass in amu's and
gas pressure in Pascal.

Sharipov&Kalempa, 2002, (13)"
  (* (/ (* R P)
	mu)
     (beta T-K (* m-amu 1e-3))))

(defun delta-1 (R P/mu m-amu T-K)
  "Same as `delta', but using a different arguments"
  (* (* R P/mu)
     (beta T-K (* m-amu 1e-3)))
  )

(defun delta-2 (Kn)
  "Rarefaction parameter, as function of Knudsen number

Sharipov & Kalempa, JVSTA 814, (2002), Eq. 11"
  (/ (sqrt +pi+)
     (* 2.0 Kn)))

(defun delta-3 (R P mu u0)
  "Rarefaction parameter as function of the characteristic dimension
  `R', pressure `P', viscosity `mu', characteristic velocity u0 and
  temperature 'T-K'

u0 is defined as (2kT/m)^1/2 and can be calculated with C_m1 (temp molar-mass-kg)

Sharipov&Kalempa, 2002, (13)"
  (* (/ (* R P)
	mu u0)))


(defun Kn (lambda-mfp R)
  "Knudsen number"
  (/ lambda-mfp R))

(defun Kn-1 (delta)
  "Knudsen number, as function of rarefaction parameter delta

Sharipov & Kalempa, JVSTA 814, (2002), Eq. 11"
  (/ (sqrt +pi+)
     (* 2.0 delta)))


