;; Mirko Vukovic
;; Time-stamp: <2011-05-10 11:25:04EDT speeds.lisp>
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

(export '(beta c_m c_m1 c_a c_a1 c_s c_s1))

(define-test beta
  (let ((*epsilon* 1e-4)
	(molar-mass-g 40))
    (assert-numerical-equal (sqrt (/ (* 2 +kb+ 300.0)
				     (* molar-mass-g +amu+)))
			    (/ (beta 300.0 (* molar-mass-g 1e-3))))))

(defun beta (T-K m-molar-kg)
  "Inverse characteristic velocity (Bird1994-4.1)

 sqrt (m-molecular-kg / 2 k T), or sqrt (m-molar-kg / 2 R T)

NOTE: the molar mass is in kg.  Thus for hydrogen, you use 1e-3
 (approximately), not 1."
  (sqrt (/ m-molar-kg
	   (* 2 +R+ T-k))))

(define-test c-m-a-s
    (assert-numerical-equal 1.0 (c-m 1.0))
  (assert-numerical-equal (/ 2 (sqrt pi)) (c-a 1.0))
  (assert-numerical-equal (sqrt 1.5) (c-s 1.0)))


(defun c_m (beta)
  "(Bird1994-4.7)"
  (/ beta))

(defun c_m1 (T-K m-amu)
  "(Bird1994-4.7)"
  (/ (beta T-K m-amu)))

(defun c_a (beta)
  "(Bird1994-4.8)"
  (/ 2
     (* (sqrt pi) beta)))

(defun c_a1 (T-K m-amu)
  "(Bird1994-4.8)"
  (/ 2
     (* (sqrt pi) (beta T-K m-amu))))

(defun c_s (beta)
  "(Bird1994-4.9)"
  (* (sqrt 1.5) beta))

(defun c_s1 (T-K m-amu)
  "(Bird1994-4.9)"
  (* (sqrt 1.5) (beta T-K m-amu)))


