;; Mirko Vukovic
;; Time-stamp: <2011-10-17 10:42:03 hs-collision-integrals.lisp>
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

;; this file creates calls to functions in the `collision-integrals'
;; package that calculate the integrals

(export '(bind-omega11-hs-alpha-beta bind-omega12-hs-alpha-beta bind-omega13-hs-alpha-beta bind-omega22-hs-alpha-beta
	  sigma-1 sigma-2))

(defun&vars&eval-calls (1 1 2 2) (1 2 1 2)
    Omega11-hs-alpha-beta (Temp m*-alpha-beta sigma-alpha sigma-beta)
    "Hard sphere collision integral"
    (omega-11-hs1 m*-alpha-beta temp (* 0.5 (+ sigma-alpha sigma-beta))))

(define-test Omega11-Hs-mass-dependence
  "Simplest test where the argument of the root should be unity"
  (let ((temp (/ (* +pi+ +kb+)))
	(m-1 1.0)
	(m-2 4.0)
	(sigma-1 1.0)
	(sigma-2 1.0)
	(lisp-unit:*epsilon* 1e-6))
    (bind-m*-alpha-beta
      (bind-omega11-hs-alpha-beta
	(assert-number-equal (* 0.5 omega11-hs-1-1)  Omega11-Hs-2-2)
	(assert-number-equal (* (sqrt (/ 1.6)) omega11-hs-1-1)  Omega11-Hs-1-2)
	(assert-number-equal (* (sqrt (/ 1.6)) omega11-hs-1-1)  Omega11-Hs-2-1)))))

(define-test Omega11-Hs-sigma-dependence
  "Simplest test where the argument of the root should be unity"
  (let ((temp (/ (* +pi+ +kb+)))
	(m-1 1.0)
	(m-2 1.0)
	(sigma-1 1.0)
	(sigma-2 2.0)
	(lisp-unit:*epsilon* 1e-6))
    (bind-m*-alpha-beta
      (bind-omega11-hs-alpha-beta
	(assert-number-equal (* 4 omega11-hs-1-1)  Omega11-Hs-2-2)
	(assert-number-equal (* 2.25 omega11-hs-1-1)  Omega11-Hs-1-2)
	(assert-number-equal (* 2.25 omega11-hs-1-1)  Omega11-Hs-2-1)))))



(defun&vars&eval-calls (1 1 2 2) (1 2 1 2) Omega12-alpha-beta (Temp m*-alpha-beta sigma-alpha sigma-beta)
  (omega-12-hs1 m*-alpha-beta temp (* 0.5 (+ sigma-alpha sigma-beta))))

(defun&vars&eval-calls (1 1 2 2) (1 2 1 2) Omega13-alpha-beta (Temp m*-alpha-beta sigma-alpha sigma-beta)
  (omega-13-hs1 m*-alpha-beta temp (* 0.5 (+ sigma-alpha sigma-beta))))

(defun&vars&eval-calls (1 1 2 2) (1 2 1 2) Omega22-alpha-beta (Temp m*-alpha-beta sigma-alpha sigma-beta)
  (omega-22-hs1 m*-alpha-beta temp (* 0.5 (+ sigma-alpha sigma-beta))))

(define-test Omega12&13&22
  (let ((temp (/ (* +pi+ +kb+)))
	(m-1 4.0)
	(m-2 4.0)
	(sigma-1 0.5)
	(sigma-2 0.5))
    (bind-m*-alpha-beta
      (bind-omega11-hs-alpha-beta
	(bind-omega12-alpha-beta
	  (assert-number-equal (* 3.0 Omega11-Hs-1-1) Omega12-1-1)
	  (assert-number-equal (* 3.0 Omega11-Hs-1-2) Omega12-1-2)
	  (assert-number-equal (* 3.0 Omega11-Hs-2-1) Omega12-2-1)
	  (assert-number-equal (* 3.0 Omega11-Hs-2-2) Omega12-2-2))
	(bind-omega13-alpha-beta
	  (assert-number-equal (* 12.0 Omega11-Hs-1-1) Omega13-1-1)
	  (assert-number-equal (* 12.0 Omega11-Hs-1-2) Omega13-1-2)
	  (assert-number-equal (* 12.0 Omega11-Hs-2-1) Omega13-2-1)
	  (assert-number-equal (* 12.0 Omega11-Hs-2-2) Omega13-2-2))
	(bind-omega22-alpha-beta
	  (assert-number-equal (* 2.0 Omega11-Hs-1-1) Omega22-1-1)
	  (assert-number-equal (* 2.0 Omega11-Hs-1-2) Omega22-1-2)
	  (assert-number-equal (* 2.0 Omega11-Hs-2-1) Omega22-2-1)
	  (assert-number-equal (* 2.0 Omega11-Hs-2-2) Omega22-2-2))))))