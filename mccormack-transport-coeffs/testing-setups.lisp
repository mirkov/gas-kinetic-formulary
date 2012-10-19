;; Mirko Vukovic
;; Time-stamp: <2011-08-03 22:08:48 testing-setups.lisp>
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

(in-package :gkf-tc)

(defmacro with-simple-equal-species (&body body)
  "Both species with same numerically simple properties"
  `(let ((temp (/ 16.0 (* +pi+ +k+)))
	 (m-1 1.0)
	 (m-2 1.0)
	 (sigma-1 0.5)
	 (sigma-2 0.5))
     ,@body))



(defparameter *m1* (* 4.0026d0 +amu+))
(defparameter *m2* (* 39.948d0 +amu+))
(defparameter *sigma1* 60d-11)
(defparameter *sigma2* (* 1.342d0 *sigma1*))
(defparameter *temp* 300.0d0)

(defmacro with-ref-problem (&body body)
  "Reference problem with realistic and physical values"
  `(let ((m-1 *m1*)
	 (m-2 *m2*)
	 (Temp *temp*)
	 (sigma-1 *sigma1*)
	 (sigma-2 *sigma2*)
	 (n-1 1e20)
	 (n-2 2e20))
     ,@body))

(defmacro with-equal-species (&body body)
  "Environment in which both molecules are identical"
  `(let ((m-1 1d-26)
	 (m-2 1d-26)
	 (Temp 300)
	 (sigma-1 1d-10)
	 (sigma-2 1d-10))
     ,@body))



(defmacro with-equal-species-50/50 (&body body)
  "Environment with same molecules and same partial pressures"
  `(let ((m-1 1d-26)
	 (m-2 1d-26)
	 (Temp 300)
	 (sigma-1 1d-10)
	 (sigma-2 1d-10)
	 (n-1 1d22)
	 (n-2 1d22))
     ,@body))

(defmacro def-var-density-tests (name mc-call ss1-call ss2-call)
  "Define tests for `mc-call' for a two identical gases, but a variety of densities"
  (let ((single-spec1 (symbolicate name "-single-spec"))
	(single-spec2 (symbolicate name "-single-spec-var-conc"))
	(two-spec (symbolicate name "-two-spec")))
    `(progn
       (define-test ,single-spec1
	 (with-single-species
	   (let ((n-1 1d22)
		 (n-2 1d22))
	     ;;	      (+k+ 1.3807d-23))
	     (let ((lisp-unit:*epsilon* 1e-4))
	       (assert-number-equal ,ss1-call ,mc-call
				    ,(format nil "~a single species test" name))))))
       (define-test ,single-spec2
	 (let ((lisp-unit:*epsilon* 1e-4))
	   (with-single-species
	     (let ((n-1 1d22)
		   (n-2 1d16))
	       (assert-number-equal ,ss1-call ,mc-call
				    ,(format nil "~a n1>>n2" name)))
	     (let ((n-1 1d22)
		   (n-2 1d21))
	       (assert-number-equal ,ss1-call ,mc-call
				    ,(format nil "~a n1>n2" name)))
	     (let ((n-1 1d22)
		   (n-2 1d22))
	       (assert-number-equal ,ss1-call ,mc-call
				    ,(format nil "~a n1=n2" name)))
	     (let ((n-1 1d21)
		   (n-2 1d22))
	       (assert-number-equal ,ss1-call ,mc-call
				    ,(format nil "~a n1<n2" name)))
	     (let ((n-1 1d16)
		   (n-2 1d22))
	       (assert-number-equal ,ss1-call ,mc-call
				    ,(format nil "~a n1<<n2" name))))))
       (define-test ,two-spec
	 (let ((lisp-unit:*epsilon* 1e-4))
	   (let ((m-1 1d-26)
		 (m-2 1d-24)
		 (Temp 300)
		 (sigma-1 1d-10)
		 (sigma-2 8d-10))
	     (let ((n-1 1d22)
		   (n-2 1d16))
	       (assert-number-equal ,ss1-call ,mc-call
				    ,(format nil "~a n1>>n2" name)))
	     (let ((n-1 1d16)
		   (n-2 1d22))
	       (assert-number-equal ,ss2-call ,mc-call
				    ,(format nil "~a n1<<n2" name)))))))))