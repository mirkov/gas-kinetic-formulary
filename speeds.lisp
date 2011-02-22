;; Mirko Vukovic
;; Time-stamp: <2011-02-22 09:46:30 speeds.lisp>
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
    (assert-numerical-equal 1.0 (beta 1.0 (* 2 +R+)))
  (assert-numerical-equal 2.0 (beta 0.25 (* 2 +R+))))

(defun beta (T-K m-amu)
  "(B-4.1)"
  (sqrt (/ m
	   (* 2 +R+ T-k))))

(define-test c-m-a-s
    (assert-numerical-equal 1.0 (c-m 1.0))
  (assert-numerical-equal (/ 2 (sqrt pi)) (c-a 1.0))
  (assert-numerical-equal (sqrt 1.5) (c-s 1.0)))


(defun c_m (beta)
  "(B-4.7)"
  (/ beta))

(defun c_m1 (T-K m-amu)
  "(B-4.7)"
  (/ (beta T-K m-amu)))

(defun c_a (beta)
  "(B-4.8)"
  (/ 2
     (* (sqrt pi) beta)))

(defun c_a1 (T-K m-amu)
  "(B-4.8)"
  (/ 2
     (* (sqrt pi) (beta T-K m-amu))))

(defun c_s (beta)
  "(B-4.9)"
  (* (sqrt 1.5) beta))

(defun c_s1 (T-K m-amu)
  "(B-4.9)"
  (* (sqrt 1.5) (beta T-K m-amu)))


