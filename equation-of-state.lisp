;; Mirko Vukovic
;; Time-stamp: <2011-02-22 09:45:38 equation-of-state.lisp>
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


(export '(n p))
(in-package :gas-kinetic-formulary)


(define-test gas-number-density
  (assert-number-equal (1/ +kb+) (n 1 1))
  (assert-number-equal (/ 2 +kb+) (n 2 1))
  (assert-number-equal (1/ (* 2 +kb+)) (n 1 2)))

(defun n (P T-K)
  (/ P (* +kb+ T-K)))

(defun p (n T-K)
  (* n +kb+ T-K))

