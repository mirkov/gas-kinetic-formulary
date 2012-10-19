;; Mirko Vukovic
;; Time-stamp: <2011-08-08 09:12:03 collision-parameters.lisp>
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

(export '(m* m*-alpha-beta bind-m*-alpha-beta m-1 m-2))
(defun m* (m_1 m_2)
  "Reduced mass, calclated in a way to hopefully eliminate overeflows when using SI"
  (/ (+ (/ m_1)
	(/ m_2))))

(defun&vars&eval-calls (1 1 2 2) (1 2 1 2) M*-ALPHA-BETA (M-ALPHA M-BETA)
  "Reduced mass

Sharipov&Kalempa, 2002 (69)"
  (/ (+ (/ m-alpha) (/ m-beta))))


(define-test m*-alpha-beta
  (let ((m-1 1.0)
	(m-2 0.5))
    (bind-m*-alpha-beta
      (let ((lisp-unit:*epsilon* 1e-4))
	(assert-number-equal 0.5 m*-1-1)
	(assert-number-equal (/ 3) m*-1-2)
	(assert-number-equal (/ 3) m*-2-1)
	(assert-number-equal 0.25 m*-2-2)))))


