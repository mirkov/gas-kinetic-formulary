;; Mirko Vukovic
;; Time-stamp: <2011-02-22 10:00:58 parallel-plates-heat-transfer.lisp>
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

;; Heat flux between parallel plates in limit of free molecular flow.
;;
;; Bird, Section 7.3

(define-symbol-macro pi^1/2 (sqrt pi))


(define-test +^
  (assert-number-equal
   3d0 (+^ 4d0 1d0)))

(defun +^ (&rest args)
  (apply #'+ (mapcar #'sqrt args)))

(define-test -^
  (assert-number-equal
   1d0 (-^ 4d0 1d0)))

(defun -^ (x y)
  (- (sqrt x) (sqrt y)))


(define-test *^
  (assert-number-equal
   2d0 (*^ 4d0 1d0)))


(defun *^ (&rest args)
  (apply #'* (mapcar #'sqrt args)))



(define-test n_s
  (assert-number-equal
   (/ 2.0 3.0)
   (n_s 1.0 1.0 4.0))
  (assert-number-equal
   (/ 1.0 3.0)
   (n_s 1.0 4.0 1.0)))
  
(defun n_s (n T_s T_o)
  "Flux of gas from surface `s' at temperature `T_s' towards surface
  `o' at temperature `T_o'.  `n' is the total gas density.

Bird (7.23)"
  (/ (* n (sqrt T_o))
     (+^ T_s T_o)))

(defun n_l (n T_l T_u)
  "Bird (7.23)"
  (n_s n T_l T_u))

(defun n_u (n T_u T_l)
  "Bird (7.23)"
  (n_s n t_u T_l))

(define-test q_s
  ;; If m=+kb+, R=1
  ;; Then with T_s =0.5, the exponent term is unity
  ;; n=sqrt(pi) cancels (/ pi^1/2)
  ;; the only term left is the leading m which is +kb+
  (assert-number-equal
   +kb+
   (q_s +kb+ pi^1/2 0.5)))

(defun q_s (m n_s T_s)
  "Heat flux from surface s.

 - n_s Density of gas streaming away from surface `s'
 - T_s Surface `s' temperature
 - m gas mass

Bird Sect. 7.3"
  
  (let ((R (/ +kb+ m)))
    (* m n_s (/ pi^1/2) (expt (* 2 R T_s) 1.5))))


(defun T_eff (T_1 T_2)
  "Effective temperature

 (see Bird 7.24)"
  (*^ T_1 T_2))

(define-test q_f%
  (assert-number-equal
   0
   (q_f% 12 51 1 1))
  ;; T_l=1 & T_u=4 make their contribution 2.0
  ;; R=2 cancels out the numerical 2 factor
  ;; rho=sqrt(pi) cancels out the pi^1/2
  (assert-number-equal
   -2
   (q_f% pi^1/2 0.5 1.0 4.0)))

(defun q_f% (rho R T_l T_u)
  (declare (inline q_f%))
  (- (* (print (expt (* 2 R) 1.5))
	rho
	(/ pi^1/2 )
	(*^ T_l T_u)
	(-^ T_u T_l))))


(define-test q_f
  (let ((p +kb+)
	(T_l 1)
	(T_u 4)
	(m 1.0))
    (let* ((T_eff (T_eff T_l T_u))
	   (n (n p T_eff))
	   (n_l (n_l n T_l T_u))
	   (n_u (n_u n T_u T_l)))
      (let* ((q_l (q_s m n_l T_l))
	     (q_u (q_s m n_u T_u))
	     (q_diff (- q_l q_u))
	     (q_net (qf m n T_l T_u)))
	(let ((lisp-unit:*epsilon* 1e-5))
	  (assert-number-equal q_diff q_net))))))


(defun q_f (m n T_l T_u)
  "Heat flux from surface `l' to surface `u'

Bird (7.24)"
  (let ((R (/ +kb+ m))
	(rho (* n m)))
    (q_f% rho R T_l T_u)))


#|
(let ((p +kb+)
      (T_l 1)
      (T_u 4)
      (m 1.0))
  (let* ((T_eff (T_eff T_l T_u))
	 (n (n p T_eff))
	 (n_l (n_l n T_l T_u))
	 (n_u (n_u n T_u T_l)))
    (let* ((q_l (q_s m n_l T_l))
	   (q_u (q_s m n_u T_u))
	   (q_diff (- q_u q_l))
	   (q_net (qf m n T_l T_u)))
      (values q_net (/ q_diff q_net)))))

|#
