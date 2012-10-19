;; Mirko Vukovic
;; Time-stamp: <2012-05-14 21:50:26 thermal-conductivity.lisp>
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

;;; Equations from 7.3b, thermal conductivity

;; In equations for lambda (7.3-26,27), I pull in the mass into the
;; square root numeratlor.  This gives k/m or R/M.
;; I leave

(defun lambda-i/1 (m-i sigma-i temp T*)
  "7.3-26"
  (* (/ 25d0 32d0)
     (/ (sqrt (* +pi+ +R1+ temp (/ m-i))
	(* +pi+ (expt sigma-i 2) (omega*-22 T*)))
     0.75d0 +R1+ )))