;; Mirko Vukovic
;; Time-stamp: <2011-02-22 09:59:06 gas-kinetic-formulary-package-def.lisp>
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

;; References
;; [B] - Bird

;; Symbols are initially taken from Bird

(defvar *symbol-dictionary*
  (list
   '("Characteristic speed" beta^-1)
   '("Molecular speed" c)
   '("Most probably molecular speed" c-m)
   '("Average thermal speed" c-a)
   '("Root mean thermal speed" c-s)
   '("Viscosity" mu)
   '("Thermal conductivity" k)
   '("Reduced mass" m-*)
   '("mass" m)
   '("Pressure" P)
   '("Density" n)
   '("Diameter" d)
   '("Cross section" sigma)

)
  "Dictionary of variable symbols used in computations")

(defpackage :tokyo-electron.mv.gas-kinetic-formulary
  (:nicknames :gas-kinetic-formulary :gkf)
  (:use :common-lisp :lisp-unit)
  (:import-from :my-utils
		:^2
		:1/
		:^3)
  (:export
   :gas-number-density ;; depreciated function, leaves informative message
   ))


(defpackage :gas-kinetic-formulary-unit-tests
  (:use :cl :gas-kinetic-formulary :lisp-unit :my-lisp-unit :my-utils))
