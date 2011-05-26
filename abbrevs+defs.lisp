;; Mirko Vukovic
;; Time-stamp: <2011-02-22 09:59:40 abbrevs+defs.lisp>
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


(export '(+kb+ +R+ +T0+ +P0+ +amu+ +V-molar@STP+))

(defconstant +kb+ physics-constants:+boltzmann-constant-SP+)
(defconstant +R+ physics-constants:+gas-constant-SP+)
(defconstant +T0+ physics-constants:+standard-temperature-SP+)
(defconstant +P0+ physics-constants:+standard-pressure-SP+)
(defconstant +amu+ physics-constants:+atomic-mass-unit-SP+)
(defconstant +A+ physics-constants:+avogadros-number-sp+)
(defconstant +V-molar@STP+ (/ (* +R+ +T0+) +P0+))
