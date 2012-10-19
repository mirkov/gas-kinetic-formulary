;; Mirko Vukovic
;; Time-stamp: <2011-10-16 19:53:37 user-package-def.lisp>
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

(defpackage :gas-kinetic-formulary-user
  (:nicknames :gkf-user)
  (:use :cl :gas-kinetic-formulary :collision-integrals
	:molecular-potentials :lisp-unit
	:mv-grid :mv-gnuplot )
  (:documentation "Template package for deploying applications on top of the
gas-kinetic-formulary.  It also includes helpful packages such as
mv-grid and mv-gnuplot"))