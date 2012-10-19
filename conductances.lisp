;; Mirko Vukovic
;; Time-stamp: <2012-06-06 10:21:16 conductances.lisp>
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

(export '(Cc-fm))

(defun Cc-fm (A temp m-molar-kg)
  "Conductance of a circular orifice in the free-molecular regime

Johnsen & Chatterjee, JVSTA 011002-1 (2011), Eq. 2"
  ;; we use the thermal flux equation, which requires a density.  We
  ;; use a default density, and divide by it.
  (let ((density 1e20))
    (* A (thermal-flux temp density m-molar-kg)
       (/ density))))

