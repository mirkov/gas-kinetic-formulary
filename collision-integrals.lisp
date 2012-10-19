;; Mirko Vukovic
;; Time-stamp: <2011-10-17 14:33:48 collision-integrals.lisp>
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

(export '(bind-omega11-alpha-beta bind-omega12-alpha-beta bind-omega13-alpha-beta bind-omega22-alpha-beta
	  sigma-1 sigma-2))

(bind-alpha-beta-vars (1 1 2 2) (1 2 1 2)
    Omega11-alpha-beta (species-alpha species-beta Temp)
      (omega-11 (make-collision-parameters species-alpha species-beta) temp))

(bind-alpha-beta-vars (1 1 2 2) (1 2 1 2)
    Omega12-alpha-beta (species-alpha species-beta Temp)
    (omega-12 (make-collision-parameters species-alpha species-beta) temp))

(bind-alpha-beta-vars (1 1 2 2) (1 2 1 2)
    Omega13-alpha-beta (species-alpha species-beta Temp)
    (omega-13 (make-collision-parameters species-alpha species-beta) temp))

(bind-alpha-beta-vars (1 1 2 2) (1 2 1 2)
    Omega14-alpha-beta (species-alpha species-beta Temp)
    (omega-14 (make-collision-parameters species-alpha species-beta) temp))

(bind-alpha-beta-vars (1 1 2 2) (1 2 1 2)
    Omega15-alpha-beta (species-alpha species-beta Temp)
    (omega-15 (make-collision-parameters species-alpha species-beta) temp))

(bind-alpha-beta-vars (1 1 2 2) (1 2 1 2)
    Omega22-alpha-beta (species-alpha species-beta Temp)
    (omega-22 (make-collision-parameters species-alpha species-beta) temp))






