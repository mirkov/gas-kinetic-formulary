;; Mirko Vukovic
;; Time-stamp: <2012-06-06 10:03:12 gas-kinetic-formulary.asd>
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

(asdf:defsystem gas-kinetic-formulary
  :name "gas-kinetic-formulary"
  :author "Mirko Vukovic <mirko.vukovic@gmail.com>"
  :version "0.1"
  :description "Mirko's gas-kinetic formulary"
  :components
  ((:module "package-setup"
	    :pathname #p"./"
	    :components
	    ((:file "gas-kinetic-formulary-package-def")))
   (:module "utilities"
	    :pathname #p"./"
	    :depends-on ("package-setup")
	    :components
	    ((:file "abbrevs+defs")
	     (:file "two-species-function-utilities")))
   (:module "collisions"
	    :pathname #p"./"
	    :depends-on ("utilities")
	    :components
	    ((:file "collision-parameters")
	     #|(:file "hs-collision-integrals")|#
	     (:file "collision-integrals")))
   (:module "formulary"
	    :pathname #p"./"
	    :depends-on ("package-setup")
	    :serial t
	    :components
	    ((:file "speeds")
	     (:file "fundamental-parameters"
		    :depends-on ("speeds"))
	     (:file "transport-coefficients")
	     (:file "equation-of-state")
	     (:file "fluxes"
		    :depends-on ("speeds"))
	     (:file "conductances")))
   (:module "mccormack-transport-coeffs-setup"
	    :pathname #p"mccormack-transport-coeffs/"
	    :depends-on ("utilities" "collisions" "formulary")
	    :components((:file "helper-macros")
			(:file "collision-freqs"
			       #|:depends-on ("helper-macros")|#)
			;; modified naming convention
			(:file "collision-freqs1")))
   (:module "mccormack-transport-coeffs"
	    :pathname #p"mccormack-transport-coeffs/"
	    :depends-on ("mccormack-transport-coeffs-setup")
	    :components ((:file "viscosity")
			 (:file "thermal-conductivity")
			 (:file "diffusivity")
			 (:file "thermal-diffusivity")))
   (:module "mixtures"
	    :pathname #p"./"
	    :depends-on ("utilities")
	    :components ((:file "mixture-parameters"))))
  :depends-on (:split-sequence
	       :alexandria
	       :lisp-unit
	       :my-utils
	       :collision-integrals
	       :physics-constants
	       ))

(asdf:defsystem gas-kinetic-formulary-user
  :components
  ((:module "user-setup"
	    :pathname "./"
	    :components
	    ((:file "user-package-def")
	     (:file "user-setup")))
   (:module "user"
	    :pathname "./"
	    :depends-on ("user-setup")
	    :components
	    ((:file "McCormack-transport-coeffs-user"))))
  :depends-on (#:gas-kinetic-formulary
	       #:mv-grid-utils
	       #:mv-gnuplot
	       #:lisp-unit
	       #:defgeneric+default))