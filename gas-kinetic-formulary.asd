;; Mirko Vukovic
;; Time-stamp: <2011-02-22 10:00:04 gas-kinetic-formulary.asd>
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
  ((:module "gkf-setup"
	    :pathname "./"
	    :components
	    ((:file "gas-kinetic-formulary-package-def")
	     (:file "abbrevs+defs"
		    :depends-on ("gas-kinetic-formulary-package-def"))))
   (:module "gkf-components"
	    :pathname "./"
	    :depends-on ("gkf-setup")
	    :components
	    ((:file "speeds")
	     (:file "transport-coefficients")
	     (:file "collisions")
	     (:file "equation-of-state"))))
  :depends-on (:my-utils
	       :physics-constants
	       :lisp-unit
	       :my-lisp-unit))

(asdf:defsystem gas-kinetic-formulary-user
  :components
  ((:module "gkfu-setup"
	    :pathname "./"
	    :components
	    ((:file "gkf-user-package-def")
	     (:file "gkf-user-setup")))
   (:module "gkf-user"
	    :pathname "./"
	    :depends-on ("gkfu-setup")
	    :components
	    ((:file "gkf-user"))))
  :depends-on (:gas-kinetic-formulary))