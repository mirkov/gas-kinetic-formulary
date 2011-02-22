;; Mirko Vukovic
;; Time-stamp: <2011-02-22 09:41:11 collisions.lisp>
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

(export '(m*))
(defun m* (m_1 m_2)
  "Reduced mass, calclated in a way to hopefully eliminate overeflows when using SI"
  (1/ (+ (1/ m_1)
	 (1/ m_2))))