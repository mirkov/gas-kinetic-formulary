;; Mirko Vukovic
;; Time-stamp: <2011-10-18 08:46:43 two-species-function-utilities.lisp>
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


;; Macros for definining functions for encounters of molecules of two kinds

(in-package :gkf)

(eval-when (:compile-toplevel :load-toplevel :execute)
  (defun subst-alpha-beta (string alpha beta)
    "Substitute occurances of \"alpha\" and \"beta\" in `string' by
numeric values of `alpha' and `beta':
 (subst-alpha-beta \"foo-alpha-beta\" 3 8) results in \"foo-3-8\"
"
    (let* ((s-alpha (format nil "~D" alpha))
	   (s-beta (format nil "~D" beta))
	   (comps (split-sequence #\- string))
	   (res (first comps)))
      (dolist (comp (rest comps))
	(setf res (concatenate 'string res "-"
			       (cond ((string= comp "ALPHA") s-alpha)
				     ((string= comp "BETA") s-beta)
				     (t comp)))))
      res)))

(define-test subst-alpha-beta
  (assert-equal "foo-3-8" (subst-alpha-beta "foo-ALPHA-BETA" 3 8))
  (assert-equal "foo-alpha-8" (subst-alpha-beta "foo-alpha-BETA" 3 8)))


(defmacro defun&vars&eval-calls (alpha-list beta-list fun args &body fun-body)
  "Define function `fun' of `args' with `body'.  Using the
`alpha-list' and `beta-list', define a bind- macro that will create a
local binding to all possible variations.


As an example,

 (defun&vars&eval-calls M*-ALPHA-BETA (1 2) (2 1) (M-ALPHA M-BETA)
   (/ (* m-alpha m-beta) (+ m-alpha m-beta)))

will result in the following being done:

Function definition
 (DEFUN M*-ALPHA-BETA (M-ALPHA M-BETA)
  (/ (* M-ALPHA M-BETA) (+ M-ALPHA M-BETA)))

A bind-m*-alpha-beta form
  (let ((m*-1-2 (m*-alpha-beta m-1 m-2))
        (m*-2-1 (m*-alpha-beta m-2 m-1)))
     ,@body)
"
  (let* ((fun-name (symbol-name fun))
	 (core-name (first (split-sequence #\= fun-name)))
;;	 (calc-fun-name (symbolicate "CALC-" fun-name))
	 (bind-vars-name (symbolicate "BIND-" fun-name))
	 (bindings
	  (loop for alpha in alpha-list
	     for beta in beta-list
	     collect (let ((dsm-name (symbolicate (subst-alpha-beta core-name alpha beta)))
			   (exp-args (loop for arg in args
					collect (symbolicate (subst-alpha-beta (symbol-name arg) alpha beta)))))
		       `'(,dsm-name (,fun ,@exp-args))))))
    (unless core-name
      (error "Function name has to end with a ="))
    `(progn
       (defun ,fun ,args
	 ,@fun-body)
       (defmacro ,bind-vars-name (&body body)
	 `(let (,,@bindings)
	   ,@body)))))


(define-test defun&vars&eval-calls
  (assert-expands
   '(PROGN
     (DEFUN M*-ALPHA-BETA (M-ALPHA M-BETA)
       (/ (* M-ALPHA M-BETA) (+ M-ALPHA M-BETA)))
     (DEFMACRO BIND-M*-ALPHA-BETA (&BODY BODY)
       (CONS 'LET
	     (CONS (LIST '(M*-1-1 (M*-ALPHA-BETA M-1 M-1))
			 '(M*-1-2 (M*-ALPHA-BETA M-1 M-2))
			 '(M*-2-1 (M*-ALPHA-BETA M-2 M-1))
			 '(M*-2-2 (M*-ALPHA-BETA M-2 M-2)))
		   BODY))))
   (defun&vars&eval-calls (1 1 2 2) (1 2 1 2) M*-ALPHA-BETA (M-ALPHA M-BETA)
     (/ (* m-alpha m-beta) (+ m-alpha m-beta))))
    (assert-expands
     '(PROGN
     (DEFUN M*-ALPHA-BETA (M-ALPHA M-BETA)
       (/ (* M-ALPHA M-BETA) (+ M-ALPHA M-BETA)))
     (DEFMACRO BIND-M*-ALPHA-BETA (&BODY BODY)
      (CONS 'LET
	    (CONS (LIST '(M*-1-2 (M*-ALPHA-BETA M-1 M-2))
			'(M*-2-1 (M*-ALPHA-BETA M-2 M-1)))
		  BODY))))
   (defun&vars&eval-calls (1 2) (2 1) M*-ALPHA-BETA (M-ALPHA M-BETA)
     (/ (* m-alpha m-beta) (+ m-alpha m-beta)))))



(defmacro bind-alpha-beta-vars (alpha-list beta-list var fun-args &body fun-body)
  "Establish a binding to `var-name' using permutations in `alpha-list' and `beta-list'
Each binding has a value of applying `fun-body' to `fun-args'


As an example,

 (defun&vars&eval-calls (1 2) (2 1) M*-ALPHA-BETA (M-ALPHA M-BETA)
   (/ (* m-alpha m-beta) (+ m-alpha m-beta)))

will result in the following being done:"

  (let* (;;(fun (gensym (symbol-name var)))
	 (fun (gensym (symbol-name var)))
;;	 (fun-name (symbol-name fun))
	 (var-name (symbol-name var))
	 (core-name (first (split-sequence #\= var-name)))
	 (bind-vars-name (symbolicate "BIND-" var-name))
	 (bindings
	  (loop for alpha in alpha-list
	     for beta in beta-list
	     collect (let ((dsm-name (symbolicate (subst-alpha-beta core-name alpha beta)))
			   (exp-args (loop for arg in fun-args
					collect (symbolicate (subst-alpha-beta (symbol-name arg) alpha beta)))))
		       `'(,dsm-name (,fun ,@exp-args))))))
    (unless core-name
      (error "Function name has to end with a ="))
    ;; fallback form if the form with `labels' is failing
    #|`(progn
       (defun ,fun ,fun-args
	 ,@fun-body)
       (defmacro ,bind-vars-name (&body body)
	 `(let (,,@bindings)
	   ,@body)))|#
    ;; preferable form
    `(defmacro ,bind-vars-name (&body body)
       `(labels ((,',fun ,',fun-args
		   ,',@fun-body))
	  (let (,,@bindings)
	    ,@body)))))


(define-test bind-alpha-beta-vars
  (assert-expands
   '(labels ((M*-ALPHA-BETA (M-ALPHA M-BETA)
	      (/ (* M-ALPHA M-BETA) (+ M-ALPHA M-BETA))))
     (DEFMACRO BIND-M*-ALPHA-BETA (&BODY BODY)
       (CONS 'LET
	     (CONS (LIST '(M*-1-1 (M*-ALPHA-BETA M-1 M-1))
			 '(M*-1-2 (M*-ALPHA-BETA M-1 M-2))
			 '(M*-2-1 (M*-ALPHA-BETA M-2 M-1))
			 '(M*-2-2 (M*-ALPHA-BETA M-2 M-2)))
		   BODY))))
   (bind-alpha-beta-vars (1 1 2 2) (1 2 1 2) M1*-ALPHA-BETA (M-ALPHA M-BETA)
     (/ (* m-alpha m-beta) (+ m-alpha m-beta)))))


#|
(let ((m-1 1)
      (m-2 2))
  (bind-m1*-alpha-beta
    (values m1*-1-1 m1*-2-1 m1*-2-2)))

|#