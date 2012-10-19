(in-package #:coc)

(defun trace-gas-reduced-conductance (delta mt/mc)
  "Calculate reduced trace gas flux as function of the rarefaction
parameter DELTA and the trace/carrier gas mass ratio MT/MC"
  (let* ((|PHI| (reduced-conductance delta))
	 (|U| (ud/phi/ |PHI|))
	 (|u| (* |U| (sqrt mt/mc)))
	 (|phi| (phi |u|)))
    |phi|))

(define-test trace-relative-phi
  (let* ((delta 0.5d0)
	 (phi (reduced-conductance delta)))
    (let ((lisp-unit:*epsilon* 1d-6))
      (assert-numerical-equal
       phi (trace-gas-reduced-conductance delta 1d0)))))