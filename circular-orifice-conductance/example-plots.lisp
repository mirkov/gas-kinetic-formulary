(in-package #:coc-user)

(define-test w-plot
  (let ((delta (lseq 0d0 12d0)))
    (set-to ((xlabel "delta")
	     (ylabel "W")
	     (title "Reduced orifice conductance as function of delta"))
	(plot-xy delta
		 (gpmap (reduced-conductance @!delta) delta)))))

(define-test truncation-effects
  (let ((ud (lseq 0d0 1d0)))
    (set-to ((xlabel "u_d")
	     (ylabel "Phi")
	     (title "Reduced flux for the full and truncated drift Maxwellian"))
      (plot-xys ud
		(list
		 (list (gpmap (phi% +drift-maxwellian+ @!ud) ud) :title "full")
		 (list (gpmap (phi% +truncated-drift-maxwellian+ @!ud) ud)
		       :title "truncated"))))))


(define-test root-finder-test
  "For a scan of values of Phi, we calculate ud using ud/phi/.
We then calculae back Phi using Phi.

The two should agree"
  (let* ((phi0 (lseq 1d0 2.5d0))
	 (ud (gpmap (ud/phi/ @!phi) phi0))
	 (phi1 (gpmap (phi @!ud) ud)))
    (set-to ((xlabel "u_d")
	     (ylabel "Phi")
	     (title "Two-way calculation of Phi"))
      (plot-xys ud
		(list
		 (list phi0 :title "Phi")
		 (list phi1 :title "Phi(u_d(Phi))"))))))

(define-test flux-vs-mass
  (let ((delta (lseq 0d0 12d0))
	(m-ratio 1.5d0))
    (let* ((Phi0 (gpmap (reduced-conductance @!delta) delta))
	   (ud0 (gpmap (ud/phi/ @!phi) Phi0))
	   (phi1 (let ((ud1 (gmap #'(lambda (arg)
				      (/ arg m-ratio)) ud0)))
		   (gmap #'phi ud1)))
	   (phi2 (let ((ud2 (gmap #'(lambda (arg)
				      (* arg m-ratio)) ud0)))
		   (gmap #'phi ud2))))
      (set-to ((xlabel "Delta")
	       (ylabel "Relative flux")
	       (title "Mass ratio effect on trace gas flux"))
	(plot-xys delta
		  (list
		   (list Phi0 :title "Carrier")
		   (list Phi1 :title (format nil
					     "m_t/m_c=~5,3f" (/ m-ratio)))
		   (list Phi2 :title (format nil
					     "m_t/m_c=~f" m-ratio))))))))
   

