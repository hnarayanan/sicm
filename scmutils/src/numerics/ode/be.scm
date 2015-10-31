#| -*-Scheme-*-

Copyright (C) 1986, 1987, 1988, 1989, 1990, 1991, 1992, 1993, 1994,
    1995, 1996, 1997, 1998, 1999, 2000, 2001, 2002, 2003, 2004, 2005,
    2006, 2007, 2008, 2009, 2010, 2011, 2012, 2013, 2014 Massachusetts
    Institute of Technology

This file is part of MIT/GNU Scheme.

MIT/GNU Scheme is free software; you can redistribute it and/or modify
it under the terms of the GNU General Public License as published by
the Free Software Foundation; either version 2 of the License, or (at
your option) any later version.

MIT/GNU Scheme is distributed in the hope that it will be useful, but
WITHOUT ANY WARRANTY; without even the implied warranty of
MERCHANTABILITY or FITNESS FOR A PARTICULAR PURPOSE.  See the GNU
General Public License for more details.

You should have received a copy of the GNU General Public License
along with MIT/GNU Scheme; if not, write to the Free Software
Foundation, Inc., 51 Franklin St, Fifth Floor, Boston, MA 02110-1301,
USA.

|#

(declare (usual-integrations))

;;; Backwards Euler implicit integrator. 

#|
((advance-generator
  ((quality-control c-euler 1)		;integration method
   (lambda (v) v)			;x' = x
   0.0001				;qc error tolerated
   1.0e-5))				;corrector convergence
 #(1.0)					;initial state (at t = t0)
 1.0					;proceed to t = t0 + 1
 0.1					;first step no larger than .1
 0.5					;no step larger than .5
 (lambda (ns dt h cont)
   (pp ns)
   (cont))
 (lambda (ns dt sdt)
   ;; assert ns = #(2.718...)
   ;; assert dt = 1.000...+-
   (list ns dt sdt)))
|#


(define (c-euler f qc-tolerance #!optional convergence-tolerance)
  (let ((error-measure
	 (parse-error-measure
	  (if (default-object? convergence-tolerance) qc-tolerance convergence-tolerance))))
    (lambda (xn)
      (let ((d (f xn)))
	(define (estep dt succeed fail)
	  (let* ((predicted (vector+vector xn (scalar*vector dt d)))
		 (corrected
		  (vector+vector xn (scalar*vector dt (f predicted)))))
	    (let lp ((predicted predicted) (corrected corrected) (count 1))
	      (let ((verr (error-measure predicted corrected)))
		(if (< verr 2.0)
		    (succeed corrected count)
		    (let* ((ncorr
			    (vector+vector xn (scalar*vector dt (f corrected))))
			   (nverr (error-measure ncorr corrected)))
		      (if (< nverr verr)
			  (lp corrected ncorr (fix:+ count 1))
			  (begin (if pc-wallp? (write-line `(pc failed: ,nverr ,verr)))
				 (fail)))))))))
	estep))))
