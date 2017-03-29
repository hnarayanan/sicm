#| -*-Scheme-*-

Copyright (C) 1986, 1987, 1988, 1989, 1990, 1991, 1992, 1993, 1994,
    1995, 1996, 1997, 1998, 1999, 2000, 2001, 2002, 2003, 2004, 2005,
    2006, 2007, 2008, 2009, 2010, 2011, 2012, 2013, 2014, 2015, 2016
    Massachusetts Institute of Technology

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

;;;;              Gaussian ellipsoids

;;; Assume that we have a list of measurements in an n-dimensional
;;; space.  Each measurement is characterized by a center and an
;;; ellipsoid representing the error in the measurement.  The
;;; ellipsoids represent an n-dimensional Gaussian probability
;;; distribution.  Thus, for example, if
;;;   a1*(x-x0)^2 + a2*(x-x0)*(y-y0) + a3*(y-y0)^2 
;;; is an ellipse then 
;;;   K*exp(a1*(x-x0)^2 + a2*(x-x0)*(y-y0) + a3*(y-y0)^2)
;;; is the probability that the correct value is (x, y), so long as K
;;; is chosen so that the integral over all (x, y) is 1.

;;; Given a set of measurements, the single measurement consistent
;;; with all of them is computed by finding the ellipsoid produced by
;;; adding together the equations of the given ellipsoids.  This works
;;; because we are interpreting the ellipsoids as Gaussian probability
;;; distributions.

;;; Given a list of ellipsoids JOINT-DISTRIBUTION computes the
;;; consistent sum, as described above.

;;; We represent the ellipsoid by a center point and a covariance
;;; matrix in rectangular coordinates.  The coefficients are inverses
;;; of variances (units).

;;; JOINT-DISTRIBUTION returns a new ellipsoid.

(define make-ellipsoid list)
(define ellipsoid:center-point car)
(define ellipsoid:covariance-matrix cadr)

(define pdv vector-vector)		;for flat manifold

(define (joint-distribution . given-ellipsoids) ;must be one or more
  (let ((ellipsoids
	 (let ((e1 (car given-ellipsoids))
	       (es (cdr given-ellipsoids)))
	   (let ((ce1 (ellipsoid:center-point e1)))
	     (cons e1
		   (map (lambda (e)
			  (make-ellipsoid
                           (vector+vector ce1
                                          (pdv (ellipsoid:center-point e)
                                                         ce1))
                           (ellipsoid:covariance-matrix e)))
			es))))))
    (let ((new-ellipsoid:covariance-matrix
	   (a-reduce matrix+matrix (map ellipsoid:covariance-matrix ellipsoids)))
	  (foob
	   (map (lambda (ellipsoid)
		  (matrix*vector (ellipsoid:covariance-matrix ellipsoid)
				 (ellipsoid:center-point ellipsoid)))
		ellipsoids)))
      (let ((new-center
	     (m:solve-linear new-ellipsoid:covariance-matrix
                             (a-reduce vector+vector foob))))
	;; This matrix is real, positive, and symmetric.
	(make-ellipsoid new-center new-ellipsoid:covariance-matrix)))))


;;; The following is a probability-density function... Its integral
;;; over a region of phase space gives the probability that the true
;;; value is in that region.

(define (probability-density ellipsoid)
  (let ((scale-factor
	 (sqrt (/ (m:determinant (ellipsoid:covariance-matrix ellipsoid))
		  (expt :pi (num-rows (ellipsoid:covariance-matrix ellipsoid)))))))
    (lambda (point)
      (let ((x-x0 (pdv point (ellipsoid:center-point ellipsoid))))
	(* (exp (- (v:dot-product x-x0
                                  (matrix*vector (ellipsoid:covariance-matrix ellipsoid)
                                                 x-x0))))
	   scale-factor)))))


;;; The following requires a function that is the derivative of the
;;; inverse map.  This is, in general, a Jacobian.

(define (push-ellipsoid-thru-map f df^-1 ellipsoid)
  (let* ((new-center (f (ellipsoid:center-point ellipsoid)))
	 (variations (df^-1 new-center)))
    (make-ellipsoid new-center
		    (matrix*matrix (m:transpose variations)
                                   (ellipsoid:covariance-matrix ellipsoid)
                                   variations))))
