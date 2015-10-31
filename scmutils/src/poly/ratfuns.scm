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

;;; Given a list of datapoints, constructs a rational function of
;;; numerator degree n and denominator degree m that least-squares
;;; best approximates the data.

;;; Needs SVD to work.


;;; We seek An, ..., A0, Bm-1, ..., B0 such that for each of a
;;; set of datapoints ( x y ) we have:

;;;                      An*x^n + ... + A0
;;;                ----------------------------- = y
;;;                 x^m + Bm-1*x^m-1 + ... + B0


;;; The method is to make up for each datapoint a linear equation:

;;; An*x^n + ... + A0 + y*x^m + Bm-1*y*x^m-1 + ... + B0*y = 0

;;;  and least-squares solve the system for Ai, Bj.


;;; The equations are written as the matrix equation M*U = V

;;; The unknown vector U = #(A0 ... An B0 ... Bm).
;;; The ith row corresponds to the ith datapoint:

;;; Mi = #(1 x x^2 ... x^n -y -y*x ... -y*x^m-1);  Vi = y*x^m


(define (data->ratfun datapoints n m . optional-epsilon)
  (define (y pt) (cadr pt))
  (define (x pt) (car pt))
  (let* ((n+1 (fix:+ n 1))
	 (n+1+m (fix:+ n+1 m))
	 (coeffs
	  (apply svd:least-squares
		 (list->vector
		  (map (lambda (pt)
			 (let ((xi (x pt)) (yi (y pt)))
			   (generate-vector
			    n+1+m
			    (lambda (j)
			      (if (fix:< j n+1)
				  (expt xi j)
				  (* -1 yi (expt xi (- j n+1))))))))
		       datapoints))
		 (list->vector
		  (map (lambda (pt) (* (y pt) (expt (x pt) m)))
		       datapoints))
		 optional-epsilon)))
    (ratfun:/ (poly:dense->
	       (vector->list (subvector coeffs 0 n+1)))
	      (poly:dense->
	       (append (vector->list (subvector coeffs n+1 n+1+m))
		       '(1))))))

(define (ratfun->alt r)
  (if (poly:characteristic-predicate r)
      r
      (let ((n (ratform-numerator r)) (d (ratform-denominator r)))
	(let ((dn (poly:trailing-coefficient d)))
	  (if (poly:one? dn)
	      (make-ratform n d)
	      (make-ratform (poly:normalize-by n dn)
			    (poly:normalize-by d dn)))))))

		     
    
				  