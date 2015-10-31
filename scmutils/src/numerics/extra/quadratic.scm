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

;;; From Hamming, roots of quadratic without bad roundoff.
;;; a*x^2 + b*x + c = 0

(define (sgn x) (if (negative? x) -1 1))

(define (quadratic a b c
		   ;; continuations for each case
		   two-roots
		   #!optional
		   complex-roots
		   double-root
		   linear
		   no-solution)
  (if (zero? a)
      (if (zero? b)
	  (if (default-object? no-solution)
	      (error "No solution -- QUADRATIC" a b c)
	      (no-solution a b c))
	  (if (default-object? linear)
	      (error "Not QUADRATIC" a b c)
	      (linear (/ (- c) b))))
      (let ((d (- (* b b) (* 4 a c))))
	(if (zero? d)
	    (let ((root (/ b (* -2 a))))
	      (if (default-object? double-root)
		  (two-roots root root)
		  (double-root root)))
	    (let ((q (* -1/2 (+ b (* (sgn b) (sqrt d))))))
	      (let ((r1 (/ q a)) (r2 (/ c q)))
		(if (or (> d 0)
			(default-object? complex-roots))
		    (two-roots r1 r2)
		    (complex-roots r1 r2))))))))

#|
(define (test-quadratic a b c)
  (quadratic a b c
	     (lambda (r1 r2) `(two-roots ,r1 ,r2))
	     (lambda (r1 r2) `(complex-roots ,r1 ,r2))
	     (lambda (r) `(double-root ,r))
	     (lambda (r) `(linear ,r))
	     (lambda (a b c) `(no-solution ,a ,b ,c))))

;;; Examples

(test-quadratic 1 0 2)
;Value: (complex-roots -1.4142135623730951i
;;;                    +1.414213562373095i)

(test-quadratic 2 -14 20)
;Value: (two-roots 5 2)

(test-quadratic 0 1 2)
;Value: (linear -2)

(test-quadratic 1 -2 1)
;Value: (double-root 1)

(test-quadratic 0 0 0)
;Value: (no-solution 0 0 0)

(test-quadratic 0 0 1)
;Value: (no-solution 0 0 1)

(test-quadratic 0 1 0)
;Value: (linear 0)

(test-quadratic 1 0 0)
;Value: (double-root 0)

(test-quadratic 2 10 100)
;Value: (complex-roots -5/2-6.614378277661476i
;;;                    -2.5+6.614378277661476i)

;;; Slight weirdness here.  Root 1 has exact real part, 
;;; but root 2 has inexact real part...
|#