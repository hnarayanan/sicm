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

;;;; Yun's algorithm for square-free decomposition 
;;;   of univariate polynomials

(define (square-free-factors pexp)
  (pcf:expression-> pexp
    (lambda (f vars)
      (assert (= (length vars) 1)
	      "SQUARE-FREE-FACTORS only for univariate polys"
	      pexp)
      (map (lambda (factor)
	     (pcf:->expression factor vars))
	   (pcf:square-free-factors f)))))

(define (pcf:square-free-factors f)
  (let ((fp (poly:derivative f)))

    (let ((a0 (poly:gcd f fp)))
      (let ((b1 (poly:quotient f a0))
	    (c1 (poly:quotient fp a0)))
	(let ((d1 (poly:- c1 (poly:derivative b1))))

	  (let lp ((a a0) (b b1) (c c1) (d d1) (as '()))
	    (if (poly:one? b)
		as
		(let ((a (poly:gcd b d)))
		  (let ((b (poly:quotient b a)))
		    (let ((c (poly:quotient d a))
			  (d (poly:- c (poly:derivative b))))

		      (lp a b c d
			  (if (poly:one? a)
			      as
			      (cons a as)))))))))))))
		      


#|
(square-free-factors
 '(expt (* (+ 1 x)
	   (expt (+ 1 (expt x 2)) 3)
	   (expt (+ 1 x (expt x 2)) 4))
	2))
#|
((+ 1 (expt x 2) x)
 (+ 1 (expt x 2))
 (+ 1 x))
|#

|#