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

;;; LEGENDRE.SCM -- the Legendre Polynomials returned as a stream or singly

;;; Edited by GJS 10Jan09
(declare (usual-integrations))

;;; The following defines a stream whose nth term (0-based) is
;;; P[n]. We use the recurrence relation:
;;;      P[0](x) = 1, P[1](x) = x
;;;   and for n > 1
;;;      P[n](x) = ((2n-1)/n)*x*P[n-1](x) - ((n-1)/n)*P[n-2](x)

(define legendre-polynomials
  (cons-stream poly:one
    (cons-stream poly:identity
      (map-streams (lambda (p1 p2)
		     (let* ((n (+ (poly:degree p1) 1))
			    (a (/ (- (* 2 n) 1) n))
			    (b (/ (- n 1) n)))
		       (poly:- (poly:* (poly:scale poly:identity a) p1)
			       (poly:scale p2 b))))
		   (tail legendre-polynomials)
		   legendre-polynomials))))

(define (legendre-polynomial n)
  (stream-ref legendre-polynomials n))
