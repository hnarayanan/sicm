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

(define (unit-step #!optional place)
  (if (default-object? place) (set! place 0))
  (define (step t)
    (if (< t place) 0 1))
  step)

(define (unit-boxcar #!optional half-width place)
  (if (default-object? half-width) (set! half-width 1))
  (if (default-object? place) (set! place 0))
  (define (boxcar t)
    (if (< (abs (- t place)) half-width) 1 0))
  boxcar)

#|
;;; sgn is provided somewhere else.

(define (sgn x)
  (if (negative? x)
      -1
      1))

;;; constant is provided somewhere else.

(define ((constant x) y)
  x)
|#

(define (ramp #!optional slope place)
  (if (default-object? slope) (set! slope 1))
  (if (default-object? place) (set! place 0))
  (define (the-ramp t)
    (* slope (- t place)))
  the-ramp)


;;; peak of height 1 at x1; zero and all derivs zero at x0, x2.

(define (general-bump x0 x1 x2)			
  (let* ((a 1)
	 (b (* (square (/ (- x2 x1) (- x1 x0))) a)))
    (define (g x)
      (exp (- (+ (/ a (- x x0)) (/ b (- x2 x))))))
    (let ((g1 (g x1)))
      (define (h x)
	(cond ((<= x x0) 0)
	      ((<= x2 x) 0)
	      (else (/ (g x) g1)))) 
      h)))

(define (sigfun:unit-impulse span)
  (let* ((minx (sigfun:min span))
	 (maxx (sigfun:max span))
	 (period (- maxx minx))
	 (height (/ *nsamples* period)))
    (sigfun:make (circular-interpolate
		  (generate-list *nsamples*
				 (lambda (i)
				   (let ((x (fix:- i (quotient *nsamples* 2))))
				     (if (fix:= 0 x)
					 height
					 0))))
		  span)
		 span)))


(define (sigfun:impulse-train spacing span)
  (let* ((minx (sigfun:min span))
	 (maxx (sigfun:max span))
	 (period (- maxx minx))
	 (height (/ *nsamples* period))
	 (space (* height spacing))
	 (ispace (round->exact space)))
    (assert (= (- space ispace) 0))
    (sigfun:make (circular-interpolate
		  (generate-list *nsamples*
				 (lambda (i)
				   (let ((x (fix:- i (quotient *nsamples* 2))))
				     (if (fix:= 0 (modulo x ispace))
					 height
					 0))))
		  span)
		 span)))

(define (evenly-divides? x y)
  (= (* (round (/ y x)) x) y))