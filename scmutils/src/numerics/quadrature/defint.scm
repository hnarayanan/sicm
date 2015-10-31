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

;;;; Numerical definite integration system interface.
;;;   The integrand is a procedure that computes a function.

;;; Use: (definite-integral <integrand> <from> <to> [compile? #t/#f])
#|
(definite-integral (lambda (x) (* (exp (- x)) (log x))) 0.0 :+infinity)
;Value: -.5772156647120303

(define (foo n)
  (definite-integral (lambda (x) (expt (log (/ 1 x)) n)) 0.0 1.0))

(foo 1)
;Value: .9999999998053075

(foo 2)
;Value: 1.9999999997719113

(foo 3)
;Value: 5.999999999805274

(foo 4)
;Value: 23.999999999815316

(foo 5)
;Value: 119.99999999980271

(foo 6)
;Value: 719.9999999997759
|#

(declare (usual-integrations))

;;; Default compile integrand
(define *compile-integrand? #t)

;;; Default error specification
(define *definite-integral-allowable-error* 1.0e-11)

;;; Default ditherer off (see rational.scm)
(set! *quadrature-neighborhood-width* #f)


;;; Interface to quadrature.scm

(define (definite-integral-with-tolerance f x1 x2 tolerance)
  ((make-definite-integrator f x1 x2 tolerance)
   'integral))


;;; Assumes f is purely numerical, and no units on t1, t2

(define (definite-integral-numerical f t1 t2 tolerance compile?)
  (cond ((and (number? t1) (number? t2) (= t1 t2)) 0)
	((not compile?)
	 (definite-integral-with-tolerance f t1 t2 tolerance))
	(else
	 (definite-integral-with-tolerance (compile-procedure f)
	   t1 t2 tolerance))))

(define (definite-integral f t1 t2 #!optional tolerance compile?)
  (if (default-object? tolerance)
      (set! tolerance *definite-integral-allowable-error*))
  (if (default-object? compile?)
      (set! compile? *compile-integrand?))
  (let ((fx (f (choose-interior-point t1 t2))))
    (if (with-units? fx)		;Must extract numerical procedure
	(let* ((input-value (generate-uninterned-symbol))
	       (input-units (u:units t1))
	       (input (with-units input-value input-units))
	       (output (f input))
	       (output-units (u:units output))
	       (output-value (u:value output))
	       (integral-units (*units output-units input-units))
	       (lexp
		(text/cselim
		 `(lambda (,input-value)
		    ,(*compiler-simplifier* output-value))))
	       (nf (eval lexp scmutils-base-environment)))
	  (with-units
	      (definite-integral-numerical nf (u:value t1) (u:value t2)
		tolerance compile?)
	    integral-units))
	(definite-integral-numerical f (u:value t1) (u:value t2)
	  tolerance compile?))))

(define (choose-interior-point x1 x2)
  (assert (units:= x1 x2)
	  "Limits of integration must have same units.")
  (let ((u (u:units x1))
	(t1 (u:value x1))
	(t2 (u:value x2)))
    (cond ((and (number? t1) (number? t2))
	   (with-units (/ (+ t1 t2) 2.0) u))

	  ((and (eq? t1 :-infinity) (number? t2))
	   (with-units (- t2 1.0) u))
	  ((and (number? t1) (eq? t2 :+infinity))
	   (with-units (+ t1 1.0) u))

	  ((and (eq? t1 :+infinity) (number? t2))
	   (with-units (+ t2 1.0) u))
	  ((and (number? t1) (eq? t2 :-infinity))
	   (with-units (- t1 1.0) u))

	  ((and (eq? t1 :-infinity) (eq? t2 :+infinity))
	   (with-units 0 u))
	  ((and (eq? t1 :+infinity) (eq? t2 :-infinity))
	   (with-units 0 u))

	  (else
	   (error "No interior point: CHOOSE-INTERIOR-POINT")))))

#|
(define (definite-integral f t1 t2 #!optional epsilon compile?)
  (if (default-object? epsilon)
      (set! epsilon *definite-integral-allowable-error*))
  (if (default-object? compile?)
      (set! compile? *compile-integrand?))
  (cond ((and (number? t1) (number? t2) (= t1 t2)) 0)
	((not compile?)
	 ((make-definite-integrator f t1 t2 epsilon)
	  'integral))
	(else
	 ((make-definite-integrator (compile-procedure f) t1 t2 epsilon)
	  'integral))))
|#




