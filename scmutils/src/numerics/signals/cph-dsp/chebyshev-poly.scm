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

;;; -*-Scheme-*-
;;;
;;; $Id: chebyshev-poly.scm,v 1.1 1994/07/20 19:42:26 cph Exp $
;;;
;;; Copyright (c) 1993 Massachusetts Institute of Technology
;;;
;;; This material was developed by the Scheme project at the
;;; Massachusetts Institute of Technology, Department of Electrical
;;; Engineering and Computer Science.  Permission to copy this
;;; software, to redistribute it, and to use it for any purpose is
;;; granted, subject to the following restrictions and understandings.
;;;
;;; 1. Any copy made of this software must include this copyright
;;; notice in full.
;;;
;;; 2. Users of this software agree to make their best efforts (a) to
;;; return to the MIT Scheme project any improvements or extensions
;;; that they make, so that these may be included in future releases;
;;; and (b) to inform MIT of noteworthy uses of this software.
;;;
;;; 3. All materials developed as a consequence of the use of this
;;; software shall duly acknowledge such use, in accordance with the
;;; usual standards of acknowledging credit in academic research.
;;;
;;; 4. MIT has made no warrantee or representation that the operation
;;; of this software will be error-free, and MIT is under no
;;; obligation to provide any services, by way of maintenance, update,
;;; or otherwise.
;;;
;;; 5. In conjunction with products arising from the use of this
;;; material, there shall be no use of the name of the Massachusetts
;;; Institute of Technology nor of any adaptation thereof in any
;;; advertising, promotional, or sales literature without prior
;;; written consent from MIT in each case.

;;;; Polynomial Arithmetic for Chebyshev Polynomials

(declare (usual-integrations))

(define (chebyshev-poly order)
  (let loop ((a '(1. 0.)) (b '(1.)) (order order))
    (if (fix:= order 0)
	b
	(loop (poly/sub (poly/mul '(2. 0.) a) b)
	      a
	      (fix:- order 1)))))

#|
;; For given Chebyshev polynomials, definition below is faster.
(define (poly/horner p x)
  (if (poly/zero? p)
      0.
      (let lp ((restp (poly/except-leading-term p))
	       (value (poly/leading-coefficient p))
	       (degree (poly/degree p)))
	(if (poly/zero? restp)
	    (flo:* value (fast-expt x degree))
	    (let ((next-degree (poly/degree restp))
		  (next-coeff (poly/leading-coefficient restp)))
	      (lp (poly/except-leading-term restp)
		  (flo:+ (flo:* value (fast-expt x (fix:- degree next-degree)))
			 next-coeff)
		  next-degree))))))
|#

(define (poly/horner p x)
  (if (null? p)
      0.
      (let loop ((accum (car p)) (p (cdr p)))
	(if (null? p)
	    accum
	    (loop (flo:+ (flo:* accum x) (car p)) (cdr p))))))

(define (fast-expt b e)
  ;; Assume B a flonum, E a nonnegative fixnum, result a flonum.
  (cond ((or (flo:= b 0.)
	     (flo:= b 1.))
	 b)
	((fix:< e 4)
	 (if (fix:< e 2)
	     (if (fix:< e 1) 1. b)
	     (flo:* b (if (fix:< e 3) b (flo:* b b)))))
	(else
	 (let loop
	     ((b (flo:* b b))
	      (e (fix:quotient e 2))
	      (answer (if (fix:= (fix:remainder e 2) 0) 1. b)))
	   (if (fix:< e 4)
	       (flo:* answer
		      (if (fix:< e 2)
			  b
			  (flo:* b (if (fix:< e 3) b (flo:* b b)))))
	       (loop (flo:* b b)
		     (fix:quotient e 2)
		     (if (fix:= (fix:remainder e 2) 0)
			 answer
			 (flo:* answer b))))))))

(define (poly/add p1 p2)
  (cond ((poly/zero? p1) p2)
	((poly/zero? p2) p1)
	(else
	 (let ((degree1 (poly/degree p1))
	       (degree2 (poly/degree p2)))
	   (cond ((fix:= degree1 degree2)
		  (let ((c (coeff/add (poly/leading-coefficient p1)
				      (poly/leading-coefficient p2)))
			(s (poly/add (poly/except-leading-term p1)
				     (poly/except-leading-term p2))))
		    (if (coeff/zero? c)
			s
			(poly/adjoin degree1 c s))))
		 ((fix:> degree1 degree2)
		  (poly/adjoin degree1
			       (poly/leading-coefficient p1)
			       (poly/add (poly/except-leading-term p1) p2)))
		 (else		;(fix:< degree1 degree2)
		  (poly/adjoin degree2
			       (poly/leading-coefficient p2)
			       (poly/add p1
					 (poly/except-leading-term p2)))))))))

(define (poly/sub p1 p2)
  (cond ((poly/zero? p1) (poly/negate p2))
	((poly/zero? p2) p1)
	(else
	 (let ((degree1 (poly/degree p1))
	       (degree2 (poly/degree p2)))
	   (cond ((fix:= degree1 degree2)
		  (let ((c (coeff/sub (poly/leading-coefficient p1)
				      (poly/leading-coefficient p2)))
			(s (poly/sub (poly/except-leading-term p1)
				     (poly/except-leading-term p2))))
		    (if (coeff/zero? c)
			s
			(poly/adjoin degree1 c s))))
		 ((fix:> degree1 degree2)
		  (poly/adjoin degree1
			       (poly/leading-coefficient p1)
			       (poly/sub (poly/except-leading-term p1) p2)))
		 (else		;(fix:< degree1 degree2)
		  (poly/adjoin degree2
			       (coeff/negate (poly/leading-coefficient p2))
			       (poly/sub p1
					 (poly/except-leading-term p2)))))))))

(define (poly/negate p)
  (if (poly/zero? p)
      p
      (poly/adjoin (poly/degree p)
		   (coeff/negate (poly/leading-coefficient p))
		   (poly/negate (poly/except-leading-term p)))))

(define (poly/mul p1 p2)
  (cond ((poly/zero? p1) poly/zero)
	((poly/zero? p2) poly/zero)
	((poly/one? p1) p2)
	((poly/one? p2) p1)
	(else
	 (poly/add (poly/mul-term (poly/degree p1)
				  (poly/leading-coefficient p1)
				  p2)
		   (poly/mul (poly/except-leading-term p1) p2)))))

(define (poly/mul-term d c p)
  (let loop ((p p))
    (if (poly/zero? p)
	p
	(poly/adjoin (fix:+ d (poly/degree p))
		     (coeff/mul c (poly/leading-coefficient p))
		     (loop (poly/except-leading-term p))))))

(define-integrable poly/zero
  '())

(define-integrable (poly/zero? p)
  (null? p))

(define (poly/one? p)
  (and (pair? p)
       (flo:= 1. (car p))
       (null? (cdr p))))

(define-integrable (poly/degree p)
  (fix:- (length p) 1))

(define-integrable (poly/leading-coefficient p)
  (car p))

(define (poly/except-leading-term p)
  (if (and (not (null? (cdr p)))
	   (flo:= (cadr p) 0.))
      (poly/except-leading-term (cdr p))
      (cdr p)))

(define (poly/adjoin d c p)
  (let ((d* (length p)))
    (cond ((fix:= d d*)
	   (cons c p))
	  ((fix:> d d*)
	   (let loop ((d* (fix:+ d* 1)) (p (cons 0. p)))
	     (if (fix:= d d*)
		 (cons c p)
		 (loop (fix:+ d* 1) (cons 0. p)))))
	  (else
	   (error "Term not in order:" 'POLY/ADJOIN (list d c p))))))

(define-integrable coeff/zero? flo:zero?)
(define-integrable coeff/add flo:+)
(define-integrable coeff/sub flo:-)
(define-integrable coeff/mul flo:*)
(define-integrable coeff/negate flo:negate)