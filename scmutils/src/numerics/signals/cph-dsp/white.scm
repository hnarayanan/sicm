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
;;; $Id: white.scm,v 1.1 1994/07/20 19:42:43 cph Exp $
;;;
;;; Copyright (c) 1993-94 Massachusetts Institute of Technology
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

;;;; White Noise Generation

(declare (usual-integrations))

(define (make-noise-vector method state length)
  (let ((v (flo:vector-cons length)))
    (do ((i 0 (fix:+ i 1)))
	((fix:= i length))
      (flo:vector-set! v i (method state)))
    v))

(define (make-uniform-noise-vector state length)
  (let ((v (flo:vector-cons length)))
    (do ((i 0 (fix:+ i 1)))
	((fix:= i length))
      (flo:vector-set! v i (flo:- (flo:random-unit state) .5)))
    v))

(define (make-gaussian-noise-vector:polar-method state length)
  (let ((length (if (odd? length) (+ length 1) length)))
    (let ((v (flo:vector-cons length))
	  (t (flo:vector-cons 3)))
      (let ((x1 (lambda () (flo:vector-ref t 0)))
	    (set-x1! (lambda (x) (flo:vector-set! t 0 x)))
	    (x2 (lambda () (flo:vector-ref t 1)))
	    (set-x2! (lambda (x) (flo:vector-set! t 1 x)))
	    (s (lambda () (flo:vector-ref t 2)))
	    (set-s! (lambda (x) (flo:vector-set! t 2 x))))
	(declare (integrate-operator x1 set-x1! x2 set-x2! s set-s!))
	(do ((i 0 (fix:+ i 2)))
	    ((fix:= i length))
	  (let loop ()
	    (set-x1! (flo:random-unit state))
	    (set-x1! (flo:- (flo:+ (x1) (x1)) 1.))
	    (set-x2! (flo:random-unit state))
	    (set-x2! (flo:- (flo:+ (x2) (x2)) 1.))
	    (set-s!  (flo:+ (flo:* (x1) (x1)) (flo:* (x2) (x2))))
	    (if (flo:< (s) 1.)
		(begin
		  (set-s! (flo:sqrt (flo:/ (flo:* -2. (flo:log (s))) (s))))
		  (flo:vector-set! v i (flo:* (x1) (s)))
		  (flo:vector-set! v (fix:+ i 1) (flo:* (x2) (s))))
		(loop)))))
      v)))

(define (marsaglia-maclaren-method f f-limits h v)
  ;; F is the (1-sided) normalized probability density function.
  ;; F-LIMITS is special; see Knuth text and code below.
  ;; H is the number of divisions per horizontal unit.
  ;; V is the number of divisions per vertical unit.
  ;; Assumptions:
  ;; (1) F is defined for non-negative reals.
  ;; (2) F is positive and monotonically non-increasing.
  ;; (3) The total area under F is 1.
  ;; (4) Nearly all of F's area occurs between 0 and 3.
  ;; (5) H and V are exact powers of two.
  ;; **** The procedure MM-WORST-CASE is specifically for the Gaussian PDF.
  (call-with-values (lambda () (mm-tables f f-limits h v))
    (lambda (aj sj pj qj dj bj fj+6h)
      (let ((h*3 (* h 3))
	    (hv (exact->inexact (* h v)))
	    (h (exact->inexact h)))
	(lambda (state)
	  (let ((body
		 (lambda (U)
		   (let ((a
			  (flo:vector-ref aj
					  (flo:truncate->exact (flo:* U hv)))))
		     (if (flo:< a 0.)
			 (let loop ((j 0))
			   (cond ((fix:= j h*3)
				  (mm-worst-case state))
				 ((flo:< U (flo:vector-ref pj j))
				  (if (flo:< U (flo:vector-ref qj j))
				      (flo:+ (flo:vector-ref sj j)
					     (flo:/ (flo:random-unit state) h))
				      (mm-algorithm-L state
						      h
						      (flo:vector-ref sj j)
						      (flo:vector-ref dj j)
						      (flo:vector-ref bj j)
						      (vector-ref fj+6h j))))
				 (else
				  (loop (fix:+ j 1)))))
			 (flo:+ a (flo:/ (flo:random-unit state) h)))))))
	    (let ((U (flo:random-unit state)))
	      (if (flo:< U .5)
		  (body (flo:* U 2.))
		  (flo:negate (body (flo:* (flo:- U .5) 2.)))))))))))

(define (mm-algorithm-L state h s a/b b f)
  (let loop ()
    (call-with-values
	(lambda ()
	  (let ((U (flo:random-unit state))
		(V (flo:random-unit state)))
	    (if (flo:< V U)
		(values V U)
		(values U V))))
      (lambda (U V)
	(let ((X (flo:+ s (flo:/ U h))))
	  (if (and (flo:> V a/b)
		   (flo:> V (flo:+ U (flo:/ (f X) b))))
	      (loop)
	      X))))))

(define (mm-worst-case state)
  (let loop ()
    (let ((U (flo:random-unit state))
	  (V (flo:random-unit state)))
      (let ((W (flo:+ (flo:* U U) (flo:* V V))))
	(if (flo:< W 1.)
	    (let ((T (flo:sqrt (flo:/ (flo:- 9. (flo:* 2. (flo:log W))) W))))
	      (let ((X (flo:* U T)))
		(if (flo:< X 3.)
		    (let ((X (flo:* V T)))
		      (if (flo:< X 3.)
			  (loop)
			  X))
		    X)))
	    (loop))))))

(define (mm-tables f f-limits h v)
  (let ((pj (mm-pj f h v)))
    (let ((pj+6h (mm-pj+6h f h)))
      (call-with-values (lambda () (mm-abj f f-limits h pj+6h))
	(lambda (aj bj)
	  (let ((*pj (mm-*pj h pj (mm-pj+3h f h pj) pj+6h)))
	    (values (vector->flonum-vector (mm-large-table h v pj))
		    (vector->flonum-vector (mm-sj h))
		    (vector->flonum-vector *pj)
		    (vector->flonum-vector (mm-qj h *pj pj+6h))
		    (vector->flonum-vector (mm-dj aj bj))
		    (vector->flonum-vector bj)
		    (mm-fj+6h f h pj+6h))))))))

(define (mm-large-table h v pj)
  (let ((h*3 (* h 3))
	(hv (* h v)))
    (let ((table (make-vector hv -1)))
      (let loop ((j 0) (i 0))
	(if (not (= j h*3))
	    (let ((i* (+ i (* (vector-ref pj j) hv))))
	      (subvector-fill! table i i* (/ j h))
	      (loop (+ j 1) i*))))
      table)))

(define (mm-sj h)
  (make-initialized-vector (+ (* h 3) 1)
    (lambda (j)
      (/ j h))))

(define (mm-pj f h v)
  (make-initialized-vector (* h 3)
    (let ((hv (* h v)))
      (lambda (j)
	(/ (truncate->exact (* v (f (/ (+ j 1) h)))) hv)))))

(define (mm-pj+3h f h pj)
  (make-initialized-vector (* h 3)
    (lambda (j)
      (- (/ (f (/ (+ j 1) h)) h)
	 (vector-ref pj j)))))

(define (mm-pj+6h f h)
  (make-initialized-vector (* h 3)
    (lambda (j)
      (let ((xu (/ (+ j 1) h)))
	(- (simpsons-rule f (/ j h) xu 128)
	   (/ (f xu) h))))))

(define (mm-abj f f-limits h pj+6h)
  (let ((h*3 (* h 3)))
    (let ((aj (make-vector h*3))
	  (bj (make-vector h*3)))
      (do ((j 0 (+ j 1)))
	  ((= j h*3))
	(let ((xu (/ (+ j 1) h)))
	  (call-with-values (lambda () (f-limits (/ j h) xu))
	    (lambda (a b)
	      (let ((fxu (f xu))
		    (p (vector-ref pj+6h j)))
		(vector-set! aj j (/ (- a fxu) p))
		(vector-set! bj j (/ (- b fxu) p)))))))
      (values aj bj))))

(define (mm-dj aj bj)
  (make-initialized-vector (vector-length aj)
    (lambda (j)
      (/ (vector-ref aj j) (vector-ref bj j)))))

(define (mm-*pj h pj pj+3h pj+6h)
  (let ((h*3 (* h 3)))
    (let ((*pj (make-vector (+ h*3 1))))
      (do ((j 0 (+ j 1)))
	  ((= j h*3))
	(vector-set! *pj j
		     (+ (if (= j 0)
			    (do ((j 0 (+ j 1))
				 (p 0 (+ p (vector-ref pj j))))
				((= j h*3) p))
			    (vector-ref *pj (- j 1)))
			(vector-ref pj+3h j)
			(vector-ref pj+6h j))))
      (vector-set! *pj h*3 1)
      *pj)))

(define (mm-qj h *pj pj+6h)
  (make-initialized-vector (* h 3)
    (lambda (j)
      (- (vector-ref *pj j) (vector-ref pj+6h j)))))

(define (mm-fj+6h f h pj+6h)
  (make-initialized-vector (* h 3)
    (lambda (j)
      (let ((base (exact->inexact (f (/ (+ j 1) h))))
	    (scale (exact->inexact (vector-ref pj+6h j))))
	(lambda (x)
	  (flo:/ (flo:- (f x) base) scale))))))

(define (simpsons-rule f a b n/2)
  (let ((b-a (- b a))
	(n (* 2 n/2)))
    (let ((x
	   (lambda (i)
	     (+ a (* (/ i n) b-a)))))
      (let loop ((i 1) (sum (f a)))
	(let ((i (+ i 1))
	      (sum (+ sum (* 4 (f (x i))))))
	  (if (= i n)
	      (/ (* (/ b-a n) (+ sum (f b))) 3)
	      (loop (+ i 1)
		    (+ sum (* 2 (f (x i)))))))))))

(define one-sided-unit-gaussian-pdf
  (let ((sqrt-pi/2 (sqrt (* 2 (atan 1 1)))))
    (lambda (x)
      (/ (exp (/ (* x x) -2))
	 sqrt-pi/2))))

(define (one-sided-unit-gaussian-pdf-limits xl xu)
  (let ((xd (- xu xl))
	(fxl (one-sided-unit-gaussian-pdf xl))
	(fxu (one-sided-unit-gaussian-pdf xu)))
    (if (<= xu 1)
	(values fxl (* (+ 1 (* xd xu)) fxu))
	(values
	 (let ((y (/ (- fxu fxl) xd)))
	   (let ((x
		  (let ((f
			 (lambda (x)
			   (* (- x) (one-sided-unit-gaussian-pdf x)))))
		    (let ((limit (* (abs y) 1e-6))
			  (d0 (/ xd 2)))
		      (let loop ((x (+ xl d0)) (d (/ d0 2)))
			(let ((fx (f x)))
			  (cond ((< (abs (- fx y)) limit)
				 x)
				((< fx y)
				 (if (>= x xu) (error "Can't find root."))
				 (loop (+ x d) (/ d 2)))
				(else
				 (if (<= x xl) (error "Can't find root."))
				 (loop (- x d) (/ d 2))))))))))
	     (- (one-sided-unit-gaussian-pdf x)
		(* y (- x xl)))))
	 fxl))))