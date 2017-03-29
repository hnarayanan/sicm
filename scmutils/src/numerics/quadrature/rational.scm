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

;;; JW & GJS

(declare (usual-integrations))

;;; *INTEGRATE-N* is the number of step sizes used before aborting
;;;   to smaller intervals.  n = 10 seems to work well.

(define *INTEGRATE-N* 10)

(define (integrate-closed-closed f a b eps)
  (let ((ans (integrate-closed-finite f a b *INTEGRATE-N* eps)))
    (if (null? ans)
	(let ((m (from-neighborhood a b)))
          (+ (integrate-closed-closed f a m eps)
             (integrate-closed-closed f m b eps)))
	ans)))


(define (integrate-open-closed f a b eps)
  (let ((ans (integrate-open-finite f a b *INTEGRATE-N* eps)))
    (if (null? ans)
	(let ((m (from-neighborhood a b)))
          (+ (integrate-open-closed f a m eps)
             (integrate-closed-closed f m b eps)))
	ans)))


(define (integrate-closed-open f a b eps)
  (let ((ans (integrate-open-finite f a b *INTEGRATE-N* eps)))
    (if (null? ans)
	(let ((m (from-neighborhood a b)))
          (+ (integrate-closed-closed f a m eps)
             (integrate-closed-open f m b eps)))
	ans)))


(define (integrate-open-open f a b eps)
  (let ((ans
	 (integrate-open-finite f a b *INTEGRATE-N* eps)))
    (if (null? ans)
	(let ((m (from-neighborhood a b)))
          (+ (integrate-open-closed f a m eps)
             (integrate-closed-open f m b eps)))
	ans)))

(define integrate-open integrate-open-open)

(define (make-finite-integrator estimate-area integrator-stream)
  (define (the-finite-integrator f a b n eps)
    (if (integrate-narrow-strip? a b)
        (begin (if integrate-roundoff-wallp?
                   (write-line `(roundoff-cutoff ,a ,b)))
               (estimate-area f a b))
        (extrapolate-streams-to-zero
         (shorten-stream n (make-bs-intervals a b))
         (integrator-stream f a b)
         eps)))
  the-finite-integrator)


;;; Set this to true, if you want to see the cases where 
;;; roundoff cutoff occurs.
(define integrate-roundoff-wallp? #f)

(define *roundoff-cutoff* 1e-14)

(define (integrate-narrow-strip? a b)
  (<= (abs (- b a))
      (* *roundoff-cutoff*
         (+ (abs b) (abs a)))))


(define integrate-closed-finite
  (make-finite-integrator
   (lambda (f a b)
     (* (/ (+ (f a) (f b)) 2.0)
        (- b a)))
   (lambda (f a b)
     (merge-streams (trapezoid-stream f a b 2)
                    (trapezoid-stream f a b 3)))))

(define integrate-open-finite
  (make-finite-integrator
   (lambda (f a b)
     (* (f (/ (+ a b) 2.0))
        (- b a)))
   (lambda (f a b)
     (map-stream (second-euler-maclaurin f a b)
                 *new-bs-steps*))))

(define (trapezoid-stream f a b n0)
  (let ((steps (stream-of-iterates (lambda (x) (* 2 x)) n0))
	(first-S ((rat-trapezoid f a b) n0)))
    (let loop ((steps (stream-cdr steps)) (Sn/2 first-S))
      (let ((S
             (trapezoid-using-previous-sum f a b Sn/2 (stream-car steps))))
	(cons-stream S (loop (stream-cdr steps) S))))))

(define ((rat-trapezoid f a b) n)
  (let ((h (flo:/ (flo:- b a) (exact->inexact n))))
    (let ((fx (lambda (i)
		(exact->inexact
                 (f (flo:+ a (flo:* (exact->inexact i) h)))))))
      (flo:* h (flo:+ (flo:/ (flo:+ (exact->inexact (f a))
				    (exact->inexact (f b)))
			     2.0)
		      (flo:sigma fx 1 (fix:- n 1)))))))

(define (trapezoid-using-previous-sum f a b Sn/2 n)
  (let ((h (flo:/ (flo:- b a) (exact->inexact n))))
    (let ((fx
	   (lambda (i)
	     (exact->inexact
	      (f (flo:+ a (flo:* (exact->inexact
                                  (fix:- (fix:+ i i) 1)) h)))))))
      (flo:+ (flo:/ Sn/2 2.0)
	     (flo:* h (flo:sigma fx 1 (quotient n 2)))))))


(define ((second-euler-maclaurin f a b) n)
  (let ((h (flo:/ (flo:- b a) (exact->inexact n))))
    (let ((h/2 (flo:/ h 2.0)))
      (let ((fx
	     (lambda (i)
	       (exact->inexact
		(f (flo:+ a (flo:+ h/2 (flo:* (exact->inexact i) h))))))))
	(flo:* h (flo:sigma fx 0 (fix:- n 1)))))))

;;; Utilities

#|
(define (flo:sigma f low high)
  (let lp ((i low) (sum 0.0))
    (if (fix:> i high)
	sum
	(lp (fix:+ i 1) (flo:+ sum (f i))))))

(define (flo:sigma-list l)  ; from small end up
  (if (null? (cdr l)) 
      (car l) 
      (flo:+ (car l) (flo:sigma-list (cdr l)))))

;;; The following uses Kahan's compensated summation trick.
|#

(define (flo:sigma f low high)
  (let lp ((i low) (sum 0.0) (c 0.0))
    (if (fix:> i high)
	sum
	(let* ((y (flo:- (f i) c)) (t (flo:+ sum y)))
	  (lp (fix:+ i 1) t (flo:- (flo:- t sum) y))))))

(define (flo:sigma-list lst)
  (let lp ((lst lst) (sum 0.0) (c 0.0))
    (if (null? lst)
        sum
        (let* ((y (flo:- (car lst) c)) (t (flo:+ sum y)))
	  (lp (cdr lst) t (flo:- (flo:- t sum) y))))))



#|
(define *new-bs-steps* 
  (merge-streams (stream-of-iterates (lambda (x) (* 2 x)) 2)
		 (stream-of-iterates (lambda (x) (* 2 x)) 3)))
|#

(define *new-bs-steps*
  (merge-streams (stream-of-iterates (lambda (x) (* 2 x)) 4)
		 (stream-of-iterates (lambda (x) (* 2 x)) 6)))

(define (make-bs-intervals a b)
  (define (rat-square x)
    (let ((fx (exact->inexact x)))
      (flo:* fx fx)))
  (map-stream (lambda (x) (rat-square (/ (- b a) x)))
	      *new-bs-steps*))

;;;----------------------------------------------------------------
;;; The following is the core of the rational interpolation,
;;;   with the zero denominator fix used by BS and Henon.

;;; Original version
(define (rational-interpolation dt c dx-list dx-new eps)
  (if (null? dt) 
      '()
      (let* ((dt1 (car dt))
	     (w (flo:- c dt1))
	     (b1 (flo:* (flo:/ (car dx-list) dx-new) dt1))
	     (den (flo:- b1 c)))
	(if (flo:= den 0.0)
	    (begin (if zd-wallp? (display "zd "))
		   (cons dt1
			 (rational-interpolation (cdr dt)
						 c
						 (cdr dx-list)
						 dx-new
						 eps)))
	    (let* ((b (flo:/ w den))
		   (new-d (flo:* c b)))
	      (cons new-d
		    (rational-interpolation (cdr dt)
					    (flo:* b1 b)
					    (cdr dx-list)
					    dx-new
					    eps)))))))
(define zd-wallp? #f)

;;;; Quadrature Routines

;;; Rational interpolation on streams is used in the quadrature code below.

(define (extrapolate-streams-to-zero x-stream y-stream eps)
  (build-tableau-streams '()
                         '()
                         x-stream
                         y-stream
                         eps
                         (stream-car y-stream)))

(define (build-tableau-streams dt dx-list x-stream y-stream eps estimate)
  (if (null? x-stream) 
      '()
      (let ((dx-new (stream-car x-stream))
	    (c (stream-car y-stream)))
	(let ((new-dt
	       (cons c (rational-interpolation dt c dx-list dx-new eps))))
	  (let ((new-estimate (flo:sigma-list new-dt)))
	    (if (and (close-enuf? new-estimate estimate eps)
		     (> (length new-dt) 2))
		new-estimate
		(build-tableau-streams new-dt (cons dx-new dx-list) 
				       (stream-cdr x-stream)
				       (stream-cdr y-stream)
				       eps
				       new-estimate)))))))

;;; To make quadrature deterministic, but sensitive to special choices
;;;  make the choice: (define *quadrature-neighborhood-width* #f)

(define *quadrature-neighborhood-width* 0.05)

(define (from-neighborhood a b)
  (if *quadrature-neighborhood-width*
      (+ a
	 (* (+ 0.5	 
	       (* (- (* 2.0 (uniform-random)) 1.0)
		  *quadrature-neighborhood-width*))
	    (- b a)))
      (* 0.5 (+ a b))))
      
#|
;;; Produces a uniformly distributed x such that 0 <= x < 1.
;;; Defined in statistics/gauss.scm

(define (uniform-random) (random 1.))
|#

