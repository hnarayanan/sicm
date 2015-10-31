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

;;; JW

(declare (usual-integrations))

;;; To find the limit of (f x) as x goes to zero, rationally extrapolated 
;;;  from the given x-list.

(define (extrapolate-function-to-zero f x-list eps)
  (build-tableau-f '() '() f x-list eps 666.0))	; a beastly estimate.

(define (build-tableau-f dt dx-list f x-list eps estimate)
  (if (null? x-list) 
      (error "RATIONAL-FUNCTIONS: INACCURATE" estimate)
      (let ((dx-new (car x-list)))
	(let ((c (f dx-new)))
	  (let ((new-dt
		 (cons c (rational-interpolation dt c dx-list dx-new eps))))
	    (let ((new-estimate (sum-list-flo new-dt)))
	      (if (and (close-enuf? new-estimate estimate eps)
		       (> (length new-dt) 2))
		  (list new-estimate (length dt))
		  (build-tableau-f new-dt (cons dx-new dx-list) 
				   f (cdr x-list) eps new-estimate))))))))


;;;----------------------------------------------------------------
;;; Rational interpolation on a pair of lists

(define ((rational-function-interpolation x-list y-list eps) x)
  (let ((data (map (lambda (xl y) (cons (- x xl) y)) x-list y-list)))
    (let ((sdata (sort data (lambda (a b) (< (abs (car a)) (abs (car b)))))))
      (let ((cd (car sdata)))
	(let ((sdx (map car sdata))
	      (sdy (map cdr sdata)))
	  (if (= (car cd) 0.0)
	      (cdr cd)
	      (build-tableau-lists '() '() sdx sdy eps (cdr cd))))))))


(define (build-tableau-lists dt dx-list x-list y-list eps estimate)
  (if (null? x-list) 
      (error "RATIONAL-FUNCTIONS: INACCURATE" estimate)
      (let ((dx-new (car x-list))
	    (c (car y-list)))
	(let ((new-dt
	       (cons c (rational-interpolation dt c dx-list dx-new eps))))
	  (let ((new-estimate (sum-list-flo new-dt)))
	    (if (and (close-enuf? new-estimate estimate eps)
		     (> (length new-dt) 2))
		new-estimate
		(build-tableau-lists new-dt
				     (cons dx-new dx-list) 
				     (cdr x-list) (cdr y-list)
				     eps
				     new-estimate)))))))

(define (sum-list-flo l)  ; from small end up
  (if (null? (cdr l)) 
      (car l) 
      (flo:+ (car l) (sum-list-flo (cdr l)))))

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

#|
Date: Sun, 30 Nov 2008 16:47:59 -0500
From: Jack Wisdom <wisdom@MIT.EDU>
To: gjs@mit.edu
Subject: quadrature

The problem seems to occur when the rational-interpolation
gets a zero denominator.  Try setting zd-wallp? to #t.

I do not yet fully understand it, but if we quit the
rational-interpolation instead of using the "zero denominator fix of
BS and Henon", then the quadrature seems to work, in this case.

So replace rational-interpolation with

(define (rational-interpolation dt c dx-list dx-new eps)
  (if (null? dt)
      '()
      (let* ((dt1 (car dt))
	     (w (flo:- c dt1))
	     (b1 (flo:* (flo:/ (car dx-list) dx-new) dt1))
	     (den (flo:- b1 c)))
	(if (flo:= den 0.0)
	    '()
	    (let* ((b (flo:/ w den))
		   (new-d (flo:* c b)))
	      (cons new-d
		    (rational-interpolation (cdr dt)
					    (flo:* b1 b)
					    (cdr dx-list)
					    dx-new
					    eps)))))))

I'll keep looking at it.
|#

;;; Utilities

#|
(define (sigma-flo f low high)
  (let lp ((i low) (sum 0.0))
    (if (fix:> i high)
	sum
	(lp (fix:+ i 1) (flo:+ sum (f i))))))

;;; The following uses Kahan's compensated summation trick.
|#

(define (sigma-flo f low high)
  (let lp ((i low) (sum 0.0) (c 0.0))
    (if (fix:> i high)
	sum
	(let* ((y (flo:- (f i) c)) (t (flo:+ sum y)))
	  (lp (fix:+ i 1) t (flo:- (flo:- t sum) y))))))

(define (rat-square x)
  (let ((fx (exact->inexact x)))
    (flo:* fx fx)))

#|
(define *new-bs-steps* 
  (merge-streams (stream-of-iterates (lambda (x) (* 2 x)) 2)
		 (stream-of-iterates (lambda (x) (* 2 x)) 3)))
|#

(define *new-bs-steps*
  (merge-streams (stream-of-iterates (lambda (x) (* 2 x)) 4)
		 (stream-of-iterates (lambda (x) (* 2 x)) 6)))

(define (make-bs-intervals a b)
  (map-stream (lambda (x) (rat-square (/ (- b a) x)))
	      *new-bs-steps*))

;;;; Quadrature Routines

;;; Rational interpolation on streams is used in the quadrature code below.

(define (extrapolate-streams-to-zero x-stream y-stream eps)
  (build-tableau-streams '() '() x-stream y-stream eps (stream-car y-stream)))

(define (build-tableau-streams dt dx-list x-stream y-stream eps estimate)
  (if (null? x-stream) 
      '()
      (let ((dx-new (stream-car x-stream))
	    (c (stream-car y-stream)))
	(let ((new-dt
	       (cons c (rational-interpolation dt c dx-list dx-new eps))))
	  (let ((new-estimate (sum-list-flo new-dt)))
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


;;; *INTEGRATE-N* is the number of step sizes used before aborting
;;;   to smaller intervals.  n = 10 seems to work well.
(define *INTEGRATE-N* 10)

(define (integrate-closed-closed f a b eps)
  (let ((m (from-neighborhood a b)))
    (+ (integrate-closed-closed-1 f a m eps)
       (integrate-closed-closed-1 f m b eps))))

(define (integrate-closed-closed-1 f a b eps)
  (let ((ans
	 (integrate-closed-finite f a b *INTEGRATE-N* eps)))
    (if (null? ans)
	(integrate-closed-closed f a b eps)
	ans)))


(define (integrate-open-closed f a b eps)
  (let ((m (from-neighborhood a b)))
    (+ (integrate-open-closed-1 f a m eps)
       (integrate-closed-closed-1 f m b eps))))

(define (integrate-open-closed-1 f a b eps)
  (let ((ans
	 (integrate-open-finite f a b *INTEGRATE-N* eps)))
    (if (null? ans)
	(integrate-open-closed f a b eps)
	ans)))


(define (integrate-closed-open f a b eps)
  (let ((m (from-neighborhood a b)))
    (+ (integrate-closed-closed-1 f a m eps)
       (integrate-closed-open-1 f m b eps))))

(define (integrate-closed-open-1 f a b eps)
  (let ((ans
	 (integrate-open-finite f a b *INTEGRATE-N* eps)))
    (if (null? ans)
	(integrate-closed-open f a b eps)
	ans)))


(define (integrate-open-open f a b eps)
  (let* ((m (from-neighborhood a b)))
    (+ (integrate-open-closed-1 f a m eps)
       (integrate-closed-open-1 f m b eps))))

(define (integrate-open-open-1 f a b eps)
  (let ((ans
	 (integrate-open-finite f a b *INTEGRATE-N* eps)))
    (if (null? ans)
	(integrate-open-open f a b eps)
	ans)))

(define integrate-open integrate-open-open-1)

(define *roundoff-cutoff* 1e-14)


;;; Set this to true, if you want to see the cases where roundoff cutoff occurs.

(define integrate-roundoff-wallp? #f)

(define (integrate-closed-finite f a b n eps)
  (if (<= (abs (- b a))
	  (* *roundoff-cutoff*
	     (+ (abs b) (abs a))))
      (begin (if integrate-roundoff-wallp?
		 (write-line `(roundoff-cutoff ,a ,b)))
	     (* (/ (+ (f a) (f b)) 2.0) (- b a)))
      (extrapolate-streams-to-zero
       (shorten-stream n (make-bs-intervals a b))
       (merge-streams (trapezoid-stream f a b 2)
		      (trapezoid-stream f a b 3))
       eps)))

(define (integrate-open-finite f a b n eps)
  (if (<= (abs (- b a))
	  (* *roundoff-cutoff*
	     (+ (abs b) (abs a))))
      (begin (if integrate-roundoff-wallp?
		 (write-line `(roundoff-cutoff ,a ,b)))
	     (* (f (/ (+ a b) 2.0)) (- b a)))
      (extrapolate-streams-to-zero
       (shorten-stream n (make-bs-intervals a b))
       (map-stream (second-euler-maclaurin f a b)
		   *new-bs-steps*)
       eps)))

(define (trapezoid-stream f a b n0)
  (let ((steps (stream-of-iterates (lambda (x) (* 2 x)) n0))
	(first-S ((rat-trapezoid f a b) n0)))
    (let loop ((steps (stream-cdr steps)) (Sn/2 first-S))
      (let ((S (trapezoid-using-previous-sum f a b Sn/2 (stream-car steps))))
	(cons-stream S (loop (stream-cdr steps) S))))))

(define ((rat-trapezoid f a b) n)
  (let ((h (flo:/ (flo:- b a) (exact->inexact n))))
    (let ((fx (lambda (i)
		(exact->inexact (f (flo:+ a (flo:* (exact->inexact i) h)))))))
      (flo:* h (flo:+ (flo:/ (flo:+ (exact->inexact (f a))
				    (exact->inexact (f b)))
			     2.0)
		      (sigma-flo fx 1 (fix:- n 1)))))))

(define (trapezoid-using-previous-sum f a b Sn/2 n)
  (let ((h (flo:/ (flo:- b a) (exact->inexact n))))
    (let ((fx
	   (lambda (i)
	     (exact->inexact
	      (f (flo:+ a (flo:* (exact->inexact (fix:- (fix:+ i i) 1)) h)))))))
      (flo:+ (flo:/ Sn/2 2.0)
	     (flo:* h (sigma-flo fx 1 (quotient n 2)))))))


(define ((second-euler-maclaurin f a b) n)
  (let ((h (flo:/ (flo:- b a) (exact->inexact n))))
    (let ((h/2 (flo:/ h 2.0)))
      (let ((fx
	     (lambda (i)
	       (exact->inexact
		(f (flo:+ a (flo:+ h/2 (flo:* (exact->inexact i) h))))))))
	(flo:* h (sigma-flo fx 0 (fix:- n 1)))))))

#|
;;; ************ All text below this point is comments ************

;;; Other plans

;;; same as Romberg, but with rational extrapolation.
(define (integrate-rtrap f a b eps steps) 
  (extrapolate-streams-to-zero
   (map-stream (lambda (x) (rat-square (/ (- b a) x))) steps)
   (trapezoid-stream f a b (stream-car steps))
   eps))

(define (general-integrate method steps f a b eps)
  (extrapolate-streams-to-zero
   (map-stream (lambda (x) (rat-square (/ (- b a) x)))
	       steps)
   (map-stream (method f a b) steps)
   eps))


(define (integrate-inf f a b eps)
  (extrapolate-streams-to-zero
   (map-stream (lambda (x) (rat-square (/ (- b a) x)))
	       *new-bs-steps*)
   (merge-streams (trapezoid-stream f a b 2)
		  (trapezoid-stream f a b 3))
   eps))

(define (integrate-open-inf f a b eps)
  (extrapolate-streams-to-zero
   (map-stream (lambda (x) (rat-square (/ (- b a) x)))
	       *new-bs-steps*)
   (map-stream (second-euler-maclaurin f a b)
	       *new-bs-steps*)
   eps))
|#

#|
;;;;quadrature samples 

(define count)

(set! count 0)

(define (fc x)
  (set! count (+ count 1))
  (if (= x 0.0) 1.0 (/ (sin x) x)))

(integrate-open-open fc 0.0 pi 1.0e-15)
;Value: 1.8519370519824663
(set! count 0)
;Value: 70

(define (xx x)
  (set! count (fix:+ count 1))
  (if (flo:= x 0.0) 0.0 (flo:expt x x)))

(integrate-closed-closed xx 0.0 1.0 1.0e-10)
;Value: .7834305105286976

(set! count 0)
;Value: 7792

(integrate-open-open xx 0.0 1.0 1.0e-10)
;Value: .7834305106529134

(set! count 0)
;Value: 1897

(define (xsin1/x x)
  (set! count (+ count 1))
  (if (= x 0.0) 
      0.0
      (* x (sin (/ 1 x)))))

(integrate-closed-closed xsin1/x 0.1 1.0 1.0e-10)
;Value: .3794280654186952
(set! count 0)
;Value: 568

(integrate-open-open xsin1/x 0.1 1.0 1.0e-10)
;Value: .3794280654199685
(set! count 0)
;Value: 680

(romberg-quadrature xsin1/x 0.1 1.0 1.0e-10)
;Value: .3794280654204438
(set! count 0)
;Value: 1025

(integrate-open-closed xsin1/x 0.05 1.0 1.0e-10)
;Value: .3784640968911057
(set! count 0)
;Value: 980

(integrate-closed-closed xsin1/x 0.05 1.0 1.0e-10)
;Value: .3784640968910794
(set! count 0)
;Value: 932

(romberg-quadrature xsin1/x 0.05 1.0 1.0e-10)
;Value: .37846409689114535
(set! count 0)
;Value: 4097
|#

#|
;;;; bulirsch-stoer integrator
;;;   modified midpoint takes vector state -> vector state

(define-integrable (modified-midpoint n f state htot)
  (lambda (nsteps)
    (let ((2*nsteps (fix:* 2 nsteps)))
      (let ((h (flo:/ htot (exact->inexact 2*nsteps))))
	(let ((2h (flo:* 2.0 h))
	      (f0 (f state)))
	  (let loop 
	      ((m (fix:- 2*nsteps 1))
	       (zm-1 state) 
	       (zm (make-initialized-vector n
		    (lambda (i)
		      (flo:+ (vector-ref state i)
			     (flo:* h (vector-ref f0 i)))))))
	    (let ((fn (f zm)))
	      (if (fix:= m 0)
		  (make-initialized-vector 
		   n
		   (lambda (i) 
		     (flo:* 0.5 
			    (flo:+ (vector-ref zm i)
				   (flo:+ (vector-ref zm-1 i)
					  (flo:* h
						 (vector-ref fn i)))))))
		  (loop (fix:-1+ m) 
			zm 
			(make-initialized-vector 
			 n 
			 (lambda (i) 
			   (flo:+ (vector-ref zm-1 i)
				  (flo:* 2h (vector-ref fn i))))))))))))))

#|
(define (kepler s)
  (let ((x (vector-ref s 0))
	(y (vector-ref s 1)))
    (let ((radius^3 (flo:expt (flo:+ (flo:* x x) (flo:* y y)) 1.5)))
      (vector 
       (vector-ref s 2) 
       (vector-ref s 3) 
       (flo:negate (flo:/ x radius^3))
       (flo:negate (flo:/ y radius^3))))))
|#

#| this one uses continuations rather than vectors 
   about as fast as following one 24 sec compared to 23 for the
     next inline coded one
   generic vector routine with define-integrable is about 30 sec
   C code is about 10 sec

(define-integrable (modified-midpoint n f state htot)
  (if (not (fix:= n 4)) (error "only n=4 for this midpoint integrator"))
  (let ((s.0 (vector-ref state 0))
	(s.1 (vector-ref state 1))
	(s.2 (vector-ref state 2))
	(s.3 (vector-ref state 3)))
    (lambda (nsteps)
      (let ((2*nsteps (fix:* 2 nsteps)))
	(let ((h (flo:/ htot (exact->inexact 2*nsteps))))
	  (let ((2h (flo:* 2.0 h)))
	    (f (lambda (d.0 d.1 d.2 d.3) 
		 (midpoint-steps (fix:- 2*nsteps 1)
				 f h 2h
				 s.0 s.1 s.2 s.3
				 (flo:+ s.0 (flo:* h d.0)) ; first half step
				 (flo:+ s.1 (flo:* h d.1))
				 (flo:+ s.2 (flo:* h d.2))
				 (flo:+ s.3 (flo:* h d.3))))
	       s.0 s.1 s.2 s.3)))))))

(define (midpoint-steps m f h 2h zm-1.0 zm-1.1 zm-1.2 zm-1.3 zm.0 zm.1 zm.2 zm.3)
  (if (fix:= m 0)
      (f (lambda (d.0 d.1 d.2 d.3)
	   (vector
	    (flo:* 0.5 (flo:+ zm.0 (flo:+ zm-1.0 (flo:* h d.0))))  ; last half step
	    (flo:* 0.5 (flo:+ zm.1 (flo:+ zm-1.1 (flo:* h d.1))))
	    (flo:* 0.5 (flo:+ zm.2 (flo:+ zm-1.2 (flo:* h d.2))))
	    (flo:* 0.5 (flo:+ zm.3 (flo:+ zm-1.3 (flo:* h d.3))))))
	 zm.0 zm.1 zm.2 zm.3)
      (f (lambda (d.0 d.1 d.2 d.3)
	   (midpoint-steps (fix:- m 1) f h 2h
			   zm.0 zm.1 zm.2 zm.3
			   (flo:+ zm-1.0 (flo:* 2h d.0))   ; middle steps
			   (flo:+ zm-1.1 (flo:* 2h d.1))
			   (flo:+ zm-1.2 (flo:* 2h d.2))
			   (flo:+ zm-1.3 (flo:* 2h d.3))))
	 zm.0 zm.1 zm.2 zm.3)))

(define (f-kepler c s.0 s.1 s.2 s.3)
  (let ((radius^3 (flo:expt (flo:+ (flo:* s.0 s.0) (flo:* s.1 s.1)) 1.5)))
    (c s.2 
       s.3 
       (flo:negate (flo:/ s.0 radius^3))
       (flo:negate (flo:/ s.1 radius^3)))))

;;; this is the fastest so far 
(define (modified-midpoint-kepler n f state htot)
  (let ((s.0 (vector-ref state 0))
	(s.1 (vector-ref state 1))
	(s.2 (vector-ref state 2))
	(s.3 (vector-ref state 3)))
    (let ((radius^3 (flo:expt (flo:+ (flo:* s.0 s.0) (flo:* s.1 s.1)) 1.5)))
      (lambda (nsteps)
	(let ((2*nsteps (fix:* 2 nsteps)))
	  (let ((h (flo:/ htot (exact->inexact 2*nsteps))))
	    (let ((2h (flo:* 2.0 h)))
	      (let loop 
		  ((m (fix:- 2*nsteps 1))
		   (zm-1.0 s.0) 
		   (zm-1.1 s.1) 
		   (zm-1.2 s.2) 
		   (zm-1.3 s.3)
		   (zm.0 (flo:+ s.0 (flo:* h s.2)))
		   (zm.1 (flo:+ s.1 (flo:* h s.3)))
		   (zm.2 (flo:+ s.2 (flo:* h (flo:negate (flo:/ s.0 radius^3)))))
		   (zm.3 (flo:+ s.3 (flo:* h (flo:negate (flo:/ s.1 radius^3))))))
		(let ((radius^3
		       (flo:expt (flo:+ (flo:* zm.0 zm.0) (flo:* zm.1 zm.1)) 1.5)))
		  (if (fix:= m 0)
		      (vector 
		       (flo:* 0.5 (flo:+ zm.0 (flo:+ zm-1.0 (flo:* h zm.2))))
		       (flo:* 0.5 (flo:+ zm.1 (flo:+ zm-1.1 (flo:* h zm.3))))
		       (flo:* 0.5
			      (flo:+ zm.2
				     (flo:+ zm-1.2
					    (flo:* h
						   (flo:negate (flo:/ zm.0
								      radius^3))))))
		       (flo:* 0.5
			      (flo:+ zm.3
				     (flo:+ zm-1.3
					    (flo:* h
						   (flo:negate (flo:/ zm.1
								      radius^3)))))))
		      (loop (fix:-1+ m) 
			    zm.0 zm.1 zm.2 zm.3
			    (flo:+ zm-1.0 (flo:* 2h zm.2))
			    (flo:+ zm-1.1 (flo:* 2h zm.3))
			    (flo:+ zm-1.2
				   (flo:* 2h
					  (flo:negate (flo:/ zm.0
							     radius^3))))
			    (flo:+ zm-1.3
				   (flo:* 2h
					  (flo:negate (flo:/ zm.1
							     radius^3)))))))))))))))
|#

;;; (define (rat-next-h h m)
;;;  (* h 1.5 (if (< m 7) 1 (expt 0.6 (- m 6)))))

;;; STEP-SIZE-N is related to the desired level of rational tableau (equal?)
(define STEP-SIZE-N 9) 

(define (rat-next-h h m)
  (if bs-print? (write-line (list h m)))
  (if (not (fix:> m STEP-SIZE-N))
      (flo:* h 1.5)
      (flo:* h (flo:* 1.5 (flo:expt 0.6 (exact->inexact (- m STEP-SIZE-N)))))))

(define (bulirsch-stoer-step n f eps t.htot.state)
  (let ((t (car t.htot.state))
	(htot (cadr t.htot.state))
	(state (caddr t.htot.state)))
    (if bs-print? (begin (write-line 'state) (write-line t.htot.state)))
    (let ((m (modified-midpoint n f state htot))
	  (x-list (list 1 2 3 4 6 8 12 16 24 32 48 64)))
      (let ((ans (extrapolate-vector-function-to-zero-2 n m x-list eps)))
	(let ((ms (map tableau-data->table-length ans)))
	  (if (null? ans)
	      (bulirsch-stoer-step n f eps (list t (flo:/ htot 2.0) state))
	      (list (flo:+ t htot) 
		    (rat-next-h htot (apply max ms))
		    (list->vector (map tableau-data->estimate ans)))))))))

(define (advance-state n f t.dt.state target eps monitor)
  (let loop ((t.dt.s t.dt.state))
    (monitor t.dt.s)
    (let ((t (car t.dt.s)))
      (if (flo:< (flo:abs (flo:- target t)) eps)
	  t.dt.s
	  (let ((dt (cadr t.dt.s))
		(state (caddr t.dt.s)))
	    (if (< (apply + (map abs (cdr (vector->list state))))
		   1.0e-3)
		(bkpt "foo"))
	    (loop
	     (bulirsch-stoer-step
	      n f eps 
	      (list t
		    (flo:* dt
			   (min 1.0
				(flo:/ (flo:- target t)
				       dt)))
		    state))))))))

;;;; extrapolation of vector valued functions (elementwise unfortunately)

(define *huge* 1.0e30)
(define bs-print? false)

;;; tableau-data is a list of estimate error table-length dt
(define make-tableau-data list)
(define tableau-data->estimate car)
(define tableau-data->error cadr)
(define tableau-data->table-length caddr)
(define tableau-data->tableau cadddr)

(define (extrapolate-vector-function-to-zero n f x-list eps)
  (let ((tableaus
	 (make-list n (make-tableau-data *huge* *huge* false '())))
	(step->dx (lambda (x) (exact->inexact x))))
    (build-tableau-vector-f step->dx tableaus '() f x-list eps)))

;;; assumes f(x) = g(1/x^2)
(define (extrapolate-vector-function-to-zero-2 n f step-list eps)
  (let ((tableaus
	 (make-list n (make-tableau-data *huge* *huge* false '())))
	(step->dx (lambda (x) 
		    (let ((xf (exact->inexact x))) 
		      (flo:/ 1.0 (flo:* xf xf))))))
    (build-tableau-vector-f step->dx tableaus '() f step-list eps)))

(define (build-tableau-vector-f step->dx tableaus dx-list f step-list eps)
  (if (null? step-list) 
      '()
      (let ((step-new (car step-list)))
	(let ((c (vector->list (f step-new))))
	  (let ((dx-new (step->dx step-new)))
	    (let ((new-tableaus
		   (map (individual-extrapolation dx-list dx-new eps)
			c
			tableaus)))
	      (if bs-print? (write-line (list 'new-tableaus new-tableaus)))
	      (cond ((all-done? new-tableaus) new-tableaus)
		    ((diverging? tableaus new-tableaus) '())
		    (else
		     (build-tableau-vector-f 
		      step->dx new-tableaus (cons dx-new dx-list)
		      f (cdr step-list) eps)))))))))

(define (individual-extrapolation dx-list dx-new eps)
  (lambda (c tableau-data)
    (if (tableau-data->table-length tableau-data)
	tableau-data
	(let ((estimate (tableau-data->estimate tableau-data))
	      (dt (tableau-data->tableau tableau-data)))
	  (let ((new-dt
		 (cons c (rational-interpolation dt c dx-list dx-new eps))))
	    (let ((new-estimate (sum-list-flo new-dt)))
	      (let ((new-error
		     (abs (/ (* 2.0 (- estimate new-estimate))
			     (+ 2.0 (abs estimate) (abs new-estimate))))))
		(make-tableau-data new-estimate new-error
			      (if (< new-error eps) (length new-dt) false)
			      new-dt))))))))


(define (diverging? tableaus new-tableaus)
  (let ((max-error (apply max (map tableau-data->error tableaus)))
	(new-max-error (apply max (map tableau-data->error new-tableaus))))
    (> new-max-error max-error)))

(define (all-done? tableaus)
  (reduce (lambda (a b) (and a b))
	  true
	  (map tableau-data->table-length tableaus)))
|#

#|----------------------------------------------------------------
;;;;numerical integration examples 

;;; use compiled diffeq in test-difeq!!

(define harmonic   
  (lambda (s) ; (set! count (+ count 1)) 
	  (vector (vector-ref s 1) (flo:negate (vector-ref s 0)))))

(show-time 
 (lambda () 
   (advance-state 2 harmonic
		  (list 0.0 1.0 (vector 0.0 1.0))
		  (* 100.0 2pi) 1.0e-13
		  (lambda args '()))))
process time: 10140; real time: 12186
;Value: (628.3185307179587
         2.0149000002514867
         #(-5.71774615501508e-14 .9999999999999801))

(define (stiff x) 
  (let ((u (vector-ref x 0)) (v (vector-ref x 1)))
    (vector (flo:+ (flo:* 998.0 u) (flo:* 1998.0 v))
	    (flo:+ (flo:* -999.0 u) (flo:* -1999.0 v)))))

(define (stiff-answer x)
  (vector (- (* 2 (exp (- x))) (exp (* -1000 x)))
	  (+ (- (exp (- x))) (exp (* -1000 x)))
	  x))

(define (stiff-hamiltonian x) 
  (let ((u (vector-ref x 0)) (v (vector-ref x 1)))
    (vector (flo:+ (flo:* 500.5 u) v)
	    (flo:+ (flo:* 998.0 u) (flo:* -500.5 v)))))

(show-time 
 (lambda () 
   (advance-state 2 stiff
		  (list 0.0 0.01 (vector 1.0 0.0)) 0.01 1.0e-12
		  (lambda args (write-line args)))))
(show-time 
 (lambda () 
   (advance-state 2 stiff-vec
		  (list 0.0 0.01 (vector 1.0 0.0)) 1.0 1.0e-12
		  (lambda args '()))))

process time: 9380; real time: 10762
;Value: (1. 1.3233357542007995e-2 #(.7357588824655332 -.36787944124663186))

(define (kepler s)
  (let ((x (vector-ref s 0)) (y (vector-ref s 1)))
    (let ((radius^3
           (flonum-expt
            (flonum-add (flonum-multiply x x) (flonum-multiply y y))
            1.5)))
      (vector (vector-ref s 2)
              (vector-ref s 3)
              (flonum-negate (flonum-divide x radius^3))
              (flonum-negate (flonum-divide y radius^3))))))
(show-time 
 (lambda () 
   (advance-state 4 kepler
		  (list 0.0 1.0 (vector 1.0 0.0 0.0 1.1)) 
		  (* 100.0 2pi) 1.0e-13
		  (lambda args '()))))
process time: 79200; real time: 93153
;Value: (628.3185307179587
         .9771894138036714
         #(-.2670827485008997
	   1.2375960779353004
	   -.8886332522158626
	   -8.647969066388888e-4))
|#

#|;;; Got caught on trick cases, such as 1-cos32t, from 0 to 2pi.
;;; *INTEGRATE-N* is the number of step sizes used before aborting
;;;   to smaller intervals.  n = 10 seems to work well.
(define *INTEGRATE-N* 10)

(define (integrate-closed-closed f a b eps)
  (let ((ans
	 (integrate-closed-finite f a b *INTEGRATE-N* eps)))
    (if (null? ans)
	(let ((m (/ (+ a b) 2)))
	  (+ (integrate-closed-closed f a m eps)
	     (integrate-closed-closed f m b eps)))
	ans)))


(define (integrate-open-closed f a b eps)
  (let ((ans
	 (integrate-open-finite f a b *INTEGRATE-N* eps)))
    (if (null? ans)
	(let ((m (/ (+ a b) 2)))
	  (+ (integrate-open-closed f a m eps)
	     (integrate-closed-closed f m b eps)))
	ans)))

(define (integrate-closed-open f a b eps)
  (let ((ans
	 (integrate-open-finite f a b *INTEGRATE-N* eps)))
    (if (null? ans)
	(let ((m (/ (+ a b) 2)))
	  (+ (integrate-closed-closed f a m eps)
	     (integrate-closed-open f m b eps)))
	ans)))

(define (integrate-open-open f a b eps)
  (let ((ans
	 (integrate-open-finite f a b *INTEGRATE-N* eps)))
    (if (null? ans)
	(let* ((s (+ a b))
	       (m1 (/ s 3))
	       (m2 (/ (* 2 s) 3)))
	  (+ (integrate-open-closed f a m1 eps)
	     (integrate-closed-closed f m1 m2 eps)
	     (integrate-closed-open f m2 b eps)))
	ans)))

(define (integrate-open f a b eps)
  (let ((ans
	 (integrate-open-finite f a b *INTEGRATE-N* eps)))
    (if (null? ans)
	(let ((m (/ (+ a b) 2)))
	  (+ (integrate-open f a m eps)
	     (integrate-open f m b eps)))
	ans)))
|#
