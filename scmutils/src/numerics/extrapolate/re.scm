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

;;;; Richardson Extrapolation of a sequence, represented as a stream.

(declare (usual-integrations))


;;; Assumption: The sequence is the values of a function at successive
;;; reciprocal powers of 2.  The function is assumed to be analytic in
;;; a region around h=0 with a power-series expansion.  The first
;;; non-constant term of the series has power p and successive terms
;;; are separated by powers of q.

(define (make-zeno-sequence R h)
  (cons-stream (R h) (make-zeno-sequence R (/ h 2))))

(define (accelerate-zeno-sequence seq p)
  (let* ((2^p (expt 2 p)) (2^p-1 (- 2^p 1)))
    (map-streams (lambda (Rh Rh/2)
                    (/ (- (* 2^p Rh/2) Rh)
                       2^p-1))
                 seq
                 (tail seq))))

(define (make-zeno-tableau seq p q)
  (define (sequences seq order)
    (cons-stream seq
    		 (sequences (accelerate-zeno-sequence seq order)
		 	    (+ order q))))
  (sequences seq p))

(define (first-terms-of-zeno-tableau tableau)
  (map-stream head tableau))

(define (richardson-sequence seq p q)
  (first-terms-of-zeno-tableau (make-zeno-tableau seq p q)))

(define (stream-limit s tolerance . optionals)
  (let ((M (default-lookup 'MAXTERMS -1 optionals))
        (ok? (default-lookup 'CONVERGENCE-TEST close-enuf? optionals)))
    (let loop ((s s) (count 2))
      ;;(write-line 'main-loop)
      (let* ((h1 (head s)) (t (tail s)) (h2 (head t)))
	;;(write-line (list count h1 h2))	
        (cond ((ok? h1 h2 tolerance) h2)
              ((and (positive? M) (>= count M))
	       ((default-lookup 'IFFAIL 
                                (lambda args 
                                  (error "STREAM-LIMIT not reached" M))
                                optionals)
	        'stream-limit
                'not-converged
		M
		h1
		h2))
              (else (loop t (+ count 1))))))))

(define (richardson-limit f start-h ord inc tolerance . opts)
  (apply stream-limit
	 (richardson-sequence (make-zeno-sequence f start-h)
			      ord
			      inc)
	 tolerance
	 opts))

#|
;;; Archimedean computation of Pi.

(define (refine-by-doubling s)
  (/ s (sqrt (+ 2 (sqrt (- 4 (square s)))))))

(define (stream-of-iterates next value)
  (cons-stream value (stream-of-iterates next (next value))))

(define side-lengths
  (stream-of-iterates refine-by-doubling (sqrt 2)))

(define side-numbers
  (stream-of-iterates (lambda (n) (* 2 n)) 4))

(define (semi-perimeter length-of-side number-of-sides)
  (* (/ number-of-sides 2) length-of-side))

(define archimedean-pi-sequence
  (map-streams semi-perimeter side-lengths side-numbers))

(print-stream archimedean-pi-sequence 24)

2.8284271247461903
3.0614674589207183
3.1214451522580524
3.1365484905459393
3.140331156954753
3.141277250932773
3.1415138011443013
3.141572940367092
3.14158772527716
3.1415914215112
3.141592345570118
3.141592576584873
3.1415926343385636
3.141592648776986
3.1415926523865916
3.1415926532889933
3.1415926535145937
3.141592653570994
3.141592653585094
3.141592653588619
3.1415926535895
3.1415926535897203
3.1415926535897754
3.141592653589789
;Value: ...
|#

#|
(print-stream (richardson-sequence archimedean-pi-sequence 2 2) 7)
2.8284271247461903
3.1391475703122276
3.1415903931299374
3.141592653286045
3.1415926535897865
3.141592653589793
3.1415926535897936
;Value: ...

(guess-ord-and-inc archimedean-pi-sequence)
;Value: (2. 2.)




|#

(define ord-estimate-stream
  (let ((log2 (log 2)))
    (lambda (s)
      (let* ((s0 (head s)) (st (tail s))
             (s1 (head st)) (s2 (head (tail st))))
	(cons-stream (/ (log (abs (/ (- s0 s1) (- s1 s2))))
			log2)
	             (ord-estimate-stream st))))))

(define (guess-integer-convergent s)
  (let* ((s0 (head s)) (st (tail s))
	 (s1 (head st)) (s2 (head (tail st))))
    (let* ((N (round s0))
           (e2 (magnitude (- s2 N)))
	   (e1 (magnitude (- s1 N)))
	   (e0 (magnitude (- s0 N))))
      (if (and (<= e2 e1) (<= e1 e0))
          N
          (guess-integer-convergent st)))))

(define (guess-ord-inc-etc s) ;tries to return { ord inc inc inc ... }
  (let ((psequence (ord-estimate-stream s)))
    (cons-stream (guess-integer-convergent psequence)
                 (guess-ord-inc-etc psequence))))

(define (guess-ord-and-inc s)
  (let ((g (guess-ord-inc-etc s)))
    ;; ... and if we make it this far ...
    (list (head g) (head (tail g)))))

;;; For example, we can make a Richardson extrapolation to get the
;;; first derivative of a function to a required tolerance.
		
(define richardson-derivative
  (let ((log2 (log 2.)))
    (define (rderiv f tolerance #!optional plausible-h)
      (define (the-richardson-derivative x)
	(let ((h
	       (if (default-object? plausible-h)
		   (if (zero? x)
		       0.1
		       (* 0.5 (magnitude x)))
		   plausible-h)))
	  (let* ((delta (- (f (+ x h)) (f (- x h))))
		 (roundoff (* *machine-epsilon*
			      (+ 1 (floor (magnitude (/ (f x)
							(if (zero? delta)
							    1
							    delta)))))))
		 (n (floor (/ (log (/ tolerance roundoff)) log2))))
	    (richardson-limit (lambda (dx)
				(/ (- (f (+ x dx))
				      (f (- x dx)))
				   (* 2 dx)))
			      h
			      2
			      2
			      tolerance
			      'MAXTERMS (+ n 1)))))
      the-richardson-derivative)
    rderiv))


(define richardson-second-derivative
  (let ((log4 (log 4)))
    (lambda (f tolerance)
      (lambda (x)
	(let* ((plausible-h (* 0.5 (magnitude x)))
	       (h (if (zero? plausible-h) 0.1 plausible-h))
               (2fx (* 2 (f x)))
	       (delta (+ (f (+ x h h)) (- 2fx) (f (- x h h))))
	       (roundoff (* *machine-epsilon*
			    (+ 1 (floor (magnitude (/ 2fx
						      (if (zero? delta)
							  1
							  delta)))))))
	       (n (floor (/ (log (/ tolerance roundoff)) log4))))
	  (richardson-limit (lambda (h)
			      (/ (+ (f (+ x h h))
				    (* -2 (f x))
				    (f (- x h h)))
				 (* 4 h h)))
			    h
			    2
			    2
			    tolerance
			    'MAXTERMS (+ n 1)))))))

;;; Romberg integration is the result of a Richardson extrapolation
;;; of the trapezoid method.

(define (romberg-quadrature f a b tolerance)
  (define (trapezoid-sums f a b)
    (define (next-S S n)
      (let* ((h (/ (- b a) 2 n))
	     (fx (lambda (i) (f (+ a (* (+ i i -1) h))))))
	(+ (/ S 2) (* h (sigma fx 1 n)))))
    (define (S-and-n-stream S n)
      (cons-stream (list S n)
		   (S-and-n-stream (next-S S n) (* n 2))))
    (let* ((h (- b a))
	   (S (* (/ h 2) (+ (f a) (f b)))))
      (map-stream car (S-and-n-stream S 1))))
  (stream-limit
   (richardson-sequence (trapezoid-sums f a b) 
			2
			2)
   tolerance))

;;; Given a sequence (stream) of abscissas and ordinates for a function,
;;; the following procedure constructs a sequence of constant terms of 
;;; polynomials that interpolate the successive initial segments of the 
;;; given sequences.
;;; This is done by incrementally constructing a Neville-like tableau
;;; for the interpolating polynomials, specialized for argument 0.

(define polynomial-extrapolation
  (letrec ((pt-lp
             (lambda (xs ys xstate ystate)
               (let ((x (head xs)))
                 (letrec ((poly-lp
                            (lambda (y xstate ystate)
                              (if (null? ystate)
                                  (list y)
                                  (cons y
                                        (poly-lp (/ (- (* y (car xstate))
                                                       (* x (car ystate)))
                                                    (- (car xstate) x))
                                                 (cdr xstate)
                                                 (cdr ystate)))))))
                   (let ((new (poly-lp (head ys) xstate ystate)))
                     (cons-stream (car (last-pair new))
                                  (pt-lp (tail xs)
                                         (tail ys)
                                         (cons x xstate)
                                         new))))))))
    (lambda (abscissas ordinates)
      (pt-lp abscissas ordinates '() '()))))

;;; Given a sequence (stream) of abscissas and ordinates for a function,
;;; the following procedure constructs a sequence of constant terms of 
;;; rational functions that interpolate the successive initial segments
;;; of the given sequences.
;;; This is done by incrementally constructing a Bulirsch-Stoer tableau
;;; for the interpolating rational functions, specialized for argument 0.

(define rational-extrapolation
  (letrec ((pt-lp
             (lambda (xs ys xstate ystate)
               (let ((x (head xs)))
                 (letrec ((rat-lp
                            (lambda (y xstate ystate z)
                              (if (null? ystate)
                                  (list y)
				  (let ((u (* (/ (car xstate) x) 
                                              (- (car ystate) z))))
                                    (cons y
                                      (rat-lp
                                        (/ (+ (* y (- u (car ystate)))
                                              (* z (car ystate)))
                                           (- u (- y z)))
                                        (cdr xstate)
                                        (cdr ystate)
					(car ystate))))))))
                   (let ((new (rat-lp (head ys) xstate ystate 0)))
                     (cons-stream (car (last-pair new))
                                  (pt-lp (tail xs)
                                         (tail ys)
                                         (cons x xstate)
                                         new))))))))
    (lambda (abscissas ordinates)
      (pt-lp abscissas ordinates '() '()))))
