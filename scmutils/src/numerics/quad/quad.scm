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

;;;     Fundamental quad manipulation 
;;;        given double arguments.

;;; Knuth's theorem B provides a method of adding 2 floats
;;;  to produce a sum and an error estimate.
;;;   It takes 6 floating operations and no comparisions.

(define (+B u v k)			;k=(lambda (sum error) ...)
  (let ((s (flo:+ u v)))
    (let ((up (flo:- s v)))
      (let ((vpp (flo:- s up)))
	(k s (flo:+ (flo:- u up) (flo:- v vpp)))))))


(define (-B u v k)			;k=(lambda (sum error) ...)
  (let ((s (flo:- u v)))
    (let ((up (flo:+ s v)))
      (let ((vpp (flo:- s up)))
	(k s
	   (flo:- (flo:- u up)
		  (flo:+ v vpp)))))))


;;; Multiplication of two floating operations, transcribed from 
;;;  Quinn & Tremaine 1990.

(define IEEE-p 53)			;see standard.
(define tremaine-j
  (ceiling (/ IEEE-p 2)))
(define tremaine-c
  (exact->inexact (int:+ (expt 2 tremaine-j) 1)))

(define (*T u v k)			;k=(lambda (prod error) ...)
  (let* ((up (flo:* u tremaine-c))
	 (u1 (flo:- u up))
	 (u1 (flo:+ u1 up))
	 (u2 (flo:- u u1))
	 (vp (flo:* v tremaine-c))
	 (v1 (flo:- v vp))
	 (v1 (flo:+ v1 vp))
	 (v2 (flo:- v v1))
	 (prod (flo:* u v))
	 (e1 (flo:* u1 v1))
	 (e2 (flo:* u2 v2))
	 (e12 (flo:* u1 v2))
	 (e21 (flo:* u2 v1))
	 (err (flo:- e1 prod))
	 (err (flo:+ err e12))
	 (err (flo:+ err e21))
	 (perr (flo:+ err e2)))
    (k prod perr)))


(define (/T u v k)			;k=(lambda (prod error) ...)
  (let ((quot (flo:/ u v)))
    (*T quot v
	(lambda (rat err)
	  (let* ((qerr (flo:- u rat))
		 (qerr (flo:/ (flo:- qerr err) v)))
	    (k quot qerr))))))

;;;  Quad arithmetic -- a quad is a cons of the high and low part.

(define quad:high car)
(define quad:low cdr)

(define (quad:+ q1 q2)
  (let ((q1h (car q1))
	(q1l (cdr q1))
	(q2h (car q2))
	(q2l (cdr q2)))
    (+B q1h q2h
	(lambda (s e)
	  (+B s (flo:+ (flo:+ q1l q2l) e)
	      cons)))))

(define (quad:- q1 q2)
  (let ((q1h (car q1))
	(q1l (cdr q1))
	(q2h (car q2))
	(q2l (cdr q2)))
    (-B q1h q2h
	(lambda (s e)
	  (+B s (flo:+ (flo:- q1l q2l) e)
	      cons)))))

(define (quad:* q1 q2)
  (let ((q1h (car q1))
	(q1l (cdr q1))
	(q2h (car q2))
	(q2l (cdr q2)))
    (*T q1h q2h
	(lambda (p e)
	  (+B p
	      (flo:+ (flo:+ (flo:* q1l q2h) (flo:* q1h q2l)) e)
	      cons)))))

(define (quad:/ q1 q2)
  (let ((q1h (car q1))
	(q1l (cdr q1))
	(q2h (car q2))
	(q2l (cdr q2)))
    (/T q1h q2h
	(lambda (q e)
	  (+B q
	      (flo:+ (flo:+ (flo:/ q1l q2h)
			    (flo:* -1.0 (flo:* q (flo:/ q2l q2h))))
		     e)
	      cons)))))


(define (quad:negate q)
  (quad:- quad:0 q))


(define (->quad r)
  (cons (exact->inexact r) 0.0))

(define (rat->quad rat)
  (quad:/ (->quad (numerator rat))
	  (->quad (denominator rat))))


(define quad:0 (->quad 0.0))
(define quad:1 (->quad 1.0))
(define quad:2 (->quad 2.0))
(define quad:3 (->quad 3.0))
(define quad:4 (->quad 4.0))

(define *quad-epsilon*
  (flo:* *machine-epsilon* *machine-epsilon*))


(define (quad:sqrt x)
  (let ((xh (car x)))
    (if (zero? xh)
	quad:0
	(let lp ((guess (->quad (sqrt xh))))
	  (let ((new (quad:/ (quad:+ guess (quad:/ x guess)) quad:2)))
	    (if (flo:= (car guess) (car new))
		new
		(lp new)))))))


(define quad:sin-bugger-factor
  (expt 2. (* -1 IEEE-p)))

(define (quad:sin x)
  (let ((xh (quad:high x)))
    (if (flo:< (flo:abs xh) quad:sin-bugger-factor)
	x
	(let ((y (quad:sin (quad:/ x quad:3))))
	  (quad:- (quad:* quad:3 y)
		  (quad:* quad:4
			  (quad:* y
				  (quad:* y y))))))))


(define (quad:exp x)
  (let lp ((n 1) (xp x) (f quad:1) (sum quad:1))
    (let ((term (quad:/ xp f)))
      (if (< (abs (quad:high term)) *quad-epsilon*)
	  sum
	  (lp (fix:+ n 1)
	      (quad:* x xp)
	      (quad:* f (->quad (fix:+ n 1)))
	      (quad:+ sum term))))))

(define (alternate-sine x)
  (let ((xsq (quad:* x x)))
    (let lp ((n 1) (xp x) (f quad:1) (sum quad:0))
      (let ((term (quad:/ xp f)))
	(if (< (abs (quad:high term)) *quad-epsilon*)
	    sum
	    (lp (fix:+ n 2)
		(quad:* (->quad -1.0) (quad:* xsq xp))
		(quad:* f (->quad (fix:* (fix:+ n 1) (fix:+ n 2))))
		(quad:+ sum term)))))))


(define (quad:magnitude z)
  (quad:sqrt (quad:+ (quad:* (car z) (car z)) (quad:* (cadr z) (cadr z)))))

;;; some other trig functions

(define (compute-quad:pi)
  (let lp ((guess (->quad pi)))
    (write-line guess)
    (let ((new-guess (quad:+ guess (quad:sin guess))))
      (if (< (abs (quad:high (quad:- new-guess guess))) *quad-epsilon*)
	  new-guess
	  (lp new-guess)))))
			    
(define quad:pi '(3.141592653589793 . 1.2246467991473535e-16))
(define quad:pi/2 '(1.5707963267948966 . 6.123233995736767e-17))

(define (quad:cos x)
  (quad:sin (quad:+ x quad:pi/2)))

(define (quad:tan x) (quad:/ (quad:sin x) (quad:cos x)))

(define (quad:atan x)
  (let ((fp (quad:+ quad:1 (quad:* x x)))
	(guess (->quad (atan (quad:high x)))))
    (let lp ((guess guess))
      (let ((new-guess (quad:- guess (quad:/ (quad:- (quad:tan guess) x) fp))))
	(if (< (abs (quad:high (quad:- new-guess guess))) *quad-epsilon*)
	    new-guess
	    (lp new-guess))))))



(define quad-type-tag '*quad*)

(define (quad? v)
  (and (pair? v)
       (eq? (car v) quad-type-tag)))

(define (quad-quantity? v)
  (quad? v))

(define (quad:type x) quad-type-tag)

(define (quad:type-predicate v) quad-quantity?)

(define (make-quad x)
  (list quad-type-tag x))

(define (quad-contents x) (cadr x))

(define (quad:zero-like q)
  (make-quad quad:0))

(define (quad:->quad number)
  (cond ((real? number)
	 (make-quad (->quad  number)))
	((quad? number) number)
	(else (error "Not a real" number))))

(define (quad:->real quad)
  (cond ((quad? quad)
	 (quad:high (quad-contents quad)))
	((real? quad) quad)
	(else (error "Not a quad" quad))))

(define ((quad-unary f) x)
  (make-quad (f (quad-contents x))))

(define ((quad-binary f) x y)
  (make-quad (f (quad-contents x) (quad-contents y))))

(define quad+quad (quad-binary quad:+))

(define (real+quad x y)
  (quad+quad (quad:->quad x) y))

(define (quad+real x y)
  (quad+quad x (quad:->quad y)))


(define quad-quad (quad-binary quad:-))

(define (real-quad x y)
  (quad-quad (quad:->quad x) y))

(define (quad-real x y)
  (quad-quad x (quad:->quad y)))


(define quad*quad (quad-binary quad:*))

(define (real*quad x y)
  (quad*quad (quad:->quad x) y))

(define (quad*real x y)
  (quad*quad x (quad:->quad y)))


(define quad/quad (quad-binary quad:/))

(define (real/quad x y)
  (quad/quad (quad:->quad x) y))

(define (quad/real x y)
  (quad/quad x (quad:->quad y)))


(define quad-atan-quad (quad-binary quad:atan))

(define (real-atan-quad x y)
  (quad-atan-quad (quad:->quad x) y))

(define (quad-atan-real x y)
  (quad-atan-quad x (quad:->quad y)))

(define :quad:pi (make-quad quad:pi))
(define :quad:0  (make-quad quad:0))
(define :quad:1  (make-quad quad:1))


(assign-operation 'type             quad:type           quad?)
(assign-operation 'type-predicate   quad:type-predicate quad?)

(assign-operation 'zero-like        quad:zero-like      quad?)

(assign-operation 'negate (quad-unary quad:negate) quad?)
(assign-operation 'sqrt   (quad-unary quad:sqrt)   quad?)
(assign-operation 'sin    (quad-unary quad:sin)    quad?)
(assign-operation 'cos    (quad-unary quad:cos)    quad?)
(assign-operation 'tan    (quad-unary quad:tan)    quad?)

(assign-operation '+      quad+quad     quad? quad?)
(assign-operation '+      real+quad     real? quad?)
(assign-operation '+      quad+real     quad? real?)

(assign-operation '-      quad-quad     quad? quad?)
(assign-operation '-      real-quad     real? quad?)
(assign-operation '-      quad-real     quad? real?)

(assign-operation '*      quad*quad     quad? quad?)
(assign-operation '*      real*quad     real? quad?)
(assign-operation '*      quad*real     quad? real?)

(assign-operation '/      quad/quad     quad? quad?)
(assign-operation '/      real/quad     real? quad?)
(assign-operation '/      quad/real     quad? real?)

(assign-operation 'atan quad-atan-quad  quad? quad?)
(assign-operation 'atan real-atan-quad  real? quad?)
(assign-operation 'atan quad-atan-real  quad? real?)

#| probably wrong

(define rat:pi 314159265358979323846264338327950288419716939937510/100000000000000000000000000000000000000000000000000)

(define (quad:floor x)
  (floor (quad:high x)))

(define (quad:truncate x)
  (->quad (truncate (quad:high x))))  ;;; what if the error has an integer part?

(define continued-wallp true)

(define (best-fraction-from-quad x maxint)
  (let* ((a0 (inexact->exact (floor (quad:high x))))
	 (r0 (quad:- x (quad:truncate x))))
    (if (<= (quad:high r0) (* (abs (quad:high x)) *quad-epsilon*))
	a0 
	(let* ((d (quad:/ quad:1 r0))
	       (a1 (inexact->exact (truncate (quad:high d)))))
	  (let lp ((s d) (last-p a0) (last-q 1) (p (+ (* a0 a1) 1)) (q a1))
	    (if continued-wallp
		(write-line (list s last-p last-q p q)))
	    (if (> q maxint)
		(/ last-p last-q)
		(let ((r (quad:- s (quad:truncate s))))
		  (if (<= (quad:high r) (* (quad:high s) *quad-epsilon*))
		      (/ p q)
		      (let* ((new-d (quad:/ quad:1 r))
			     (a (inexact->exact (truncate (quad:high new-d)))))
			(lp new-d
			    p
			    q
			    (+ last-p (* a p))
			    (+ last-q (* a q))))))))))))

|#

(define (cquad:+ z1 z2)
  (map quad:+ z1 z2))

(define (cquad:- z1 z2)
  (map quad:- z1 z2))

(define (cquad:* z1 z2)
  (list (quad:- (quad:* (car z1) (car z2))
	     (quad:* (cadr z1) (cadr z2)))
	(quad:+ (quad:* (car z1) (cadr z2))
	     (quad:* (cadr z1) (car z2)))))

(define (cquad:/ z1 z2)
  (let ((m (quad:+ (quad:* (car z2) (car z2))
		(quad:* (cadr z2) (cadr z2)))))
    (list (quad:/ (quad:+ (quad:* (car z1) (car z2))
		    (quad:* (cadr z1) (cadr z2)))
	       m)
	  (quad:/ (quad:- (quad:* (cadr z1) (car z2))
		    (quad:* (car z1) (cadr z2)))
	       m))))

(define (q->cq q)
  (list q quad:0))

(define (c->cq c)
  (list (->quad (real-part c))
	(->quad (imag-part c))))

(define cquad:0 (q->cq quad:0))
(define cquad:1 (q->cq quad:1))
(define cquad:i (list quad:0 quad:1))

