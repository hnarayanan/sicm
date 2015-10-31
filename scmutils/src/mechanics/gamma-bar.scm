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

(define ((Gamma-bar f) local)
  ((f (osculating-path local)) (time local)))

;;; An alternative method allows taking derivatives in the
;;; construction of the Lagrangian.

(define ((osculating-path state0) t)
  (let ((t0 (time state0))
	(q0 (coordinate state0))
	(k (vector-length state0)))
    (let ((dt (- t t0)))
      (let loop ((n 2) (sum q0) (dt^n/n! dt))
	(if (fix:= n k)
	    sum
	    (loop (+ n 1)
		  (+ sum (* (vector-ref state0 n) dt^n/n!))
		  (/ (* dt^n/n! dt) n)))))))

;;; "total time derivative"

#|
(define (Dt-procedure F)
  (define (DF-on-path q)
    (D (compose F (Gamma q))))
  (Gamma-bar DF-on-path))
|#

#|
(define ((Dt-procedure F) state)
  (let ((n (vector-length state)))
    (define (DF-on-path q)
      (D (compose F (Gamma q (- n 1)))))
    ((Gamma-bar DF-on-path) state)))
|#

(define (Dt-procedure F)
  (define (DtF state)
    (let ((n (vector-length state)))
      (define (DF-on-path q)
	(D (compose F (Gamma q (- n 1)))))
      ((Gamma-bar DF-on-path) state)))
  DtF)

(define Dt
  (make-operator Dt-procedure 'Dt))

#|
(print-expression
 ((Dt
   (lambda (state)
     (let ((t (time state))
	   (q (coordinate state)))
       (square q))))
  (up 't (up 'x 'y) (up 'vx 'vy))))
(+ (* 2 vx x) (* 2 vy y))


(print-expression
 ((Dt (Dt (lambda (state) (coordinate state))))
  (up 't 'x 'v 'a 'j)))
a

(print-expression
 ((Dt (Dt (lambda (state)
	    (square (coordinate state)))))
  (up 't 'x 'v 'a 'j)))
(+ (* 2 a x) (* 2 (expt v 2)))

(define L (literal-function 'L (Lagrangian 2)))

(print-expression
 ((Dt L) (up 't (up 'x 'y) (up 'vx 'vy))))
<error, not enuf args>

(print-expression
 ((Dt L) (up 't (up 'x 'y) (up 'vx 'vy) (up 'ax 'ay))))
(+ (* ax (((partial 2 0) L) (up t (up x y) (up vx vy))))
   (* ay (((partial 2 1) L) (up t (up x y) (up vx vy))))
   (* vx (((partial 1 0) L) (up t (up x y) (up vx vy))))
   (* vy (((partial 1 1) L) (up t (up x y) (up vx vy))))
   (((partial 0) L) (up t (up x y) (up vx vy))))
|#

(define (Euler-Lagrange-operator Lagrangian)
  (- (Dt ((partial 2) Lagrangian))
     (compose ((partial 1) Lagrangian) trim-last-argument)))

(define (trim-last-argument local)
  (apply up (except-last-pair (vector->list (up->vector local)))))

(define LE Euler-Lagrange-operator)
(define Lagrange-equations-operator LE)
 
;;; Given a local tuple, produces a finite state.

#|
(define ((L-harmonic m k) s)
  (- (* 1/2 m (square (velocity s)))
     (* 1/2 k (square (coordinate s)))))

(print-expression
 ((LE (L-harmonic 'm 'k))
  (up 't 'x 'v 'a)))
(+ (* a m) (* k x))

(print-expression
 ((LE (L-harmonic 'm 'k))
  (up 't #(x y) #(vx vy) #(ax ay))))
(down (+ (* ax m) (* k x))
      (+ (* ay m) (* k y)))


(print-expression
 ((LE L) (up 't (up 'x 'y) (up 'vx 'vy) (up 'ax 'ay))))
(down
 (+ (* ax (((partial 2 0) ((partial 2 0) L)) (up t (up x y) (up vx vy))))
    (* ay (((partial 2 0) ((partial 2 1) L)) (up t (up x y) (up vx vy))))
    (* vx (((partial 1 0) ((partial 2 0) L)) (up t (up x y) (up vx vy))))
    (* vy (((partial 1 1) ((partial 2 0) L)) (up t (up x y) (up vx vy))))
    (* -1 (((partial 1 0) L) (up t (up x y) (up vx vy))))
    (((partial 0) ((partial 2 0) L)) (up t (up x y) (up vx vy))))
 (+ (* ax (((partial 2 0) ((partial 2 1) L)) (up t (up x y) (up vx vy))))
    (* ay (((partial 2 1) ((partial 2 1) L)) (up t (up x y) (up vx vy))))
    (* vx (((partial 1 0) ((partial 2 1) L)) (up t (up x y) (up vx vy))))
    (* vy (((partial 1 1) ((partial 2 1) L)) (up t (up x y) (up vx vy))))
    (* -1 (((partial 1 1) L) (up t (up x y) (up vx vy))))
    (((partial 0) ((partial 2 1) L)) (up t (up x y) (up vx vy)))))


;;; Adding extra state components is harmless, because L-harmonic does
;;; not check the length of the jet.

(print-expression
 ((LE (L-harmonic 'm 'k))
  (up 't 'x 'v 'a 'j)))
(+ (* a m) (* k x))

;;; But watch out.  If not enuf local componenents
;;;  are specified we lose.

(print-expression
 ((LE (L-harmonic 'm 'k))
  (up 't 'x 'v)))
;Cannot extract velocity from #((*diff* ... ...) x)
;;; error

(print-expression
 ((LE (L-central-polar 'm (literal-function 'V)))
  (up 't
      (up 'r 'phi)
      (up 'rdot 'phidot)
      (up 'rdotdot 'phidotdot))))
(down (+ (* -1 m (expt phidot 2) r) (* m rdotdot) ((D V) r))
      (+ (* 2 m phidot r rdot) (* m phidotdot (expt r 2))))

(print-expression
 ((compose (LE (L-central-polar 'm (literal-function 'V)))
	   (Gamma
	    (coordinate-tuple (literal-function 'r)
			      (literal-function 'phi))
	    4))
  't))
(down
 (+ (* -1 m (expt ((D phi) t) 2) (r t))
    (* m (((expt D 2) r) t))
    ((D V) (r t)))
 (+ (* 2 m ((D r) t) ((D phi) t) (r t))
    (* m (((expt D 2) phi) t) (expt (r t) 2))))
|#

(define (clip-state n)
  (lambda (u)
    (vector-head u n)))

(define (clip state)
  (vector-head state
	       (- (vector-length state)
		  1)))


(define ((generalized-LE Lagrangian) state)
  (let ((m (s:length state)))
    (assert (and (fix:> m 3) (even? m))
	    "Incorrect state size for Lagrange Equations")
    (let lp ((i (quotient m 2)) (state state))
      (if (fix:= i 0)
	  0
	  (- (((expt Dt (fix:- i 1))
	       ((partial i) Lagrangian))
	      state)
	     (lp (fix:- i 1) (clip state)))))))

#|
(define ((L2harmonic m k) state)
  (let ((x (coordinate state))
	(a (acceleration state)))
    (+ (* 1/2 m x a) (* 1/2 k (square x)))))

(print-expression
 ((generalized-LE (L2harmonic 'm 'k))
  (up 't 'x 'v 'a 'j 'p)))
(+ (* a m) (* k x))


(pe ((generalized-LE
      (literal-function 'L (-> (UP Real Real Real) Real)))
     (up 't 'x 'v 'a)))
(+ (* a (((partial 2) ((partial 2) L)) (up t x v)))
   (* v (((partial 1) ((partial 2) L)) (up t x v)))
   (((partial 0) ((partial 2) L)) (up t x v))
   (* -1 (((partial 1) L) (up t x v))))


(pe ((generalized-LE
      (literal-function 'L (-> (UP Real Real Real Real) Real)))
     (up 't 'x 'v 'a 'j 'p)))
(+ (* (expt a 2) (((partial 2) ((partial 2) ((partial 3) L))) (up t x v a)))
   (* 2 a j (((partial 2) ((partial 3) ((partial 3) L))) (up t x v a)))
   (* 2 a v (((partial 1) ((partial 2) ((partial 3) L))) (up t x v a)))
   (* (expt j 2) (((partial 3) ((partial 3) ((partial 3) L))) (up t x v a)))
   (* 2 j v (((partial 1) ((partial 3) ((partial 3) L))) (up t x v a)))
   (* (expt v 2) (((partial 1) ((partial 1) ((partial 3) L))) (up t x v a)))
   (* 2 a (((partial 0) ((partial 2) ((partial 3) L))) (up t x v a)))
   (* a (((partial 1) ((partial 3) L)) (up t x v a)))
   (* -1 a (((partial 2) ((partial 2) L)) (up t x v a)))
   (* 2 j (((partial 0) ((partial 3) ((partial 3) L))) (up t x v a)))
   (* p (((partial 3) ((partial 3) L)) (up t x v a)))
   (* 2 v (((partial 0) ((partial 1) ((partial 3) L))) (up t x v a)))
   (* -1 v (((partial 1) ((partial 2) L)) (up t x v a)))
   (((partial 0) ((partial 0) ((partial 3) L))) (up t x v a))
   (* -1 (((partial 0) ((partial 2) L)) (up t x v a)))
   (((partial 1) L) (up t x v a)))
|#
