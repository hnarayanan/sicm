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

;;; Coordinate Transformation to State Transformation

#|
;;; if defined F as state function F(t, q, v); (partial 2) F = 0

(define (F->C F)
  (define (C state)
    (up (time state)
	(F state)
	(+ (((partial 0) F) state)
	   (* (((partial 1) F) state) 
	      (velocity state)))))
  C)
|#

#|
(define ((F->C F) local)
  (let ((n (vector-length local)))
    ((Gamma-bar
      (lambda (qp)
	(Gamma
          (compose F (Gamma qp))
	 n)))
     local)))
|#

#|
;;; version for display in text

(define (F->C F)
  (define (f-bar q-prime)
    (define q
      (compose F (Gamma q-prime)))
    (Gamma q))
  (Gamma-bar f-bar))
|#

(define (F->C F)
  (define (C local)
    (let ((n (vector-length local)))
      (define (f-bar q-prime)
	(define q
	  (compose F (Gamma q-prime)))
	(Gamma q n))
      ((Gamma-bar f-bar) local)))
  C)

;;; The following transformations are applicable to 
;;;  configuration coordinates. 

(define (rectangular->polar rectangular-tuple)
  (let ((x (ref rectangular-tuple 0))
        (y (ref rectangular-tuple 1)))
    (let ((r (sqrt (+ (square x) (square y))))
          (phi (atan y x)))
      (up r phi))))

(define (r->p tqv)
  (rectangular->polar (coordinate tqv)))


(define (polar->rectangular polar-tuple)
  (let ((r (ref polar-tuple 0)) 
        (phi (ref polar-tuple 1)))
    (let ((x (* r (cos phi))) 
          (y (* r (sin phi))))
      (up x y))))

(define (p->r tqv)
  (polar->rectangular (coordinate tqv)))

#|
(show-expression 
 (velocity
  ((F->C p->r)
   (->local 't 
	    (coordinate-tuple 'r 'phi) 
	    (velocity-tuple 'rdot 'phidot)))))
(up (+ (* -1 r phidot (sin phi)) (* rdot (cos phi)))
    (+ (* r phidot (cos phi)) (* rdot (sin phi))))


(define (L-central-polar m V)
  (compose (L-central-rectangular m V)
	   (F->C p->r)))

(show-expression
  ((L-central-polar 'm (literal-function 'V))
   (->local 't (coordinate-tuple 'r 'phi) 
               (velocity-tuple 'rdot 'phidot))))
(+ (* 1/2 m (expt phidot 2) (expt r 2))
   (* 1/2 m (expt rdot 2))
   (* -1 (V r)))
|#

#|
;;; Driven pendulum example

(define ((T-pend m l g ys) local)
  (let ((t (time local))
        (theta (coordinate local))
        (thetadot (velocity local)))
    (let ((ysdot (D ys)))
      (* 1/2 m
         (+ (square (* l thetadot))
            (square (ysdot t))
            (* 2 (ysdot t) l (sin theta) thetadot))))))

(define ((V-pend m l g ys) local)
  (let ((t (time local))
        (theta (coordinate local)))
    (* m g (- (ys t) (* l (cos theta))))))

(define L-pend (- T-pend V-pend))

(show-expression
 ((L-pend 'm 'l 'g (literal-function 'y_s))
  (->local 't 'theta 'thetadot)))
(+ (* 1/2 (expt l 2) m (expt thetadot 2))
   (* l m thetadot ((D y_s) t) (sin theta))
   (* g l m (cos theta))
   (* -1 g m (y_s t))
   (* 1/2 m (expt ((D y_s) t) 2)))

(show-expression
 (((Lagrange-equations
    (L-pend 'm 'l 'g (literal-function 'y_s)))
   (literal-function 'theta))
  't))
(+ (* g l m (sin (theta t)))
   (* (expt l 2) m (((expt D 2) theta) t))
   (* l m (((expt D 2) y_s) t) (sin (theta t))))
|#

#|
;;; Same driven pendulum by coordinate transformation

(define ((Lf m g) local)
  (let ((q (coordinate local))
        (v (velocity local)))
    (let ((h (ref q 1)))
      (- (* 1/2 m (square v)) (* m g h)))))

(define ((dp-coordinates l y_s) local)
  (let ((t (time local))
	(theta (coordinate local)))
    (let ((x (* l (sin theta)))
	  (y (- (y_s t) (* l (cos theta)))))
      (coordinate-tuple x y))))

(define (L-pend m l g y_s)
  (compose (Lf m g) 
           (F->C (dp-coordinates l y_s))))

(show-expression
 ((L-pend 'm 'l 'g (literal-function 'y_s))
  (->local 't 'theta 'thetadot)))
(+ (* 1/2 (expt l 2) m (expt thetadot 2))
   (* l m thetadot (sin theta) ((D y_s) t))
   (* g l m (cos theta))
   (* -1 g m (y_s t))
   (* 1/2 m (expt ((D y_s) t) 2)))

(show-expression
 (((Lagrange-equations
    (L-pend 'm 'l 'g (literal-function 'y_s)))
   (literal-function 'theta))
  't))
(+ (* g l m (sin (theta t)))
   (* (expt l 2) m (((expt D 2) theta) t))
   (* l m (((expt D 2) y_s) t) (sin (theta t))))
|#

;;; Spherical Coordinates (radius, colatitude, longitude)

(define (spherical->rectangular q)
  (let ((r (ref q 0))
	(theta (ref q 1))
	(phi (ref q 2)))
    (let ((x (* r (sin theta) (cos phi)))
	  (y (* r (sin theta) (sin phi)))
	  (z (* r (cos theta))))
      (coordinate-tuple x y z))))  

(define (s->r local)
  (spherical->rectangular (coordinate local)))

(define (rectangular->spherical q)
  (let ((x (ref q 0))
	(y (ref q 1))
	(z (ref q 2)))
    (let ((r (sqrt (+ (* x x) (* y y) (* z z)))))
      (let ((theta (acos (/ z r)))
	    (phi (atan y x)))
	(up r theta phi)))))

(define (r->s local)
  (rectangular->spherical (coordinate local)))

#|
(define (L3-central m Vr)
  (define (Vs local)
    (let ((r (ref (coordinate local) 0)))
      (Vr r)))
  (- (T3-spherical m) Vs))

(define ((ang-mom-z m) local)
  (let ((q (coordinate local))
        (v (velocity local)))
     (ref (cross-product q (* m v)) 2)))

(show-expression
  ((compose (ang-mom-z 'm) (F->C s->r))
   (->local 't 
            (coordinate-tuple 'r 'theta 'phi)
            (velocity-tuple 'rdot 'thetadot 'phidot))))
(* m (expt r 2) phidot (expt (sin theta) 2))

(show-expression
 ((Lagrangian->energy
   (L3-central 'm (literal-function 'V)))
  (->local 't
           (coordinate-tuple 'r 'theta 'phi)
           (velocity-tuple 'rdot 'thetadot 'phidot))))
(+ (* 1/2 m (expt r 2) (expt phidot 2) (expt (sin theta) 2))
   (* 1/2 m (expt r 2) (expt thetadot 2))
   (* 1/2 m (expt rdot 2))
   (V r))
|#

;;; Rotations about the rectangular axes

(define ((Rx angle) q)
  (let ((ca (cos angle))
	(sa (sin angle)))
    (let ((x (ref q 0))
	  (y (ref q 1))
	  (z (ref q 2)))
      (up
       x
       (- (* ca y) (* sa z))
       (+ (* ca z) (* sa y))))))

(define ((Ry angle) q)
  (let ((ca (cos angle))
	(sa (sin angle)))
    (let ((x (ref q 0))
	  (y (ref q 1))
	  (z (ref q 2)))
      (up
       (+ (* ca x) (* sa z))
       y
       (- (* ca z) (* sa x))))))

(define ((Rz angle) q)
  (let ((ca (cos angle))
	(sa (sin angle)))
    (let ((x (ref q 0))
	  (y (ref q 1))
	  (z (ref q 2)))
      (up
       (- (* ca x) (* sa y))
       (+ (* ca y) (* sa x))
       z))))



