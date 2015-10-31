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

;;;;           Variational Mechanics
  
;;; Caution... This file is case sensitive!

;;; However, there are alternative names for the actual data types.

(define coordinate-tuple up)
(define velocity-tuple up)
(define acceleration-tuple up)
(define momentum-tuple down)

;;; Lagrangian mechanics requires a configuration
;;;   space Q, and a function L:RxQxQ' --> R

;;; Mechanical systems have state at each instant.  The state is the
;;;  information required, along with the equations of motion, to
;;;  determine the future of the system.

;;; At every instant a system has a kinematic state, which has the
;;;  time, the configuration, and the rate of change of the
;;;  configuration.  Lagrangian mechanics is formulated in terms of
;;;  the kinematic state.

;;; Kinematic states and their derivatives are represented as Scheme
;;; vectors, with components time, configuration, and derivatives.

(define (->local t q qdot . derivs)
  (apply vector t q qdot derivs))

(define ->state ->local)

(define ->L-state ->local)

(define (state->n-dof state)
  (let ((q (vector-ref state 1)))
    (if (up? q)
	(s:length q)
	1)))


;;; Selectors are provided for the components of a state.

(define (state->t state)
  (if (not (and (vector? state) (fix:> (vector-length state) 0)))
      (error "Cannot extract time from" state))
 (ref state 0))

(define (state->q state)
  (if (not (and (vector? state) (fix:> (vector-length state) 1)))
      (error "Cannot extract coordinate from" state))
  (ref state 1))

(define (state->qdot state)
  (if (not (and (vector? state) (fix:> (vector-length state) 2)))
      (error "Cannot extract velocity from" state))
  (ref state 2))

(define (state->qddot state)
  (if (not (and (vector? state) (fix:> (vector-length state) 3)))
      (error "Cannot extract acceleration from" state))
  (ref state 3))
    
(define time state->t)

(define coordinate state->q)
(define velocity state->qdot)
(define acceleration state->qddot)

(define coordinates state->q)
(define velocities state->qdot)
(define accelerations state->qddot)

(define Q     state->q)
(define Qdot  state->qdot)
(define Qdotdot  state->qddot)

(define (literal-Lagrangian-state n-dof)
  (up (literal-number (generate-uninterned-symbol 't))
      (s:generate n-dof 'up
		  (lambda (i)
		    (literal-number (generate-uninterned-symbol 'x))))
      (s:generate n-dof 'up
		  (lambda (i)
		    (literal-number (generate-uninterned-symbol 'v))))))

;;;; Chapter 1

;;; Paths in the configuration manifold are functions that give a
;;; configuration for each time.  From such a path we can construct a
;;; path in the kinematic state space.  If such a path is described 
;;; in terms of generalized coordinates, we have

#|
(define (path->state-path q)
  (lambda (t)
    (->local t
	     (q t)
	     ((D q) t))))
|#

(define (path->state-path q #!optional n)
  (if (default-object? n)
      (set! n 3)
      (assert (fix:> n 1)))
  (lambda (t)
    (list->vector
     (cons t
	   (cons (q t)
		 (let lp ((i (fix:- n 2)) (fi (D q)))
		   (if (fix:= i 0)
		       '()
		       (cons (fi t)
			     (lp (- i 1)
				 (D fi))))))))))

(define Gamma path->state-path)


#|
;;; Another way to make Gamma

(define ((path->state-path q #!optional n) t)
  (if (default-object? n) (set! n 3))
  (list->vector
   (stream-head
    (cons-stream t
		 (cons-stream (q t)
			      (map-stream (lambda (e) (((expt D e) q) t)) 
					  natural-number-stream)))
    n)))
|#

#|
;;; Can we do it this way?  No...  
;;;  We don't know number of degrees of freedom when we build state vector.

(define (path->state q)
  (->local identity
	   q
	   (D q)))
|#

;;; A Lagrangian is an example of an L-function.
;;; An L-function takes  a scalar argument and 2 vector arguments
;;; (t, q, q-dot).  An L-function produces a scalar result.

(define (make-Lagrangian kinetic-energy potential-energy)
  (- kinetic-energy potential-energy))

#|
(define ((L-free-particle mass) local)
  (let ((v (velocity local)))
    (* 1/2 mass (square v))))

(show-expression
 ((L-free-particle 'm)
  (->local 't
	   (coordinate-tuple 'x 'y 'z)
	   (velocity-tuple 'xdot 'ydot 'zdot))))
(+ (* 1/2 m (expt xdot 2))
   (* 1/2 m (expt ydot 2))
   (* 1/2 m (expt zdot 2)))

(show-expression
 ((compose
   (L-free-particle 'm)
   (Gamma (coordinate-tuple (literal-function 'x)
			    (literal-function 'y)
			    (literal-function 'z))))
  't))
(+ (* 1/2 (expt ((D x) t) 2) m)
   (* 1/2 (expt ((D y) t) 2) m)
   (* 1/2 (expt ((D z) t) 2) m))
|#

;;; Given a Lagrangian, we can obtain Lagrange's equations of motion.

(define ((Lagrange-equations Lagrangian #!optional dissipation-function) q)
  (let ((state-path (Gamma q)))
    (if (default-object? dissipation-function)
	(- (D (compose ((partial 2) Lagrangian) state-path))
	   (compose ((partial 1) Lagrangian) state-path))
	(- (D (compose ((partial 2) Lagrangian) state-path))
	   (compose ((partial 1) Lagrangian) state-path)
	   (- (compose ((partial 2) dissipation-function) state-path))))))


#|
(define ((Lagrange-equations Lagrangian) q)
  (let ((local-path (Gamma q)))
    (- (D (compose ((partial 2) Lagrangian) local-path))
       (compose ((partial 1) Lagrangian) local-path))))
|#


#|
(define (test-path t)
  (coordinate-tuple (+ (* 'a t) 'a0)
		    (+ (* 'b t) 'b0)
		    (+ (* 'c t) 'c0)))

(print-expression
 (((Lagrange-equations (L-free-particle 'm))
   test-path)
  't))
(down 0 0 0)

(show-expression
 (((Lagrange-equations (L-free-particle 'm))
   (literal-function 'x))
  't))
(* m (((expt D 2) x) t))

;;; For example, consider the Harmonic oscillator with
;;;  spring constant, k, and mass, m.

(define ((L-harmonic m k) local)
  (let ((q (coordinate local)) 
        (v (velocity local)))
    (- (* 1/2 m (square v))
       (* 1/2 k (square q)))))

(show-expression
 (((Lagrange-equations (L-harmonic 'm 'k))
   (literal-function 'x))
  't))
(+ (* k (x t)) (* m (((expt D 2) x) t)))

(show-expression
 (((Lagrange-equations (L-harmonic 'm 'k))
   (lambda (t) (* 'a (cos (+ (* 'omega t) 'phi)))))
  't))
(+ (* a k (cos (+ (* omega t) phi)))
   (* -1 a m (expt omega 2) (cos (+ (* omega t) phi))))
|#

#|
(define ((L-uniform-acceleration m g) local)
  (let ((q (coordinate local))
        (v (velocity local)))
    (let ((y (ref q 1)))
      (- (* 1/2 m (square v)) (* m g y)))))

(show-expression
 (((Lagrange-equations
    (L-uniform-acceleration 'm 'g))
   (coordinate-tuple (literal-function 'x)
		     (literal-function 'y)))
  't))
(down (* m (((expt D 2) x) t))
      (+ (* g m) (* m (((expt D 2) y) t))))


(define ((L-central-rectangular m V) local)
  (let ((q (coordinate local))
        (v (velocity local)))
    (- (* 1/2 m (square v))
       (V (sqrt (square q))))))

(show-expression
 (((Lagrange-equations 
    (L-central-rectangular 'm (literal-function 'V)))
   (coordinate-tuple (literal-function 'x) (literal-function 'y)))
  't))
(down
 (+ (* m (((expt D 2) x) t))
    (/ (* ((D V) (sqrt (+ (expt (x t) 2) (expt (y t) 2)))) (x t))
       (sqrt (+ (expt (x t) 2) (expt (y t) 2)))))
 (+ (* m (((expt D 2) y) t))
    (/ (* ((D V) (sqrt (+ (expt (x t) 2) (expt (y t) 2)))) (y t))
       (sqrt (+ (expt (x t) 2) (expt (y t) 2))))))
|#

#|
;;; Consider planar motion in a central force field, with an arbitrary
;;; potential, U, depending only on the radius.  The generalized
;;; coordinates are polar. 


(define ((L-central-polar m V) local)
  (let ((q (coordinate local))
        (qdot (velocity local)))
    (let ((r (ref q 0))
          (phi (ref q 1))
          (rdot (ref qdot 0))
          (phidot (ref qdot 1)))
      (- (* 1/2 m
           (+ (square rdot)
              (square (* r phidot))) )
         (V r)))))

(show-expression
 (((Lagrange-equations
    (L-central-polar 'm (literal-function 'V)))
   (coordinate-tuple (literal-function 'r)
		     (literal-function 'phi)))
  't))
(down
 (+ (* -1 m (r t) (expt ((D phi) t) 2))
    (* m (((expt D 2) r) t))
    ((D V) (r t)))
 (+ (* 2 m ((D r) t) (r t) ((D phi) t))
    (* m (((expt D 2) phi) t) (expt (r t) 2))))
|#

#|
;;; Coupled harmonic oscillators.

(define ((L-coupled-harmonic m k) state)
  (let ((q (coordinate state))
	(qdot (velocity state)))
    (- (* 1/2 qdot m qdot)
       (* 1/2 q k q))))

(show-expression
 (((Lagrange-equations
    (L-coupled-harmonic (down (down 'm_1 0) (down 0 'm_2))
			(down (down 'k_1 'c) (down 'c 'k_2))))
   (coordinate-tuple (literal-function 'x)
		     (literal-function 'y)))
  't))
(down (+ (* c (y t)) (* k_1 (x t)) (* m_1 (((expt D 2) x) t)))
      (+ (* c (x t)) (* k_2 (y t)) (* m_2 (((expt D 2) y) t))))
|#

#|
;;; Pendulum of mass m2 and length b, hanging from a support of mass
;;; m1 that is free to move horizontally (from Groesberg, Advanced
;;; Mechanics, p. 72) 

(define ((L-sliding-pend m1 m2 b g) state)
  (let ((q (coordinate state))
	(qdot (velocity state)))
    (let* ((x (ref q 0))
	   (xdot (ref qdot 0))
	   (theta (ref q 1))
	   (thetadot (ref qdot 1))
	   (rel-pend-vel
	    (* b thetadot (velocity-tuple (cos theta) (sin theta))))
	   (pend-vel (+ rel-pend-vel (velocity-tuple xdot 0)))
	   (Tpend (* 1/2 m2 (square pend-vel)))
	   (Tsupport (* 1/2 m1 (square xdot)))
	   (V (- (* m2 g b (cos theta)))))
      (+ Tpend Tsupport (- V)))))

(show-expression
 (((Lagrange-equations (L-sliding-pend 'm_1 'm_2 'b 'g))
   (coordinate-tuple (literal-function 'x)
		     (literal-function 'theta)))
  't))
(down
 (+ (* -1 b m_2 (sin (theta t)) (expt ((D theta) t) 2))
    (* b m_2 (((expt D 2) theta) t) (cos (theta t)))
    (* m_1 (((expt D 2) x) t))
    (* m_2 (((expt D 2) x) t)))
 (+ (* (expt b 2) m_2 (((expt D 2) theta) t))
    (* b g m_2 (sin (theta t)))
    (* b m_2 (((expt D 2) x) t) (cos (theta t)))))


;;; Nicer treatment

(define ((F-sliding-pend l) state)
  (let ((q (coordinate state)))
    (let ((x (ref q 0))
	  (theta (ref q 1)))
      (up (up x 0)
	  (up (+ x (* l (sin theta)))
	      (* -1 l (cos theta)))))))

(define ((2-free m1 m2 g) state)
  (let ((v1 (ref (velocity state) 0))
	(v2 (ref (velocity state) 1))
	(h1 (ref (coordinate state) 0 1))
	(h2 (ref (coordinate state) 1 1)))
    (- (+ (* 1/2 m1 (square v1))
	  (* 1/2 m2 (square v2)))
       (+ (* m1 g h1)
	  (* m2 g h2)))))

(define (L-sliding-pend m1 m2 l g)
  (compose (2-free m1 m2 g)
	   (F->C (F-sliding-pend l))))


(show-expression
 (((Lagrange-equations
    (L-sliding-pend 'm_1 'm_2 'b 'g))
   (up (literal-function 'x)
       (literal-function 'theta)))
  't))
(down
 (+ (* -1 b m_2 (sin (theta t)) (expt ((D theta) t) 2))
    (* b m_2 (((expt D 2) theta) t) (cos (theta t)))
    (* m_1 (((expt D 2) x) t))
    (* m_2 (((expt D 2) x) t)))
 (+ (* (expt b 2) m_2 (((expt D 2) theta) t))
    (* b g m_2 (sin (theta t)))
    (* b m_2 (cos (theta t)) (((expt D 2) x) t))))
|# 

#|
;;; Consider a simple pendulum with Rayleigh dissipation:

(define ((L-pendulum g m l) state)
  (let ((theta (coordinate state))
	(thetadot (velocity state)))
    (+ (* 1/2 m (square (* l thetadot)))
       (* g m l (cos theta)))))

(define ((Rayleigh-dissipation k) state)
  (let ((qdot (velocity state)))
    (* qdot k qdot)))

(show-expression
 (((Lagrange-equations (L-pendulum 'g 'm 'l)
		       (Rayleigh-dissipation 'k))
   (literal-function 'theta))
  't))
(+ (* 2 k ((D theta) t))
   (* g l m (sin (theta t)))
   (* (expt l 2) m (((expt D 2) theta) t)))
|#

#|
;;; Can group coordinates.  Procedures don't care.

(define ((L-two-particle m1 m2) local)
  (let ((x (coordinate local))
	(v (velocity local))
	(V (literal-function 'V (-> (X (^ Real 2) (^ Real 2)) Real))))
    (let ((x1 (ref x 0)) (x2 (ref x 1))
          (v1 (ref v 0)) (v2 (ref v 1)))
      (- (+ (* 1/2 m1 (square v1))
	    (* 1/2 m2 (square v2)))
	 (V x1 x2)))))

(show-expression
 (((Lagrange-equations (L-two-particle 'm_1 'm_2))
   (coordinate-tuple
    (coordinate-tuple (literal-function 'x_1) (literal-function 'y_1))
    (coordinate-tuple (literal-function 'x_2) (literal-function 'y_2))))
  't))
(down
 (down
  (+ (* m_1 (((expt D 2) x_1) t))
     (((partial 0 0) V) (up (x_1 t) (y_1 t)) (up (x_2 t) (y_2 t))))
  (+ (* m_1 (((expt D 2) y_1) t))
     (((partial 0 1) V) (up (x_1 t) (y_1 t)) (up (x_2 t) (y_2 t)))))
 (down
  (+ (* m_2 (((expt D 2) x_2) t))
     (((partial 1 0) V) (up (x_1 t) (y_1 t)) (up (x_2 t) (y_2 t))))
  (+ (* m_2 (((expt D 2) y_2) t))
     (((partial 1 1) V) (up (x_1 t) (y_1 t)) (up (x_2 t) (y_2 t))))))
|#

;;; For integrating Lagrange's equations we need them in a form which 
;;; has the highest derivative isolated.
;;; The following is an explicit solution for the second-derivative 
;;; from Lagrange's equations, based on the operator form above:

#|
(define (Lagrangian->acceleration Lagrangian)
  (let ((P ((partial 2) Lagrangian))
        (F ((partial 1) Lagrangian)))
    (let ((dP/dq ((partial 1) P))
          (dP/dqdot ((partial 2) P))
          (dP/dt ((partial 0) P))
	  (qdot state->qdot))
      (/ (- F (+ (* dP/dq qdot) dP/dt))
	 dP/dqdot))))

(define (Lagrangian->acceleration L)
  (let ((P ((partial 2) L))
        (F ((partial 1) L)))
    (/ (- F
          (+ ((partial 0) P) 
             (* ((partial 1) P) velocity)))
       ((partial 2) P))))

(define ((Lagrangian->acceleration L) state)
  (let ((P ((partial 2) L))
        (F ((partial 1) L)))
    (* (s:inverse (velocity state)
		  (((partial 2) P) state)
		  (velocity state))
       ((- F
	   (+ ((partial 0) P) 
	      (* ((partial 1) P) velocity)))
	state))))
|#

#|
;;; Note: the inverse may have a determinant of zero.

(define ((Lagrangian->acceleration L #!optional dissipation-function) state)
  (if (default-object? dissipation-function)
      (let ((P ((partial 2) L))
	    (F ((partial 1) L)))
	(* (s:inverse (velocity state)
		      (((partial 2) P) state)
		      (velocity state))
	   ((- F
	       (+ ((partial 0) P) 
		  (* ((partial 1) P) velocity)))
	    state)))
      (let ((P ((partial 2) L))
	    (F ((partial 1) L))
	    (Diss ((partial 2) dissipation-function)))
	(* (s:inverse (velocity state)
		      (((partial 2) P) state)
		      (velocity state))
	   ((- (- F Diss)
	       (+ ((partial 0) P) 
		  (* ((partial 1) P) velocity)))
	    state)))))

(define ((Lagrangian->acceleration L #!optional dissipation-function) state)
  (if (default-object? dissipation-function)
      (let ((P ((partial 2) L))
	    (F ((partial 1) L)))
	(let ((minv
	       (s:inverse (velocity state)
			  (((partial 2) P) state)
			  (velocity state))))
	  (* minv
	     ((- F
		 (+ ((partial 0) P) 
		    (* ((partial 1) P) velocity)))
	      state))))
      (let ((P ((partial 2) L))
	    (F ((partial 1) L))
	    (Diss ((partial 2) dissipation-function)))
	(let ((minv
	       (s:inverse (velocity state)
			  (((partial 2) P) state)
			  (velocity state))))

	  (* minv
	     ((- (- F Diss)
		 (+ ((partial 0) P) 
		    (* ((partial 1) P) velocity)))
	      state))))))
|#

(define ((Lagrangian->acceleration L #!optional dissipation-function) state)
  (let ((P ((partial 2) L))
	(F ((partial 1) L)))
    (if (default-object? dissipation-function)
	(solve-linear-left (((partial 2) P) state)
                           ((- F
                               (+ ((partial 0) P) 
                                  (* ((partial 1) P) velocity)))
                            state))
	(solve-linear-left (((partial 2) P) state)
                           ((- (- F
                                  ((partial 2) dissipation-function))
                               (+ ((partial 0) P) 
                                  (* ((partial 1) P) velocity)))
                            state)))))

#|
;;; Thus, for example, we can obtain the general form of the vector
;;; of accelerations as a function of the positions, and velocities:

(show-expression
 ((Lagrangian->acceleration (L-sliding-pend 'm_1 'm_2 'b 'g))
  (->local 't
	   (coordinate-tuple 'x 'theta)
	   (velocity-tuple 'xdot 'thetadot))))
(up
 (+
  (/ (* b m_2 (expt thetadot 2) (sin theta))
     (+ (* m_2 (expt (sin theta) 2)) m_1))
  (/ (* g m_2 (sin theta) (cos theta))
     (+ (* m_2 (expt (sin theta) 2)) m_1)))
 (+
  (/ (* -1 m_2 (expt thetadot 2) (sin theta) (cos theta))
     (+ (* m_2 (expt (sin theta) 2)) m_1))
  (/ (* -1 g m_1 (sin theta))
     (+ (* b m_2 (expt (sin theta) 2)) (* b m_1)))
  (/ (* -1 g m_2 (sin theta))
     (+ (* b m_2 (expt (sin theta) 2)) (* b m_1)))))
|#

;;; Lagrange equations in first-order form.

(define ((Lagrange-equations-1 L) q v)
  (let ((local-path (qv->local-path q v)))
    (- (D local-path)
       (compose (local-state-derivative L)
		local-path))))

(define ((local-state-derivative L) local)
  (->local 1
	   (velocity local)
	   ((Lagrangian->acceleration L) local)))

(define Lagrange-equations-first-order
        Lagrange-equations-1)

(define ((qv->local-path q v) t)
  (->local t (q t) (v t)))

#|
(define ((Lagrange-equations-1 L) q v)
  (let ((local-path (qv->local-path q v)))
    (- (D local-path)
       (->local 1
                (compose velocity local-path)
                (compose (Lagrangian->acceleration L)
                         local-path)))))
|#

#|
(show-expression
 (((Lagrange-equations-1 (L-harmonic 'm 'k))
   (coordinate-tuple (literal-function 'x)
                     (literal-function 'y))
   (velocity-tuple (literal-function 'v_x)
                   (literal-function 'v_y)))
  't))
(up 0
    (up (+ ((D x) t) (* -1 (v_x t))) (+ ((D y) t) (* -1 (v_y t))))
    (up (+ (/ (* k (x t)) m) ((D v_x) t)) (+ (/ (* k (y t)) m) ((D v_y) t))))
|#

#|
(define (Lagrangian->state-derivative L)
  (let ((acceleration (Lagrangian->acceleration L)))
    (lambda (state)
      (up
       1
       (velocity state)
       (acceleration state)))))
|#

(define (Lagrangian->state-derivative L #!optional dissipation-function)
  (if (default-object? dissipation-function)
      (let ((acceleration (Lagrangian->acceleration L)))
	(lambda (state)
	  (up
	   1
	   (velocity state)
	   (acceleration state))))
      (let ((acceleration (Lagrangian->acceleration L dissipation-function)))
	(lambda (state)
	  (up
	   1
	   (velocity state)
	   (acceleration state))))))

#|
(print-expression
 ((Lagrangian->state-derivative (L-pendulum 'g 'm 'l)
			       (Rayleigh-dissipation 'k))
  (up 't 'theta 'thetadot)))
(up 1
    thetadot
    (+ (/ (* -1 g (sin theta)) l)
       (/ (* -2 k thetadot) (* (expt l 2) m))))
|#

;;; Given a Lagrangian, we can make an energy function on (t, Q, Qdot). 

(define (Lagrangian->energy L)
  (let ((P ((partial 2) L)))
    (- (* P velocity) L)))


;;; On a trajectory there may be power lost (if dissipation)
;;;  The following produces the power lost.  

(define ((Lagrangian->power-loss L) q)
  (D (compose (Lagrangian->energy L)
	      (Gamma q))))

#|
;;; Alternatively

(define ((Lagrangian->power-loss L) q)
  (- (* ((Lagrange-equations L) q) (D q))
     (compose ((partial 0) L)
	      (Gamma q))))
|#

#|
;;; For example, on a specified trajectory, we can compute the energy,
;;; which turns out to be T+V.

(show-expression
 ((compose
   (Lagrangian->energy (L-central-polar 'm (literal-function 'U)))
   (Gamma 
    (coordinate-tuple (literal-function 'r) (literal-function 'phi))))
  't))
(+ (* 1/2 m (expt (r t) 2) (expt ((D phi) t) 2))
   (* 1/2 m (expt ((D r) t) 2))
   (U (r t)))


;;; In fact, we can see how the energy is conserved:

(show-expression
 (((Lagrangian->power-loss (L-central-polar 'm (literal-function 'U)))
   (coordinate-tuple (literal-function 'r) (literal-function 'phi)))
  't))
(+ (* m (((expt D 2) phi) t) ((D phi) t) (expt (r t) 2))
   (* m (expt ((D phi) t) 2) (r t) ((D r) t))
   (* m (((expt D 2) r) t) ((D r) t))
   (* ((D U) (r t)) ((D r) t)))

;;; This last expression is (nontrivially!) zero on any trajectory
;;; which satisfies Lagrange's equations.
|#

#|
;;; Note, this can be implemented in terms of T-CURVILINEAR.

(define ((T3-spherical m) local)
  (let ((t (time local))
        (q (coordinate local))
        (qdot (velocity local)))
    (let ((r (ref q 0))
          (theta (ref q 1))
          (phi (ref q 2))
          (rdot (ref qdot 0))
          (thetadot (ref qdot 1))
          (phidot (ref qdot 2)))
      (* 1/2 m
        (+ (square rdot)
           (square (* r thetadot))
           (square (* r (sin theta) phidot)))))))

(define (L3-central m Vr)
  (define (Vs local)
    (let ((r (ref (coordinate local) 0)))
      (Vr r)))
  (- (T3-spherical m) Vs))


(show-expression
 (((partial 1) (L3-central 'm (literal-function 'V)))
  (->local 't
           (coordinate-tuple 'r 'theta 'phi)
           (velocity-tuple 'rdot 'thetadot 'phidot))))
(down
 (+ (* m r (expt phidot 2) (expt (sin theta) 2))
    (* m r (expt thetadot 2))
    (* -1 ((D V) r)))
 (* m (expt r 2) (expt phidot 2) (cos theta) (sin theta))
 0)

(show-expression
 (((partial 2) (L3-central 'm (literal-function 'V)))
  (->local 't 
           (coordinate-tuple 'r 'theta 'phi)
           (velocity-tuple 'rdot 'thetadot 'phidot))))
(down (* m rdot)
      (* m (expt r 2) thetadot)
      (* m (expt r 2) phidot (expt (sin theta) 2)))
|#
