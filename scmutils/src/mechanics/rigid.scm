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

;;;; Chapter 2

;;; Generalized coordinates to angular velocities.

(define (m:antisymmetric? A)
  (m:zero? ((m:elementwise careful-simplify)
	    (matrix+matrix (transpose A) A))))

(define (antisymmetric->column-matrix A)
  (assert (m:antisymmetric? A))
  (column-matrix (matrix-ref A 2 1)
		 (matrix-ref A 0 2)
		 (matrix-ref A 1 0)))

(define (3vector-components->antisymmetric v)
  (matrix-by-rows
   (list 0 (- (ref v 2)) (ref v 1))
   (list (ref v 2) 0 (- (ref v 0)))
   (list (- (ref v 1)) (ref v 0) 0)))

(define (((M-of-q->omega-of-t M-of-q) q) t)
  (define M-on-path (compose M-of-q q))
  (define (omega-cross t)
    (* ((D M-on-path) t)
       (transpose (M-on-path t))))
  (antisymmetric->column-matrix (omega-cross t)))

(define (((M-of-q->omega-body-of-t M-of-q) q) t)
  (* (transpose (M-of-q (q t)))
     (((M-of-q->omega-of-t M-of-q) q) t)))

(define (M->omega M-of-q)
  (Gamma-bar (M-of-q->omega-of-t M-of-q)))

(define (M->omega-body M-of-q)
  (Gamma-bar (M-of-q->omega-body-of-t M-of-q)))


;;; Assuming omega-body is on principal axes,
;;; and A, B, C are the principal moments.
;;; Angular velocity to kinetic energy and angular momenta

(define ((T-body A B C) omega-body)
  (* 1/2
     (+ (* A (square (ref omega-body 0)))
	(* B (square (ref omega-body 1)))
	(* C (square (ref omega-body 2))))))

#|
(define ((L-body A B C) omega-body)
  (column-matrix (* A (ref omega-body 0))
		 (* B (ref omega-body 1))
		 (* C (ref omega-body 2))))
|#

(define ((L-body A B C) omega-body)
  (down (* A (ref omega-body 0))
	(* B (ref omega-body 1))
	(* C (ref omega-body 2))))

(define (((L-space M) A B C) omega-body)
  (* ((L-body A B C) omega-body)
     (transpose M)))

;;; Euler Angles

(define (Euler->M angles)
  (let ((theta (ref angles 0))
	(phi (ref angles 1))
	(psi (ref angles 2)))
    (* (rotate-z-matrix phi)
       (* (rotate-x-matrix theta)
	  (rotate-z-matrix psi)))))

(define ((Euler->omega angles-path) t)
  (define (M-on-path t)
    (Euler->M (angles-path t)))
  (define (w-cross t)
    (* ((D M-on-path) t)
       (transpose (M-on-path t))))
  (antisymmetric->column-matrix (w-cross t)))

(define ((Euler->omega-body angles-path) t)
  (* (transpose (Euler->M (angles-path t)))
     ((Euler->omega angles-path) t)))

#|
(show-expression
  ((Euler->omega-body
    (up (literal-function 'theta)
	(literal-function 'phi)
	(literal-function 'psi)))
   't))
(matrix-by-rows
 (list (+ (* (sin (theta t)) (sin (psi t)) ((D phi) t))
	  (* ((D theta) t) (cos (psi t)))))
 (list (+ (* (sin (theta t)) (cos (psi t)) ((D phi) t))
	  (* -1 ((D theta) t) (sin (psi t)))))
 (list (+ (* (cos (theta t)) ((D phi) t))
	  ((D psi) t))))
|#

#|
(show-expression
 (((M-of-q->omega-body-of-t Euler->M)
   (up (literal-function 'theta)
       (literal-function 'phi)
       (literal-function 'psi)))
  't))
(matrix-by-rows
 (list (+ (* (sin (theta t)) (sin (psi t)) ((D phi) t))
	  (* ((D theta) t) (cos (psi t)))))
 (list (+ (* (sin (theta t)) (cos (psi t)) ((D phi) t))
	  (* -1 ((D theta) t) (sin (psi t)))))
 (list (+ (* (cos (theta t)) ((D phi) t))
	  ((D psi) t))))

(show-expression
 ((M->omega-body Euler->M)
  (up 't 
      (up 'theta 'phi 'psi)
      (up 'thetadot 'phidot 'psidot))))
(matrix-by-rows
 (list (+ (* phidot (sin psi) (sin theta)) (* thetadot (cos psi))))
 (list (+ (* phidot (cos psi) (sin theta)) (* -1 thetadot (sin psi))))
 (list (+ (* phidot (cos theta)) psidot)))
|#

;;; Assuming Euler angles rotate principal axes from reference
;;; orientation.

#|
(define (Euler-state->omega-body local)
  (let ((q (coordinate local))
        (qdot (velocity local)))
    (let ((theta (ref q 0))
          (psi (ref q 2))
          (thetadot (ref qdot 0))
          (phidot (ref qdot 1))
          (psidot (ref qdot 2)))
      (let ((omega-a (+ (* thetadot (cos psi))
                        (* phidot (sin theta) (sin psi))))
            (omega-b (+ (* -1 thetadot (sin psi))
                        (* phidot (sin theta) (cos psi))))
            (omega-c (+ (* phidot (cos theta)) psidot)))
        (column-matrix omega-a omega-b omega-c)))))
|#

;;; Although this just appears to summarize (M->omega-body Euler->M)
;;;  it is actually essential to prevent intermediate expression
;;;  explosion.

(define (Euler-state->omega-body local)
  (let ((q (coordinate local))
        (qdot (velocity local)))
    (let ((theta (ref q 0))
          (psi (ref q 2))
          (thetadot (ref qdot 0))
          (phidot (ref qdot 1))
          (psidot (ref qdot 2)))
      (let ((omega-a (+ (* thetadot (cos psi))
                        (* phidot (sin theta) (sin psi))))
            (omega-b (+ (* -1 thetadot (sin psi))
                        (* phidot (sin theta) (cos psi))))
            (omega-c (+ (* phidot (cos theta)) psidot)))
        (up omega-a omega-b omega-c)))))

(define ((T-body-Euler A B C) local)
  ((T-body A B C)
   (Euler-state->omega-body local)))

(define T-rigid-body T-body-Euler)


(define ((L-body-Euler A B C) local)
  ((L-body A B C)
   (Euler-state->omega-body local)))

(define Euler-state->L-body L-body-Euler)


#| Wrong up/down
(define ((Euler-state->L-space A B C) local)
  (let ((angles (coordinate local)))
    (* (Euler->M angles)
       ((Euler-state->L-body A B C) local))))
|#

#|
(define ((Euler-state->L-space A B C) local)
  (let ((angles (coordinate local)))
    (((L-space (Euler->M angles)) A B C) 
     (Euler-state->omega-body local))))
|#

(define ((L-space-Euler A B C) local)
  (let ((angles (coordinate local)))
    (* ((L-body-Euler A B C) local)
       (transpose (Euler->M angles)))))

(define Euler-state->L-space L-space-Euler)

#|
(define an-Euler-state
  (up 't
      (up 'theta 'phi 'psi)
      (up 'thetadot 'phidot 'psidot)))

(show-expression
 (ref
   (((partial 2) (T-body-Euler 'A 'B 'C))
    an-Euler-state)
   1))
(+ (* A phidot (expt (sin psi) 2) (expt (sin theta) 2))
   (* B phidot (expt (cos psi) 2) (expt (sin theta) 2))
   (* A thetadot (cos psi) (sin psi) (sin theta))
   (* -1 B thetadot (cos psi) (sin psi) (sin theta))
   (* C phidot (expt (cos theta) 2))
   (* C psidot (cos theta)))

(print-expression
 (- (ref ((L-space-Euler 'A 'B 'C) an-Euler-state) 2)        ;$L_z$
    (ref (((partial 2) (T-body-Euler 'A 'B 'C)) an-Euler-state) 1)  ;$p_\phi$
    ))
0

(print-expression
 (determinant
  (((compose (partial 2) (partial 2)) 
    (T-body-Euler 'A 'B 'C))
   an-Euler-state)))
(* A B C (expt (sin theta) 2))
|#

(define (relative-error value reference-value)
  (if (zero? reference-value)
      (error "Zero reference value -- RELATIVE-ERROR")
      (/ (- value reference-value) reference-value)))

#|
(define (rigid-sysder A B C)
  (Lagrangian->state-derivative (T-body-Euler A B C)))

(define ((monitor-errors win A B C L0 E0) state)
  (let ((t (time state))
	(L ((L-space-Euler A B C) state))
	(E ((T-body-Euler A B C) state)))
    (plot-point win t (relative-error (ref L 0) (ref L0 0)))
    (plot-point win t (relative-error (ref L 1) (ref L0 1)))
    (plot-point win t (relative-error (ref L 2) (ref L0 2)))
    (plot-point win t (relative-error E E0))))


;;; rkqc4
;;;(set! *ode-integration-method* 'qcrk4)
;;;(define win (frame 0. 100. -1.e-12 1.e-12))

;;; bulirsch-stoer
(set! *ode-integration-method* 'bulirsch-stoer)
(define win (frame 0. 100. -2.e-13 2.e-13))

(let ((A 1.) (B (sqrt 2.)) (C 2.)
      (state0 (up 0.0
		  (up 1. 0. 0.)
		  (up 0.1 0.1 0.1))))
  (let ((L0 ((L-space-Euler A B C) state0))
	(E0 ((T-body-Euler A B C) state0)))
    ((evolve rigid-sysder A B C)
     state0
     (monitor-errors win A B C L0 E0)
     0.1
     100.0
     1.0e-12)))
#|
(up 99.99999999999864
    (up .6319896958334494 1.3610271540875034 17.437900484737938)
    (up -.12343716197181527 .09016109524808046 .07567921658605782))
|#

(graphics-clear win)
(graphics-close win)
|#

#|
(show-expression
 ((T-body-Euler 'A 'A 'C) 
   (up 't 
       (up 'theta 'phi 'psi)
       (up 'thetadot 'phidot 'psidot))))
(+ (* 1/2 A (expt phidot 2) (expt (sin theta) 2))
   (* 1/2 C (expt phidot 2) (expt (cos theta) 2))
   (* C phidot psidot (cos theta))
   (* 1/2 A (expt thetadot 2))
   (* 1/2 C (expt psidot 2)))

;;; Transformation of A(v):
;;;  M^T A(Mv) M = A(v) for arbitrary v orthogonal M

(print-expression
  (let ((Euler (up 'theta 'phi 'psi))
	(v (up 'x 'y 'z)))
    (let ((M (Euler->M Euler)))
      (- (* (3vector-components->antisymmetric (* M v))
	    M)
	 (* M
	    (3vector-components->antisymmetric v))))))
(matrix-by-rows (list 0 0 0) (list 0 0 0) (list 0 0 0))
|#

#| 
;;; Configuration equations for Euler's equations with Euler angles
   
(print-expression
  (let ((Euler (up (literal-function 'theta)
				 (literal-function 'phi)
				 (literal-function 'psi))))
    (antisymmetric->column-matrix 
     (* (transpose ((Euler->M Euler) 't))
	((D (Euler->M Euler)) 't)))))
(matrix-by-rows
 (list
  (+ (* ((D phi) t) (sin (psi t)) (sin (theta t)))
     (* ((D theta) t) (cos (psi t)))))
 (list
  (+ (* ((D phi) t) (sin (theta t)) (cos (psi t)))
     (* -1 (sin (psi t)) ((D theta) t))))
 (list (+ (* (cos (theta t)) ((D phi) t)) ((D psi) t))))
|#

#|
(define ((L-axisymmetric-top A C gMR) local)
  (let ((q (coordinate local))
        (qdot (velocity local)))
    (let ((theta (ref q 0))
          (thetadot (ref qdot 0))
          (phidot (ref qdot 1))
          (psidot (ref qdot 2)))
      (+ (* 1/2 A
            (+ (square thetadot)
               (square (* phidot (sin theta)))))
         (* 1/2 C
            (square (+ psidot (* phidot (cos theta)))))
         (* -1 gMR (cos theta))))))


(define ((V_eff p A C gMR) theta)
  (+ (/ (square p) (* 2 C))
     (* (/ (square p) (* 2 A))
	(square (tan (/ theta 2))))
     (* gMR (cos theta))))


;;; Critical value of bifurcation when D^2 V_eff (0) = 0

(print-expression
 (((square derivative) (V_eff 'p_c 'A 'C 'gMR)) 0))
(+ (* -1 gMR) (/ (* 1/4 (expt p_c 2)) A))

;;; critical angular speed in RPM is:
(* (/ 60 2pi) (/ 7.734804457773965e-3 6.6e-5))
;Value: 1119.1203302763215
|#

;;; Quaternion representation

(define (quaternion-state->omega-body s)
  (let ((q (coordinates s)) (qdot (velocities s)))
    (let* ((m^2 (dot-product q q)))
      (let ((omega^a 
             (/ (* 2 (dot-product q (* q:i qdot))) m^2))
            (omega^b 
             (/ (* 2 (dot-product q (* q:j qdot))) m^2))
            (omega^c 
             (/ (* 2 (dot-product q (* q:k qdot))) m^2)))
        (up omega^a omega^b omega^c)))))

(define (quaternion-state->omega-space s)
  (define q:a
    (matrix-by-rows (list  0 +1  0  0)
		    (list -1  0  0  0)
		    (list  0  0  0 +1)
		    (list  0  0 -1  0)))
  (define q:b
    (matrix-by-rows (list  0  0 +1  0)
		    (list  0  0  0 -1)
		    (list -1  0  0  0)
		    (list  0 +1  0  0)))
  (define q:c
    (matrix-by-rows (list  0  0  0 +1)
		    (list  0  0 +1  0)
		    (list  0 -1  0  0)
		    (list -1  0  0  0)))
  (let ((q (coordinates s))
	(qdot (velocities s)))
    (let ((Q (up->column-matrix q))
	  (QdotT (m:transpose (up->column-matrix qdot))))
      (let ((m^2 (ref (* (m:transpose Q) Q) 0 0)))
	(let ((omega^x (/ (ref (* -2 QdotT q:a Q) 0 0) m^2))
	      (omega^y (/ (ref (* -2 QdotT q:b Q) 0 0) m^2))
	      (omega^z (/ (ref (* -2 QdotT q:c Q) 0 0) m^2)))
	  (up omega^x omega^y omega^z))))))

(define ((qw-state->L-body A B C) qw-state)
  ((L-body A B C) (ref qw-state 2)))

(define ((qw-state->L-space A B C) qw-state)
  (let ((q (coordinates qw-state)))
    (let ((Lbody ((qw-state->L-body A B C) qw-state))
          (M (quaternion->rotation-matrix (make-quaternion q))))
      (* Lbody (transpose M)))))

(define ((T-quaternion-state A B C) s)
  (let ((q (coordinates s))
	(qdot (velocities s)))
    (let ((Q (up->column-matrix q))
	  (Qdot (up->column-matrix qdot)))
      (let ((m^2 (ref (* (m:transpose Q) Q) 0 0)))
	(let ((x (/ (* q:i Qdot) m^2))
	      (y (/ (* q:j Qdot) m^2))
	      (z (/ (* q:k Qdot) m^2))
	      (M (* Q (m:transpose Q))))
	  (* 2
	     (+ (* A (ref (* (m:transpose x) M x) 0 0))
		(* B (ref (* (m:transpose y) M y) 0 0))
		(* C (ref (* (m:transpose z) M z) 0 0)))))))))


#|
(define (qw-sysder A B C)
  (let ((B-C/A (/ (- B C) A))
        (C-A/B (/ (- C A) B))
        (A-B/C (/ (- A B) C)))
    (define (the-deriv qw-state)
      (let ((t (time qw-state))
            (q (coordinates qw-state))
            (omega-body (ref qw-state 2)))
        (let ((omega^a (ref omega-body 0))
              (omega^b (ref omega-body 1))
              (omega^c (ref omega-body 2)))
          (let ((tdot 1)
                (qdot      ;driven quaternion
                 (* -1/2
                     (+ (* omega^a q:i)
                        (* omega^b q:j)
                        (* omega^c q:k))
                     q))
                (omegadot  ;Euler's equations
                 (up (* B-C/A omega^b omega^c)
                     (* C-A/B omega^c omega^a)
                     (* A-B/C omega^a omega^b))))
            (up tdot qdot omegadot)))))
    the-deriv))

(define ((monitor-errors win A B C L0 E0) qw-state)
  (let ((t (time qw-state))
        (L ((qw-state->L-space A B C) qw-state))
        (E ((T-body A B C) (ref qw-state 2))))
    (plot-point win t (relative-error (ref L 0) (ref L0 0)))
    (plot-point win t (relative-error (ref L 1) (ref L0 1)))
    (plot-point win t (relative-error (ref L 2) (ref L0 2)))
    (plot-point win t (relative-error E E0))
    qw-state))

(define win (frame 0. 100. -1.e-13 1.e-13))

(let* ((A 1.) (B (sqrt 2.)) (C 2.)   ; moments of inertia
      (Euler-state (up 0.0           ; initial state
                       (up 1. 0. 0.)
                       (up 0.1 0.1 0.1)))
      (M (Euler->M (coordinates Euler-state)))
      (q (quaternion->vector (rotation-matrix->quaternion M)))
      (qw-state0 
       (up (time Euler-state)
           q
           (Euler-state->omega-body Euler-state))))
  (let ((L0 ((qw-state->L-space A B C) qw-state0))
        (E0 ((T-body A B C) (ref qw-state0 2))))
    ((evolve qw-sysder A B C)
     qw-state0
     (monitor-errors win A B C L0 E0)
     0.1                  ; step between plotted points
     100.0                ; final time
     1.0e-12)))
|#

