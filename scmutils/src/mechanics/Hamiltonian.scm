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

;;; Hamiltonian mechanics requires a phase
;;;   space QxP, and a function H:RxQxP --> R

;;; A system has a dynamic state, which has the time, the
;;; configuration, and the momenta.  Hamiltonian mechanics is
;;; formulated in terms of the dynamic state.

(define (->H-state t q p)
  (vector t q p))

(define (H-state? s)
  (and (up? s)
       (fix:= (s:length s) 3)
       (numerical-quantity? (ref s 0))
       (or (and (numerical-quantity? (ref s 1))
                (numerical-quantity? (ref s 2)))
           (and (up? (ref s 1))
                (down? (ref s 2))
                (= (s:dimension (ref s 1))
                   (s:dimension (ref s 2)))))))

(define (compatible-H-state? s)
  (and (down? s)
       (fix:= (s:length s) 3)
       (numerical-quantity? (ref s 0))
       (or (and (numerical-quantity? (ref s 1))
                (numerical-quantity? (ref s 2)))
           (and (down? (ref s 1))
                (up? (ref s 2))
                (= (s:dimension (ref s 1))
                   (s:dimension (ref s 2)))))))


(define (state->p state)
  (if (not (and (vector? state) (fix:> (vector-length state) 2)))
      (error "Cannot extract momentum from" state))
  (ref state 2))

(define momentum state->p)
(define momenta state->p)
(define P     state->p)


(define (state->qp dynamic-state)
  (vector-tail dynamic-state 1))

(define (literal-Hamiltonian-state n-dof)
  (up (literal-number (generate-uninterned-symbol 't))
      (s:generate n-dof 'up
		  (lambda (i)
		    (literal-number (generate-uninterned-symbol 'x))))
      (s:generate n-dof 'down
		  (lambda (i)
		    (literal-number (generate-uninterned-symbol 'p))))))


(define ((Lstate->Hstate L) Ls)
  (up (time Ls)
      (coordinate Ls)
      (((partial 2) L) Ls)))

(define ((Hstate->Lstate H) Hs)
  (up (time Hs)
      (coordinate Hs)
      (((partial 2) H) Hs)))

(define (H-state->matrix s)
  (s->m (compatible-shape s) s 1))

;; (define (matrix->H-state m)
;;   (assert (= (m:num-cols m) 1))
;;   (assert (and (odd? (m:num-rows m))
;; 	       (> (m:num-rows m) 2)))
;;   (let ((n (quotient (- (m:num-rows m) 1) 2)))
;;     (let ((s (up (generate-uninterned-symbol)
;; 		 (s:generate n 'up generate-uninterned-symbol)
;; 		 (s:generate n 'down generate-uninterned-symbol))))
;;       (m->s (compatible-shape s) m 1))))

(define (matrix->H-state m s)
  (assert (= (m:num-cols m) 1))
  (assert (and (odd? (m:num-rows m))
	       (> (m:num-rows m) 2)))
  (m->s (compatible-shape s) m 1))

(define (degrees-of-freedom H-state)
  (assert (= (s:length H-state) 3))
  (assert (= (s:dimension (coordinate H-state))
             (s:dimension (momentum H-state))))
  (s:dimension (coordinate H-state)))

#|
(matrix->H-state (H-state->matrix (up 't (up 'x 'y) (down 'p_x 'p_y))))
#|
(up t (up x y) (down p_x p_y))
|#

(H-state->matrix
 (matrix->H-state
  (matrix-by-rows (list 't)
		  (list 'x)
		  (list 'y)
		  (list 'p_x)
		  (list 'p_y))))
#|
(matrix-by-rows (list t) (list x) (list y) (list p_x) (list p_y))
|#
|#

(define (make-Hamiltonian kinetic-energy potential-energy)
  (+ kinetic-energy potential-energy))

(define ((Hamilton-equations Hamiltonian) q p)
  (let ((H-state-path (qp->H-state-path q p))
	(dH (Hamiltonian->state-derivative Hamiltonian)))
    (- (D H-state-path)
       (compose dH
                H-state-path))))

(define ((qp->H-state-path q p) t)
  (->H-state t (q t) (p t)))

(define ((Hamiltonian->state-derivative Hamiltonian) H-state)
  (->H-state 1
             (((partial 2) Hamiltonian) H-state)
             (- (((partial 1) Hamiltonian) H-state))))

;;; For compatibility with 1st edition
(define phase-space-derivative
  Hamiltonian->state-derivative)


(define ((D-phase-space H) s)
  (up 0 (((partial 2) H) s) (- (((partial 1) H) s))))

;;; If we express the energy in terms of t,Q,P we have the Hamiltonian.
;;; A Hamiltonian is an example of an H-function: an H-function takes
;;; 2 vector arguments and a scalar argument (t, Q, P).  It produces a
;;; scalar result.   

#|
(define ((H-rectangular m V) H-state)
  (let ((q (coordinate H-state))
        (p (momentum H-state)))
    (+ (/ (square p) (* 2 m))
       (V (ref q 0) (ref q 1)))))

(show-expression 
 (((Hamilton-equations
      (H-rectangular 
          'm
          (literal-function 'V (-> (X Real Real) Real))))
     (coordinate-tuple (literal-function 'x)
                       (literal-function 'y))
     (momentum-tuple (literal-function 'p_x)
                     (literal-function 'p_y)))
    't))
(up
 0
 (up (+ ((D x) t) (/ (* -1 (p_x t)) m))
     (+ ((D y) t) (/ (* -1 (p_y t)) m)))
 (down (+ ((D p_x) t) (((partial 0) V) (x t) (y t)))
       (+ ((D p_y) t) (((partial 1) V) (x t) (y t)))))
|#

;;; If we express the energy in terms of t,Q,P we have the Hamiltonian
;;;        H(t,Q,P) = P*Qdot - L(t, Q, Qdot(t, Q, P))
;;; To do this we need to invert P(t, Q, Qdot) to get Qdot(t, Q, P).
;;; This is easy when L is a quadratic form in Qdot:
;;;        L(t, Q, Qdot) = 1/2*Qdot*M*Qdot + B*Qdot - V
;;; Fortunately this is the case in almost all of Newtonian mechanics,
;;; otherwise the P(t,Q,Qdot) function would be much more difficult to
;;; invert to obtain Qdot(t,Q,P).

;;; Assume that F is quadratic in its arguments
;;;  F(u) = 1/2 A u u + b u + c
;;;  then v = A u + b, so u = A^(-1) (v - b)

#|
(define (Legendre-transform-procedure F)
  (let ((w-of-v (D F)))
    (define (G w)
      (let ((z (dual-zero w)))
        (let ((M ((D w-of-v) z))
              (b (w-of-v z)))
          (let ((v (/ (- w b) M)))
            (- (* w v) (F v))))))
    G))

(define (dual-zero v)
  (if (structure? v)
      (s:generate (s:length v) (s:opposite v)
		  (lambda (i) :zero))
      :zero))
|#

;;; A better definition of Legendre transform that works for
;;;   structured coordinates that have substructure

(define (Legendre-transform-procedure F)
  (let ((w-of-v (D F)))
    (define (G w)
      (let ((z (compatible-zero w)))
        (let ((M ((D w-of-v) z))
              (b (w-of-v z)))
	  ;; DM=0 for this code to be correct.
          (let ((v (solve-linear-left M (- w b))))
            (- (* w v) (F v))))))
    G))

(define Legendre-transform
  (make-operator Legendre-transform-procedure
                 'Legendre-transform))

;;; Notice that Lagrangians and Hamiltonians are symmetrical with
;;; respect to the Legendre transform.

(define ((Lagrangian->Hamiltonian-procedure the-Lagrangian) H-state)
  (let ((t (time H-state))
	(q (coordinate H-state))
	(p (momentum H-state)))
    (define (L qdot)
      (the-Lagrangian (->L-state t q qdot)))
    ((Legendre-transform-procedure L) p)))

(define Lagrangian->Hamiltonian
  (make-operator Lagrangian->Hamiltonian-procedure
                 'Lagrangian->Hamiltonian))


(define ((Hamiltonian->Lagrangian-procedure the-Hamiltonian) L-state)
  (let ((t (time L-state))
	(q (coordinate L-state))
	(qdot (velocity L-state)))
    (define (H p)
      (the-Hamiltonian (->H-state t q p)))
    ((Legendre-transform-procedure H) qdot)))

(define Hamiltonian->Lagrangian
  (make-operator Hamiltonian->Lagrangian-procedure
                 'Hamiltonian->Lagrangian))


#|
(define ((L-rectangular m V) local)
  (let ((q (coordinate local))
        (qdot (velocity local)))
    (- (* 1/2 m (square qdot))
       (V (ref q 0) (ref q 1)))))

(show-expression 
 ((Lagrangian->Hamiltonian
   (L-rectangular 'm
		  (literal-function 'V
				    (-> (X Real Real) Real))))
  (->H-state 't
             (coordinate-tuple 'x 'y)
             (momentum-tuple 'p_x 'p_y))))
(+ (V x y)
   (/ (* 1/2 (expt p_x 2)) m)
   (/ (* 1/2 (expt p_y 2)) m))
|#

#|
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
 ((Lagrangian->Hamiltonian 
    (L-central-polar 'm (literal-function 'V)))
  (->H-state 't
             (coordinate-tuple 'r 'phi)
             (momentum-tuple 'p_r 'p_phi))))
(+ (V r)
   (/ (* 1/2 (expt p_r 2)) m)
   (/ (* 1/2 (expt p_phi 2)) (* m (expt r 2))))

(show-expression
 (((Hamilton-equations
     (Lagrangian->Hamiltonian 
       (L-central-polar 'm (literal-function 'V))))
   (coordinate-tuple (literal-function 'r)
                     (literal-function 'phi))
   (momentum-tuple (literal-function 'p_r)
                   (literal-function 'p_phi)))
  't))
(up
 0
 (up (+ ((D r) t) (/ (* -1 (p_r t)) m))
     (+ ((D phi) t) (/ (* -1 (p_phi t)) (* m (expt (r t) 2)))))
 (down
  (+ ((D p_r) t)
     ((D V) (r t))
     (/ (* -1 (expt (p_phi t) 2)) (* m (expt (r t) 3))))
  ((D p_phi) t)))

;;; If we substitute a Coulomb potential in for V we get the equations 
;;;  for satellite motion around a spherical primary.

(show-expression
 (((Hamilton-equations
    (Lagrangian->Hamiltonian
     (L-central-polar 'm
		      (lambda (r)
			(- (/ (* 'GM 'm) r))))))
   (coordinate-tuple (literal-function 'r)
		     (literal-function 'phi))
   (momentum-tuple (literal-function 'p_r)
		   (literal-function 'p_phi)))
  't))
(up 0
    (up (+ ((D r) t) (/ (* -1 (p_r t)) m))
	(+ ((D phi) t) (/ (* -1 (p_phi t)) (* m (expt (r t) 2)))))
    (down
     (+ ((D p_r) t)
	(/ (* GM m) (expt (r t) 2))
	(/ (* -1 (expt (p_phi t) 2)) (* m (expt (r t) 3))))
     ((D p_phi) t)))

(define ((H-central-polar m V) state)
  (let ((q (coordinate state))
        (p (momentum state)))
    (let ((r ((component 0) q))
          (phi ((component 1) q))
          (pr ((component 0) p))
          (pphi ((component 1) p)))
      (+ (/ (+ (square pr)
	       (square (/ pphi r)))
	    (* 2 m))
         (V r)))))
|#

#|
(define ((L-harmonic m k) local)
  (let ((q (coordinate local)) 
        (v (velocity local)))
    (- (* 1/2 m (square v))
       (* 1/2 k (square q)))))

(show-expression
 (((Hamilton-equations
    (Lagrangian->Hamiltonian (L-harmonic 'm 'k)))
   (coordinate-tuple (literal-function 'x_1)
		     (literal-function 'x_2))
   (momentum-tuple (literal-function 'p_1)
		   (literal-function 'p_2)))
  't))
(up 0
    (up (+ ((D x_1) t) (/ (* -1 (p_1 t)) m_1))
	(+ ((D x_2) t) (/ (* -1 (p_2 t)) m_2)))
    (down (+ (* c (x_2 t)) (* k_1 (x_1 t)) ((D p_1) t))
	  (+ (* c (x_1 t)) (* k_2 (x_2 t)) ((D p_2) t))))

;;; Continuing with our coupled harmonic oscillators
;;;  we obtain the Hamiltonian:

(define ((L-coupled-harmonic m k) state)
  (let ((q (coordinate state))
	(qdot (velocity state)))
    (- (* 1/2 qdot m qdot)
       (* 1/2 q k q))))

(show-expression
 ((Lagrangian->Hamiltonian
   (L-coupled-harmonic (down (down 'm_1 0)
			     (down 0 'm_2))
		       (down (down 'k_1 'c)
			     (down 'c 'k_2))))
  (->H-state 't
	      (coordinate-tuple 'x_1 'x_2)
	      (momentum-tuple 'p_1 'p_2))))
(+ (* c x_1 x_2)
   (* 1/2 k_1 (expt x_1 2))
   (* 1/2 k_2 (expt x_2 2))
   (/ (* 1/2 (expt p_2 2)) m_2)
   (/ (* 1/2 (expt p_1 2)) m_1))

(show-expression
 (((Hamilton-equations
    (Lagrangian->Hamiltonian
     (L-coupled-harmonic (down (down 'm_1 0)
			       (down 0 'm_2))
			 (down (down 'k_1 'c)
			       (down 'c 'k_2)))))
   (coordinate-tuple (literal-function 'x_1)
		     (literal-function 'x_2))
   (momentum-tuple (literal-function 'p_1)
		   (literal-function 'p_2)))
  't))
(up
 0
 (up (+ ((D x_1) t) (/ (* -1 (p_1 t)) m_1))
     (+ ((D x_2) t) (/ (* -1 (p_2 t)) m_2)))
 (down (+ (* c (x_2 t)) (* k_1 (x_1 t)) ((D p_1) t))
       (+ (* c (x_1 t)) (* k_2 (x_2 t)) ((D p_2) t))))

|#

#|
;;; Continuation of demonstration of bundled coordinates.

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
 (((Hamilton-equations
    (Lagrangian->Hamiltonian
     (L-two-particle 'm_1 'm_2)))
   (coordinate-tuple (coordinate-tuple (literal-function 'x_1)
				       (literal-function 'y_1))
		     (coordinate-tuple (literal-function 'x_2)
				       (literal-function 'y_2)))
   (momentum-tuple (momentum-tuple (literal-function 'p_x_1)
				   (literal-function 'p_y_1))
		   (momentum-tuple (literal-function 'p_x_2)
				   (literal-function 'p_y_2))))
  't))
(up 0
    (up (up (+ ((D x_1) t) (/ (* -1 (px_1 t)) m_1))
	    (+ ((D y_1) t) (/ (* -1 (py_1 t)) m_1)))
	(up (+ ((D x_2) t) (/ (* -1 (px_2 t)) m_2))
	    (+ ((D y_2) t) (/ (* -1 (py_2 t)) m_2))))
    (down (down
	   (+ ((D px_1) t)
	      (((partial 0 0) V)
	       (up (x_1 t) (y_1 t)) (up (x_2 t) (y_2 t))))
	   (+ ((D py_1) t)
	      (((partial 0 1) V)
	       (up (x_1 t) (y_1 t)) (up (x_2 t) (y_2 t)))))
	  (down
	   (+ ((D px_2) t)
	      (((partial 1 0) V)
	       (up (x_1 t) (y_1 t)) (up (x_2 t) (y_2 t))))
	   (+ ((D py_2) t)
	      (((partial 1 1) V)
	       (up (x_1 t) (y_1 t)) (up (x_2 t) (y_2 t)))))))
|#

#|
;;; From 3.3 -- Phase-space reduction


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


(show-expression
 ((Lagrangian->Hamiltonian (L-axisymmetric-top 'A 'C 'gMR)) 
  (->H-state 't
             (vector 'theta 'phi 'psi)
             (vector 'p_theta 'p_phi 'p_psi))))
(+ (* gMR (cos theta))
   (/ (* 1/2 (expt p_psi 2)) C)
   (/ (* 1/2 (expt p_psi 2) (expt (cos theta) 2)) (* A (expt (sin theta) 2)))
   (/ (* 1/2 (expt p_theta 2)) A)
   (/ (* -1 p_phi p_psi (cos theta)) (* A (expt (sin theta) 2)))
   (/ (* 1/2 (expt p_phi 2)) (* A (expt (sin theta) 2))))
|#

#|
;;; ********** This got ugly *************

(show-expression
 (((Hamilton-equations
    (Lagrangian->Hamiltonian
     (L-axisymmetric-top 'A 'C 'gMR)))
   (coordinate-tuple (literal-function 'theta)
		     (literal-function 'phi)
		     (literal-function 'psi))
   (momentum-tuple (literal-function 'p_theta)
		   (literal-function 'p_phi)
		   (literal-function 'p_psi)))
  't))
(up
 0
 (up
  (+ ((D theta) t) (/ (* -1 (p_theta t)) A))
  (+ ((D phi) t)
     (/ (* (cos (theta t)) (p_psi t)) (* A (expt (sin (theta t)) 2)))
     (/ (* -1 (p_phi t)) (* A (expt (sin (theta t)) 2))))
  (+
   ((D psi) t)
   (/ (* -1 (p_psi t)) C)
   (/ (* -1 (expt (cos (theta t)) 2) (p_psi t)) (* A (expt (sin (theta t)) 2)))
   (/ (* (p_phi t) (cos (theta t))) (* A (expt (sin (theta t)) 2)))))
 (down
  (+
   (/ (* -1 gMR (expt (cos (theta t)) 4)) (expt (sin (theta t)) 3))
   ((D p_theta) t)
   (/ (* 2 gMR (expt (cos (theta t)) 2)) (expt (sin (theta t)) 3))
   (/ (* (p_phi t) (expt (cos (theta t)) 2) (p_psi t))
      (* A (expt (sin (theta t)) 3)))
   (/ (* -1 (cos (theta t)) (expt (p_psi t) 2)) (* A (expt (sin (theta t)) 3)))
   (/ (* -1 (expt (p_phi t) 2) (cos (theta t))) (* A (expt (sin (theta t)) 3)))
   (/ (* -1 gMR) (expt (sin (theta t)) 3))
   (/ (* (p_phi t) (p_psi t)) (* A (expt (sin (theta t)) 3))))
  ((D p_phi) t)
  ((D p_psi) t)))
|#

;;; The Poisson Bracket is a differential operator on H-functions:
#|
;;; This can give WRONG ANSWERS on structure-valued functions...
(define (Poisson-bracket f g)
  (- (* ((partial 1) f) ((partial 2) g))
     (* ((partial 2) f) ((partial 1) g))))

(define (Poisson-bracket f g)
  (cond ((and (structure? f) (structure? g))
	 (s:map/r (lambda (fi)
		    (s:map/r (lambda (gj)
			       (Poisson-bracket fi gj))
			     g))
		  f)
	 #;(error "Poisson bracket of two structures" f g)
	 )
	((structure? f)
	 (s:generate (s:length f) (s:same f)
		     (lambda (i)
		       (Poisson-bracket (ref f i) g))))
	((structure? g)
	 (s:generate (s:length g) (s:same g)
		     (lambda (i)
		       (Poisson-bracket f (ref g i)))))
	(else
	 (- (* ((partial 1) f) ((partial 2) g))
	    (* ((partial 2) f) ((partial 1) g))))))
|#

(define ((Poisson-bracket f g) x)
  (let ((fx (f x)) (gx (g x)))
    (cond ((or (structure? fx) (structure? gx))
	   (s:map/r (lambda (af)
		      (s:map/r (lambda (ag)
				 ((Poisson-bracket
				   (compose (apply component af) f)
				   (compose (apply component ag) g))
				  x))
			       (structure->access-chains gx)))
		    (structure->access-chains fx)))
	  (else
	   ((- (* ((partial 1) f) ((partial 2) g))
	       (* ((partial 2) f) ((partial 1) g)))
	    x)))))

#|
(pe ((Poisson-bracket
      (up (compose (component 0) coordinate)
	  (compose (component 1) coordinate)
	  (compose (component 2) coordinate))
      (down (compose (component 0) momentum)
	    (compose (component 1) momentum)
	    (compose (component 2) momentum)))
     a-state))
(up (down 1 0 0) (down 0 1 0) (down 0 0 1))
|#

#|
(define FF
  (literal-function 'F
		    (-> (UP Real
			    (UP Real Real)
			    (DOWN Real Real))
			Real)))

(define GG
  (literal-function 'G
		    (-> (UP Real
			    (UP Real Real)
			    (DOWN Real Real))
			Real)))

(pe ((* (D FF)
	(Poisson-bracket identity identity)
	(D GG))
     (up 't (up 'x 'y) (down 'px 'py))))
(+ (* -1
      (((partial 1 0) G) (up t (up x y) (down px py)))
      (((partial 2 0) F) (up t (up x y) (down px py))))
   (* -1
      (((partial 1 1) G) (up t (up x y) (down px py)))
      (((partial 2 1) F) (up t (up x y) (down px py))))
   (* (((partial 2 0) G) (up t (up x y) (down px py)))
      (((partial 1 0) F) (up t (up x y) (down px py))))
   (* (((partial 2 1) G) (up t (up x y) (down px py)))
      (((partial 1 1) F) (up t (up x y) (down px py)))))
|#

#|
(define F (literal-function 'F (Hamiltonian 2)))
(define G (literal-function 'G (Hamiltonian 2)))
(define H (literal-function 'H (Hamiltonian 2)))

;;; Jacobi identity
(pe ((+ (Poisson-bracket F (Poisson-bracket G H))
	(Poisson-bracket G (Poisson-bracket H F))
	(Poisson-bracket H (Poisson-bracket F G)))
     (up 't (up 'x 'y) (down 'px 'py))))
0
|#

#|
(define Sx (compose (component 0) coordinate))
(define Sy (compose (component 1) coordinate))
(define Sz (compose (component 2) coordinate))

(define Spx (compose (component 0) momentum))
(define Spy (compose (component 1) momentum))
(define Spz (compose (component 2) momentum))

;;; for example L = [Lx, Ly, Lz] 
;;; where Li are components of angular momentum

(define Lx (- (* Sy Spz) (* Spy Sz)))
(define Ly (- (* Sz Spx) (* Spz Sx)))
(define Lz (- (* Sx Spy) (* Spx Sy)))
(define L (down Lx Ly Lz))

(define 3-state 
  (->H-state 't 
	     (coordinate-tuple 'x 'y 'z)
	     (momentum-tuple 'p_x 'p_y 'p_z)))

(pe ((Poisson-bracket Lx L) 3-state))
(down 0 (+ (* -1 p_x y) (* p_y x)) (+ (* -1 p_x z) (* p_z x)))

;;; Poisson brackets are compositional with canonical transformations
;;; (see point-transformations.scm for F->CT and time-varying.scm for
;;; C-rotating, repeated here.

(define ((rotating n) state)
  (let ((t (time state))
	(q (coordinate state)))
    (let ((x (ref q 0))
	  (y (ref q 1))
	  (z (ref q 2)))
      (coordinate-tuple (+ (* (cos (* n t)) x) (* (sin (* n t)) y))
			(- (* (cos (* n t)) y) (* (sin (* n t)) x))
			z))))

(define (C-rotating n) (F->CT (rotating n)))

(pe ((- (compose (Poisson-bracket Lx Ly) (C-rotating 'n))
	(Poisson-bracket (compose Lx (C-rotating 'n))
			 (compose Ly (C-rotating 'n))) )
     3-state))
0
|#

#|
;;; Poisson brackets in terms of J 
;;;  Guaranteed to work only for scalar valued functions

#| 
;;; From canonical.scm

(define (J-func DH)
  (->H-state 0
	     (ref DH 2)
	     (- (ref DH 1))))
|#

(define ((PB f g) s)
  (* ((D f) s) (J-func ((D g) s))))

(define a-state 
  (->H-state 't 
	     (coordinate-tuple 'x 'y 'z)
	     (momentum-tuple 'p_x 'p_y 'p_z)))

(pe ((- (Poisson-bracket Lx Ly) Lz) a-state))
0
(pe ((- (PB Lx Ly) Lz) a-state))
0

(define ((PB f g) s)
  (let ((J ((D J-func) ((D g) s))))
    (* ((D f) s) (* J ((D g) s)))))

(define ((PB f g) s)
  (let ((J (linear-function->multiplier J-func ((D g) s))))
    (* ((D f) s) (* J ((D g) s)))))

(pe 
 (- ((Poisson-bracket (H-harmonic 'm 'k)
		      ((component 0) coordinate)) 
     a-state)
    ((PB (H-harmonic 'm 'k)
	 (compose (component 0) coordinate))
     a-state)))
0

(pe 
 (- ((Poisson-bracket (H-harmonic 'm 'k) coordinate) 
     a-state)
    ((PB (H-harmonic 'm 'k) coordinate)
     a-state)))
(up 0 0 0)

(pe ((PB momentum (H-harmonic 'm 'k))
     a-state))
(down (* -1 k x) (* -1 k y) (* -1 k z))

(pe ((PB coordinate (H-harmonic 'm 'k))
     a-state))
(up (/ p_x m) (/ p_y m) (/ p_z m))
|#

(define (commutator op1 op2)
  (- (* op1 op2) (* op2 op1)))

(define (anticommutator op1 op2)
  (+ (* op1 op2) (* op2 op1)))


;;; We define the Lie derivative of F, as a derivative-like operator,
;;;  relative to the given Hamiltonian-like function, H.
;;; Generalization and redefinition in calculus/Lie.scm

(define (Lie-derivative H)
  (make-operator
   (lambda (F)
     (Poisson-bracket F H))
   `(Lie-derivative ,H)))

;;; the flow derivative generalizes the Lie derivative to 
;;; allow for time dependent H and F ---
;;; computes the "time" derivative of F along the flow specified by H

(define (flow-derivative H)
  (make-operator 
   (lambda (F)
     (+ ((partial 0) F)
	(Poisson-bracket F H)))
   `(flow-derivative ,H)))

#|
;;; for Lie derivatives

(define F (literal-function 'F (Hamiltonian 2)))
(define G (literal-function 'G (Hamiltonian 2)))

(define L_F (Lie-derivative F))
(define L_G (Lie-derivative G))

(pe (((+ (commutator L_F L_G)
	 (Lie-derivative (Poisson-bracket F G)))
      H)
     (up 't (up 'x 'y) (down 'px 'py))))
0
|#
