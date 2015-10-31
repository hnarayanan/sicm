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

;;;; Hodge-star dual

;;; spec may be a coordinate system with an orthonormal basis
;;;             an orthonormal basis
;;;             a basis

;;; if the spec is a basis that needs to be orthonormalized, 
;;; the optional orthonormalize? argument must be a coordinate system

(define (Hodge-star metric spec #!optional orthonormalize?)  
  (let* ((basis
	  (if (basis? spec)
	      (if (default-object? orthonormalize?)
		  spec
		  ;; orthonormalize? must be a coordinate system
		  (orthonormalize spec metric orthonormalize?))
	      ;; spec must be a coordinate system.
	      (if (default-object? orthonormalize?)
		  (coordinate-system->basis spec)
		  (orthonormalize (coordinate-system->basis spec)
				  metric
				  spec))))
	 (vector-basis (basis->vector-basis basis))
	 (on-vector-basis (ultra-flatten vector-basis))
	 (basis-check
	  (matrix-by-row-list
	   (map (lambda (ei) (map (lambda (ej) (metric ei ej))
				  on-vector-basis))
		on-vector-basis)))
	 (bsigns
	  (make-initialized-list (basis->dimension basis)
				 (lambda (i) (matrix-ref basis-check i i))))
	 (on-1form-basis (ultra-flatten (basis->1form-basis basis))))
    (define (the-star pform-field)
      (assert (or (function? pform-field) (form-field? pform-field)))
      (let ((p (get-rank pform-field)))
	(if (= p 0)
	    (* pform-field (apply wedge on-1form-basis))
	    (let* ((pvect-basis-lists
		    (combinations on-vector-basis p))
		   (coeffs
		    (map (lambda (pvect)
			   (apply pform-field pvect))
			 pvect-basis-lists))
		   (pform-basis-lists
		    (combinations on-1form-basis p))
		   (n-p:form-basis-lists
		    (map (lambda (1fbl)
			   (list-difference on-1form-basis 1fbl))
			 pform-basis-lists))
		   (n-p:basis
		    (map (lambda (n-p:basis-list)
			   (apply wedge n-p:basis-list))
			 n-p:form-basis-lists))
		   (signs
		    (map (lambda (bsign-list p:basis-list n-p:basis-list)
			   (* (apply * bsign-list)
			      (permutation-parity
			       (append p:basis-list n-p:basis-list)
			       on-1form-basis)))
			 (combinations bsigns p)
			 pform-basis-lists
			 n-p:form-basis-lists))
		   (val
		    (apply +
			   (map (lambda (sign coeff basis-element)
				  (* sign coeff basis-element))
				signs
				coeffs
				n-p:basis))))
	      val))))
    ;;(assert (orthonormal? basis-check))  ;Currently assumed OK.
    the-star))

(define (orthonormalize basis metric coordinate-system)
    (let ((ovb (Gram-Schmidt (basis->vector-basis basis) metric)))
      (make-basis ovb
		  (vector-basis->dual ovb coordinate-system))))

#|
(define-coordinates (up x y) R2-rect)

(define (E2-metric v1 v2)
  (+ (* (dx v1) (dx v2))
     (* (dy v1) (dy v2))))
#| E2-metric |#

(define omega (wedge dx dy))
#| omega |#

(define E2-star
  (Hodge-star E2-metric
	      (coordinate-system->basis R2-rect)))
#| E2-star |#

((E2-star omega)
 ((point R2-rect) (up 'x 'y)))
#| 1 |#

;;; What is a rank 0 form?


(((E2-star dx)
  (literal-vector-field 'V R2-rect))
 ((point R2-rect) (up 'x 'y)))
#|
(V^1 (up x y))
|#

(((E2-star dy)
  (literal-vector-field 'V R2-rect))
 ((point R2-rect) (up 'x 'y)))
#|
(* -1 (V^0 (up x y)))
|#

(((E2-star (lambda (pt) 1))
  (literal-vector-field 'V R2-rect)
  (literal-vector-field 'W R2-rect))
 ((point R2-rect) (up 'x 'y)))
#|
(+ (* (V^0 (up x y)) (W^1 (up x y)))
   (* -1 (V^1 (up x y)) (W^0 (up x y))))
|#
|#

#|
;;; First, some simple tests on 3-dimensional Euclidean space.
(clear-arguments)
(suppress-arguments (list '(up x0 y0 z0)))

(define-coordinates (up x y z) R3-rect)
(define R3-point ((R3-rect '->point) (up 'x0 'y0 'z0)))
(define R3-basis (coordinate-system->basis R3-rect))

(define (E3-metric v1 v2)
  (+ (* (dx v1) (dx v2))
     (* (dy v1) (dy v2))
     (* (dz v1) (dz v2))))

(define E3-star (Hodge-star E3-metric R3-rect))

#|
(define E3-star
  (Hodge-star E3-metric
	      (coordinate-system->basis R3-rect)))
|#

(((- (E3-star (lambda (pt) 1))
     (wedge dx dy dz))
       (literal-vector-field 'u R3-rect)
       (literal-vector-field 'v R3-rect)
       (literal-vector-field 'w R3-rect))
      R3-point)
#| 0 |#

(((- (E3-star dx)
     (wedge dy dz))
  (literal-vector-field 'u R3-rect)
  (literal-vector-field 'v R3-rect))
 R3-point)
#| 0 |#


(((+ (E3-star (wedge dx dz)) dy)
  (literal-vector-field 'u R3-rect))
 R3-point)
#| 0 |#


((- (E3-star (wedge dx dy dz)) 1)
 R3-point)
#| 0 |#


(pec (((E3-star (literal-scalar-field 'f R3-rect))
       (literal-vector-field 'u R3-rect)
       (literal-vector-field 'v R3-rect)
       (literal-vector-field 'w R3-rect))
      R3-point)
     (compose arg-suppressor simplify))
#| Result:
(+ (* w^2 u^0 f v^1)
   (* -1 w^2 u^1 v^0 f)
   (* -1 u^0 v^2 w^1 f)
   (* v^2 u^1 w^0 f)
   (* u^2 w^1 v^0 f)
   (* -1 u^2 w^0 f v^1))
|#

(pec (((E3-star (literal-1form-field 'omega R3-rect))
       (literal-vector-field 'u R3-rect)
       (literal-vector-field 'v R3-rect))
      R3-point)
     (compose arg-suppressor simplify))
#| Result:
(+ (* v^1 u^0 omega_2)
   (* -1 v^1 u^2 omega_0)
   (* -1 v^2 u^0 omega_1)
   (* v^2 u^1 omega_0)
   (* u^2 v^0 omega_1)
   (* -1 u^1 v^0 omega_2))
|#

(pec (((E3-star
	(+ (* (literal-scalar-field 'alpha R3-rect) (wedge dx dy))
	   (* (literal-scalar-field 'beta R3-rect) (wedge dy dz))
	   (* (literal-scalar-field 'gamma R3-rect) (wedge dz dx))))
       (literal-vector-field 'u R3-rect))
      R3-point)
     (compose arg-suppressor simplify))
#| Result:
(+ (* u^0 beta) (* u^2 alpha) (* u^1 gamma))
|#

(pec ((E3-star
       (* (literal-scalar-field 'alpha R3-rect) (wedge dx dy dz)))
      R3-point)
     (compose arg-suppressor simplify))
#| Result:
alpha
|#

(define omega
  (+ (* (literal-scalar-field 'alpha R3-rect)  dx)
     (* (literal-scalar-field 'beta R3-rect)   dy)
     (* (literal-scalar-field 'gamma R3-rect)  dz)))
;;; omega = alpha*dx + beta*dy + gamma*dz

(pec (((E3-star omega)
       (literal-vector-field 'u R3-rect)
       (literal-vector-field 'v R3-rect))
      R3-point)
     (compose arg-suppressor simplify))
#| Result:
(+ (* v^1 u^0 gamma)
   (* -1 v^1 u^2 alpha)
   (* -1 v^2 u^0 beta)
   (* v^2 u^1 alpha)
   (* u^2 v^0 beta)
   (* -1 u^1 v^0 gamma))
|#
;;; *omega = alpha*dy^dz - beta*dx^dz + gamma*dx^dy


(pec (((E3-star (d omega))
       (literal-vector-field 'u R3-rect))
      R3-point)
     (compose arg-suppressor simplify))
#| Result:
(+ (* u^0 ((partial 1) gamma))
   (* -1 u^0 ((partial 2) beta))
   (* u^2 ((partial 0) beta))
   (* -1 u^2 ((partial 1) alpha))
   (* -1 u^1 ((partial 0) gamma))
   (* u^1 ((partial 2) alpha)))
|#
;;; Indeed, *d is the curl operator.

(pec (((d (E3-star omega))
       (literal-vector-field 'u R3-rect)
       (literal-vector-field 'v R3-rect)
       (literal-vector-field 'w R3-rect))
      R3-point)
     (compose arg-suppressor simplify))
#| Result:
(+ (* w^2 v^1 u^0 ((partial 0) alpha))
   (* w^2 v^1 u^0 ((partial 1) beta))
   (* w^2 v^1 u^0 ((partial 2) gamma))
   (* -1 w^2 u^1 v^0 ((partial 0) alpha))
   (* -1 w^2 u^1 v^0 ((partial 1) beta))
   (* -1 w^2 u^1 v^0 ((partial 2) gamma))
   (* -1 v^1 w^0 u^2 ((partial 0) alpha))
   (* -1 v^1 w^0 u^2 ((partial 1) beta))
   (* -1 v^1 w^0 u^2 ((partial 2) gamma))
   (* -1 v^2 w^1 u^0 ((partial 0) alpha))
   (* -1 v^2 w^1 u^0 ((partial 1) beta))
   (* -1 v^2 w^1 u^0 ((partial 2) gamma))
   (* v^2 w^0 u^1 ((partial 0) alpha))
   (* v^2 w^0 u^1 ((partial 1) beta))
   (* v^2 w^0 u^1 ((partial 2) gamma))
   (* w^1 u^2 v^0 ((partial 0) alpha))
   (* w^1 u^2 v^0 ((partial 1) beta))
   (* w^1 u^2 v^0 ((partial 2) gamma)))
|#

(pec ((E3-star (d (E3-star omega)))
      R3-point)
     (compose arg-suppressor simplify))
#| Result:
(+ ((partial 0) alpha) ((partial 1) beta) ((partial 2) gamma))
|#
;;; Indeed, *d* is the divergence operator...

(clear-arguments)
|#

#|
;;; Now for a 2+1 Minkowski space with c=1.

(define-coordinates (up t x y) R3-rect)
(define R3-point
  ((R3-rect '->point) (up 't0 'x0 'y0)))
(define R3-basis
  (coordinate-system->basis R3-rect))

(define (L3-metric u v)
  (+ (* -1 (dt u) (dt v))
     (* (dx u) (dx v))
     (* (dy u) (dy v))))

(define L3-star
  (Hodge-star L3-metric R3-rect))

((L3-metric d/dt d/dt) R3-point)
#| -1 |#


(((- (L3-star (lambda (m) 1))
     (wedge dx dy dt))
  (literal-vector-field 'U R3-rect)
  (literal-vector-field 'V R3-rect)
  (literal-vector-field 'W R3-rect))
 R3-point)
#| 0 |#

(((- (L3-star dx)
     (wedge dy dt))
  (literal-vector-field 'U R3-rect)
  (literal-vector-field 'V R3-rect))
 R3-point)
#| 0 |#

(((- (L3-star dy)
     (wedge dt dx))
  (literal-vector-field 'U R3-rect)
  (literal-vector-field 'V R3-rect))
 R3-point)
#| 0 |#


(((- (L3-star dt)
     (wedge dy dx))
  (literal-vector-field 'U R3-rect)
  (literal-vector-field 'V R3-rect))
 R3-point)
#| 0 |#


(((- (L3-star (wedge dx dy)) dt)
  (literal-vector-field 'U R3-rect))
 R3-point)
#| 0 |#


(((+ (L3-star (wedge dy dt)) dx)
  (literal-vector-field 'U R3-rect))
 R3-point)
#| 0 |#


(((+ (L3-star (wedge dt dx)) dy)
  (literal-vector-field 'U R3-rect))
 R3-point)
#| 0 |#

((+ (L3-star (wedge dx dy dt)) 1)
 R3-point)
#| 0 |#
|#

#|
;;; Now for a 1-1 Minkowski space with c.

(define-coordinates (up t x) R2-rect)
(define R2-point ((R2-rect '->point) (up 't0 'x0)))
(define R2-basis (coordinate-system->basis R2-rect))
(define c 'c)

(define (L2-metric u v)
  (+ (* -1 c c (dt u) (dt v))
     (* 1 (dx u) (dx v))))


(define L2-Hodge-star
  (Hodge-star L2-metric R2-rect))

(pec (((L2-Hodge-star (lambda (x) 1))
       (literal-vector-field 'u R2-rect)
       (literal-vector-field 'v R2-rect))
      R2-point))
#| Result:
(+ (* (u^0 (up t0 x0)) (v^1 (up t0 x0)))
   (* -1 (u^1 (up t0 x0)) (v^0 (up t0 x0))))
|#
;;; Wrong.  Must generally orthonormalize.


(define L2-Hodge-star
  (Hodge-star L2-metric R2-rect #t))

(pec (((L2-Hodge-star (lambda (x) 1))
       (literal-vector-field 'u R2-rect)
       (literal-vector-field 'v R2-rect))
      R2-point))
#| Result:
(+ (* c (u^0 (up t0 x0)) (v^1 (up t0 x0)))
   (* -1 c (v^0 (up t0 x0)) (u^1 (up t0 x0))))
  = cdt^dx(u v)
|#

#|
;;; Can accelerate by explicitly passing in an explicitly constructed
;;; orthonormal constant basis.

(define L2-basis (orthonormalize R2-basis L2-metric R2-rect))

(define L2-vector-basis (basis->vector-basis L2-basis))

(s:foreach (lambda (v)
	     (pe ((v (literal-manifold-function 'f R2-rect))
		  R2-point)))
	   L2-vector-basis)
#|
(/ (((partial 0) f) (up t0 x0)) c)
(((partial 1) f) (up t0 x0))
|#

(define L2-1form-basis (vector-basis->dual L2-vector-basis R2-rect))

(s:foreach (lambda (omega)
	     (pe ((omega (literal-vector-field 'v R2-rect))
		  R2-point)))
	   L2-1form-basis)
#|
(* c (v^0 (up t0 x0)))
(v^1 (up t0 x0))
|#

(pec ((L2-1form-basis L2-vector-basis) R2-point))
#| Result:
(up (down 1 0) (down 0 1))
|#

;;; Now make constant basis...

(define L2-constant-vector-basis
  (down (* (/ 1 c) d/dt) d/dx))

(define L2-constant-1form-basis
  (up (* c dt) dx))

(define L2-constant-basis
  (make-basis L2-constant-vector-basis
	      L2-constant-1form-basis))

(define L2-Hodge-star
  (Hodge-star L2-metric L2-constant-basis))
|#

(pec (((L2-Hodge-star (lambda (x) 1))
       (literal-vector-field 'u R2-rect)
       (literal-vector-field 'v R2-rect))
      R2-point))
#| Result:
(+ (* -1 c (v^0 (up t0 x0)) (u^1 (up t0 x0)))
   (* c (v^1 (up t0 x0)) (u^0 (up t0 x0))))
|#
;;; As desired.

(pec (((L2-Hodge-star
	(* (literal-manifold-function 'alpha R2-rect)
	   (* c dt)))
       (literal-vector-field 'u R2-rect))
      R2-point))
#| Result:
(* -1 (alpha (up t0 x0)) (u^1 (up t0 x0)))
  = -alpha dx(u)
|# 

(pec (((L2-Hodge-star
	(* (literal-manifold-function 'alpha R2-rect)
	   dx))
       (literal-vector-field 'u R2-rect))
      R2-point))
#| Result:
(* -1 c (alpha (up t0 x0)) (u^0 (up t0 x0)))
 = -alpha c dt(u)
|#

(pec ((L2-Hodge-star
       (* (literal-manifold-function 'alpha R2-rect)
	  (wedge (* c dt) dx)))
      R2-point))
#| Result:
(* -1 (alpha (up t0 x0)))
|#
|#

#|
(install-coordinates R2-rect (up 'x 'y))
(define R2-point ((R2-rect '->point) (up 'x0 'y0)))
(define R2-basis (coordinate-system->basis R2-rect))

(define ((g-R2 g_00 g_01 g_11) u v)
  (+ (* g_00 (dx u) (dx v))
     (* g_01 (+ (* (dx u) (dy v)) (* (dy u) (dx v))))
     (* g_11 (dy u) (dy v))))

(define R2-metric (g-R2 'a 'b 'c))

;;; Hodge-star must Orthonormalize here

(define R2-star (Hodge-star R2-metric R2-rect #t))

(pec (((R2-star (lambda (x) 1)) d/dx d/dy) R2-point))
#| Result:
(sqrt (+ (* a c) (* -1 (expt b 2))))
|#

(pec (((R2-star dx) d/dx) R2-point))
#| Result:
(/ b (sqrt (+ (* a c) (* -1 (expt b 2)))))
|#

(pec (((R2-star dx) d/dy) R2-point))
#| Result:
(/ c (sqrt (+ (* a c) (* -1 (expt b 2)))))
|#

(pec (((R2-star dy) d/dx) R2-point))
#| Result:
(/ (* -1 a) (sqrt (+ (* a c) (* -1 (expt b 2)))))
|#

(pec (((R2-star dy) d/dy) R2-point))
#| Result:
(/ (* -1 b) (sqrt (+ (* a c) (* -1 (expt b 2)))))
|#

(pec ((R2-star (wedge dx dy)) R2-point))
#| Result:
(/ 1 (sqrt (+ (* a c) (* -1 (expt b 2)))))
|#
|#

#|
;;; Example: Lorentz metric on R^4

(define SR R4-rect)
(install-coordinates SR (up 't 'x 'y 'z))
(define SR-point ((SR '->point) (up 't0 'x0 'y0 'z0)))
(define c 'c)

(define SR-constant-vector-basis
  (down (* (/ 1 c) d/dt) d/dx d/dy d/dz))

(define SR-constant-1form-basis
  (up (* c dt) dx dy dz))

(define SR-constant-basis
  (make-basis SR-constant-vector-basis
	      SR-constant-1form-basis))

(define (g-Lorentz u v)
  (+ (* (dx u) (dx v))
     (* (dy u) (dy v))
     (* (dz u) (dz v))
     (* -1 (square c) (dt u) (dt v))))

(define SR-star
  (Hodge-star g-Lorentz SR-constant-basis))


(define u
  (+ (* (literal-manifold-function 'ut SR) (/ 1 c) d/dt)
     (* (literal-manifold-function 'ux SR) d/dx)
     (* (literal-manifold-function 'uy SR) d/dy)
     (* (literal-manifold-function 'uz SR) d/dz)))

(define v
  (+ (* (literal-manifold-function 'vt SR) (/ 1 c) d/dt)
     (* (literal-manifold-function 'vx SR) d/dx)
     (* (literal-manifold-function 'vy SR) d/dy)
     (* (literal-manifold-function 'vz SR) d/dz)))

(pec (((- (SR-star (wedge dy dz)) (wedge (* c dt) dx))
       u v)
      SR-point))
#| Result:
0
|#

(pec (((- (SR-star (wedge dz dx)) (wedge (* c dt) dy))
       u v)
      SR-point))
#| Result:
0
|#
;;; Other rotations of variables are all similar
|#

#|
;;; Claim: this is the interior product in a metric space

(define (((ip metric basis) X) alpha)
  (let ((k (get-rank alpha))
	(n (basis->dimension basis))
	(dual (Hodge-star metric basis)))
    (let ((sign (if (even? (* k (- n k))) +1 -1)))
      (* sign
	 (dual (wedge (dual alpha)
		      ((lower metric) X)))))))


(install-coordinates R3-rect (up 'x 'y 'z))
(define R3-basis (coordinate-system->basis R3-rect))
(define R3-point ((R3-rect '->point) (up 'x0 'y0 'z0)))

(define u (literal-vector-field 'u R3-rect))
(define v (literal-vector-field 'v R3-rect))
(define w (literal-vector-field 'w R3-rect))

(define (E3-metric v1 v2)
  (+ (* (dx v1) (dx v2))
     (* (dy v1) (dy v2))
     (* (dz v1) (dz v2))))

(define omega
  (+ (* (literal-manifold-function 'alpha R3-rect) (wedge dx dy))
     (* (literal-manifold-function 'beta  R3-rect) (wedge dy dz))
     (* (literal-manifold-function 'gamma R3-rect) (wedge dz dx))))

(pec (- (((((ip E3-metric R3-basis) u) omega) v) R3-point)
	((((interior-product u) omega) v) R3-point)))
#| Result:
0
|#

(define theta 
  (* (literal-scalar-field 'delta R3-rect) (wedge dx dy dz)))

(pec (- (((((ip E3-metric R3-basis) u) theta) v w) R3-point)
	((((interior-product u) theta) v w) R3-point)))
#| Result:
0
|#

|#

;;; Electrodynamics...
#|
(define SR R4-rect)
(install-coordinates SR (up 't 'x 'y 'z))
(define SR-basis (coordinate-system->basis SR))
(define an-event ((SR '->point) (up 't0 'x0 'y0 'z0)))
(define c 'c)

(define (g-Lorentz u v)
  (+ (* (dx u) (dx v))
     (* (dy u) (dy v))
     (* (dz u) (dz v))
     (* -1 (square c) (dt u) (dt v))))

(define L4-constant-vector-basis
  (down (* (/ 1 c) d/dt) d/dx d/dy d/dz))

(define L4-constant-1form-basis
  (up (* c dt) dx dy dz))

(define L4-constant-basis
  (make-basis L4-constant-vector-basis
	      L4-constant-1form-basis))

(define SR-star
  (Hodge-star g-Lorentz L4-constant-basis))

(pec (((SR-star
	(* (literal-manifold-function 'Bx SR)
	   (wedge dy dz)))
       (* (/ 1 c) d/dt)
       d/dx)
      an-event))
#| Result:
(Bx (up t0 x0 y0 z0))
|#

;;; Fields E, B.  From MTW p.108

(define (Faraday Ex Ey Ez Bx By Bz)
  (+ (* Ex c (wedge dx dt))
     (* Ey c (wedge dy dt))
     (* Ez c (wedge dz dt))
     (* Bx (wedge dy dz))
     (* By (wedge dz dx))
     (* Bz (wedge dx dy))))

(define (Maxwell Ex Ey Ez Bx By Bz)
  (+ (* -1 Bx c (wedge dx dt))
     (* -1 By c (wedge dy dt))
     (* -1 Bz c (wedge dz dt))
     (* Ex (wedge dy dz))
     (* Ey (wedge dz dx))
     (* Ez (wedge dx dy))))

(pec (((- (SR-star (Faraday 'Ex 'Ey 'Ez 'Bx 'By 'Bz))
	  (Maxwell 'Ex 'Ey 'Ez 'Bx 'By 'Bz))
       (literal-vector-field 'u SR)
       (literal-vector-field 'v SR))
      an-event))
#| Result:
0
|#


;;; **F + F = 0

(pec (((+ ((compose SR-star SR-star) (Faraday 'Ex 'Ey 'Ez 'Bx 'By 'Bz))
	  (Faraday 'Ex 'Ey 'Ez 'Bx 'By 'Bz))
       (literal-vector-field 'u SR)
       (literal-vector-field 'v SR))
      an-event))
#| Result:
0
|#


;;; Defining the 4-current density J.

;;; Charge density is a manifold function.  Current density is a
;;; vector field having only spatial components.

(define (J charge-density Jx Jy Jz)
  (- (* (/ 1 c) (+ (* Jx dx) (* Jy dy) (* Jz dz)))
     (* charge-density c dt)))

(define rho (literal-manifold-function 'rho SR))

(define 4-current
  (J rho
     (literal-manifold-function 'Ix SR)
     (literal-manifold-function 'Iy SR)
     (literal-manifold-function 'Iz SR)))

(pec (((d (SR-star 4-current))
       (literal-vector-field 'a SR)
       (literal-vector-field 'b SR)
       (literal-vector-field 'c SR)
       (literal-vector-field 'd SR))
      an-event))
#| Result:
;;; The charge conservation equations are too ugly to include.
|#

(pec (((SR-star 4-current) d/dx d/dy d/dz) an-event))
#| Result:
(rho (up t0 x0 y0 z0))
|#

(pec (((SR-star 4-current)
       (* (/ 1 c) d/dt) d/dy d/dz)
      an-event))
#| Result:
(/ (* -1 (Ix (up t0 x0 y0 z0))) c)
|#

(pec (((SR-star 4-current)
       (* (/ 1 c) d/dt) d/dz d/dx)
      an-event))
#| Result:
(/ (* -1 (Iy (up t0 x0 y0 z0))) c)
|#

(pec (((SR-star 4-current)
       (* (/ 1 c) d/dt) d/dx d/dy)
      an-event))
#| Result:
(/ (* -1 (Iz (up t0 x0 y0 z0))) c)
|#

;;; Maxwell's equations in the form language are:
;;; dF=0, d(*F)=4pi *J
  
(define F
  (Faraday (literal-manifold-function 'Ex SR)
	   (literal-manifold-function 'Ey SR)
	   (literal-manifold-function 'Ez SR)
	   (literal-manifold-function 'Bx SR)
	   (literal-manifold-function 'By SR)
	   (literal-manifold-function 'Bz SR)))

;;; div B = 0
(pec (((d F) d/dx d/dy d/dz) an-event))
#| Result: 
(+ (((partial 1) Bx) (up t0 x0 y0 z0))
   (((partial 2) By) (up t0 x0 y0 z0))
   (((partial 3) Bz) (up t0 x0 y0 z0)))
|#

;;; curl E = -1/c dB/dt

(pec (((d F) (* (/ 1 c) d/dt) d/dy d/dz) an-event))
#| Result:
(+ (((partial 2) Ez) (up t0 x0 y0 z0))
   (* -1 (((partial 3) Ey) (up t0 x0 y0 z0)))
   (/ (((partial 0) Bx) (up t0 x0 y0 z0)) c))
|#

(pec (((d F) (* (/ 1 c) d/dt) d/dz d/dx) an-event))
#| Result:
(+ (((partial 3) Ex) (up t0 x0 y0 z0))
   (* -1 (((partial 1) Ez) (up t0 x0 y0 z0)))
   (/ (((partial 0) By) (up t0 x0 y0 z0)) c))
|#

(pec (((d F) (* (/ 1 c) d/dt) d/dx d/dy) an-event))
#| Result:
(+ (((partial 1) Ey) (up t0 x0 y0 z0))
   (* -1 (((partial 2) Ex) (up t0 x0 y0 z0)))
   (/ (((partial 0) Bz) (up t0 x0 y0 z0)) c))
|#

;;; div E = 4pi rho

(pec (((- (d (SR-star F)) (* '4pi (SR-star 4-current)))
       d/dx d/dy d/dz)
      an-event))
#| Result:
(+ (* -1 4pi (rho (up t0 x0 y0 z0)))
   (((partial 1) Ex) (up t0 x0 y0 z0))
   (((partial 2) Ey) (up t0 x0 y0 z0))
   (((partial 3) Ez) (up t0 x0 y0 z0)))
|#



;;; curl B = 1/c dE/dt + 4pi I

(pec (((- (d (SR-star F)) (* '4pi (SR-star 4-current)))
       (* (/ 1 'c) d/dt) d/dy d/dz)
      an-event))
#| Result:
(+ (/ (* 4pi (Ix (up t0 x0 y0 z0))) c)
   (* -1 (((partial 2) Bz) (up t0 x0 y0 z0)))
   (((partial 3) By) (up t0 x0 y0 z0))
   (/ (((partial 0) Ex) (up t0 x0 y0 z0)) c))
|#

(pec (((- (d (SR-star F)) (* '4pi (SR-star 4-current)))
       (* (/ 1 c) d/dt) d/dz d/dx)
      an-event))
#| Result:
(+ (/ (* 4pi (Iy (up t0 x0 y0 z0))) c)
   (* -1 (((partial 3) Bx) (up t0 x0 y0 z0)))
   (((partial 1) Bz) (up t0 x0 y0 z0))
   (/ (((partial 0) Ey) (up t0 x0 y0 z0)) c))
|#

(pec (((- (d (SR-star F)) (* '4pi (SR-star 4-current)))
       (* (/ 1 c) d/dt) d/dx d/dy)
      an-event))
#| Result:
(+ (/ (* 4pi (Iz (up t0 x0 y0 z0))) c)
   (* -1 (((partial 1) By) (up t0 x0 y0 z0)))
   (((partial 2) Bx) (up t0 x0 y0 z0))
   (/ (((partial 0) Ez) (up t0 x0 y0 z0)) c))
|#
|#
