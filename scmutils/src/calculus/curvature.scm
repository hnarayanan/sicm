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

;;; Riemann curvature "tensor" is pretty easy

;;; Hawking and Ellis equation 2.18, page 35.

(define ((Riemann-curvature nabla) u v)
  (- (commutator (nabla u) (nabla v))
     (nabla (commutator u v))))

;;; The traditional Riemann tensor R^i_jkl:

(define (Riemann nabla)
  (define (the-Riemann-tensor w x u v)
    (w (((Riemann-curvature nabla) u v) x)))
  (declare-argument-types! the-Riemann-tensor
			   (list 1form-field?
				 vector-field?
				 vector-field?
				 vector-field?))
  the-Riemann-tensor)

(define (Ricci nabla basis)
  (define (Ricci-tensor u v)
    (contract
     (lambda (ei wi)
       ((Riemann nabla) wi u ei v))
     basis))
  (declare-argument-types! Ricci-tensor
			   (list vector-field? vector-field?))
  Ricci-tensor)


;;; Hawking and Ellis page 34.

(define ((torsion-vector nabla) X Y)
  (+ ((nabla X) Y)
     (* -1 ((nabla Y) X))
     (* -1 (commutator X Y))))

;;; The torsion tensor T^i_jk

(define (torsion nabla)
  (define (the-torsion w x y)
    (w ((torsion-vector nabla) x y)))
  (declare-argument-types! the-torsion
			   (list 1form-field?
				 vector-field?
				 vector-field?))
  the-torsion)


;;; Components of the curvature tensor R^i_{jkl}

(define (curvature-components nabla coord-sys)
  (let ((d/dxs (coordinate-system->vector-basis coord-sys))
	(dxs (coordinate-system->1form-basis coord-sys))
	(m ((point coord-sys) (up 'x 'y 'z))))
    ((s:map 
      (lambda (dx)
	(s:map 
	 (lambda (d/dx)
	   (s:map
	    (lambda (d/dy)
	      (s:map 
	       (lambda (d/dz)
		 (dx (((Riemann-curvature nabla) d/dy d/dz)
		      d/dx)))
	       d/dxs))
	    d/dxs))
	 d/dxs))
      dxs)
     m)))

#|
;;; General torsion is not too complicated to compute 

(define-coordinates (up x y) R2-rect)

(define R2-rect-basis
  (coordinate-system->basis R2-rect))
(define R2-rect-point
  ((R2-rect '->point) (up 'x0 'y0)))

(define (Gijk i j k)
  (literal-manifold-function
   (string->symbol
    (string-append "G^"
		   (number->string i)
		   "_"
		   (number->string j)
		   (number->string k)))
   R2-rect))
				    
(define G
  (down (down (up (Gijk 0 0 0)
		  (Gijk 1 0 0))
	      (up (Gijk 0 1 0)
		  (Gijk 1 1 0)))
	(down (up (Gijk 0 0 1)
		  (Gijk 1 0 1))
	      (up (Gijk 0 1 1)
		  (Gijk 1 1 1)))))


(define CG (make-Christoffel G R2-rect-basis))

(define CF (Christoffel->Cartan CG))

(define v (literal-vector-field 'v R2-rect))
(define w (literal-vector-field 'w R2-rect))
(define f (literal-manifold-function 'f R2-rect))

	      
(clear-arguments)
(suppress-arguments '((up x0 y0)))

(pec ((((torsion-vector (covariant-derivative CF)) v w) f)
      R2-rect-point))
#| Result:
(+ (* -1 G^1_01 v^0 ((partial 1) f) w^1)
   (* G^1_01 w^0 ((partial 1) f) v^1)
   (* -1 G^0_01 v^0 ((partial 0) f) w^1)
   (* G^0_01 w^0 ((partial 0) f) v^1)
   (* G^1_10 v^0 ((partial 1) f) w^1)
   (* -1 G^1_10 w^0 ((partial 1) f) v^1)
   (* G^0_10 v^0 ((partial 0) f) w^1)
   (* -1 G^0_10 w^0 ((partial 0) f) v^1))
|#

;;; Unfortunately, this says only that the 
;;; Christoffel symbols are symmetric in the
;;; lower two indices iff the torsion is zero.
|#

#|
(define-coordinates (up theta phi) S2-spherical)

(define S2-spherical-basis (coordinate-system->basis S2-spherical))
(define a-point ((S2-spherical '->point) (up 'theta 'phi)))
(define a-function (literal-scalar-field 'f S2-spherical))

;;; the Christoffel symbols (for r=1) (p.341 mtw) are:
 
;;; (the up-down-down Christoffel symbols do not depend on R)

(define G-S2-1
  (make-Christoffel
   (let ((zero  (lambda (point) 0))) 
     (down (down (up zero zero)
		 (up zero (/ 1 (tan theta))))
	   (down (up zero (/ 1 (tan theta)))
		 (up (- (* (sin theta) (cos theta))) zero))))
   S2-spherical-basis))

(pec (((commutator d/dtheta d/dphi) a-function) a-point))
#| result:
0
|#

(let ((nabla
       (covariant-derivative
	(Christoffel->Cartan G-S2-1))))
  (pec ((((nabla d/dtheta) d/dphi)
	 a-function)
	a-point)))
#| result:
(/ (* (cos theta)
      (((partial 1) f) (up theta phi)))
   (sin theta))
|#

(let ((nabla
       (covariant-derivative
	(Christoffel->Cartan G-S2-1))))
  (pec ((((nabla d/dphi) ((nabla d/dtheta) d/dphi))
	 a-function)
	a-point)))
#| result:
(* -1 (((partial 0) f) (up theta phi)) (expt (cos theta) 2))
|#

(let ((nabla
       (covariant-derivative
	(Christoffel->Cartan G-S2-1))))
  (for-each
   (lambda (x)
     (for-each
      (lambda (y)
	(pec ((((torsion-vector nabla) x y)
	       a-function)
	      a-point)))
      (list  d/dtheta d/dphi)))
   (list  d/dtheta d/dphi)))
#| result:
0					;four of these
|#

(let ((nabla
       (covariant-derivative
	(Christoffel->Cartan G-S2-1))))
  (pec (((Riemann nabla)
	 dphi d/dtheta d/dphi d/dtheta)
	a-point)))
#| Result:
1
|#
|#

#|
;;; We can work without embedding the sphere in R^3
;;; We need another copy of R2...

(define M (make-manifold R^n 2))
(define M-rect (coordinate-system-at 'rectangular 'origin M))
(define M-polar (coordinate-system-at 'polar/cylindrical 'origin M))

(define-coordinates (up theta phi) M-rect)
(define M-basis (coordinate-system->basis M-rect))
(define a-point ((M-rect '->point) (up 'theta0 'phi0)))
(define a-function (literal-scalar-field 'f M-rect))

(define G-S2-1
  (make-Christoffel
   (let ((zero  (lambda (point) 0))) 
     (down (down (up zero zero)
                 (up zero (/ 1 (tan theta))))
           (down (up zero (/ 1 (tan theta)))
                 (up (- (* (sin theta) (cos theta))) zero))))
   M-basis))

(let ((nabla
       (covariant-derivative
	(Christoffel->Cartan G-S2-1))))
  (for-each
   (lambda (x)
     (for-each
      (lambda (y)
	(pec ((((torsion-vector nabla) x y)
	       a-function)
	      a-point)))
      (list  d/dtheta d/dphi)))
   (list  d/dtheta d/dphi)))
#| Result:
0
|#
#| Result:
0
|#
#| Result:
0
|#
#| Result:
0
|#
|#

#|
(let ((nabla
       (covariant-derivative
	(Christoffel->Cartan G-S2-1))))
  (pec (((Riemann nabla)
	 dphi d/dtheta d/dphi d/dtheta)
	a-point)))
#| Result:
1
|#
;;; Computes instantly, with little memory.
|#

#|
(set! *divide-out-terms* #f)

;;; R^alpha_{beta gamma delta}

(let ((nabla
       (covariant-derivative
	(Christoffel->Cartan G-S2-1))))
  (for-each
   (lambda (alpha)
     (for-each
      (lambda (beta)
	(for-each
	 (lambda (gamma)
	   (for-each
	    (lambda (delta)
	      (newline)
	      (pe `(,alpha ,beta ,gamma ,delta))
	      (pe (((Riemann nabla)
		    alpha beta gamma delta)
		   a-point)))
	    (list d/dtheta d/dphi)))
	 (list d/dtheta d/dphi)))
      (list d/dtheta d/dphi)))
   (list dtheta dphi)))

;;; p351 MTW has efficient method for computing curvature (eq 14.18)

;;alpha  beta     gamma    delta

(dtheta d/dtheta d/dtheta d/dtheta)
0

(dtheta d/dtheta d/dtheta d/dphi)
0

(dtheta d/dtheta d/dphi d/dtheta)
0

(dtheta d/dtheta d/dphi d/dphi)
0

(dtheta d/dphi d/dtheta d/dtheta)
0

(dtheta d/dphi d/dtheta d/dphi)
(expt (sin theta^0) 2)

(dtheta d/dphi d/dphi d/dtheta)
(* -1 (expt (sin theta^0) 2))

(dtheta d/dphi d/dphi d/dphi)
0

(dphi d/dtheta d/dtheta d/dtheta)
0

(dphi d/dtheta d/dtheta d/dphi)
-1

(dphi d/dtheta d/dphi d/dtheta)
1

(dphi d/dtheta d/dphi d/dphi)
0

(dphi d/dphi d/dtheta d/dtheta)
0

(dphi d/dphi d/dtheta d/dphi)
0

(dphi d/dphi d/dphi d/dtheta)
0

(dphi d/dphi d/dphi d/dphi)
0
|#

#|
;;; Equation of geodesic deviation (MTW p275 eq.11.10) has a type
;;; error.  The variation is ambiguously a vector-field over a map and
;;; a vector field.  Riemann must take uniform stuff, and U is a
;;; vector field on N (the-real-line), however variation is defined
;;; only over the map.  The following does not work!

(pec (let ((U (components->vector-field (lambda (x) 1) the-real-line 'U))
	   (mu:N->M (compose (M '->point)
			     (up (literal-function 'f^theta)
				 (literal-function 'f^phi)))))
       (let* ((basis-over-mu (basis->basis-over-map mu:N->M S2-spherical-basis))
	      (1form-basis (basis->1form-basis basis-over-mu))
	      (vector-basis (basis->vector-basis basis-over-mu))

	      (Cartan (Christoffel->Cartan G-S2-1))
	      (variation (basis-components->vector-field
			  (up (literal-function 'd_theta)
			      (literal-function 'd_phi))
			  vector-basis))
	      (nabla (covariant-derivative-over-map Cartan mu:N->M))
	      (nablau (nabla U))
	      (d1 (nablau (nablau variation)))
	      (d2 (((Riemann-curvature nabla) variation U) U))
	      (deviation (+ d1 d2)))
	 (s:map/r 
	  (lambda (w)
	    ((w deviation) ((the-real-line '->point) 'tau)))
	  1form-basis))))
(f^theta #[manifold-point 16])
;Wrong type argument -- LITERAL-FUNCTION

;;; OK, in considering the variational problem, the map is actually
;;; two dimensional, time is one direction and variation the other.

;;; The Christoffel symbols (for R=1) (p.341 MTW) are:
;;; (the up-down-down Christoffel symbols do not depend on R)

(define-coordinates (up t n) R2-rect)

(define f^theta
  (literal-function 'f^theta (-> (UP Real Real) Real)))

(define f^phi
  (literal-function 'f^phi (-> (UP Real Real) Real)))

(define s0
  (simplify
   (let* ( ;; d/dt and d/dn exist
	  (mu:N->M (compose (M-rect '->point)
			    (up f^theta f^phi)
			    (R2-rect '->coords)))
	  (basis-over-mu (basis->basis-over-map mu:N->M M-basis))
	  (1form-basis (basis->1form-basis basis-over-mu))
	  (Cartan (Christoffel->Cartan G-S2-1))
	  (nabla (covariant-derivative Cartan mu:N->M))
	  (nablau (nabla d/dt))
	  (d1 (nablau (nablau ((differential mu:N->M) d/dn))))
	  (d2 (((Riemann-curvature nabla) d/dn d/dt)
	       ((differential mu:N->M) d/dt)))
	  (deviation (+ d1 d2)))
     (s:map/r 
      (lambda (w)
	((w deviation) ((R2-rect '->point) (up 'tau 0))))
      1form-basis))))

(define s1
  (substitute 'xidotdot '(((partial 0) ((partial 0) ((partial 1) f^theta))) (up tau 0)) s0))

(define s2
  (substitute 'etadotdot '(((partial 0) ((partial 0) ((partial 1) f^phi))) (up tau 0)) s1))

(define s3
  (substitute 'phidotdot '(((partial 0) ((partial 0) f^phi)) (up tau 0)) s2))

(define s4
  (substitute 'thetadotdot '(((partial 0) ((partial 0) f^theta)) (up tau 0)) s3))

(define s5
  (substitute 'etadot '(((partial 0) ((partial 1) f^phi)) (up tau 0)) s4))

(define s6
  (substitute 'xidot '(((partial 0) ((partial 1) f^theta)) (up tau 0)) s5))

(define s7
  (substitute 'xi '(((partial 1) f^theta) (up tau 0)) s6))

(define s8
  (substitute 'eta '(((partial 1) f^phi) (up tau 0)) s7))

(define s9
  (substitute 'thetadot '(((partial 0) f^theta) (up tau 0)) s8))

(define s10
  (substitute 'phidot '(((partial 0) f^phi) (up tau 0)) s9))

(define s11
  (substitute 'theta '(f^theta (up tau 0)) s10))

(define s12
  (substitute 'phi '(f^phi (up tau 0)) s11))


;;; Substituting from the geodesic equation (equation of motion) to
;;; make make use of the fact that the trajectory is a geodesic.

(define s13
  (substitute '(* -2 thetadot phidot (/ (cos theta) (sin theta))) 'phidotdot s12))

(define s14
  (substitute '(* phidot phidot (cos theta) (sin theta)) 'thetadotdot s13))

(pec s14)
#| Result:
(up
 (+ (* -2 (expt phidot 2) xi (expt (cos theta) 2))
    (* -2 etadot phidot (cos theta) (sin theta))
    (* (expt phidot 2) xi)
    xidotdot)
 (/
  (+ (* 2 etadot thetadot (cos theta) (sin theta))
     (* 2 phidot xidot (cos theta) (sin theta))
     (* etadotdot (expt (sin theta) 2))
     (* -2 phidot thetadot xi))
  (expt (sin theta) 2)))
|#

;;; These geodesic deviation equations are the variational equations
;;; driven by the geodesic equation.
|#

#|
;;; Testing equation 3 on MTW p272

(define s0
  (simplify
   (let* ( ;; d/dt and d/dn exist
	  (mu:N->M (compose 
		    (M-rect '->point)
		    (up f^theta f^phi)
		    (R2-rect '->coords)))
	  (basis-over-mu (basis->basis-over-map mu:N->M M-basis))
	  (1form-basis (basis->1form-basis basis-over-mu))
	  (Cartan (Christoffel->Cartan G-S2-1))
	  (nabla (covariant-derivative Cartan mu:N->M))
	  (nablau (nabla d/dt))
	  (nablan (nabla d/dn))
	  (deviation (nablan (nablau ((differential mu:N->M) d/dt)))))
     (s:map/r 
      (lambda (w)
	((w deviation) ((R2-rect '->point) (up 'tau 0))))
      1form-basis))))

do all substitutions again...
(pec s12)
#| Result:
(up
	 (+ (* -2 eta phidot thetadot (expt (cos theta) 2))
	    (* -2 (expt phidot 2) xi (expt (cos theta) 2))
	    (* -1 eta phidotdot (cos theta) (sin theta))
	    (* -2 etadot phidot (cos theta) (sin theta))
	    (* (expt phidot 2) xi)
	    xidotdot)
	 (/
	  (+ (* -1 eta (expt phidot 2) (expt (cos theta) 2) (sin theta))
	     (* -2 phidot thetadot xi (sin theta))
	     (* eta thetadotdot (cos theta))
	     (* 2 etadot thetadot (cos theta))
	     (* 2 phidot xidot (cos theta))
	     (* phidotdot xi (cos theta))
	     (* etadotdot (sin theta)))
	  (sin theta)))
|#

(pec s14)
#| Result:
(up
 (+ (* -2 (expt phidot 2) xi (expt (cos theta) 2))
    (* -2 etadot phidot (cos theta) (sin theta))
    (* (expt phidot 2) xi)
    xidotdot)
 (/
  (+ (* 2 etadot thetadot (cos theta) (sin theta))
     (* 2 phidot xidot (cos theta) (sin theta))
     (* etadotdot (expt (sin theta) 2))
     (* -2 phidot thetadot xi))
  (expt (sin theta) 2)))
|#

agrees with Riemann calculation

shouldn't this be zero?

|#

#|
;;; parallel transport of vector about a loop

(define-coordinates t the-real-line)

;;; The coordinates on the unit sphere

(define-coordinates (up theta phi) S2-spherical)

(define S2-spherical-basis (coordinate-system->basis S2-spherical))

;;; The Christoffel symbols (for r=1) (p.341 MTW) are:
 
(define G-S2-1
  (make-Christoffel
   (let ((zero  (lambda (point) 0))) 
     (down (down (up zero zero)
		 (up zero (/ 1 (tan theta))))
	   (down (up zero (/ 1 (tan theta)))
		 (up (- (* (sin theta) (cos theta))) zero))))
   S2-spherical-basis))


;;; Ordinary Lagrange Equations (= Geodesic Equations)

(pec (let ((U d/dt)
	   (mu:N->M (compose (S2-spherical '->point)
			     (up (literal-function 'f^theta)
				 (literal-function 'f^phi))
			     (the-real-line '->coords))))
       (let* ((basis-over-mu (basis->basis-over-map mu:N->M S2-spherical-basis))
	      (1form-basis (basis->1form-basis basis-over-mu))
	      (Cartan (Christoffel->Cartan G-S2-1)))
	 (s:map/r 
	  (lambda (w)
	    ((w (((covariant-derivative Cartan mu:N->M) U)
		 ((differential mu:N->M) U)))
	     ((the-real-line '->point) 'tau)))
	  1form-basis))))
#| Result:
(up
 (+ (((expt D 2) f^theta) tau)
    (* -1 (cos (f^theta tau)) (sin (f^theta tau)) (expt ((D f^phi) tau) 2)))
 (/ (+ (* (sin (f^theta tau)) (((expt D 2) f^phi) tau))
       (* 2 (cos (f^theta tau)) ((D f^phi) tau) ((D f^theta) tau)))
    (sin (f^theta tau))))
|#

;;; Parallel transport of vector W over path mu

(pec (let ((U d/dt)
	   (mu:N->M (compose (S2-spherical '->point)
			     (up (literal-function 'f^theta)
				 (literal-function 'f^phi))
			     (the-real-line '->coords))))
       (let* ((basis-over-mu
	       (basis->basis-over-map mu:N->M S2-spherical-basis))
	      (1form-basis (basis->1form-basis basis-over-mu))
	      (vector-basis (basis->vector-basis basis-over-mu))
	      (Cartan (Christoffel->Cartan G-S2-1))
	      (transported-vector-over-map 
	       (basis-components->vector-field
		(up (compose (literal-function 'w^0)
			     (the-real-line '->coords))
		    (compose (literal-function 'w^1)
			     (the-real-line '->coords)))
		vector-basis)))
	 (s:map/r 
	  (lambda (w)
	    ((w
	      (((covariant-derivative Cartan mu:N->M) U)
	       transported-vector-over-map))
	     ((the-real-line '->point) 'tau)))
	  1form-basis))))
#| Result:
(up
 (+ ((D w^0) tau)
    (* -1 (cos (f^theta tau)) ((D f^phi) tau) (w^1 tau) (sin (f^theta tau))))
 (/ (+ (* (sin (f^theta tau)) ((D w^1) tau))
       (* (cos (f^theta tau)) ((D f^phi) tau) (w^0 tau))
       (* (cos (f^theta tau)) (w^1 tau) ((D f^theta) tau)))
    (sin (f^theta tau))))
|#
 
#| was  ...  looks like right hand side

(up (* (sin (theta tau)) (cos (theta tau)) (w^1 tau)
       ((D phi) tau))
    (/ (+ (* -1 (w^0 tau) (cos (theta tau)) ((D phi) tau))
	  (* -1 ((D theta) tau) (cos (theta tau)) (w^1 tau)))
       (sin (theta tau))))

|#

;;; To set up for solving for the derivatives, we lift off of the path

(pec (let ((U d/dt)
	   (mu:N->M (compose (S2-spherical '->point)
			     (up (literal-function 'f^theta)
				 (literal-function 'f^phi))
			     (the-real-line '->coords))))
       (let* ((basis-over-mu (basis->basis-over-map mu:N->M S2-spherical-basis))
	      (1form-basis (basis->1form-basis basis-over-mu))
	      (vector-basis (basis->vector-basis basis-over-mu))
	      (Cartan (Christoffel->Cartan G-S2-1))
	      (transported-vector-over-map 
	       (basis-components->vector-field
		(up (compose (osculating-path (up 'tau 'w^0 'dw^0/dt))
			     (the-real-line '->coords))
		    (compose (osculating-path (up 'tau 'w^1 'dw^1/dt))
			     (the-real-line '->coords)))
		vector-basis)))
	 (s:map/r 
	  (lambda (w)
	    ((w
	      (((covariant-derivative Cartan mu:N->M)
		U)
	       transported-vector-over-map))
	     ((the-real-line '->point) 'tau)))
	  1form-basis))))
#| Result:
(up (+ dw^0/dt
       (* -1 (cos (f^theta tau)) ((D f^phi) tau) (sin (f^theta tau)) w^1))
    (/ (+ (* (sin (f^theta tau)) dw^1/dt)
	  (* (cos (f^theta tau)) ((D f^phi) tau) w^0)
	  (* (cos (f^theta tau)) ((D f^theta) tau) w^1))
       (sin (f^theta tau))))
|#

;;; Loaded solve by (load "/usr/local/scmutils/src/solve/linreduce")

(set! *divide-out-terms* #f)
;Value: #t

(let ((tau 'tau)
      (theta (literal-function 'f^theta))
      (phi (literal-function 'f^phi))
      (w^0 (literal-function 'w^0))
      (w^1 (literal-function 'w^1)))
  (pec (solve
	(lambda (v)
	  (let ((dw^0/dt (ref v 0))
		(dw^1/dt (ref v 1)))
	    (up
	     (+ (* -1
		   (w^1 tau)
		   (sin (theta tau))
		   (cos (theta tau))
		   ((D phi) tau))
		dw^0/dt)
	     (+ (/ (* (w^0 tau) (cos (theta tau)) ((D phi) tau))
		   (sin (theta tau)))
		(/ (* (w^1 tau) ((D theta) tau) (cos (theta tau)))
		   (sin (theta tau)))
		dw^1/dt))))
	2 2)))
#| Result:
(up (* (w^1 tau) (sin (f^theta tau)) (cos (f^theta tau)) ((D f^phi) tau))
    (/ (+ (* -1 (w^1 tau) (cos (f^theta tau)) ((D f^theta) tau))
	  (* -1 (cos (f^theta tau)) ((D f^phi) tau) (w^0 tau)))
       (sin (f^theta tau))))
|#

(pec (let ((U d/dt)
	   (mu:N->M (compose (S2-spherical '->point)
			     (up (literal-function 'f^theta)
				 (literal-function 'f^phi))
			     (the-real-line '->coords))))
       (solve 
	(lambda (v)
	  (let ((dw^0/dt (ref v 0))
		(dw^1/dt (ref v 1)))
	    (let* ((basis-over-mu (basis->basis-over-map mu:N->M S2-spherical-basis))
		   (1form-basis (basis->1form-basis basis-over-mu))
		   (vector-basis (basis->vector-basis basis-over-mu))
		   (Cartan (Christoffel->Cartan G-S2-1))
		   (transported-vector-over-map 
		    (basis-components->vector-field
		     (up (compose (osculating-path (up 'tau 'w^0 dw^0/dt))
				  (the-real-line '->coords))
			 (compose (osculating-path (up 'tau 'w^1 dw^1/dt))
				  (the-real-line '->coords)))
		     vector-basis)))
	      (s:map/r 
	       (lambda (w)
		 ((w
		   (((covariant-derivative Cartan mu:N->M)
		     U)
		    transported-vector-over-map))
		  ((the-real-line '->point) 'tau)))
	       1form-basis))))
	(S2-spherical 'dimension)
	(S2-spherical 'dimension))))
#| Result:
(up
 (* w^1 (cos (f^theta tau)) (sin (f^theta tau)) ((D f^phi) tau))
 (/
  (+ (* -1 w^0 (cos (f^theta tau)) ((D f^phi) tau))
     (* -1 w^1 ((D f^theta) tau) (cos (f^theta tau))))
  (sin (f^theta tau))))
|#
|#

#|
;;; Computing parallel transport without the embedding

(define-coordinates t the-real-line)

(define-coordinates (up theta phi) M-rect)
(define M-basis (coordinate-system->basis M-rect))
 
(define G-S2-1
  (make-Christoffel
   (let ((zero  (lambda (point) 0))) 
     (down (down (up zero zero)
		 (up zero (/ 1 (tan theta))))
	   (down (up zero (/ 1 (tan theta)))
		 (up (- (* (sin theta) (cos theta))) zero))))
   M-basis))


;;; Parallel transport of vector w over path mu

(define mu:N->M
  (compose (M-rect '->point)
	   (up (literal-function 'mu^theta)
	       (literal-function 'mu^phi))
	   (the-real-line '->coords)))

(define basis-over-mu
  (basis->basis-over-map mu:N->M M-basis))

(define w
  (basis-components->vector-field
   (up (compose (literal-function 'w^0)
		(the-real-line '->coords))
       (compose (literal-function 'w^1)
		(the-real-line '->coords)))
   (basis->vector-basis basis-over-mu)))

(pec (let ((Cartan (Christoffel->Cartan G-S2-1)))
	(s:map/r 
	 (lambda (omega)
	   ((omega
	     (((covariant-derivative Cartan mu:N->M) d/dt) w))
	    ((the-real-line '->point) 'tau)))
	 (basis->1form-basis basis-over-mu))))
#| Result:
(up
 (+ (* -1 (w^1 tau) ((D mu^phi) tau) (cos (mu^theta tau)) (sin (mu^theta tau)))
    ((D w^0) tau))
 (/
  (+ (* (w^1 tau) (cos (mu^theta tau)) ((D mu^theta) tau))
     (* (w^0 tau) ((D mu^phi) tau) (cos (mu^theta tau)))
     (* ((D w^1) tau) (sin (mu^theta tau))))
  (sin (mu^theta tau))))
|#
;;; These are the equations of the coordinates of a vector being
;;; parallel transported along the path defined by f.
|#

#|
;;; To integrate these equations of the coordinates of the vector
;;; being transported along a path (mu^theta(tau), mu^phi(tau)), defined
;;; by differential equations we need to make a state space that
;;; represents both the path and the coordinates of the vector being
;;; transported.  The states are s=(sigma, w)=((theta, phi), (w0, w1))
;;; and the differential equations for the path are Dsigma(tau) =
;;; b(sigma(tau)).  The differential equations for the coordinates of
;;; the vector are driven by this path.

;;; To represent these states we make a new manifold with 4
;;; coordinates.  The first two coordinates are tha coordinates of the
;;; path.  The second two coordinates are the components of the vector
;;; to be transported, relative to the coordinate directions in the
;;; original manifold.  The right-hand side of the composite
;;; differential equation is a vector field on this manifold.


(define R4 (make-manifold R^n 4))
(define states (coordinate-system-at 'rectangular 'origin R4))
(define-coordinates (up theta phi w0 w1) states)

(define initial-state-d/dphi
  ((states '->point) (up 'theta0 'phi0 0 1)))
(define initial-state-d/dtheta
  ((states '->point) (up 'theta0 'phi0 1 0)))


;;; Assuming that the paths are integral curves of a vector field v,
;;; we supply the vector field:

(define (G v)
  (let ((alphadot (dtheta v)) (betadot (dphi v)))
    (+ v
       (* (compose (* sin cos) theta) betadot w1 d/dw0)
       (* -1
	  (compose (/ cos sin) theta)
	  (+ (* w0 betadot) (* w1 alphadot))
	  d/dw1))))

(define Gu (G d/dtheta))

(define Gv (G d/dphi))

(define (initial-state initial-coords w)
  (let ((theta0 (ref initial-coords 0))
	(phi0 (ref initial-coords 1)))
    (let ((dummy
	   ((states '->point)
	    (up theta0 phi0 'foo 'bar))))
      ((states '->point)
       (up theta0 phi0
	   ((dw0 w) dummy)
	   ((dw1 w) dummy))))))


(pec ((dw0 (commutator Gu Gv))
      (initial-state (up 'theta0 'phi0) d/dw1)))
#| Result:
(* -1 (expt (sin theta0) 2))
|#

(pec ((dw1 (commutator Gu Gv))
      (initial-state (up 'theta0 'phi0) d/dw0)))
#| Result:
1
|#
;;; Gee, this gets the right answer.
|#

#|
;;; To integrate these equations of the coordinates of the vector
;;; being transported along a path (mu^theta(tau), mu^phi(tau)), defined
;;; by differential equations we need to make a state space that
;;; represents both the path and the coordinates of the vector being
;;; transported.  The states are s=(sigma, w)=((theta, phi), (w0, w1))
;;; and the differential equations for the path are Dsigma(tau) =
;;; b(sigma(tau)).  The differential equations for the coordinates of
;;; the vector are driven by this path.

;;; To represent these states we make a new manifold with 4
;;; coordinates.  The first two coordinates are tha coordinates of the
;;; path.  The second two coordinates are the components of the vector
;;; to be transported, relative to the coordinate directions in the
;;; original manifold.  The right-hand side of the composite
;;; differential equation is a vector field on this manifold.

(define-coordinates (up theta phi) M-rect)
(define-coordinates (up Theta Phi w0 w1) states)

;;; Assuming that the paths are integral curves of a vector field v,
;;; we supply the vector field:

(define (G v)
  (let ((alphadot (dTheta v)) (betadot (dPhi v)))
    (+ v
       (* (compose (* sin cos) Theta) betadot w1 d/dw0)
       (* -1
	  (compose (/ cos sin) Theta)
	  (+ (* w0 betadot) (* w1 alphadot))
	  d/dw1))))

(define Gu (G d/dTheta))
(define Gv (G d/dPhi))

(define (initial-state initial-coords w)
  (let ((Theta0 (ref initial-coords 0))
	(Phi0 (ref initial-coords 1)))
    (let ((m ((M-rect '->point) (up Theta0 Phi0))))
      ((states '->point)
       (up Theta0 Phi0
	   ((dtheta w) m) ((dphi w) m))))))


(pec ((dw0 (commutator Gu Gv))
      (initial-state (up 'Theta0 'Phi0) d/dphi)))
#| Result:
(* -1 (expt (sin Theta0) 2))
|#

(pec ((dw1 (commutator Gu Gv))
      (initial-state (up 'Theta0 'Phi0) d/dtheta)))
#| Result:
1
|#
;;; Gee, this gets the right answer.
|#


;;;----------------------------------------------------------------
;;; try to improve this

#|

let gamma be the path that we are transporting along
gamma(t)->M

dgamma(d/dt)(f)(t) is the velocity vector, a vector over the map gamma

when gamma is an integral curve of v, then 
v(f)(gamma(t)) = dgamma(d/dt)(f)(t)

let w be an arbitrary vector over the map
w(f)(t) = d/dtheta (f)(gamma(t)) a_0(t) + d/dphi (f)(gamma(t)) a_1(t) 



|#