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

;;;; Maps between manifolds.

;;; If we have a function on a manifold M and a map from manifold N to
;;; manifold M we can define a function on N:

(define ((pullback-function mu:N->M) f-on-M)
  (compose f-on-M mu:N->M))


;;; If we have an inverse map mu^-1:M->N, we can push a function
;;; on N forward through the map:

(define ((pushforward-function mu^-1:M->N) f-on-N)
  (compose f-on-N mu^-1:M->N))



;;; The map between manifolds induces various ways to transport
;;; vectors from one manifold to another.  The simplest of these is
;;; the differential.

;;; The differential of a function mu:N->M from N to M takes a vector
;;; field on the source manifold N to a vector field-like operator on
;;; the target manifold M.  This results in a vector field over the
;;; map mu:N->M.  The result takes directional derivatives of
;;; functions defined on M, at points of M that are targets of points
;;; of N.

(define ((differential-of-map mu:N->M) v-on-N)
  (define (v-on-M g-on-M)
    (v-on-N (compose g-on-M mu:N->M)))
  (assert (vector-field? v-on-N))
  (procedure->vector-field v-on-M
			   `((d ,(diffop-name mu:N->M))
			     ,(diffop-name v-on-N))))


(define differential differential-of-map)

;;; For a long time we were confused between the concepts of
;;; differential and pushforward.  The resolution seems to be that the
;;; differential takes the manifold position in the source manifold
;;; and the pushforward takes the manifold position in the target
;;; manifold of the map.  So the pushforward needs an inverse map to
;;; define it and so the pushforward is not a very useful idea.

(define ((pushforward-vector mu:N->M mu^-1:M->N) v-on-N)
  ;; Assume (compose mu^-1:M->N mu:N->M) = identity
  (procedure->vector-field
   (lambda (f)
     (compose (((differential mu:N->M) v-on-N) f) mu^-1:M->N))
   `((pushforward ,(diffop-name mu:N->M))
     ,(diffop-name v-on-N))))

(define (literal-manifold-map name source target)
  (let ((n (source 'dimension))
	(m (target 'dimension)))
    (let ((sig (if (fix:= n 1) (-> Real Real) (-> (UP* Real n) Real))))
      (compose (target '->point)
	       (s:generate m 'up
			   (lambda (i)
			     (literal-function
			      (string->symbol
			       (string-append (symbol->string name)
					      "^"
					      (number->string i)))
			      sig)))
	       (source '->coords)))))


#|
;;; Explanation of the connection between the basis forms and the
;;; differentials of coordinate functions.

(install-coordinates R3-rect (up 'x 'y 'z))

(define R3-rect-point ((R3-rect '->point) (up 'x0 'y0 'z0)))

(install-coordinates R3-cyl (up 'r 'theta 'zeta))

(define R3-cyl-point ((R3-cyl '->point) (up 'r0 'theta0 'zeta0)))


(define counter-clockwise (- (* x d/dy) (* y d/dx)))
(define outward (+ (* x d/dx) (* y d/dy)))

(pec ((dx counter-clockwise) R3-rect-point))
#| Result:
(* -1 y0)
|#

(pec ((((differential x) counter-clockwise) identity) R3-rect-point))
#| Result:
(* -1 y0)
|#

(pec ((dx outward) R3-rect-point))
#| Result:
x0
|#

(pec ((((differential x) outward) identity) R3-rect-point))
#| Result:
x0
|#

(pec ((dy counter-clockwise) R3-rect-point))
#| Result:
x0
|#

(pec ((((differential y) counter-clockwise) identity) R3-rect-point))
#| Result:
x0
|#

(pec ((dy outward) R3-rect-point))
#| Result:
y0
|#

(pec ((((differential y) outward) identity) R3-rect-point))
#| Result:
y0
|#

(pec ((dr counter-clockwise) R3-cyl-point))
#| Result:
0
|#

(pec ((((differential r) counter-clockwise) identity) R3-cyl-point))
#| Result:
0
|#

(pec ((dr outward) R3-cyl-point))
#| Result:
r0
|#

(pec ((((differential r) outward) identity) R3-cyl-point))
#| Result:
r0
|#

(pec ((dtheta counter-clockwise) R3-cyl-point))
#| Result:
1
|#

(pec ((((differential theta) counter-clockwise) identity) R3-cyl-point))
#| Result:
1
|#

(pec ((dtheta outward) R3-cyl-point))
#| Result:
0
|#

(pec ((((differential theta) outward) identity) R3-cyl-point))
#| Result:
0
|#
|#

#|
(install-coordinates R2-rect (up 'x 'y))
(define R2-rect-point ((R2-rect '->point) (up 'x0 'y0)))

(install-coordinates R1-rect 't)

(define mu (literal-manifold-map 'mu R1-rect R2-rect))

(define f (literal-scalar-field 'f R2-rect))

(pec ((((differential mu) d/dt) f)
      ((R1-rect '->point) 'tau)))
#| Result:
(+ (* (((partial 1) f) (up (mu0 tau) (mu1 tau))) ((D mu1) tau))
   (* (((partial 0) f) (up (mu0 tau) (mu1 tau))) ((D mu0) tau)))
|#

(pec ((dx ((differential mu) d/dt))
      ((R1-rect '->point) 'tau)))
#| Result:
((D mu0) tau)
|#
    
(pec ((dy ((differential mu) d/dt))
      ((R1-rect '->point) 'tau)))
#| Result:
((D mu1) tau)
|#


;;; but this is a fraud... Note that if we have a non-coordinate basis
;;; the dual does not work on the transported vector.

(define e0 (literal-vector-field 'e0 R2-rect))

(define e1 (literal-vector-field 'e1 R2-rect))

(define edual (vector-basis->dual (down e0 e1) R2-rect))

(pec (((ref edual 0) ((differential mu) d/dt))
      ((R1-rect '->point) 'tau)))
;Bad point: rectangular #(tau)

;;; However, if we kludge the correct argument it gives the expected
;;; answer.

(pec (((ref edual 0)
       (procedure->vector-field
	(lambda (f)
	  (lambda (m)
	    ((((differential mu) d/dt) f)
	     ((R1-rect '->point) 't))))))
      R2-rect-point))
#| Result:
(/ (+ (* -1 (e1^1 (up x0 y0)) ((D mu0) t))
      (* (e1^0 (up x0 y0)) ((D mu1) t)))
   (+ (* -1 (e0^0 (up x0 y0)) (e1^1 (up x0 y0)))
      (* (e0^1 (up x0 y0)) (e1^0 (up x0 y0)))))
|#

;;; General path on the sphere

(define mu
  (compose (S2-spherical '->point)
	   (up (literal-function 'theta)
	       (literal-function 'phi))
	   (R1-rect '->coords)))

(define f
  (compose (literal-function 'f (-> (UP Real Real) Real))
	   (S2-spherical '->coords)))


(pec ((((differential mu) d/dt) f)
      ((R1-rect '->point) 't)))
#| Result:
(+ (* ((D theta) t) (((partial 0) f) (up (theta t) (phi t))))
   (* (((partial 1) f) (up (theta t) (phi t))) ((D phi) t)))
|#
|#

;;; Another way to obtain a vector field over a map is to start with a
;;; vector field on the target manifold.  Given a vector field v-on-M
;;; and a map mu:N->M, we obtain a vector field over the map.  This is
;;; a thing like a vector field on M restricted to the targets of
;;; mu:N->M and evaluated on points of N.

(define ((vector-field->vector-field-over-map mu:N->M) v-on-M)
  (procedure->vector-field
   (lambda (f-on-M)
     (compose (v-on-M f-on-M)
	      mu:N->M))
   `((vector-field->vector-field-over-map ,(diffop-name mu:N->M))
     ,(diffop-name v-on-M))))

;;; A form field can also be transported across a map.  Given a form
;;; field on M and a map mu:N->M, we obtain a thing like a form field
;;; on M that measures vectors over the map mu:N->M and is evaluated
;;; on points of N.
#|
(define ((1form-field-over-map mu:N->M) w-on-M)
  (procedure->1form-field
   (lambda (V-over-mu)
     (lambda (n)
       ((w-on-M
	 (vector-field-over-map->vector-field V-over-mu n))
	(mu:N->M n))))
   `((1form-field-over-map ,(diffop-name mu:N->M))
     ,(diffop-name w-on-M))))
|#

(define ((form-field->form-field-over-map mu:N->M) w-on-M)
  
  (define (vector-field-over-map->vector-field V-over-mu n) 
    ;; This helper has no clear meaning.
    (procedure->vector-field
     (lambda (f)
       (lambda (m)
	 ;;(assert (= m (mu:N->M n)))
	 ((V-over-mu f) n)))
     `(vector-field-over-map->vector-field
       ,(diffop-name V-over-mu))))

  (procedure->nform-field
   (lambda vectors-over-map
     (assert (= (length vectors-over-map) (get-rank w-on-M)))
     (lambda (n)
       ((apply w-on-M
	       (map (lambda (V-over-mu)
		      (vector-field-over-map->vector-field V-over-mu n))
		    vectors-over-map))
	(mu:N->M n))))
   (get-rank w-on-M)
   `((form-field->form-field-over-map ,(diffop-name mu:N->M))
     ,(diffop-name w-on-M))))

(define (basis->basis-over-map mu:N->M basis-on-M)
  (let ((vector-basis-on-M (basis->vector-basis basis-on-M))
	(dual-basis-on-M (basis->1form-basis basis-on-M)))
    (make-basis
     (s:map/r (vector-field->vector-field-over-map mu:N->M)
	      vector-basis-on-M)
     (s:map/r (form-field->form-field-over-map mu:N->M)
	      dual-basis-on-M))))

#|
(install-coordinates S2-spherical (up 'theta 'phi))

(define f (literal-scalar-field 'f S2-spherical))

;;; General path on the sphere
(define mu
  (compose (S2-spherical '->point)
	   (up (literal-function 'theta)
	       (literal-function 'phi))
	   (R1-rect '->coords)))

(pec ((((vector-field->vector-field-over-map mu) d/dtheta) f)
      ((R1-rect '->point) 't)))
#| Result:
(((partial 0) f) (up (theta t) (phi t)))
|#

(pec ((((form-field->form-field-over-map mu) dtheta)
       ((differential mu) d/dt))
      ((R1-rect '->point) 't)))
#| Result:
((D theta) t)
|#

(define foo
  (basis->basis-over-map mu
			 (coordinate-system->basis S2-spherical)))

(pec
 (((basis->1form-basis foo)
   (basis->vector-basis foo))
  ((R1-rect '->point) 't)))
#| Result:
(up (down 1 0) (down 0 1))
|#

(pec
 (((basis->1form-basis foo)
   ((differential mu) d/dt))
  ((R1-rect '->point) 't)))
#| Result:
(up ((D theta) t) ((D phi) t))
|#
|#

;;; The following helper is used to define pullbacks of forms.
#|
(define ((effective-pushforward mu:N->M n) v-on-N)
  (procedure->vector-field
   (lambda (g-on-M)
     (lambda (m)
       ;;(assert (= m (mu:N->M n)))
       ((((differential mu:N->M) v-on-N) 
	 g-on-M) 
	n)))
   `((differential ,(diffop-name mu:N->M))
     ,(diffop-name v-on-N))))

;;; We extend the pullback to 1-forms:

(define ((pullback-1form mu:N->M) omega-on-M)
  (procedure->1form-field
   (lambda (v-on-N)
     (lambda (n)
       ((omega-on-M
	 ((effective-pushforward mu:N->M n) v-on-N))
	(mu:N->M n))))
   `((pullback ,(diffop-name mu:N->M))
     ,(diffop-name omega-on-M))))

(define ((pullback-1form mu:N->M) omega-on-M)
  (procedure->1form-field
   (lambda (X-on-N)
     (((form-field->form-field-over-map mu:N->M) omega-on-M)
      ((differential mu:N->M) X-on-N)))
   `((pullback ,(diffop-name mu:N->M))
     ,(diffop-name omega-on-M))))

(define ((pullback mu:N->M) omega-on-M)
  (let ((k (get-rank omega-on-M)))
    (if (= k 0)
	((pullback-function mu:N->M) omega-on-M)
	(let ((the-pullback
	       (lambda args
		 (assert (fix:= (length args) k))
		 (lambda (n)
		   ((apply omega-on-M
			   (map (effective-pushforward mu:N->M n)
				args))
		    (mu:N->M n))))))
	  (procedure->nform-field the-pullback
				  k
				  `((pullback ,(diffop-name mu:N->M))
				    ,(diffop-name omega-on-M)))))))
|#
;;; The general case
;;; ((mu^* w) v) = w (mu_* v) = (w^mu ((d mu) v))

(define ((pullback-form mu:N->M) omega-on-M)
  (let ((k (get-rank omega-on-M)))
    (if (= k 0)
	((pullback-function mu:N->M) omega-on-M)
	(procedure->nform-field
	 (lambda vectors-on-N
	   (apply ((form-field->form-field-over-map mu:N->M) omega-on-M)
		  (map (differential mu:N->M)
		       vectors-on-N)))
	 k
	 `((pullback ,(diffop-name mu:N->M))
	   ,(diffop-name omega-on-M))))))

(define (pullback-vector-field mu:N->M mu^-1:M->N)
  (pushforward-vector mu^-1:M->N mu:N->M))

(define ((pullback mu:N->M #!optional mu^-1:M->N) thing)
  (if (vector-field? thing)
      (if (default-object? mu^-1:M->N)
	  (error "Pullback vector needs inverse map")
	  ((pullback-vector-field mu:N->M mu^-1:M->N) thing))
      ((pullback-form mu:N->M) thing)))

#|
(pec (((pullback mu) f)
      ((R1-rect '->point) 't)))
#| Result:
(f (up (theta t) (phi t)))
|#

(pec
  ((((pullback mu) dtheta) d/dt)
   ((R1-rect '->point) 't)))
#| Result:
((D theta) t)
|#

(pec
  ((((pullback mu)
     (wedge dtheta dphi))
    d/dt d/dt)
   ((R1-rect '->point) 't)))
#| Result:
0
|#

|#

#|
(install-coordinates R3-rect (up 'x 'y 'z))

(install-coordinates R3-cyl (up 'r 'theta 'zeta))

(define mu
  (compose
   (R3-cyl '->point)
   (up (literal-function 'mu^r
			 (-> (UP Real Real Real) Real))
       (literal-function 'mu^theta
			 (-> (UP Real Real Real) Real))
       (literal-function 'mu^zeta
			 (-> (UP Real Real Real) Real)))
   (R3-rect '->coords)))

(pec
  ((((pullback mu) dtheta) d/dx)
   ((R3-rect '->point) (up 'x 'y 'z))))
#| Result:
(((partial 0) mu^theta) (up x y z))
|#

(pec
  ((((pullback mu) dtheta) d/dy)
   ((R3-rect '->point) (up 'x 'y 'z))))
#| Result:
(((partial 1) mu^theta) (up x y z))
|#

(pec
  ((((pullback mu) dr) d/dx)
   ((R3-rect '->point) (up 'x 'y 'z))))
#| Result:
(((partial 0) mu^r) (up x y z))
|#

(pec
  ((((pullback mu) dr) d/dy)
   ((R3-rect '->point) (up 'x 'y 'z))))
#| Result:
(((partial 1) mu^r) (up x y z))
|#

(pec
 ((((pullback mu)
    (wedge dr dtheta))
   d/dx d/dy)
  ((R3-rect '->point)
   (up 'x 'y 'z))))
#| Result:
(+ (* (((partial 1) mu^theta) (up x y z))
      (((partial 0) mu^r) (up x y z)))
   (* -1
      (((partial 1) mu^r) (up x y z))
      (((partial 0) mu^theta) (up x y z))))
|#

|#

#|
(define m ((R2-rect '->point) (up 3 4)))

(install-coordinates R2-rect (up 'x 'y))

(define phi
  (compose (R2-rect '->point)
	   (up square cube)
	   (R1-rect '->coords)))

(pec ((((pullback phi) (* x dy)) d/dt)
      ((R1-rect '->point) 't0)))
#| Result:
(* 3 (expt t0 4))
|#

(define psi
  (compose (R1-rect '->point)
	   (lambda (v)
	     (let ((x (ref v 0))
		   (y (ref v 1)))
	       (- x y)))
	   (R2-rect '->coords)))

(pec ((((pullback psi) dt)
       (literal-vector-field 'u R2-rect))
      ((R2-rect '->point) (up 'x0 'y0))))
#| Result:
(+ (u^0 (up x0 y0)) (* -1 (u^1 (up x0 y0))))
|#
|#

#|
;;; pullback commutes with exterior derivative

(install-coordinates R3-rect (up 'x 'y 'z))

(define R3-rect-chi (R3-rect '->coords))
(define R3-rect-chi-inverse (R3-rect '->point))
(define R3-rect->R (-> (UP Real Real Real) Real))
(define m3 ((R3-rect '->point) (up 'x0 'y0 'z0)))

(define alpha (literal-function 'alpha R3-rect->R))
(define beta (literal-function 'beta R3-rect->R))
(define gamma (literal-function 'gamma R3-rect->R))

(define theta
  (+ (* (compose alpha R3-rect-chi) dx)
     (* (compose beta R3-rect-chi) dy)
     (* (compose gamma R3-rect-chi) dz)))


(define R2-chi (R2-rect '->coords))
(define R2-chi-inverse (R2-rect '->point))
(define R2-rect->R (-> (UP Real Real) Real))
(define X2 (literal-vector-field 'X R2-rect))
(define Y2 (literal-vector-field 'Y R2-rect))
(define m2 ((R2-rect '->point) (up 'u0 'v0)))

(define mu
  (compose R3-rect-chi-inverse
	   (up (literal-function 'mu^x R2-rect->R)
	       (literal-function 'mu^y R2-rect->R)
	       (literal-function 'mu^z R2-rect->R))
	   R2-chi))

;;; first pullback a function

(define f
  (compose (literal-function 'f R3-rect->R)
	   R3-rect-chi))

(pec
 (((- ((pullback mu) (d f))
      (d ((pullback mu) f)))
   X2)
  m2))
#| Result:
0
|#

;;; now pullback a form

(pec (R3-rect-chi (mu m2)))
#| Result:
(up (mu^x (up u0 v0)) (mu^y (up u0 v0)) (mu^z (up u0 v0)))
|#

(pec ((((pullback mu) theta) X2) m2))
#| Result:
(+
 (* (((partial 0) mu^x) (up u0 v0))
    (alpha (up (mu^x (up u0 v0)) (mu^y (up u0 v0)) (mu^z (up u0 v0))))
    (X^0 (up u0 v0)))
 (* (((partial 1) mu^x) (up u0 v0))
    (alpha (up (mu^x (up u0 v0)) (mu^y (up u0 v0)) (mu^z (up u0 v0))))
    (X^1 (up u0 v0)))
 (* (((partial 0) mu^y) (up u0 v0))
    (beta (up (mu^x (up u0 v0)) (mu^y (up u0 v0)) (mu^z (up u0 v0))))
    (X^0 (up u0 v0)))
 (* (((partial 1) mu^y) (up u0 v0))
    (beta (up (mu^x (up u0 v0)) (mu^y (up u0 v0)) (mu^z (up u0 v0))))
    (X^1 (up u0 v0)))
 (* (((partial 0) mu^z) (up u0 v0))
    (X^0 (up u0 v0))
    (gamma (up (mu^x (up u0 v0)) (mu^y (up u0 v0)) (mu^z (up u0 v0)))))
 (* (((partial 1) mu^z) (up u0 v0))
    (gamma (up (mu^x (up u0 v0)) (mu^y (up u0 v0)) (mu^z (up u0 v0))))
    (X^1 (up u0 v0))))
|#

(pec
 (((- ((pullback mu) (d theta))
      (d ((pullback mu) theta)))
   X2 Y2)
  m2))
#| Result:
0
|#
;;; works.
|#

#|
;;; Pullback commutes with wedge

(pec
 (let ((theta (literal-1form-field 'theta R3-rect))
       (phi (literal-1form-field 'phi R3-rect)))
   (((- (wedge ((pullback mu) theta) ((pullback mu) phi))
	((pullback mu) (wedge theta phi)))
     X2
     Y2)
    m2)))
#| Result:
0
|#

(pec
 (let ((theta (literal-manifold-function 'f R3-rect))
       (phi (literal-1form-field 'phi R3-rect)))
   (((- (wedge ((pullback mu) theta) ((pullback mu) phi))
	((pullback mu) (wedge theta phi)))
     X2)
    m2)))
#| Result:
0
|#
|#