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

;;;; Metrics 

;;; A metric is a function that takes two vector fields and produces a
;;; function on the manifold.

#|
(set! *divide-out-terms* #f)
(set! *factoring* #t)

;;; Example: natural metric on a sphere of radius R

(define 2-sphere R2-rect)
(install-coordinates 2-sphere (up 'theta 'phi))

(define ((g-sphere R) u v)
  (* (square R)
     (+ (* (dtheta u) (dtheta v))
	(* (compose (square sin) theta)
	   (dphi u)
	   (dphi v)))))

(define u (literal-vector-field 'u 2-sphere))
(define v (literal-vector-field 'v 2-sphere))

(pec (((g-sphere 'R) u v)
      ((2-sphere '->point) (up 'theta0 'phi0))))
#| Result:
(* (+ (* (v^0 (up theta0 phi0))
	 (u^0 (up theta0 phi0)))
      (* (expt (sin theta0) 2)
	 (v^1 (up theta0 phi0))
	 (u^1 (up theta0 phi0))))
   (expt R 2))
|#

;;; Example: Lorentz metric on R^4

(define SR R4-rect)
(install-coordinates SR (up 't 'x 'y 'z))

(define ((g-Lorentz c) u v)
  (+ (* (dx u) (dx v))
     (* (dy u) (dy v))
     (* (dz u) (dz v))
     (* -1 (square c) (dt u) (dt v))))


;;; Example: general metric on R^2

(install-coordinates R2-rect (up 'x 'y))
(define R2-basis (coordinate-system->basis R2-rect))

(define ((g-R2 g_00 g_01 g_11) u v)
  (+ (* g_00 (dx u) (dx v))
     (* g_01 (+ (* (dx u) (dy v)) (* (dy u) (dx v))))
     (* g_11 (dy u) (dy v))))

(pec (((g-R2 'a 'b 'c)
       (literal-vector-field 'u R2-rect)
       (literal-vector-field 'v R2-rect))
      ((R2-rect '->point) (up 'x0 'y0))))
#| Result:
(+ (* (u^0 (up x0 y0)) (v^0 (up x0 y0)) a)
   (* (+ (* (v^0 (up x0 y0)) (u^1 (up x0 y0)))
	 (* (u^0 (up x0 y0)) (v^1 (up x0 y0))))
      b)
   (* (v^1 (up x0 y0)) (u^1 (up x0 y0)) c))
|#
|#

#|
(define ((coordinate-system->metric-components coordsys) xi)
  (let ((xi->x   	;assumes internal rectangular representation
	 (lambda (xi)
	   (manifold-point-representation
	    ((point coordsys) xi)))))
    (define (Qd v)
      (* ((D xi->x) xi) v))
    (* 1/2
       ((D (D (lambda (v)
		(dot-product (Qd v) (Qd v)))))
	(zero-like xi)))))

(define ((coordinate-system->metric-components coordsys) xi)
  (let* ((n (coordsys 'dimension))
	 (xi->x    ;assumes internal rectangular representation
	  (compose manifold-point-representation
		   (point coordsys)))
	 (h ((D xi->x) xi)))
    (s:generate n 'down
		(lambda (i)
		  (s:generate n 'down
			      (lambda (j)
				(dot-product (ref h i)
					     (ref h j))))))))
|#

(define (coordinate-system->metric-components coordsys)
  (let* ((n (coordsys 'dimension))
	 (xi->x	;assumes internal rectangular representation
	  (compose manifold-point-representation
		   (point coordsys))))
    (embedding-map->metric-components n xi->x)))

(define (embedding-map->metric-components n xi->rectangular)
  (let ((h (D xi->rectangular)))
    (if (= n 1)
	(down (down (dot-product h h)))
	(s:generate n 'down
		    (lambda (i)
		      (s:generate n 'down
				  (lambda (j)
				    (dot-product (ref h i)
						 (ref h j)))))))))

#|
((coordinate-system->metric-components R3-spherical) (up 'r 'theta 'phi))
#|
(down (down 1 0 0)
      (down 0 (expt r 2) 0)
      (down 0 0 (* (expt r 2) (expt (sin theta) 2))))
|#
|#

(define (coordinate-system->metric coordinate-system)
  (let* ((basis (coordinate-system->basis coordinate-system))
	 (1form-basis (basis->1form-basis basis))
	 (->components
	  (coordinate-system->metric-components coordinate-system))
	 (Chi (chart coordinate-system)))
  (define ((the-metric v1 v2) m)
    (let ((gcoeffs (->components (Chi m))))
      (* (* gcoeffs ((1form-basis v1) m))
	 ((1form-basis v2) m))))
  (declare-argument-types! the-metric
			   (list vector-field? vector-field?))
  the-metric))

#|
(s:map/r (lambda (v1)
	   (s:map/r (lambda (v2)
		      (((coordinate-system->metric R3-spherical) v1 v2)
		       ((point R3-spherical) (up 'r 'theta 'phi))))
		    (coordinate-system->vector-basis R3-spherical)))
	 (coordinate-system->vector-basis R3-spherical))
#|
(down (down 1 0 0)
      (down 0 (expt r 2) 0)
      (down 0 0 (* (expt r 2) (expt (sin theta) 2))))
|#
|#

(define (coordinate-system->inverse-metric coordinate-system)
  (let* ((basis (coordinate-system->basis coordinate-system))
	 (vector-basis (basis->vector-basis basis))
	 (->components
	  (/ 1
	     (coordinate-system->metric-components coordinate-system)))
	 (Chi (chart coordinate-system)))
  (define ((the-inverse-metric w1 w2) m)
    (let ((gcoeffs (->components (Chi m))))
      (* (* gcoeffs
	    (s:map/r (lambda (e) ((w1 e) m))
		     vector-basis))
	 (s:map/r (lambda (e) ((w2 e) m))
		  vector-basis))))
  (declare-argument-types! the-inverse-metric
			   (list 1form-field? 1form-field?))
  the-inverse-metric))

#|
(s:map/r (lambda (w1)
	   (s:map/r (lambda (w2)
		      (((coordinate-system->inverse-metric R3-spherical) w1 w2)
		       ((point R3-spherical) (up 'r 'theta 'phi))))
		    (coordinate-system->1form-basis R3-spherical)))
	 (coordinate-system->1form-basis R3-spherical))
#|
(up (up 1 0 0)
    (up 0 (/ 1 (expt r 2)) 0)
    (up 0 0 (/ 1 (* (expt r 2) (expt (sin theta) 2)))))
|#
|#

;;; Symbolic metrics are often useful for testing.

(define (make-metric name coordinate-system)
  (define (gij i j)
    (if (<= i j)
	(literal-manifold-function
	 (string->symbol
	  (string-append (symbol->string name)
			 "_"
			 (number->string i)
			 (number->string j)))
	 coordinate-system)
	(gij j i)))
  gij)
				    
(define (literal-metric name coordinate-system)
  ;; Flat coordinate systems here only.
  (let ((basis (coordinate-system->basis coordinate-system)))
    (let ((1form-basis (basis->1form-basis basis))
	  (gij (make-metric name coordinate-system)))
      (let ((n (s:dimension 1form-basis)))
	(let ((gcoeffs
	       (s:generate n 'down
			   (lambda (i)
			     (s:generate n 'down
					 (lambda (j)
					   (gij i j)))))))
	  (define (the-metric v1 v2)
	    (* (* gcoeffs (1form-basis v1))
	       (1form-basis v2)))
	  (declare-argument-types! the-metric
				   (list vector-field? vector-field?))
	  the-metric)))))
#|
(install-coordinates R3-rect (up 'x 'y 'z))

(set! *factoring* #f)

(pec (((literal-metric 'g R3-rect)
       (literal-vector-field 'u R3-rect)
       (literal-vector-field 'v R3-rect))
      ((R3-rect '->point) (up 'x0 'y0 'z0))))
#| Result:
(+ (* (v^0 (up x0 y0 z0)) (u^0 (up x0 y0 z0)) (g_00 (up x0 y0 z0)))
   (* (v^0 (up x0 y0 z0)) (g_01 (up x0 y0 z0)) (u^1 (up x0 y0 z0)))
   (* (v^0 (up x0 y0 z0)) (g_02 (up x0 y0 z0)) (u^2 (up x0 y0 z0)))
   (* (u^0 (up x0 y0 z0)) (v^1 (up x0 y0 z0)) (g_01 (up x0 y0 z0)))
   (* (u^0 (up x0 y0 z0)) (v^2 (up x0 y0 z0)) (g_02 (up x0 y0 z0)))
   (* (v^1 (up x0 y0 z0)) (u^1 (up x0 y0 z0)) (g_11 (up x0 y0 z0)))
   (* (v^1 (up x0 y0 z0)) (g_12 (up x0 y0 z0)) (u^2 (up x0 y0 z0)))
   (* (v^2 (up x0 y0 z0)) (u^1 (up x0 y0 z0)) (g_12 (up x0 y0 z0)))
   (* (v^2 (up x0 y0 z0)) (u^2 (up x0 y0 z0)) (g_22 (up x0 y0 z0))))
|#
|#

(define (components->metric components basis)
  (let ((1form-basis (basis->1form-basis basis)))
    (define (the-metric v1 v2)
      (* (1form-basis v1) (* components (1form-basis v2))))
    the-metric))

#|
;;; Code below is OK
(define ((metric->components metric basis) m)
  (let ((vector-basis (basis->vector-basis basis)))
    (s:map/r (lambda (e_i)
	       (s:map/r (lambda (e_j)
			  ((metric e_i e_j) m))
			vector-basis))
	     vector-basis)))
|#

(define (metric->components metric basis)
  (let ((vector-basis (basis->vector-basis basis)))
    (s:map/r (lambda (e_i)
	       (s:map/r (lambda (e_j)
			  (metric e_i e_j))
			vector-basis))
	     vector-basis)))


;;; Given a metric and a basis, to compute the inverse metric

(define (metric->inverse-components metric basis)
  (define (the-coeffs m)
    (let ((g_ij ((metric->components metric basis) m))
	  (1form-basis (basis->1form-basis basis)))
      (let ((g^ij
	     (s:inverse (typical-object 1form-basis)
			g_ij
			(typical-object 1form-basis))))
	 g^ij)))
  the-coeffs)    

#|
;;; This code seriously degrades performance, use version above.
(define (metric->inverse-components metric basis)
  (let ((g_ij (metric->components metric basis))
	(1form-basis (basis->1form-basis basis)))
    (let ((g^ij
	   (s:inverse (typical-object 1form-basis)
		      g_ij
		      (typical-object 1form-basis))))
      g^ij)))
|#

#|
;;; Code below is OK
(define (metric:invert metric basis)
  (define (the-inverse-metric w1 w2)
    (lambda (m)
      (let ((vector-basis (basis->vector-basis basis))
	    (g^ij ((metric->inverse-components metric basis) m)))
	(* (* g^ij ((s:map/r w1 vector-basis) m))
	   ((s:map/r w2 vector-basis) m)))))
  (declare-argument-types! the-inverse-metric
			   (list 1form-field? 1form-field?))
  the-inverse-metric)
|#

(define (metric:invert metric basis)
  (define (the-inverse-metric w1 w2)
    (let ((vector-basis (basis->vector-basis basis))
	  (g^ij (metric->inverse-components metric basis)))
      (* (* g^ij (s:map/r w1 vector-basis))
	 (s:map/r w2 vector-basis))))
  (declare-argument-types! the-inverse-metric
			   (list 1form-field? 1form-field?))
  the-inverse-metric)

#|
(install-coordinates R2-rect (up 'x 'y))
(define R2-basis (coordinate-system->basis R2-rect))

(define ((g-R2 g_00 g_01 g_11) u v)
  (+ (* g_00 (dx u) (dx v))
     (* g_01 (+ (* (dx u) (dy v)) (* (dy u) (dx v))))
     (* g_11 (dy u) (dy v))))

(pec (((metric:invert (g-R2 'a 'b 'c) R2-basis)
       (literal-1form-field 'omega R2-rect)
       (literal-1form-field 'theta R2-rect))
      ((R2-rect '->point) (up 'x0 'y0))))
#| Result:
(/ (+ (* a (theta_1 (up x0 y0)) (omega_1 (up x0 y0)))
      (* -1 b (theta_1 (up x0 y0)) (omega_0 (up x0 y0)))
      (* -1 b (omega_1 (up x0 y0)) (theta_0 (up x0 y0)))
      (* c (omega_0 (up x0 y0)) (theta_0 (up x0 y0))))
   (+ (* a c) (* -1 (expt b 2))))
|#

;;; Test of inversion

(pec
 (let* ((g (g-R2 'a 'b 'c))
	(gi (metric:invert g R2-basis))
	(vector-basis (list d/dx d/dy))
	(dual-basis (list dx dy))
	(m ((R2-rect '->point) (up 'x0 'y0))))
   (matrix:generate 2 2
     (lambda (i k)
       (sigma (lambda (j)
		(* ((gi (ref dual-basis i) (ref dual-basis j)) m)
		   ((g  (ref vector-basis j) (ref vector-basis k)) m)))
	      0 1)))))
#| Result:
(matrix-by-rows (list 1 0) (list 0 1))
|#
|#

;;; over a map

(define (metric-over-map mu:N->M g-on-M)
  (define (vector-field-over-map->vector-field V-over-mu n) 
    ;; This helper has no clear meaning.
    (procedure->vector-field
     (lambda (f)
       (lambda (m)
	 ;;(assert (= m (mu:N->M n)))
	 ((V-over-mu f) n)))
     `(vector-field-over-map->vector-field
       ,(diffop-name V-over-mu))))
  (define (the-metric v1 v2)
    (lambda (n)
      ((g-on-M
	(vector-field-over-map->vector-field v1 n)
	(vector-field-over-map->vector-field v2 n))
       (mu:N->M n))))
  (declare-argument-types! the-metric
			   (list vector-field? vector-field?))
  the-metric)

;;; Raising and lowering indices...

;;; To make a vector field into a one-form field 
;;;  ie a (1,0) tensor into a (0,1) tensor

(define ((lower metric) u)
  (define (omega v)
    (metric v u))
  (procedure->1form-field omega
    `(lower ,(diffop-name u)
	    ,(diffop-name metric))))

(define vector-field->1form-field lower)
(define drop1 lower)


;;; To make a one-form field  into a vector field
;;;  ie a (0,1) tensor into a (1,0) tensor

(define (raise metric basis)
  (let ((gi (metric:invert metric basis)))
    (lambda (omega)
      (define v
	(contract (lambda (e_i e~i)
		    (* (gi omega e~i) e_i))
		  basis))
      (procedure->vector-field v
	`(raise ,(diffop-name omega)
		,(diffop-name metric))))))

(define 1form-field->vector-field raise)
(define raise1 raise)

;;; For making a (2,0) tensor into a (0,2) tensor

(define ((drop2 metric-tensor basis) tensor)
  (define (omega v1 v2)
    (contract
     (lambda (e1 w1)
       (contract
	(lambda (e2 w2)
	  (* (metric-tensor v1 e1)
	     (tensor w1 w2)
	     (metric-tensor e2 v2)))
	basis))
     basis))
  (declare-argument-types! omega (list vector-field? vector-field?))
  omega)


;;; For making a (0,2) tensor into a (2,0) tensor

(define (raise2 metric-tensor basis)
  (let ((gi (metric:invert metric-tensor basis)))
    (lambda (tensor02)
      (define (v2 omega1 omega2)
	(contract
	 (lambda (e1 w1)
	   (contract
	    (lambda (e2 w2)
	      (* (gi omega1 w1)
		 (tensor02 e1 e2)
		 (gi w2 omega2)))
	    basis))
	 basis))
      (declare-argument-types! v2 (list 1form-field? 1form-field?))
      v2)))


;;; To compute the trace of a (0,2) tensor

(define (trace2down metric-tensor basis)
  (let ((inverse-metric-tensor
         (metric:invert metric-tensor basis)))
    (lambda (tensor02)
      (define f
	(contract
	 (lambda (e1 w1)
	   (contract
	    (lambda (e2 w2)
	      (* (inverse-metric-tensor w1 w2)
		 (tensor02 e1 e2)))
	    basis))
	 basis))
      (declare-argument-types! f (list function?))
      f)))


;;; To compute the trace of a (2,0) tensor

(define ((trace2up metric-tensor basis) tensor20)
  (define f
    (contract
     (lambda (e1 w1)
       (contract
	(lambda (e2 w2)
	  (* (metric-tensor e1 e2)
	     (tensor20 w1 w2)))
	basis))
     basis))
  (declare-argument-types! f (list function?))
  f)



;;; Note: raise needs an extra argument -- the basis -- why?
#|
(pec
 ((((lower (g-R2 'a 'b 'c))
    (literal-vector-field 'v R2-rect))
   (literal-vector-field 'w R2-rect))
  ((R2-rect '->point) (up 'x0 'y0))))
#| Result:
(+ (* a (v^0 (up x0 y0)) (w^0 (up x0 y0)))
   (* b (v^0 (up x0 y0)) (w^1 (up x0 y0)))
   (* b (v^1 (up x0 y0)) (w^0 (up x0 y0)))
   (* c (v^1 (up x0 y0)) (w^1 (up x0 y0))))
|#

(pec
 ((((raise (g-R2 'a 'b 'c) R2-basis)
    ((lower (g-R2 'a 'b 'c)) (literal-vector-field 'v R2-rect)))
   (compose (literal-function 'w (-> (UP Real Real) Real))
	    (R2-rect '->coords)))
  ((R2-rect '->point) (up 'x0 'y0))))
#| Result:
(+ (* (v^0 (up x0 y0)) (((partial 0) w) (up x0 y0)))
   (* (v^1 (up x0 y0)) (((partial 1) w) (up x0 y0))))
|#
|#

;;; Unfortunately raise is very expensive because the matrix is
;;; inverted for each manifold point.

(define (sharpen metric basis m)
  (let ((g^ij ((metric->inverse-components metric basis) m))
	(vector-basis (basis->vector-basis basis))
	(1form-basis (basis->1form-basis basis)))
    (define (sharp 1form-field)
      (let ((1form-coeffs
	     (s:map/r (lambda (ei) ((1form-field ei) m))
		      vector-basis)))
	(let ((vector-coeffs (* g^ij 1form-coeffs)))
	  (s:sigma/r * vector-coeffs vector-basis))))
    sharp))
    
#|
(pec
 ((((sharpen (g-R2 'a 'b 'c) R2-basis ((R2-rect '->point) (up 'x0 'y0)))
    ((lower (g-R2 'a 'b 'c)) (literal-vector-field 'v R2-rect)))
   (compose (literal-function 'w (-> (UP Real Real) Real))
	    (R2-rect '->coords)))
  ((R2-rect '->point) (up 'x0 'y0))))

#| Result:
(up (* (v^0 (up x0 y0)) (((partial 0) w) (up x0 y0)))
    (* (v^1 (up x0 y0)) (((partial 1) w) (up x0 y0))))
|#
|#

;;; Useful metrics


(define S2-metric
  (let* ((chart
	  (S2-spherical 'coordinate-functions))
	 (theta (ref chart 0))
	 (phi (ref chart 1))
	 (1form-basis
	  (S2-spherical 'coordinate-basis-1form-fields))
	 (dtheta (ref 1form-basis 0))
	 (dphi (ref 1form-basis 1)))

    (define (the-metric v1 v2)
      (+ (* (dtheta v1) (dtheta v2))
	 (* (expt (sin theta) 2)
	    (dphi v1) (dphi v2))))
    (declare-argument-types! the-metric
			     (list vector-field? vector-field?))
    the-metric))
