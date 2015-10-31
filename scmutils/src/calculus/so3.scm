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

;;;  review aspects of SO3

;;; Euler-angles coordinate system is also defined in manifold.scm
(define Euler-angles
  (coordinate-system-at 'Euler 'Euler-patch SO3))
(define Euler-angles-chi (Euler-angles '->coords))
(define Euler-angles-chi-inverse (Euler-angles '->point))
(define-coordinates (up theta phi psi) Euler-angles)
(define Euler-angles-basis (coordinate-system->basis Euler-angles))

;;; for the moment define
;;; alternate-angles coordinate system is also defined in manifold.scm
(define alternate-angles
  (coordinate-system-at 'alternate 'alternate-patch SO3))
(define alternate-angles-chi (alternate-angles '->coords))
(define alternate-angles-chi-inverse (alternate-angles '->point))
(define-coordinates (up vartheta varphi varpsi) alternate-angles)
(define alternate-angles-basis (coordinate-system->basis alternate-angles))

(define time-line the-real-line)
(define time-chi (time-line '->coords))
(define time-chi-inverse (time-line '->point))
(define-coordinates t time-line)


#|
;;;----------------------------------------------------------------
;;; Derive basis for rotations about rectangular coordinate axes.
;;; Of course, these have the generators.

(pe ((D (lambda (xyz)
	  (((D rotate-x) 'theta)
	   ((rotate-x (- 'theta)) xyz))))
     (up 'x 'y 'z)))
(down (up 0 0 0)
      (up 0 0 1)
      (up 0 -1 0))

(pe ((D (lambda (xyz)
	  (((D rotate-y) 'theta)
	   ((rotate-y (- 'theta)) xyz))))
     (up 'x 'y 'z)))
(down (up 0 0 -1)
      (up 0 0 0)
      (up 1 0 0))

(pe ((D (lambda (xyz)
	  (((D rotate-z) 'theta)
	   ((rotate-z (- 'theta)) xyz))))
     (up 'x 'y 'z)))
(down (up 0 1 0)
      (up -1 0 0)
      (up 0 0 0))
|#

;;; tuple multipliers for rotation are obtained with the procedures
;;; rotate-x-tuple, rotate-y-tuple, rotate-z-tuple

#|
want to define vector fields in the group parameter manifold
that correspond to particular spatial rotations

(define (equation-x p q)
  (let ((theta (ref q 0))
	(phi (ref q 1))
	(psi (ref q 2))
	(a (ref p 0))
	(b (ref p 1))
	(c (ref p 2)))
    ((D (lambda (eps)
	  (- (* (rotate-z-tuple (+ phi (* a eps)))
		(rotate-x-tuple (+ theta (* b eps)))
		(rotate-z-tuple (+ psi (* c eps))))
	     (* (rotate-x-tuple eps)
		(rotate-z-tuple phi)
		(rotate-x-tuple theta)
		(rotate-z-tuple psi)))))
     0)))
;;; kind of big

(define (equation2-x p q)
  (let ((theta (ref q 0))
	(phi (ref q 1))
	(psi (ref q 2))
	(a (ref p 0))
	(b (ref p 1))
	(c (ref p 2)))
    ((D (lambda (eps)
	  (- (* (rotate-z-tuple (+ phi (* a eps)))
		(rotate-x-tuple (+ theta (* b eps)))
		(rotate-z-tuple (+ psi (* c eps)))
		(rotate-z-tuple (- psi))
		(rotate-x-tuple (- theta))
		(rotate-z-tuple (- phi)))
	     (rotate-x-tuple eps))))
     0)))

(pe (equation2-x (up 'a 'b 'c) (up 'theta 'phi 'psi)))
(down
 (up 0
     (+ (* c (cos theta)) a)
     (+ (* c (cos phi) (sin theta)) (* -1 b (sin phi))))
 (up (+ (* -1 c (cos theta)) (* -1 a))
     0
     (+ -1 (* c (sin phi) (sin theta)) (* b (cos phi))))
 (up (+ (* -1 c (cos phi) (sin theta)) (* b (sin phi)))
     (+ 1 (* -1 c (sin phi) (sin theta)) (* -1 b (cos phi)))
     0))

;;; mechanical solution

(load "/usr/local/scmutils/src/solve/linreduce")

(define foo-x
  (solve
   (lambda (p)
     (list->vector
      (map simplify
	   (ultra-flatten (equation2-x p (up 'theta 'phi 'psi))))))
   3 9 list))

(pe ((cadr foo-x) #()))
(up (/ (* -1 (sin phi) (cos theta)) (sin theta))
    (cos phi)
    (/ (sin phi) (sin theta)))

;;; e_x is therefore
;;; therefore

(define e_x
  (+ (* (/ (* -1 (sin phi) (cos theta)) (sin theta)) d/dphi)
     (* (cos phi) d/dtheta)
     (* (/ (sin phi) (sin theta)) d/dpsi)))

;;; checks with MTW p.243 (watch notation: phi<->psi )


;;;----------------------------------------------------------------
;;; now for the other two

(define (equation2-z p q)
  (let ((theta (ref q 0))
	(phi (ref q 1))
	(psi (ref q 2))
	(a (ref p 0))
	(b (ref p 1))
	(c (ref p 2)))
    ((D (lambda (eps)
	  (- (* (rotate-z-tuple (+ phi (* a eps)))
		(rotate-x-tuple (+ theta (* b eps)))
		(rotate-z-tuple (+ psi (* c eps)))
		(rotate-z-tuple (- psi))
		(rotate-x-tuple (- theta))
		(rotate-z-tuple (- phi)))
	     (rotate-z-tuple eps))))
     0)))

(pe (equation2-z (up 'a 'b 'c) (up 'theta 'phi 'psi)))
(down
 (up 0
     (+ -1 (* c (cos theta)) a)
     (+ (* c (cos phi) (sin theta)) (* -1 b (sin phi))))
 (up (+ 1 (* -1 c (cos theta)) (* -1 a))
     0
     (+ (* c (sin phi) (sin theta)) (* b (cos phi))))
 (up (+ (* -1 c (cos phi) (sin theta)) (* b (sin phi)))
     (+ (* -1 c (sin phi) (sin theta)) (* -1 b (cos phi)))
     0))

(define foo-z
  (solve
   (lambda (p)
     (list->vector
      (map simplify
	   (ultra-flatten (equation2-z p (up 'theta 'phi 'psi))))))
   3 9 list))

(pe ((cadr foo-z) #()))
(up 1 0 0)

so

(define e_z d/dphi)

;;;----------------------------------------------------------------

(define (equation2-y p q)
  (let ((theta (ref q 0))
	(phi (ref q 1))
	(psi (ref q 2))
	(a (ref p 0))
	(b (ref p 1))
	(c (ref p 2)))
    ((D (lambda (eps)
	  (- (* (rotate-z-tuple (+ phi (* a eps)))
		(rotate-x-tuple (+ theta (* b eps)))
		(rotate-z-tuple (+ psi (* c eps)))
		(rotate-z-tuple (- psi))
		(rotate-x-tuple (- theta))
		(rotate-z-tuple (- phi)))
	     (rotate-y-tuple eps))))
     0)))

(pe (equation2-y (up 'a 'b 'c) (up 'theta 'phi 'psi)))
(down
 (up 0
     (+ (* c (cos theta)) a)
     (+ 1 (* c (cos phi) (sin theta)) (* -1 b (sin phi))))
 (up (+ (* -1 c (cos theta)) (* -1 a))
     0
     (+ (* c (sin phi) (sin theta)) (* b (cos phi))))
 (up (+ -1 (* -1 c (cos phi) (sin theta)) (* b (sin phi)))
     (+ (* -1 c (sin phi) (sin theta)) (* -1 b (cos phi)))
     0))

(define foo-y
  (solve
   (lambda (p)
     (list->vector
      (map simplify
	   (ultra-flatten (equation2-y p (up 'theta 'phi 'psi))))))
   3 9 list))

(pe ((cadr foo-y) #()))
(up (/ (* (cos theta) (cos phi)) (sin theta))
    (sin phi)
    (/ (* -1 (cos phi)) (sin theta)))

(define e_y
  (+ (* (/ (* (cos phi) (cos theta)) (sin theta)) d/dphi)
     (* (sin phi) d/dtheta)
     (* (/ (* -1 (cos phi)) (sin theta)) d/dpsi)))

;;; interchanging psi phi to compare to mtw, checks

;;; summary
|#

(define e_x
  (+ (* (/ (* -1 (sin phi) (cos theta)) (sin theta)) d/dphi)
     (* (cos phi) d/dtheta)
     (* (/ (sin phi) (sin theta)) d/dpsi)))
(define e_y
  (+ (* (/ (* (cos phi) (cos theta)) (sin theta)) d/dphi)
     (* (sin phi) d/dtheta)
     (* (/ (* -1 (cos phi)) (sin theta)) d/dpsi)))
(define e_z d/dphi)

#|

;;;----------------------------------------------------------------

;;; [e_x , e_y] + e_z = 0

(pe (((+ (commutator e_x e_y) e_z)
      (literal-manifold-function 'f Euler-angles))
     (Euler-angles-chi-inverse (up 'theta 'phi 'psi))))
0

;;; [e_y , e_z] + e_x = 0

(pe (((+ (commutator e_y e_z) e_x)
      (literal-manifold-function 'f Euler-angles))
     (Euler-angles-chi-inverse (up 'theta 'phi 'psi))))
0

(pe (((+ (commutator e_z e_x) e_y)
      (literal-manifold-function 'f Euler-angles))
     (Euler-angles-chi-inverse (up 'theta 'phi 'psi))))
0

|#

(define so3-vector-basis
  (down e_x e_y e_z))

(define so3-dual-basis
  (vector-basis->dual so3-vector-basis Euler-angles))

(define so3-basis
  (make-basis so3-vector-basis so3-dual-basis))

#|

(pe ((so3-dual-basis so3-vector-basis)
     (Euler-angles-chi-inverse (up 'theta 'phi 'psi))))
(up (down 1 0 0) (down 0 1 0) (down 0 0 1))

;;; the structure constants
(pe (s:map
     (lambda (e~k)
       (s:map
	(lambda (e_i)
	  (s:map
	   (lambda (e_j)
	     ((e~k (commutator e_i e_j))
	      (Euler-angles-chi-inverse (up 'theta 'phi 'psi))))
	   so3-vector-basis))
	so3-vector-basis))
     so3-dual-basis))
(up (down (down 0 0 0) (down 0 0 -1) (down 0 1 0))
    (down (down 0 0 1) (down 0 0 0) (down -1 0 0))
    (down (down 0 -1 0) (down 1 0 0) (down 0 0 0)))

|#

(define so3-structure-constants
  (up (down (down 0 0 0) (down 0 0 -1) (down 0 1 0))
      (down (down 0 0 1) (down 0 0 0) (down -1 0 0))
      (down (down 0 -1 0) (down 1 0 0) (down 0 0 0))))

;;;----------------------------------------------------------------
;;; Euler velocities as components of differential...

;;; differential of d/dt takes derivatives of functions on rotations
;;; along paths in rotations
#|
(pe
 (let* ((gamma (compose
		Euler-angles-chi-inverse
		(up (literal-function 'f^theta)
		    (literal-function 'f^phi)
		    (literal-function 'f^psi))
		time-chi)))
   ((((differential gamma) d/dt)
     (literal-manifold-function 'f Euler-angles))
    (time-chi-inverse 't))))

(+ (* ((D f^phi) t)
      (((partial 1) f) (up (f^theta t) (f^phi t) (f^psi t))))
   (* ((D f^theta) t)
      (((partial 0) f) (up (f^theta t) (f^phi t) (f^psi t))))
   (* ((D f^psi) t)
      (((partial 2) f) (up (f^theta t) (f^phi t) (f^psi t)))))


;;; components of the differential of d/dt on the Euler basis
(pe (let* ((gamma (compose
		   Euler-angles-chi-inverse
		   (up (literal-function 'f^theta)
		       (literal-function 'f^phi)
		       (literal-function 'f^psi))
		   time-chi))
	   (Euler-basis-over-gamma
	    (basis->basis-over-map gamma Euler-angles-basis))
	   (1form-basis-over-gamma
	    (basis->1form-basis Euler-basis-over-gamma)))
      (s:map (lambda (w) ((w ((differential gamma) d/dt))
			  (time-chi-inverse 't)))
	     1form-basis-over-gamma)))
(up ((D f^theta) t) ((D f^phi) t) ((D f^psi) t))
|#

#|

computing components of quasi-velocity on so3-basis

(pe
 (let* ((gamma (compose
		Euler-angles-chi-inverse
		(up (literal-function 'f^theta)
		    (literal-function 'f^phi)
		    (literal-function 'f^psi))
		time-chi))
	(basis-over-gamma (basis->basis-over-map gamma so3-basis))
	(1form-basis (basis->1form-basis basis-over-gamma))
	(vector-basis (basis->vector-basis basis-over-gamma)))
   (s:map (lambda (w)
	    ((w ((differential gamma) d/dt))
	     (time-chi-inverse 't)))
	  1form-basis)))
(up
 (+ (* (sin (f^phi t)) (sin (f^theta t)) ((D f^psi) t))
    (* (cos (f^phi t)) ((D f^theta) t)))
 (+ (* -1 (cos (f^phi t)) (sin (f^theta t)) ((D f^psi) t))
    (* (sin (f^phi t)) ((D f^theta) t)))
 (+ (* (cos (f^theta t)) ((D f^psi) t)) ((D f^phi) t)))

compare to angular velocity components

(pe ((Euler->omega (up (literal-function 'f^theta)
		       (literal-function 'f^phi)
		       (literal-function 'f^psi)))
     't))
(matrix-by-rows
 (list
  (+ (* (sin (f^phi t)) (sin (f^theta t)) ((D f^psi) t))
     (* (cos (f^phi t)) ((D f^theta) t))))
 (list
  (+ (* -1 (cos (f^phi t)) (sin (f^theta t)) ((D f^psi) t))
     (* (sin (f^phi t)) ((D f^theta) t))))
 (list (+ (* (cos (f^theta t)) ((D f^psi) t)) ((D f^phi) t))))

it checks.

|#

#|

what are the vector fields for which the quasivelocities are the
body components of the angular velocities?

omega_body = M^T omega = M^-1 omega
M = Rz(phi) Rx(theta) Rz(psi)
M^T = M^-1 = Rz(-psi) Rx(-theta) Rz(-phi)

spatial components of vectors rotate by M^-1 to get body components

e_x, e_y, e_z are the basis vector fields that correspond
to rotation about the x, y, z axes
they are vector fields on the space of euler angles

e_xp, e_yp, e_zp are basis vectors that correspond to rotation
about body xp, yp, zp axes

ep(f) = e(f) M

(pe (* (rotate-z-tuple 'phi)
       (* (rotate-x-tuple 'theta)
	  (rotate-z-tuple 'psi))))
(down
 (up (+ (* -1 (sin psi) (cos theta) (sin phi)) (* (cos psi) (cos phi)))
     (+ (* (sin psi) (cos theta) (cos phi)) (* (sin phi) (cos psi)))
     (* (sin theta) (sin psi)))
 (up (+ (* -1 (cos theta) (sin phi) (cos psi)) (* -1 (sin psi) (cos phi)))
     (+ (* (cos theta) (cos psi) (cos phi)) (* -1 (sin psi) (sin phi)))
     (* (sin theta) (cos psi)))
 (up (* (sin theta) (sin phi)) (* -1 (sin theta) (cos phi)) (cos theta)))

(pe (* (down 'e_x 'e_y 'e_z)
	(* (rotate-z-tuple 'phi)
	   (* (rotate-x-tuple 'theta)
	      (rotate-z-tuple 'psi)))))
(down
 (+ (* -1 (sin psi) (cos theta) (sin phi) e_x)
    (* (sin psi) (cos theta) (cos phi) e_y)
    (* (cos psi) (cos phi) e_x)
    (* (sin phi) (cos psi) e_y)
    (* (sin theta) (sin psi) e_z))
 (+ (* -1 (cos theta) (sin phi) (cos psi) e_x)
    (* (cos theta) (cos psi) (cos phi) e_y)
    (* -1 (sin psi) (cos phi) e_x)
    (* -1 (sin psi) (sin phi) e_y)
    (* (sin theta) (cos psi) e_z))
 (+ (* (sin theta) (sin phi) e_x)
    (* -1 (sin theta) (cos phi) e_y)
    (* (cos theta) e_z)))

(pe ((dtheta
      (down
       (+ (* -1 (sin psi) (cos theta) (sin phi) e_x)
	  (* (sin psi) (cos theta) (cos phi) e_y)
	  (* (cos psi) (cos phi) e_x)
	  (* (sin phi) (cos psi) e_y)
	  (* (sin theta) (sin psi) e_z))
       (+ (* -1 (cos theta) (sin phi) (cos psi) e_x)
	  (* (cos theta) (cos psi) (cos phi) e_y)
	  (* -1 (sin psi) (cos phi) e_x)
	  (* -1 (sin psi) (sin phi) e_y)
	  (* (sin theta) (cos psi) e_z))
       (+ (* (sin theta) (sin phi) e_x)
	  (* -1 (sin theta) (cos phi) e_y)
	  (* (cos theta) e_z))))
     (Euler-angles-chi-inverse
      (up 'theta 'phi 'psi))))
(down (cos psi) (* -1 (sin psi)) 0)

(pe ((dphi
      (down
       (+ (* -1 (sin psi) (cos theta) (sin phi) e_x)
	  (* (sin psi) (cos theta) (cos phi) e_y)
	  (* (cos psi) (cos phi) e_x)
	  (* (sin phi) (cos psi) e_y)
	  (* (sin theta) (sin psi) e_z))
       (+ (* -1 (cos theta) (sin phi) (cos psi) e_x)
	  (* (cos theta) (cos psi) (cos phi) e_y)
	  (* -1 (sin psi) (cos phi) e_x)
	  (* -1 (sin psi) (sin phi) e_y)
	  (* (sin theta) (cos psi) e_z))
       (+ (* (sin theta) (sin phi) e_x)
	  (* -1 (sin theta) (cos phi) e_y)
	  (* (cos theta) e_z))))
     (Euler-angles-chi-inverse
      (up 'theta 'phi 'psi))))
(down (/ (sin psi) (sin theta)) (/ (cos psi) (sin theta)) 0)

(pe ((dpsi
      (down
       (+ (* -1 (sin psi) (cos theta) (sin phi) e_x)
	  (* (sin psi) (cos theta) (cos phi) e_y)
	  (* (cos psi) (cos phi) e_x)
	  (* (sin phi) (cos psi) e_y)
	  (* (sin theta) (sin psi) e_z))
       (+ (* -1 (cos theta) (sin phi) (cos psi) e_x)
	  (* (cos theta) (cos psi) (cos phi) e_y)
	  (* -1 (sin psi) (cos phi) e_x)
	  (* -1 (sin psi) (sin phi) e_y)
	  (* (sin theta) (cos psi) e_z))
       (+ (* (sin theta) (sin phi) e_x)
	  (* -1 (sin theta) (cos phi) e_y)
	  (* (cos theta) e_z))))
     (Euler-angles-chi-inverse
      (up 'theta 'phi 'psi))))
(down (/ (* -1 (cos theta) (sin psi)) (sin theta))
      (/ (* -1 (cos psi) (cos theta)) (sin theta))
      1)

#|	  
d/dtheta (down (cos psi)
	       (* -1 (sin psi))
	       0)
d/dphi (down (/ (sin psi) (sin theta))
	     (/ (cos psi) (sin theta))
	     0)
d/dpsi (down (/ (* -1 (sin psi) (cos theta)) (sin theta))
      (/ (* -1 (cos psi) (cos theta)) (sin theta))
      1)

deduce that
|#
|#

(define ep_x
  (+ (* (cos psi) d/dtheta)
     (* (/ (sin psi) (sin theta)) d/dphi)
     (* (/ (* -1 (sin psi) (cos theta)) (sin theta)) d/dpsi)))
(define ep_y
  (+ (* (* -1 (sin psi)) d/dtheta)
     (* (/ (cos psi) (sin theta)) d/dphi)
     (* (/ (* -1 (cos psi) (cos theta)) (sin theta)) d/dpsi)))
(define ep_z d/dpsi)

#|

what are the commutators?

;;; [ep_x , ep_y] - ep_z = 0

(pe (((- (commutator ep_x ep_y) ep_z)
      (literal-manifold-function 'f Euler-angles))
     (Euler-angles-chi-inverse
      (up 'theta 'phi 'psi))))
0

;;; [ep_y , ep_z] - ep_x = 0

(pe (((- (commutator ep_y ep_z) ep_x)
      (literal-manifold-function 'f Euler-angles))
     (Euler-angles-chi-inverse
      (up 'theta 'phi 'psi))))
0

;;; [ep_z , ep_x] - ep_y = 0

(pe (((- (commutator ep_z ep_x) ep_y)
      (literal-manifold-function 'f Euler-angles))
     (Euler-angles-chi-inverse
      (up 'theta 'phi 'psi))))
0

the signs of the structure constants are opposite
looks promising.

|#

(define so3p-vector-basis
  (down ep_x ep_y ep_z))

(define so3p-dual-basis
  (vector-basis->dual so3p-vector-basis Euler-angles))

(define so3p-basis
  (make-basis so3p-vector-basis so3p-dual-basis))

#|

;;; the structure constants
(pe (s:map
     (lambda (e~k)
       (s:map
	(lambda (e_i)
	  (s:map
	   (lambda (e_j)
	     ((e~k (commutator e_i e_j))
	      (Euler-angles-chi-inverse
	       (up 'theta 'phi 'psi))))
	   so3p-vector-basis))
	so3p-vector-basis))
     so3p-dual-basis))

(up (down (down 0 0 0) (down 0 0 1) (down 0 -1 0))
    (down (down 0 0 -1) (down 0 0 0) (down 1 0 0))
    (down (down 0 1 0) (down -1 0 0) (down 0 0 0)))

|#

(define so3p-structure-constants
  (up (down (down 0 0 0) (down 0 0 1) (down 0 -1 0))
      (down (down 0 0 -1) (down 0 0 0) (down 1 0 0))
      (down (down 0 1 0) (down -1 0 0) (down 0 0 0))))

#|

;;quasivelocities on the so3p basis

(pe
 (let* ((gamma (compose
		Euler-angles-chi-inverse
		(up (literal-function 'f^theta)
		    (literal-function 'f^phi)
		    (literal-function 'f^psi))
		time-chi))
	(basis-over-gamma (basis->basis-over-map gamma so3p-basis))
	(1form-basis (basis->1form-basis basis-over-gamma))
	(vector-basis (basis->vector-basis basis-over-gamma)))
   (s:map (lambda (w)
	    ((w ((differential gamma) d/dt))
	     (time-chi-inverse 't)))
	  1form-basis)))

(up
 (+ (* ((D f^phi) t) (sin (f^theta t)) (sin (f^psi t)))
    (* (cos (f^psi t)) ((D f^theta) t)))
 (+ (* ((D f^phi) t) (sin (f^theta t)) (cos (f^psi t)))
    (* -1 (sin (f^psi t)) ((D f^theta) t)))
 (+ (* ((D f^phi) t) (cos (f^theta t))) ((D f^psi) t)))

;;compare to omega body

(pe ((Euler->omega-body
      (up (literal-function 'theta)
	  (literal-function 'phi)
	  (literal-function 'psi))) 't))
(matrix-by-rows
 (list
  (+ (* (sin (theta t)) (sin (psi t)) ((D phi) t))
     (* ((D theta) t) (cos (psi t)))))
 (list
  (+ (* (sin (theta t)) (cos (psi t)) ((D phi) t))
     (* -1 ((D theta) t) (sin (psi t)))))
 (list (+ (* (cos (theta t)) ((D phi) t)) ((D psi) t))))

worked.

check the determining equation

(pe (-
     (*
      ((so3-vector-basis
	(literal-manifold-function 'f Euler-angles))
       (Euler-angles-chi-inverse
	(up 'theta 'phi 'psi)))
      (* (rotate-z-tuple 'phi)
	 (* (rotate-x-tuple 'theta)
	    (rotate-z-tuple 'psi))))
     ((so3p-vector-basis
       (literal-manifold-function 'f Euler-angles))
      (Euler-angles-chi-inverse
	(up 'theta 'phi 'psi)))))
(down 0 0 0)

|#

#|

;;;****************************************************************
;;; Instead of Euler use an alternate set of angles ...
;;;****************************************************************

|#

(define (Angles->M q)
  (let ((vartheta (ref q 0))
	(varphi (ref q 1))
	(varpsi (ref q 2)))
    (* (rotate-z-tuple varphi)
       (rotate-x-tuple vartheta)
       (rotate-y-tuple varpsi))))

(define (Angles->M^-1 q)
  (let ((vartheta (ref q 0))
	(varphi (ref q 1))
	(varpsi (ref q 2)))
    (* (rotate-y-tuple (- varpsi))
       (rotate-x-tuple (- vartheta))
       (rotate-z-tuple (- varphi)))))
#|

;;; (load "tracetranspose")
(define ((equation0 vartheta varphi varpsi) p)
  (let ((a (ref p 0)) (b (ref p 1)) (c (ref p 2)))
    (let ((M ((D (lambda (t) (- (* (Angles->M (up (+ vartheta (* a t))
						  (+ varphi (* b t))
						  (+ varpsi (* c t))))
				   (Angles->M^-1 (up vartheta varphi varpsi)))
				(rotate-x-tuple t))))
		 0)))
	 (up (ref M 0 1)
	     (ref M 0 2)
	     (ref M 1 2)))))
(pe ((equation0 'vartheta 'varphi 'varpsi) (up 'a 'b 'c)))
(up (+ (* c (sin vartheta)) b)
    (+ (* -1 c (cos varphi) (cos vartheta)) (* -1 a (sin varphi)))
    (+ -1 (* -1 c (cos vartheta) (sin varphi)) (* a (cos varphi))))

(pe ((cadr (solve (equation0 'vartheta 'varphi 'varpsi) 3 3 list)) #()))
(up (cos varphi) (* (tan vartheta) (sin varphi)) (/ (* -1 (sin varphi)) (cos vartheta)))

(define ((equation1 vartheta varphi varpsi) p)
  (let ((a (ref p 0)) (b (ref p 1)) (c (ref p 2)))
    (let ((M ((D (lambda (t) (- (* (Angles->M (up (+ vartheta (* a t))
						  (+ varphi (* b t))
						  (+ varpsi (* c t))))
				   (Angles->M^-1 (up vartheta varphi varpsi)))
				(rotate-y-tuple t))))
		 0)))
	 (up (ref M 0 1)
	     (ref M 0 2)
	     (ref M 1 2)))))
(pe ((cadr (solve (equation1 'vartheta 'varphi 'varpsi) 3 3 list)) #()))
(up (sin varphi) (* -1 (tan vartheta) (cos varphi)) (/ (cos varphi) (cos vartheta)))

(define ((equation2 vartheta varphi varpsi) p)
  (let ((a (ref p 0)) (b (ref p 1)) (c (ref p 2)))
    (let ((M ((D (lambda (t) (- (* (Angles->M (up (+ vartheta (* a t))
						  (+ varphi (* b t))
						  (+ varpsi (* c t))))
				   (Angles->M^-1 (up vartheta varphi varpsi)))
				(rotate-z-tuple t))))
		 0)))
	 (up (ref M 0 1)
	     (ref M 0 2)
	     (ref M 1 2)))))
(pe ((cadr (solve (equation2 'vartheta 'varphi 'varpsi) 3 3 list)) #()))
(up 0 1 0)

|#

(define ea_x
  (+ (* (cos varphi) d/dvartheta)
     (* (* (sin varphi) (tan vartheta)) d/dvarphi)
     (* (/ (* -1 (sin varphi)) (cos vartheta)) d/dvarpsi)))

(define ea_y
  (+ (* (sin varphi) d/dvartheta)
     (* (* -1 (cos varphi) (tan vartheta)) d/dvarphi)
     (* (/ (cos varphi) (cos vartheta)) d/dvarpsi)))

(define ea_z d/dvarphi)

(define so3a-vector-basis
  (down ea_x ea_y ea_z))

(define so3a-dual-basis
  (vector-basis->dual so3a-vector-basis alternate-angles))

(define so3a-basis
  (make-basis so3a-vector-basis so3a-dual-basis))

#|

(pe (((+ (commutator ea_x ea_y) ea_z)
      (literal-manifold-function 'f alternate-angles))
     (alternate-angles-chi-inverse
      (up 'vartheta 'varphi 'varpsi))))

0

(pe (((+ (commutator ea_y ea_z) ea_x)
      (literal-manifold-function 'f alternate-angles))
     (alternate-angles-chi-inverse
      (up 'vartheta 'varphi 'varpsi))))
0

(pe (((+ (commutator ea_z ea_x) ea_y)
      (literal-manifold-function 'f alternate-angles))
     (alternate-angles-chi-inverse
      (up 'vartheta 'varphi 'varpsi))))
0

(pe ((so3a-dual-basis so3a-vector-basis)
     (alternate-angles-chi-inverse
      (up 'vartheta 'varphi 'varpsi))))
(up (down 1 0 0) (down 0 1 0) (down 0 0 1))

;;; the structure constants
(pe (s:map
     (lambda (e~k)
       (s:map
	(lambda (e_i)
	  (s:map
	   (lambda (e_j)
	     ((e~k (commutator e_i e_j))
	      (alternate-angles-chi-inverse
	       (up 'vartheta 'varphi 'varpsi))))
	   so3a-vector-basis))
	so3a-vector-basis))
     so3a-dual-basis))
(up (down (down 0 0 0) (down 0 0 -1) (down 0 1 0))
    (down (down 0 0 1) (down 0 0 0) (down -1 0 0))
    (down (down 0 -1 0) (down 1 0 0) (down 0 0 0)))

|#

(define so3a-structure-constants
  (up (down (down 0 0 0) (down 0 0 -1) (down 0 1 0))
      (down (down 0 0 1) (down 0 0 0) (down -1 0 0))
      (down (down 0 -1 0) (down 1 0 0) (down 0 0 0))))

#|

(pe (* (down 'ea_x 'ea_y 'ea_z)
       (Angles->M (up 'vartheta 'varphi 'varpsi))))
(down
 (+ (* -1 ea_x (sin varpsi) (sin vartheta) (sin varphi))
    (* ea_y (sin varpsi) (sin vartheta) (cos varphi))
    (* ea_x (cos varpsi) (cos varphi))
    (* ea_y (sin varphi) (cos varpsi))
    (* -1 ea_z (cos vartheta) (sin varpsi)))
 (+ (* -1 ea_x (cos vartheta) (sin varphi))
    (* ea_y (cos vartheta) (cos varphi))
    (* ea_z (sin vartheta)))
 (+ (* ea_x (sin vartheta) (sin varphi) (cos varpsi))
    (* -1 ea_y (sin vartheta) (cos varpsi) (cos varphi))
    (* ea_x (sin varpsi) (cos varphi))
    (* ea_y (sin varpsi) (sin varphi))
    (* ea_z (cos vartheta) (cos varpsi))))

(pe ((dvartheta (down
	      (+ (* -1 ea_x (sin varpsi) (sin vartheta) (sin varphi))
		 (* ea_y (sin varpsi) (sin vartheta) (cos varphi))
		 (* ea_x (cos varpsi) (cos varphi))
		 (* ea_y (sin varphi) (cos varpsi))
		 (* -1 ea_z (cos vartheta) (sin varpsi)))
	      (+ (* -1 ea_x (cos vartheta) (sin varphi))
		 (* ea_y (cos vartheta) (cos varphi))
		 (* ea_z (sin vartheta)))
	      (+ (* ea_x (sin vartheta) (sin varphi) (cos varpsi))
		 (* -1 ea_y (sin vartheta) (cos varpsi) (cos varphi))
		 (* ea_x (sin varpsi) (cos varphi))
		 (* ea_y (sin varpsi) (sin varphi))
		 (* ea_z (cos vartheta) (cos varpsi)))))
     (alternate-angles-chi-inverse
      (up 'vartheta 'varphi 'varpsi))))
(down (cos varpsi) 0 (sin varpsi))

(pe ((dvarphi (down
	      (+ (* -1 ea_x (sin varpsi) (sin vartheta) (sin varphi))
		 (* ea_y (sin varpsi) (sin vartheta) (cos varphi))
		 (* ea_x (cos varpsi) (cos varphi))
		 (* ea_y (sin varphi) (cos varpsi))
		 (* -1 ea_z (cos vartheta) (sin varpsi)))
	      (+ (* -1 ea_x (cos vartheta) (sin varphi))
		 (* ea_y (cos vartheta) (cos varphi))
		 (* ea_z (sin vartheta)))
	      (+ (* ea_x (sin vartheta) (sin varphi) (cos varpsi))
		 (* -1 ea_y (sin vartheta) (cos varpsi) (cos varphi))
		 (* ea_x (sin varpsi) (cos varphi))
		 (* ea_y (sin varpsi) (sin varphi))
		 (* ea_z (cos vartheta) (cos varpsi)))))
     (alternate-angles-chi-inverse
      (up 'vartheta 'varphi 'varpsi))))
(down (/ (* -1 (sin varpsi)) (cos vartheta)) 0 (/ (cos varpsi) (cos vartheta)))

(pe ((dvarpsi (down
	      (+ (* -1 ea_x (sin varpsi) (sin vartheta) (sin varphi))
		 (* ea_y (sin varpsi) (sin vartheta) (cos varphi))
		 (* ea_x (cos varpsi) (cos varphi))
		 (* ea_y (sin varphi) (cos varpsi))
		 (* -1 ea_z (cos vartheta) (sin varpsi)))
	      (+ (* -1 ea_x (cos vartheta) (sin varphi))
		 (* ea_y (cos vartheta) (cos varphi))
		 (* ea_z (sin vartheta)))
	      (+ (* ea_x (sin vartheta) (sin varphi) (cos varpsi))
		 (* -1 ea_y (sin vartheta) (cos varpsi) (cos varphi))
		 (* ea_x (sin varpsi) (cos varphi))
		 (* ea_y (sin varpsi) (sin varphi))
		 (* ea_z (cos vartheta) (cos varpsi)))))
     (alternate-angles-chi-inverse
      (up 'vartheta 'varphi 'varpsi))))
(down (* (sin varpsi) (tan vartheta)) 1 (* -1 (cos varpsi) (tan vartheta)))

d/dvartheta
(down (cos varpsi) 0 (sin varpsi))
d/dvarphi
(down (/ (* -1 (sin varpsi)) (cos vartheta)) 0 (/ (cos varpsi) (cos vartheta)))
d/dvarpsi
(down (* (sin varpsi) (tan vartheta)) 1 (* -1 (cos varpsi) (tan vartheta)))

|#

(define eap_x
  (+ (* (cos varpsi) d/dvartheta)
     (* (/ (* -1 (sin varpsi)) (cos vartheta)) d/dvarphi)
     (* (* (sin varpsi) (tan vartheta)) d/dvarpsi)))

(define eap_y
  d/dvarpsi)

(define eap_z
  (+ (* (sin varpsi) d/dvartheta)
     (* (/ (cos varpsi) (cos vartheta)) d/dvarphi)
     (* (* -1 (cos varpsi) (tan vartheta)) d/dvarpsi)))

#|

(pe (((- (commutator eap_x eap_y) eap_z)
      (literal-manifold-function 'f alternate-angles))
     (alternate-angles-chi-inverse
      (up 'vartheta 'varphi 'varpsi))))
0

(pe (((- (commutator eap_y eap_z) eap_x)
      (literal-manifold-function 'f alternate-angles))
     (alternate-angles-chi-inverse
      (up 'vartheta 'varphi 'varpsi))))
0

(pe (((- (commutator eap_z eap_x) eap_y)
      (literal-manifold-function 'f alternate-angles))
     (alternate-angles-chi-inverse
      (up 'vartheta 'varphi 'varpsi))))
0

|#

(define so3ap-vector-basis
  (down eap_x eap_y eap_z))

(define so3ap-dual-basis
  (vector-basis->dual so3ap-vector-basis alternate-angles))

(define so3ap-basis
  (make-basis so3ap-vector-basis so3ap-dual-basis))

#|

(pe ((s:map
      so3ap-dual-basis
      so3ap-vector-basis)
     (alternate-angles-chi-inverse
      (up 'vartheta 'varphi 'varpsi))))
(down (up 1 0 0) (up 0 1 0) (up 0 0 1))

;;; structure-constants
(pe (s:map
     (lambda (e~k)
       (s:map
	(lambda (e_i)
	  (s:map
	   (lambda (e_j)
	     ((e~k (commutator e_i e_j))
	      (alternate-angles-chi-inverse
	       (up 'vartheta 'varphi 'varpsi))))
	   so3ap-vector-basis))
	so3ap-vector-basis))
     so3ap-dual-basis))
(up (down (down 0 0 0) (down 0 0 1) (down 0 -1 0))
    (down (down 0 0 -1) (down 0 0 0) (down 1 0 0))
    (down (down 0 1 0) (down -1 0 0) (down 0 0 0)))

|#

(define so3ap-structure-constants
  (up (down (down 0 0 0) (down 0 0 1) (down 0 -1 0))
      (down (down 0 0 -1) (down 0 0 0) (down 1 0 0))
      (down (down 0 1 0) (down -1 0 0) (down 0 0 0))))

#|

;;;&&&&&&&&&&&&&&&&&&&&&&&&&&&&&&&&&&&&&&&&&&&&&&&&&&&&&&&&&&&&&&&&

check angular velocities in alternate angles

(pe (let ((q (up (literal-function 'vartheta)
		 (literal-function 'varphi)
		 (literal-function 'varpsi))))
      (wcross->w (* (Angles->M^-1 (q 't))
		    ((D (lambda (t) (Angles->M (q t)))) 't)))))
(up
 (+ (* -1 (cos (vartheta t)) (sin (varpsi t)) ((D varphi) t))
    (* ((D vartheta) t) (cos (varpsi t))))
 (+ (* (sin (vartheta t)) ((D varphi) t)) ((D varpsi) t))
 (+ (* (cos (vartheta t)) (cos (varpsi t)) ((D varphi) t))
    (* ((D vartheta) t) (sin (varpsi t)))))

(pe
 (let* ((gamma (compose
		alternate-angles-chi-inverse
		(up (literal-function 'vartheta)
		    (literal-function 'varphi)
		    (literal-function 'varpsi))
		time-chi))
	(basis-over-gamma (basis->basis-over-map gamma so3ap-basis))
	(1form-basis (basis->1form-basis basis-over-gamma))
	(vector-basis (basis->vector-basis basis-over-gamma)))
   (s:map (lambda (w)
	    ((w ((differential gamma) d/dt))
	     (time-chi-inverse 't)))
	  1form-basis)))
(up
 (+ (* -1 (cos (vartheta t)) (sin (varpsi t)) ((D varphi) t))
    (* ((D vartheta) t) (cos (varpsi t))))
 (+ (* (sin (vartheta t)) ((D varphi) t)) ((D varpsi) t))
 (+ (* (cos (vartheta t)) (cos (varpsi t)) ((D varphi) t))
    (* ((D vartheta) t) (sin (varpsi t)))))

|#

;;; A theorem of SO3
#|
(define-coordinates (up x y z) R3-rect)

(define Jz (- (* x d/dy) (* y d/dx)))
(define Jx (- (* y d/dz) (* z d/dy)))
(define Jy (- (* z d/dx) (* x d/dz)))

(pe (- (series:sum
	(((exp (* 'alpha D))
	  (lambda (alpha)
	    (* (Euler->M
		(series:sum
		 (((exp (* alpha e_z)) Euler-angles-chi)
		  ((point Euler-angles) (up 'theta 'phi 'psi)))
		 1))
	       (up 'x 'y 'z))))
	 0)
	5)
       (series:sum
	(((exp (* 'alpha Jz)) (chart R3-rect)) 
	 ((point R3-rect)
	  (* (Euler->M (up 'theta 'phi 'psi))
	     (up 'x 'y 'z))))
	5)))
(up 0 0 0)


(pe (- (series:sum
	(((exp (* 'alpha D))
	  (lambda (alpha)
	    (* (Euler->M
		(series:sum
		 (((exp (* alpha e_x)) Euler-angles-chi)
		  ((point Euler-angles) (up 'theta 'phi 'psi)))
		 4))
	       (up 'x 'y 'z))))
	 0)
	3)
       (series:sum
	(((exp (* 'alpha Jx)) (chart R3-rect)) 
	 ((point R3-rect)
	  (* (Euler->M (up 'theta 'phi 'psi))
	     (up 'x 'y 'z))))
	3)))
(up 0 0 0)

(M((exp(a*e_x)chi_SO3) m_SO3))*xyz_R3
   = ((exp(a*J_x)chi_R3)
      (M (chi_SO3 m_SO3))*xyz_R3)

|#