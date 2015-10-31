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

;;; A vector field is an operator that takes a smooth real-valued
;;; function of a manifold and produces a new function on the manifold
;;; which computes the directional derivative of the given function at
;;; each point of the manifold.

(define (vector-field? vop)
  (and (operator? vop)
       (eq? (operator-subtype vop) 'vector-field)))


;;; As with other differential operators such as D, a vector-field
;;; operator multiplies by composition.  Like D it takes the given
;;; function to another function of a point.

(define (procedure->vector-field vfp #!optional name)
  (if (default-object? name)
      (set! name 'unnamed-vector-field))
  (let ((the-field (make-operator vfp name 'vector-field)))
    (declare-argument-types! the-field (list function?))
    the-field))


;;; A vector field is specified by a function that gives components,
;;; as an up tuple, relative to a coordinate system, for each point,
;;; specified in the given coordinate system.

(define ((vector-field-procedure components coordinate-system) f)
  (compose (* (D (compose f (coordinate-system '->point)))
	      components)
	   (coordinate-system '->coords)))

(define (components->vector-field components coordinate-system #!optional name)
  (if (default-object? name) (set! name `(vector-field ,components)))
  (procedure->vector-field
   (vector-field-procedure components coordinate-system)
   name))
   

;;; We can extract the components function for a vector field, given a
;;; coordinate system.

(define ((vector-field->components vf coordinate-system) coords)
  (assert (vector-field? vf) "Bad vector field: vector-field->components")
  ((vf (coordinate-system '->coords)) 
   ((coordinate-system '->point) coords)))

(define (vf:zero f) zero-manifold-function)

(define (vf:zero-like op)
  (assert (vector-field? op) "vf:zero-like")
  (make-op vf:zero
	   'vf:zero
	   (operator-subtype op)
	   (operator-arity op)
	   (operator-optionals op)))

(assign-operation 'zero-like vf:zero-like vector-field?)


(define (vf:zero? vf)
  (assert (vector-field? vf) "vf:zero?")
  (eq? (operator-procedure vf) vf:zero))

(assign-operation 'zero? vf:zero? vector-field?)


;;; It is often useful to construct a literal vector field

(define (literal-vector-field name coordinate-system)
  (let ((n (coordinate-system 'dimension)))
    (let ((function-signature
	   (if (fix:= n 1) (-> Real Real) (-> (UP* Real n) Real))))
      (let ((components
	     (s:generate n 'up (lambda (i)
				 (literal-function (string->symbol
						    (string-append
						     (symbol->string name)
						     "^"
						     (number->string i)))
						   function-signature)))))
	(components->vector-field components coordinate-system name)))))

;;; For any coordinate system we can make a coordinate basis.

(define ((coordinate-basis-vector-field-procedure coordinate-system . i) f)
  (compose ((apply partial i) (compose f (coordinate-system '->point)))
	   (coordinate-system '->coords)))

(define (coordinate-basis-vector-field coordinate-system name . i)
  (procedure->vector-field
   (apply coordinate-basis-vector-field-procedure coordinate-system i)
   name))

#|
(define (coordinate-system->vector-basis coordinate-system)
  (s:map (lambda (chain)
	   (apply coordinate-basis-vector-field
		  coordinate-system
		  `(e ,@chain)
		  chain))
	 (coordinate-system 'dual-chains)))
|#

(define (coordinate-system->vector-basis coordinate-system)
  (coordinate-system 'coordinate-basis-vector-fields))

#|
;;; Doesn't work.

(define ((coordinate-system->vector-basis-procedure coordinate-system) f)
  (compose (D (compose f (coordinate-system '->point)))
	   (coordinate-system '->coords)))
|#

;;; Given a vector basis, can make a vector field as a linear
;;; combination.  This is for any basis, not just a coordinate basis.
;;; The components are evaluated at the point, not the coordinates.

(define (basis-components->vector-field components vector-basis)
  (procedure->vector-field
   (lambda (f)
     (lambda (point)
       (* ((vector-basis f) point)
	  (components point))))
   `(+ ,@(map (lambda (component basis-element)
		`(* ,(diffop-name component)
		    ,(diffop-name basis-element)))
	      (s:fringe components)
	      (s:fringe vector-basis)))))


;;; And the inverse

(define (vector-field->basis-components v dual-basis)
  (s:map/r (lambda (w) (w v)) dual-basis))


#|
;;; This does not make a vector field, because of operator.

(define (((basis-components->vector-field components vector-basis) f) point)
  (* ((vector-basis f) point)
     (components point)))

;;; We note problems, due to tuple arithmetic 

(define (basis-components->vector-field components vector-basis)
  (* vector-basis components))
|#

#|
(install-coordinates R3-rect (up 'x 'y 'z))

(pec (((* (expt d/dy 2) x y d/dx) (* (sin x) (cos y)))
      ((R3-rect '->point)(up 'a 'b 'c))))
#| Result:
(+ (* -1 a b (cos a) (cos b)) (* -2 a (sin b) (cos a)))
|#
|#

#|
(define counter-clockwise (- (* x d/dy) (* y d/dx)))

(define outward (+ (* x d/dx) (* y d/dy)))

(define mr ((R3-rect '->point) (up 'x0 'y0 'z0)))

(pec ((counter-clockwise (sqrt (+ (square x) (square y)))) mr))
#| Result:
0
|#

(pec ((counter-clockwise (* x y)) mr))
#| Result:
(+ (expt x0 2) (* -1 (expt y0 2)))
|#

(pec ((outward (* x y)) mr))
#| Result:
(* 2 x0 y0)
|#
|#

#|
;;; From McQuistan: Scalar and Vector Fields, pp. 103-106

;;; We apparently need cylindrical coordinates too.

(install-coordinates R3-cyl (up 'r 'theta 'zeta))

(define A (+ (* 'A_r d/dr) (* 'A_theta d/dtheta) (* 'A_z d/dzeta)))

(pec ((vector-field->components A R3-rect) (up 'x 'y 'z)))
#| Result:
(up (+ (* -1 A_theta y) (/ (* A_r x) (sqrt (+ (expt x 2) (expt y 2)))))
    (+ (* A_theta x) (/ (* A_r y) (sqrt (+ (expt x 2) (expt y 2)))))
    A_z)
|#
;;; This disagrees with McQuistan.  Explanation follows.


(pec ((d/dtheta (up x y z))
      ((R3-rect '->point) (up 'x 'y 'z))))
#| Result:
(up (* -1 y) x 0)
|#
;;; has length (sqrt (+ (expt x 2) (expt y 2)))

(pec ((d/dr (up x y z))
      ((R3-rect '->point) (up 'x 'y 'z))))
#| Result:
(up (/ x (sqrt (+ (expt x 2) (expt y 2))))
    (/ y (sqrt (+ (expt x 2) (expt y 2))))
    0)
|#
;;; has length 1

(pec ((d/dz (up x y z))
      ((R3-rect '->point) (up 'x 'y 'z))))
#| Result:
(up 0 0 1)
|#
;;; has length 1

;;; so these coordinate basis vectors are not normalized
;;; Introduce 
(define e-theta (* (/ 1 r) d/dtheta))
(define e-r d/dr)
(define e-z d/dzeta)

;;; then
(define A (+ (* 'A_r e-r) (* 'A_theta e-theta) (* 'A_z e-z)))

(pec ((vector-field->components A R3-rect) (up 'x 'y 'z)))
#| Result:
(up
 (+ (/ (* A_r x) (sqrt (+ (expt x 2) (expt y 2))))
    (/ (* -1 A_theta y) (sqrt (+ (expt x 2) (expt y 2)))))
 (+ (/ (* A_r y) (sqrt (+ (expt x 2) (expt y 2))))
    (/ (* A_theta x) (sqrt (+ (expt x 2) (expt y 2)))))
 A_z)
|#
;;; now agrees with McQuistan.
|#

#|
(pec ((vector-field->components d/dy R3-rect)
      (up 'x0 'y0 'z0)))
#| Result:
(up 0 1 0)
|#

(pec ((vector-field->components d/dy R3-rect)
      (up 'r0 'theta0 'z0)))
#| Result:
(up 0 1 0)
|#

(pec ((vector-field->components d/dy R3-cyl)
      (up 1 pi/2 0)))
#| Result:
(up 1. 6.123031769111886e-17 0)
|#

(pec ((vector-field->components d/dy R3-cyl)
      (up 1 0 0)))
#| Result:
(up 0 1 0)
|#

(pec ((vector-field->components d/dy R3-cyl)
      (up 'r0 'theta0 'z)))
#| Result:
(up (sin theta0) (/ (cos theta0) r) 0)
|#
|#

#|
(define R3-point ((R3-rect '->point) (up 'x0 'y0 'z0)))

;;; The following does not work.  
;;;  One cannot add to a manifold point.
 
(series:print
 (((exp (* x d/dy))
   (literal-function 'f (-> (UP Real Real Real) Real)))
  R3-point)
 4)
#|
(f #[manifold-point 42])
;Wrong type argument -- LITERAL-FUNCTION
|#
|#

;;; However, one can make a coordinate version of a vector field

(define (coordinatize sfv coordsys)
  (define (v f)
    (lambda (x)
      (let ((b
             (compose (sfv (coordsys '->coords))
                      (coordsys '->point))))
        (* ((D f) x) (b x)))))
  (make-operator v))

#|
(pec
 (((coordinatize (literal-vector-field 'v R3-rect) R3-rect)
   (literal-function 'f (-> (UP Real Real Real) Real)))
  (up 'x0 'y0 'z0)))
#| Result:
(+ (* (((partial 0) f) (up x0 y0 z0)) (v^0 (up x0 y0 z0)))
   (* (((partial 1) f) (up x0 y0 z0)) (v^1 (up x0 y0 z0)))
   (* (((partial 2) f) (up x0 y0 z0)) (v^2 (up x0 y0 z0))))
|#

;;; Consider the following vector field 

(define circular (- (* x d/dy) (* y d/dx)))


;;; The coordinate version can be exponentiated

(series:for-each print-expression
                 (((exp (coordinatize (* 'a circular) R3-rect))
                   identity)
                  (up 1 0 0))
                 6)
#|
(up 1 0 0)
(up 0 a 0)
(up (* -1/2 (expt a 2)) 0 0)
(up 0 (* -1/6 (expt a 3)) 0)
(up (* 1/24 (expt a 4)) 0 0)
(up 0 (* 1/120 (expt a 5)) 0)
;Value: ...
|#
|#

;;; We can use the coordinatized vector field to build an
;;; evolution along an integral curve.

(define ((((evolution order)
           delta-t vector-field)
          manifold-function)
         manifold-point)
  (series:sum
    (((exp (* delta-t vector-field))
      manifold-function)
     manifold-point)
    order))

#|
(install-coordinates R2-rect (up 'x 'y))

(define circular (- (* x d/dy) (* y d/dx)))

(pec
 ((((evolution 6) 'a circular) (R2-rect '->coords))
  ((R2-rect '->point) (up 1 0))))
#| Result:
(up (+ (* -1/720 (expt a 6))
       (* 1/24 (expt a 4))
       (* -1/2 (expt a 2))
       1)
    (+ (* 1/120 (expt a 5))
       (* -1/6 (expt a 3))
       a)
    0)
|#
|#