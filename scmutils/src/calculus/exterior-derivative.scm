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

;;;; Exterior derivative

#|
A form field is a function of a place and a number of vector fields.
The exterior derivative of this form field is a derivative of the form
with respect to the place, removing any dependence on place of the
vector fields.

Consider w(v)(x), where b is the coefficient function for w in coordinates X:

v1(w(v2))(x) - v2(w(v1))(x)
   = v1(b v2(X))(x) - v2(b v1(X))(x)
   = v1(b)(x) v2(X)(x) + b(x) v1(v2(X))(x) 
     - v2(b)(x) v1(X)(x) - b(x) v2(v1(X))(x) 
   = v1(b)(x) v2(X)(x) - v2(b)(x) v1(X)(x) + b(x)[v1, v2](X)(x)
   = v1(b)(x) v2(X)(x) - v2(b)(x) v1(X)(x) + w([v1, v2])(x)


We define exterior derivative as follows

dw(v1, v2)(x) 
   = v1(b)(x) v2(X)(x) - v2(b)(x) v1(X)(x) 
   = v1(w(v2))(x) - v2(w(v1))(x) - w([v1, v2])(x)

It is not obvious that this is equivalent to the standard definition.
See page 91 in Spivak.

Another way to think about this is that if we were able to define
constant vector fields (v1_bar, v2_bar) that have constant
coefficient functions equal to the value of the coefficient function
at the point, then dw(v1, v2)(x) would be just
v1_bar(w(v2_bar))(x) - v2_bar(w(v1_bar))(x)
|#

;;; This definition is a generalization to k-forms, by recursion on
;;; the vector argument list.

;;; The test for k<n is best if the n is the dimension of the manifold
;;; under study.  However, if the manifold is embedded in a higher
;;; dimensional manifold n will be the dimension of the bigger
;;; manifold, making this test less effective (cutting off fewer
;;; branches). 

;;; Formula is from Spivak Vol. 1 p289.

(define (exterior-derivative-procedure kform)
  (let ((k (get-rank kform)))
    (if (fix:= k 0)
	(differential-of-function kform)
	(let ((the-k+1form
	       (lambda vectors	
		 (assert (fix:= (length vectors) (fix:+ k 1)))
		 (lambda (point)
		   (let ((n ((point->manifold point) 'dimension)))
		     ;;(s:dimension (manifold-point-representation point))
		     (if (fix:< k n)
			 (sigma
			  (lambda (i)
			    (let ((rest (delete-nth i vectors)))
			      (+ (* (if (even? i) +1 -1)
				    (((ref vectors i) (apply kform rest))
				     point))
				 (sigma
				  (lambda (j)
				    (* (if (even? (fix:+ i j)) +1 -1)
				       ((apply kform
					       (cons
						(commutator (ref vectors i)
							    (ref vectors j))
						;; j-1 because already deleted i.
						(delete-nth (fix:- j 1)
							    rest)))
					point)))
				  (fix:+ i 1) k))))
			  0 k)
			 0))))))
	  (procedure->nform-field the-k+1form
				  (fix:+ (get-rank kform) 1)
				  `(d ,(diffop-name kform)))))))

(define exterior-derivative
  (make-operator exterior-derivative-procedure
		 'd
		 'exterior-derivative))

(define d exterior-derivative)

;;; This is an excessively complicated program.  Another, more
;;; elementary program would, for a k-form, extract the cofficient
;;; functions relative to a literal basis, by applying it to the basis
;;; vectors, do the derivatives of the coefficients, to make one
;;; forms, and form the sum of the weges of the new 1-forms with the
;;; wedges of the corresponding dual basis elements.

#|
(install-coordinates R3-rect (up 'x 'y 'z))

(define R3-rect-point ((R3-rect '->point) (up 'x0 'y0 'z0)))

(install-coordinates R3-cyl (up 'r 'theta 'zeta))

(define R3-cyl-point ((R3-cyl '->point) (up 'r0 'theta0 'zeta0)))


(define w (literal-1form-field 'w R3-rect))
(define u (literal-1form-field 'u R3-rect))
(define v (literal-1form-field 'v R3-rect))

(define X (literal-vector-field 'X R3-rect))
(define Y (literal-vector-field 'Y R3-rect))
(define Z (literal-vector-field 'Z R3-rect))
(define W (literal-vector-field 'W R3-rect))


(pec (((d (literal-scalar-field 'f R3-rect)) X)
      R3-rect-point))
#| Result:
(+ (* (((partial 0) f) (up x0 y0 z0)) (X^0 (up x0 y0 z0)))
   (* (((partial 1) f) (up x0 y0 z0)) (X^1 (up x0 y0 z0)))
   (* (((partial 2) f) (up x0 y0 z0)) (X^2 (up x0 y0 z0))))
|#

(pec ((((square d) (literal-scalar-field 'f R3-rect)) X Y)
      R3-cyl-point))
#| Result:
0
|#

;;; To aid reading of expressions...

(clear-arguments)
(suppress-arguments (list '(up x0 y0 z0)))

(pec (((d w) X Y) R3-rect-point)
     (compose arg-suppressor simplify))
#| Result:
(+ (* Y^2 ((partial 0) w_2) X^0)
   (* Y^2 ((partial 1) w_2) X^1)
   (* -1 Y^2 ((partial 2) w_0) X^0)
   (* -1 Y^2 ((partial 2) w_1) X^1)
   (* -1 Y^0 ((partial 0) w_2) X^2)
   (* Y^0 ((partial 2) w_0) X^2)
   (* Y^0 ((partial 1) w_0) X^1)
   (* -1 Y^0 ((partial 0) w_1) X^1)
   (* -1 ((partial 1) w_2) Y^1 X^2)
   (* ((partial 2) w_1) Y^1 X^2)
   (* -1 Y^1 ((partial 1) w_0) X^0)
   (* Y^1 ((partial 0) w_1) X^0))
|#

(define omega
  (+ (* (literal-scalar-field 'omega_0 R3-rect)
	(wedge dx dy))
     (* (literal-scalar-field 'omega_1 R3-rect)
	(wedge dy dz))
     (* (literal-scalar-field 'omega_2 R3-rect)
	(wedge dz dx))))

(pec (((d omega) X Y Z) R3-rect-point)
     (compose arg-suppressor simplify))
#| Result:
(+ (* X^0 Z^2 ((partial 0) omega_1) Y^1)
   (* X^0 Z^2 ((partial 1) omega_2) Y^1)
   (* X^0 Z^2 ((partial 2) omega_0) Y^1)
   (* -1 X^0 Y^2 Z^1 ((partial 0) omega_1))
   (* -1 X^0 Y^2 Z^1 ((partial 1) omega_2))
   (* -1 X^0 Y^2 Z^1 ((partial 2) omega_0))
   (* -1 Z^2 X^1 Y^0 ((partial 0) omega_1))
   (* -1 Z^2 X^1 Y^0 ((partial 1) omega_2))
   (* -1 Z^2 X^1 Y^0 ((partial 2) omega_0))
   (* X^1 Y^2 Z^0 ((partial 0) omega_1))
   (* X^1 Y^2 Z^0 ((partial 1) omega_2))
   (* X^1 Y^2 Z^0 ((partial 2) omega_0))
   (* X^2 Y^0 Z^1 ((partial 0) omega_1))
   (* X^2 Y^0 Z^1 ((partial 1) omega_2))
   (* X^2 Y^0 Z^1 ((partial 2) omega_0))
   (* -1 X^2 Z^0 ((partial 0) omega_1) Y^1)
   (* -1 X^2 Z^0 ((partial 1) omega_2) Y^1)
   (* -1 X^2 Z^0 ((partial 2) omega_0) Y^1))
|#

(pec (((d (d omega)) X Y Z W) R3-rect-point))
#| Result:
0
|#
|#

#|
;;; Jack's neat method of computing the exterior derivative of a form.
;;; One problem is that this needs the coordinate system to make the
;;; constant vector field.

;;; Broken because coordinate systems must be associated with patches.
(define (make-constant-vector-field m0 v)
  (let ((coordinate-system (rectangular (s:dimension m0))))
    (components->vector-field (lambda (coords)
				((v (coordinate-system '->coords)) m0))
			      coordinate-system
			      `(constant-vector-field ,m0 ,v))))


(define (((exterior-derivative-helper kform) #!rest vectors) point)
  (let ((k (get-rank kform)))
    (assert (fix:= (length vectors) (fix:+ k 1)))
    (let ((n ((point->manifold point) 'dimension)))
      ;;(s:dimension (manifold-point-representation point))
      (cond ((fix:= k 0)
	     (((ref vectors 0) kform) point))		 
	    ((fix:< k n)
	     (let ((constant-vector-fields
		    (map (lambda (v)
			   (make-constant-vector-field point v))
			 vectors)))
	       (let lp ((i 0) (sum 0))
		 (if (fix:= i (fix:+ k 1))
		     sum
		     (lp (fix:+ i 1)
			 (let ((h (ref constant-vector-fields i)))
			   (+ sum
			      (* (if (even? i) 1 -1)
				 ((h
				   (apply kform
					  (delete-nth i
						      constant-vector-fields)))
				  point)))))))))
	    (else 0)))))
|#
