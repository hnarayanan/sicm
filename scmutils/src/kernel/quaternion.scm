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

;;;;        Quaternions
;;; 13 August 1998 -- rfrankel, gjs
;;; 12 June 2013 -- gjs

(declare (usual-integrations))


(define (q:type v) quaternion-type-tag)

(define (q:type-predicate v) quaternion-quantity?)


(define (make-quaternion v)
  (list quaternion-type-tag v))

(define q:make make-quaternion)


(define (quaternion r i j k)
  (make-quaternion (vector r i j k)))

(define (quaternion->vector q)
  (cadr q))

(define q:->vector quaternion->vector)


(define (quaternion-ref q i)
  (vector-ref (quaternion->vector q) i))

(define q:ref quaternion-ref)


(define (real&3vector->quaternion r v)
  (quaternion r
	      (vector-ref v 0)
	      (vector-ref v 1)
	      (vector-ref v 2)))

(define q:real&3vector-> real&3vector->quaternion)


(define (quaternion->3vector q)
  (vector-tail (quaternion->vector q) 1))

(define q:3vector quaternion->3vector)


(define (quaternion->real-part q)
  (quaternion-ref q 0))

(define q:real-part quaternion->real-part)

(define (quaternion+quaternion q1 q2)
  (make-quaternion
   (make-initialized-vector 4
     (lambda (i)
       (g:+ (quaternion-ref q1 i)
	    (quaternion-ref q2 i))))))

(define q:+ quaternion+quaternion)


(define (quaternion-quaternion q1 q2)
  (make-quaternion
   (make-initialized-vector 4
     (lambda (i)
       (g:- (quaternion-ref q1 i)
	    (quaternion-ref q2 i))))))

(define q:- quaternion-quaternion)


(define (quaternion*quaternion q1 q2)
  (let ((r1 (quaternion-ref q1 0))
	(i1 (quaternion-ref q1 1))
	(j1 (quaternion-ref q1 2))
	(k1 (quaternion-ref q1 3))
	(r2 (quaternion-ref q2 0))
	(i2 (quaternion-ref q2 1))
	(j2 (quaternion-ref q2 2))
	(k2 (quaternion-ref q2 3)))
    (make-quaternion
     (vector (g:- (g:* r1 r2) (g:+ (g:* i1 i2) (g:* j1 j2) (g:* k1 k2)))
	     (g:+ (g:* r1 i2) (g:* i1 r2) (g:* j1 k2) (g:* -1 k1 j2))
	     (g:+ (g:* r1 j2) (g:* -1 i1 k2) (g:* j1 r2) (g:* k1 i2))
	     (g:+ (g:* r1 k2) (g:* i1 j2) (g:* -1 j1 i2) (g:* k1 r2))))))

(define q:* quaternion*quaternion)


(define (q:conjugate q)
  (quaternion (quaternion-ref q 0)
	      (g:- (quaternion-ref q 1))
	      (g:- (quaternion-ref q 2))
	      (g:- (quaternion-ref q 3))))


(define (q:negate q)
  (make-quaternion
   (make-initialized-vector 4
     (lambda (i)
       (g:- (quaternion-ref q i))))))

(define (scalar*quaternion s q)
  (make-quaternion
   (make-initialized-vector 4
    (lambda (i)
      (g:* s (quaternion-ref q i))))))

(define (quaternion*scalar q s)
  (make-quaternion
   (make-initialized-vector 4
    (lambda (i)
      (g:* (quaternion-ref q i) s)))))

(define (quaternion/scalar q s)
  (make-quaternion
   (make-initialized-vector 4
    (lambda (i)
      (g:/ (quaternion-ref q i) s)))))

(define (q:invert q)
  (quaternion/scalar (q:conjugate q) 
		     (g:+ (g:square (quaternion-ref q 0))
			  (g:square (quaternion-ref q 1))
			  (g:square (quaternion-ref q 2))
			  (g:square (quaternion-ref q 3)))))


(define (quaternion/quaternion q1 q2)
  (quaternion*quaternion q1 (q:invert q2)))

(define q:/ quaternion/quaternion)


(define (q:magnitude q)
  (g:sqrt (g:+ (g:square (quaternion-ref q 0))
	       (g:square (quaternion-ref q 1))
	       (g:square (quaternion-ref q 2))
	       (g:square (quaternion-ref q 3)))))

(define (q:make-unit q)
  (quaternion/scalar q (q:magnitude q)))

(define (q:unit? q)
  (let ((v (q:->vector q)))
    (g:one? (v:dot-product v v))))

(define (q:exp q)
  (let ((v (quaternion->3vector q))
	(a (quaternion->real-part q)))
    (let ((vv (euclidean-norm v)))
      (g:* (g:exp a)
	   (real&3vector->quaternion (g:cos vv)
				     (g:* (g:sin vv)
					  (g:/ v vv)))))))

(define (q:log q)
  (let ((v (quaternion->3vector q))
	(a (quaternion->real-part q))
	(qq (euclidean-norm (quaternion->vector q))))
    (let ((vv (euclidean-norm v)))
      (real&3vector->quaternion (g:log qq)
				(g:* (g:acos (g:/ a qq))
				     (g:/ v vv))))))

(define (q:zero-like q)
  (make-quaternion
   (make-vector 4 0)))

(define (q:zero? q)
  (and (g:zero? (quaternion-ref q 0))
       (g:zero? (quaternion-ref q 1))
       (g:zero? (quaternion-ref q 2))
       (g:zero? (quaternion-ref q 3))))

(define (q:= q1 q2)
  (and (g:= (quaternion-ref q1 0) (quaternion-ref q2 0))
       (g:= (quaternion-ref q1 1) (quaternion-ref q2 1))
       (g:= (quaternion-ref q1 2) (quaternion-ref q2 2))
       (g:= (quaternion-ref q1 3) (quaternion-ref q2 3))))

(define (q:inexact? q)
  (or (g:inexact? (quaternion-ref q 0))
      (g:inexact? (quaternion-ref q 1))
      (g:inexact? (quaternion-ref q 2))
      (g:inexact? (quaternion-ref q 3))))

(define (q:apply q args)
  (let ((vec (quaternion->vector q)))
    (make-quaternion
     (v:generate (vector-length vec)
		 (lambda (i)
		   (g:apply (vector-ref vec i) args))))))

(define (q:arity q)
  (let ((v (quaternion->vector q)))
    (let ((n 4))
      (let lp ((i 1) (a (g:arity (vector-ref v 0))))
	(if (fix:= i n)
	    a
	    (let ((b (joint-arity a (g:arity (vector-ref v i)))))
	      (if b
		  (lp (+ i 1) b)
		  #f)))))))

(define (q:partial-derivative q varspecs)
  (let ((v (quaternion->vector q)))
    (make-quaternion
     ((v:elementwise
       (lambda (f)
	 (generic:partial-derivative f varspecs)))
      v))))

;;; Quaternions as 4x4 matrices

(define q:1
  (matrix-by-rows (list 1 0 0 0) (list 0 1 0 0) (list 0 0 1 0) (list 0 0 0 1)))

(define q:i
  (matrix-by-rows (list 0 1 0 0) (list -1 0 0 0) (list 0 0 0 -1) (list 0 0 1 0)))

(define q:j
  (matrix-by-rows (list 0 0 1 0) (list 0 0 0 1) (list -1 0 0 0) (list 0 -1 0 0)))

(define q:k
  (matrix-by-rows (list 0 0 0 1) (list 0 0 -1 0) (list 0 1 0 0) (list -1 0 0 0)))


(define s:1 (Mmn->A^m_n q:1))
(define s:i (Mmn->A^m_n q:i))
(define s:j (Mmn->A^m_n q:j))
(define s:k (Mmn->A^m_n q:k))


(define (quaternion->4x4 q)
  (let ((r (quaternion-ref q 0))
	(x (quaternion-ref q 1))
	(y (quaternion-ref q 2))
	(z (quaternion-ref q 3)))
    (matrix+matrix (matrix+matrix (scalar*matrix r q:1)
				  (scalar*matrix x q:i))
		   (matrix+matrix (scalar*matrix y q:j)
				  (scalar*matrix z q:k)))))

(define q:->4x4 quaternion->4x4)


(define (4x4->quaternion 4-matrix)
  (make-quaternion (m:nth-row 4-matrix 0)))

(define q:4x4-> 4x4->quaternion)

;;; Quaternions and 3D rotations

;;; Given a axis (a unit 3-vector) and an angle

(define (angle-axis->quaternion theta axis)
  ;; (assert (v:unit? axis)) 
  ;; This assertion is really:
  (let ((v (g:simplify (v:dot-product axis axis))))
    (assume! `(= ,v 1) 'angle-axis->quaternion))
  (real&3vector->quaternion (g:cos (g:/ theta 2))
			    (g:* (g:sin (g:/ theta 2))
				 axis)))

(define q:angle-axis-> angle-axis->quaternion)


;;; Problem: this is singular if the vector part is zero.

(define (quaternion->angle-axis q #!optional continue)
  (assert (quaternion? q))
  (let ((continue
         (if (default-object? continue) list continue)))
    (let* ((v (q:3vector q))
           (theta (g:* 2 (g:atan (euclidean-norm v)
                                 (q:real-part q))))
           (axis (vector/scalar v (euclidean-norm v))))
      (continue theta axis))))

(define q:->angle-axis quaternion->angle-axis)

#|
(quaternion->angle-axis
 (angle-axis->quaternion 'theta
    (up 'x 'y (sqrt (- 1 (square 'x) (square 'y))))))
#|
(theta (up x y (sqrt (+ 1 (* -1 (expt x 2)) (* -1 (expt y 2))))))
|#
|#

;;; To rotate a 3-vector by the angle prescribed by a unit quaternion.

(define (q:rotate q)
  (assert (quaternion? q))
  ;;(assert (q:unit? q))
  ;; This assertion is really:
  (let* ((vv (quaternion->vector q))
         (v (g:simplify (v:dot-product vv vv))))
    (assume! `(= ,v 1) 'q:rotate))
  (let ((q* (q:conjugate q)))
    (define (the-rotation 3-vector)
      (quaternion->3vector
       (quaternion*quaternion q
	  (quaternion*quaternion
	   (real&3vector->quaternion 0 3-vector)
	   q*))))
    the-rotation))

#|
;;; Relation to rotation matrices

;;; Trig method.  Has problems if real part = 0.

(define (rotation-matrix->quaternion M)
  ;;(assert (orthogonal-matrix? M))
  (let ((v (column-matrix->vector
	    (antisymmetric->column-matrix
             (g:* 1/2 (g:- M (m:transpose M))))))
	(cos-o (g:* 1/2 (g:- (m:trace M) 1))))
    (let ((sin-o (euclidean-norm v)))
      (let ((o (g:atan sin-o cos-o)))
	(let ((qv
               (g:* (g:sin (g:/ o 2))
                    (g:/ v (g:sin o)))))
	  (real&3vector->quaternion
           (g:cos (g:/ o 2)) qv))))))


;;; From Matt Mason, Lecture 7, Mechanics of Manipulation, Spring 2012.

(define (rotation-matrix->quaternion-mason M)
  (let ((r11 (matrix-ref M 0 0)) (r12 (matrix-ref M 0 1)) (r13 (matrix-ref M 0 2))
	(r21 (matrix-ref M 1 0)) (r22 (matrix-ref M 1 1)) (r23 (matrix-ref M 1 2))
	(r31 (matrix-ref M 2 0)) (r32 (matrix-ref M 2 1)) (r33 (matrix-ref M 2 2)))
    (let ((q0^2 (g:* 1/4 (g:+ 1 r11 r22 r33)))
	  (q1^2 (g:* 1/4 (g:+ 1 r11 (g:- r22) (g:- r33))))
	  (q2^2 (g:* 1/4 (g:+ 1 (g:- r11) r22 (g:- r33))))
	  (q3^2 (g:* 1/4 (g:+ 1 (g:- r11) (g:- r22) r33)))

	  (q0q1 (g:* 1/4 (g:- r32 r23)))
	  (q0q2 (g:* 1/4 (g:- r13 r31)))
	  (q0q3 (g:* 1/4 (g:- r21 r12)))
	  (q1q2 (g:* 1/4 (g:+ r12 r21)))
	  (q1q3 (g:* 1/4 (g:+ r13 r31)))
	  (q2q3 (g:* 1/4 (g:+ r23 r32))))
      ;; If numerical, choose largest of squares.  
      ;; If symbolic, choose nonzero square.
      (let ((q0 (g:sqrt q0^2)))
	(let ((q1 (g:/ q0q1 q0))
	      (q2 (g:/ q0q2 q0))
	      (q3 (g:/ q0q3 q0)))
	  (quaternion q0 q1 q2 q3))))))
|#

;;; Expanded Matt Mason method.

(define (rotation-matrix->quaternion M)
  ;; (assert (orthogonal-matrix? M))
  ;; returns a unit quaternion
  (let ((r11 (matrix-ref M 0 0)) (r12 (matrix-ref M 0 1)) (r13 (matrix-ref M 0 2))
	(r21 (matrix-ref M 1 0)) (r22 (matrix-ref M 1 1)) (r23 (matrix-ref M 1 2))
	(r31 (matrix-ref M 2 0)) (r32 (matrix-ref M 2 1)) (r33 (matrix-ref M 2 2)))
    (let ((q0^2 (g:* 1/4 (g:+ 1 r11 r22 r33)))
	  (q1^2 (g:* 1/4 (g:+ 1 r11 (g:- r22) (g:- r33))))
	  (q2^2 (g:* 1/4 (g:+ 1 (g:- r11) r22 (g:- r33))))
	  (q3^2 (g:* 1/4 (g:+ 1 (g:- r11) (g:- r22) r33)))

	  (q0q1 (g:* 1/4 (g:- r32 r23)))
	  (q0q2 (g:* 1/4 (g:- r13 r31)))
	  (q0q3 (g:* 1/4 (g:- r21 r12)))
	  (q1q2 (g:* 1/4 (g:+ r12 r21)))
	  (q1q3 (g:* 1/4 (g:+ r13 r31)))
	  (q2q3 (g:* 1/4 (g:+ r23 r32))))
      (let ((q0^2s (careful-simplify q0^2))
	    (q1^2s (careful-simplify q1^2))
	    (q2^2s (careful-simplify q2^2))
	    (q3^2s (careful-simplify q3^2)))
	(cond ((and (number? q0^2s) (number? q1^2s)
		    (number? q2^2s) (number? q3^2s))
	       (cond ((>= q0^2s (max q1^2s q2^2s q3^2s))
		      (let ((q0 (sqrt q0^2s)))
			(let ((q1 (g:/ q0q1 q0))
			      (q2 (g:/ q0q2 q0))
			      (q3 (g:/ q0q3 q0)))
			  (quaternion q0 q1 q2 q3))))
		     ((>= q1^2s (max q0^2s q2^2s q3^2s))
		      (let ((q1 (sqrt q1^2s)))
			(let ((q0 (g:/ q0q1 q1))
			      (q2 (g:/ q1q2 q1))
			      (q3 (g:/ q1q3 q1)))
			  (quaternion q0 q1 q2 q3))))
		     ((>= q2^2s (max q0^2s q1^2s q3^2s))
		      (let ((q2 (sqrt q2^2s)))
			(let ((q0 (g:/ q0q2 q2))
			      (q1 (g:/ q1q2 q2))
			      (q3 (g:/ q2q3 q2)))
			  (quaternion q0 q1 q2 q3))))
		     (else
		      (let ((q3 (sqrt q3^2s)))
			(let ((q0 (g:/ q0q3 q3))
			      (q1 (g:/ q1q3 q3))
			      (q2 (g:/ q2q3 q3)))
			  (quaternion q0 q1 q2 q3))))))
	      ((not (and (number? q0^2s) (zero? q0^2s)))
	       (let ((q0 (g:sqrt q0^2)))
		 (let ((q1 (g:/ q0q1 q0))
		       (q2 (g:/ q0q2 q0))
		       (q3 (g:/ q0q3 q0)))
		   (quaternion q0 q1 q2 q3))))
	      ((not (and (number? q1^2s) (zero? q1^2s)))
	       (let ((q1 (g:sqrt q1^2)))
		 (let ((q0 0)
		       (q2 (g:/ q1q2 q1))
		       (q3 (g:/ q1q3 q1)))
		   (quaternion q0 q1 q2 q3))))
	      ((not (and (number? q2^2s) (zero? q2^2s)))
	       (let ((q2 (g:sqrt q2^2)))
		 (let ((q0 0)
		       (q1 0)
		       (q3 (g:/ q2q3 q2)))
		   (quaternion q0 q1 q2 q3))))
	      (else
	       (quaternion 0 0 0 0)))))))

(define q:rotation-matrix-> rotation-matrix->quaternion)
	       
#|
(let ((M (* (rotate-z-matrix 0.1)
	    (rotate-x-matrix 0.2)
	    (rotate-z-matrix 0.3))))
  (- (rotation-matrix->quaternion-mason M)
     (rotation-matrix->quaternion M)))
#|
(quaternion 0. 0. 1.734723475976807e-18 0.)
|#
|#

(define (quaternion->rotation-matrix q)
  (assert (quaternion? q))
  ;;(assert (q:unit? q))
  ;; This assertion is really:
  (let* ((vv (quaternion->vector q))
         (v (g:simplify (v:dot-product vv vv))))
    (assume! `(= ,v 1) 'quaternion->rotation-matrix))
  (let ((q0 (quaternion-ref q 0)) (q1 (quaternion-ref q 1))
        (q2 (quaternion-ref q 2)) (q3 (quaternion-ref q 3)))
    (let ((m^2 (g:+ (g:expt q0 2) (g:expt q1 2)
                    (g:expt q2 2) (g:expt q3 2))))
      (matrix-by-rows
       (list (g:/ (g:+ (g:expt q0 2)
                       (g:expt q1 2)
                       (g:* -1 (g:expt q2 2))
                       (g:* -1 (g:expt q3 2)))
                  m^2)
             (g:/ (g:* 2 (g:- (g:* q1 q2) (g:* q0 q3)))
                  m^2)
             (g:/ (g:* 2 (g:+ (g:* q1 q3) (g:* q0 q2)))
                  m^2))
       (list (g:/ (g:* 2 (g:+ (g:* q1 q2) (g:* q0 q3)))
                  m^2)
             (g:/ (g:+ (g:expt q0 2)
                       (g:* -1 (g:expt q1 2))
                       (g:expt q2 2)
                       (g:* -1 (g:expt q3 2)))
                  m^2)
             (g:/ (g:* 2 (g:- (g:* q2 q3) (g:* q0 q1)))
                  m^2))
       (list (g:/ (g:* 2 (g:- (g:* q1 q3) (g:* q0 q2)))
                  m^2)
             (g:/ (g:* 2 (g:+ (g:* q2 q3) (g:* q0 q1)))
                  m^2)
             (g:/ (g:+ (g:expt q0 2)
                       (g:* -1 (g:expt q1 2))
                       (g:* -1 (g:expt q2 2))
                       (g:expt q3 2))
                  m^2))))))

(define q:->rotation-matrix quaternion->rotation-matrix)

#|
(let ((theta 'theta) (v (up 'x 'y 'z)))
  (let ((axis (v:make-unit v)))
    (let ((result
	   ((compose quaternion->angle-axis
		     rotation-matrix->quaternion
		     quaternion->rotation-matrix
		     angle-axis->quaternion)
	    theta axis)))
      (up (- (car result) theta)
	  (- (cadr result) axis)))))
#|
(up 0 (up 0 0 0))
|#
;;; But look at (show-notes) to see the assumptions.

;;; Indeed:
(let ((theta -1) (v (up 1 2 3)))
  (let ((axis (v:make-unit v)))
    (let ((result
	   ((compose quaternion->angle-axis
		     rotation-matrix->quaternion
		     quaternion->rotation-matrix
		     angle-axis->quaternion)
	    theta axis)))
      (up (- (car result) theta)
	  (- (cadr result) axis)))))
#|
(up 2.
    (up -.5345224838248488
        -1.0690449676496976
        -1.6035674514745464))
|#
|#

(assign-operation 'type             q:type            quaternion?)
(assign-operation 'type-predicate   q:type-predicate  quaternion?)

(assign-operation 'arity            q:arity           quaternion?)

(assign-operation 'inexact?         q:inexact?        quaternion?)

(assign-operation 'zero-like        q:zero-like       quaternion?)

(assign-operation 'zero?            q:zero?           quaternion?)

(assign-operation 'negate           q:negate          quaternion?)

(assign-operation 'magnitude        q:magnitude       quaternion?)


(assign-operation 'conjugate        q:conjugate       quaternion?)
(assign-operation 'invert 	    q:invert 	      quaternion?)

(assign-operation 'real-part        q:real-part       quaternion?)


(assign-operation 'exp              q:exp             quaternion?)
(assign-operation 'log              q:log             quaternion?)



(assign-operation '=     q:=                       quaternion? quaternion?)

(assign-operation '+     quaternion+quaternion     quaternion? quaternion?)
(assign-operation '-     quaternion-quaternion     quaternion? quaternion?)

(assign-operation '*     quaternion*quaternion     quaternion? quaternion?)

(assign-operation '*     scalar*quaternion         scalar?     quaternion?)
(assign-operation '*     quaternion*scalar         quaternion? scalar?)

(assign-operation '/     quaternion/scalar         quaternion? scalar?)
(assign-operation '/     quaternion/quaternion     quaternion? quaternion?)


(assign-operation 'apply            q:apply        quaternion? any?)

(assign-operation 'partial-derivative q:partial-derivative quaternion? any?)

