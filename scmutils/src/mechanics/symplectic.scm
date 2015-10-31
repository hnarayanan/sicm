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

(define (symplectic-two-form zeta1 zeta2)
  (- (* (momenta zeta2) (coordinates zeta1))
     (* (momenta zeta1) (coordinates zeta2))))


;;; Without matrices

(define ((canonical-transform? C) s)
  (let ((J ((D J-func) (compatible-shape s)))
        (DCs ((D C) s)))
    (let ((DCsT (transpose DCs s)))
      (- J (* DCs J DCsT)))))

#|
(print-expression
 ((canonical-transform? (F->CT p->r))
  (up 't
      (up 'r 'phi)
      (down 'p_r 'p_phi))))
(up (up 0 (up 0 0) (down 0 0))
    (up (up 0 (up 0 0) (down 0 0)) (up 0 (up 0 0) (down 0 0)))
    (down (up 0 (up 0 0) (down 0 0)) (up 0 (up 0 0) (down 0 0))))


(print-expression
 ((canonical-transform? (polar-canonical 'alpha))
  (up 't 'a 'I)))
(up (up 0 0 0) (up 0 0 0) (up 0 0 0))


;;; but not all transforms are

(define (a-non-canonical-transform Istate)
  (let ((t (time Istate))
        (theta (coordinate Istate))
	(p (momentum Istate)))
    (let ((x (* p (sin theta)))
	  (p_x (* p (cos theta))))
      (up t x p_x))))

(print-expression
 ((canonical-transform? a-non-canonical-transform)
  (up 't 'theta 'p)))
(up (up 0 0 0) (up 0 0 (+ -1 p)) (up 0 (+ 1 (* -1 p)) 0))
|#

#|
(define (Cmix H-state)
  (let ((t (time H-state))
	(q (coordinate H-state))
	(p (momentum H-state)))
    (up t
	(up (ref q 0) (- (ref p 1)))
	(down (ref p 0) (ref q 1)))))

(define a-state
  (up 't (up 'x 'y) (down 'p_x 'p_y)))

(print-expression
 ((canonical-transform? Cmix) a-state))
(up (up 0 (up 0 0) (down 0 0))
    (up (up 0 (up 0 0) (down 0 0)) (up 0 (up 0 0) (down 0 0)))
    (down (up 0 (up 0 0) (down 0 0)) (up 0 (up 0 0) (down 0 0))))


(define (Cmix2 H-state)
  (let ((t (time H-state))
	(q (coordinate H-state))
	(p (momentum H-state)))
    (up t
	(flip-outer-index p)
	(- (flip-outer-index q)))))

(print-expression
 ((canonical-transform? Cmix2)
  a-state))
(up (up 0 (up 0 0) (down 0 0))
    (up (up 0 (up 0 0) (down 0 0)) (up 0 (up 0 0) (down 0 0)))
    (down (up 0 (up 0 0) (down 0 0)) (up 0 (up 0 0) (down 0 0))))
|#

#|
(define ((C m0 m1) state)
  (let ((x (coordinate state))
	(p (momentum state)))
    (let ((x0 (ref x 0))
	  (x1 (ref x 1))
	  (p0 (ref p 0))
	  (p1 (ref p 1)))
      (up (time state)
	  (up (/ (+ (* m0 x0) (* m1 x1)) (+ m0 m1))
	      (- x1 x0))
	  (down (+ p0 p1)
		(/ (- (* m0 p1) (* m1 p0))
		   (+ m0 m1)))))))

(define b-state
  (up 't
      (up (up 'x_1 'y_1)
	  (up 'x_2 'y_2))
      (down (down 'p_x_1 'p_y_1)
	    (down 'p_x_2 'p_y_2))))

(print-expression
 ((canonical-transform? (C 'm1 'm2)) b-state))
(up
 (up 0 (up (up 0 0) (up 0 0)) (down (down 0 0) (down 0 0)))
 (up
  (up (up 0 (up (up 0 0) (up 0 0)) (down (down 0 0) (down 0 0)))
      (up 0 (up (up 0 0) (up 0 0)) (down (down 0 0) (down 0 0))))
  (up (up 0 (up (up 0 0) (up 0 0)) (down (down 0 0) (down 0 0)))
      (up 0 (up (up 0 0) (up 0 0)) (down (down 0 0) (down 0 0)))))
 (down
  (down (up 0 (up (up 0 0) (up 0 0)) (down (down 0 0) (down 0 0)))
        (up 0 (up (up 0 0) (up 0 0)) (down (down 0 0) (down 0 0))))
  (down (up 0 (up (up 0 0) (up 0 0)) (down (down 0 0) (down 0 0)))
        (up 0 (up (up 0 0) (up 0 0)) (down (down 0 0) (down 0 0))))))
|#

(define (J-matrix n)                    ;degrees of freedom
  (let ((2n+1 (fix:+ (fix:* 2 n) 1)))
    (m:generate 2n+1 2n+1
       (lambda (a b) 
	 (cond ((fix:= a 0) 0)
               ((fix:= b 0) 0)
               ((fix:= (fix:+ a n) b) 1)
	       ((fix:= (fix:+ b n) a) -1)
	       (else 0))))))


;;; Symplectic test in terms of matrices

(define ((symplectic? C) s)
  (let ((J (J-matrix (degrees-of-freedom s)))
        (DC ((D-as-matrix C) s)))
    (- J (* DC J (transpose DC)))))

#|
(print-expression
 ((symplectic? (F->CT p->r))
  (up 't
      (up 'r 'phi)
      (down 'p_r 'p_phi))))
(matrix-by-rows (list 0 0 0 0 0)
                (list 0 0 0 0 0)
                (list 0 0 0 0 0)
                (list 0 0 0 0 0)
                (list 0 0 0 0 0))


;;; but not all transforms are

(define (a-non-canonical-transform Istate)
  (let ((t (time Istate))
        (theta (coordinate Istate))
	(p (momentum Istate)))
    (let ((x (* p (sin theta)))
	  (p_x (* p (cos theta))))
      (up t x p_x))))

(print-expression
 ((symplectic? a-non-canonical-transform)
  (up 't 'theta 'p)))
(matrix-by-rows (list 0 0 0)
		(list 0 0 (+ 1 (* -1 p)))
		(list 0 (+ -1 p) 0))

(print-expression
 ((symplectic? (polar-canonical 'alpha))
  (up 't 'a 'I)))
(matrix-by-rows (list 0 0 0)
		(list 0 0 0)
		(list 0 0 0))

(define (Cmix H-state)
  (let ((t (time H-state))
	(q (coordinate H-state))
	(p (momentum H-state)))
    (up t
	(up (ref q 0) (- (ref p 1)))
	(down   (ref p 0) (ref q 1)))))

(define a-state
  (up 't (up 'x 'y) (down 'p_x 'p_y)))

(print-expression ((symplectic? Cmix) a-state))
(matrix-by-rows (list 0 0 0 0 0)
                (list 0 0 0 0 0)
                (list 0 0 0 0 0)
                (list 0 0 0 0 0)
                (list 0 0 0 0 0))
|#

#|
(define (Cmix2 H-state)
  (let ((t (time H-state))
	(q (coordinate H-state))
	(p (momentum H-state)))
    (up t
	(flip-outer-index p)
	(- (flip-outer-index q)))))

(print-expression
 ((canonical-transform? Cmix2)
  a-state))
(matrix-by-rows (list 0 0 0 0 0)
                (list 0 0 0 0 0)
                (list 0 0 0 0 0)
                (list 0 0 0 0 0)
                (list 0 0 0 0 0))


(define ((C m0 m1) state)
  (let ((x (coordinate state))
	(p (momentum state)))
    (let ((x0 (ref x 0))
	  (x1 (ref x 1))
	  (p0 (ref p 0))
	  (p1 (ref p 1)))
      (up (time state)
	  (up (/ (+ (* m0 x0) (* m1 x1)) (+ m0 m1))
	      (- x1 x0))
	  (down (+ p0 p1)
		(/ (- (* m0 p1) (* m1 p0))
		   (+ m0 m1)))))))

(define b-state
  (up 't
      (up (up 'x_1 'y_1)
	  (up 'x_2 'y_2))
      (down (down 'p_x_1 'p_y_1)
	    (down 'p_x_2 'p_y_2))))

(print-expression
 ((canonical-transform? (C 'm1 'm2)) b-state))
(matrix-by-rows (list 0 0 0 0 0 0 0 0 0)
                (list 0 0 0 0 0 0 0 0 0)
                (list 0 0 0 0 0 0 0 0 0)
                (list 0 0 0 0 0 0 0 0 0)
                (list 0 0 0 0 0 0 0 0 0)
                (list 0 0 0 0 0 0 0 0 0)
                (list 0 0 0 0 0 0 0 0 0)
                (list 0 0 0 0 0 0 0 0 0)
                (list 0 0 0 0 0 0 0 0 0))
|#

;;; qp version below.

(define ((symplectic-transform? C) s)
  (symplectic-matrix? (qp-submatrix ((D-as-matrix C) s))))

(define (qp-submatrix m)
  (m:submatrix m 1 (m:num-rows m) 1 (m:num-cols m)))


(define (symplectic-matrix? M)
  (let ((2n (m:dimension M)))
    (if (not (even? 2n))
	(error "Wrong type -- SYMPLECTIC-MATRIX?" M))
    (let ((J (symplectic-unit (quotient 2n 2))))
      (- J (* M J (transpose M))))))

(define (symplectic-unit n)
  (let ((2n (fix:* 2 n)))
    (m:generate 2n 2n
       (lambda (a b) 
	 (cond ((fix:= (fix:+ a n) b) 1)
	       ((fix:= (fix:+ b n) a) -1)
	       (else 0))))))

#|
;;; For example, point transforms are canonical

(print-expression
 ((symplectic-transform? (F->CT p->r))
  (up 't
      (up 'r 'theta)
      (down 'p_r 'p_theta))))
(matrix-by-rows (list 0 0 0 0)
		(list 0 0 0 0)
		(list 0 0 0 0)
		(list 0 0 0 0))
|#

#|
(print-expression
 ((symplectic-transform? a-non-canonical-transform)
  (up 't 'theta 'p)))
(matrix-by-rows (list 0 (+ 1 (* -1 p))) (list (+ -1 p) 0))
|#

#|
;;; One particularly useful canonical transform is the 
;;;  Poincare transform, which is good for simplifying 
;;;  oscillators.

(define ((polar-canonical alpha) Istate)
  (let ((t (time Istate))
        (theta (coordinate Istate))
        (I (momentum Istate)))
    (let ((x (* (sqrt (/ (* 2 I) alpha)) (sin theta)))
	  (p_x (* (sqrt (* 2 alpha I)) (cos theta))))
      (up t x p_x))))

(define ((polar-canonical-inverse alpha) s)
  (let ((t (time s))
	(x (coordinate s))
	(p (momentum s)))
    (let ((I (/ (+ (* alpha (square x))
		   (/ (square p) alpha))
		2)))
      (let ((theta (atan (/ x (sqrt (/ (* 2 I) alpha)))
			 (/ p (sqrt (* 2 I alpha))))))
	(up t theta I)))))



(pe
 ((compose (polar-canonical-inverse 'alpha)
	   (polar-canonical 'alpha))
  (up 't 'x 'p)))
(up t x p)

|#

#|
;;; It is clearly canonical.

(print-expression
 ((symplectic-transform? (polar-canonical 'alpha))
  (up 't 'a 'I)))
(matrix-by-rows (list 0 0) (list 0 0))
|#
