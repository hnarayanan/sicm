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


(define D     derivative)

(define I     g:identity)


(define ((D-as-matrix F) s)
  (s->m (compatible-shape (F s))
	((D F) s)
	s))
#|
((D-as-matrix (literal-function 'H (Hamiltonian 2)))
 (up 't (up 'x 'y) (down 'p_x 'p_y)))
#|
(matrix-by-rows
 (list (((partial 0) H) (up t (up x y) (down p_x p_y)))
       (((partial 1 0) H) (up t (up x y) (down p_x p_y)))
       (((partial 1 1) H) (up t (up x y) (down p_x p_y)))
       (((partial 2 0) H) (up t (up x y) (down p_x p_y)))
       (((partial 2 1) H) (up t (up x y) (down p_x p_y)))))
|#
|#

;;; This does not produce a function.  It is a 
;;; symbolic manipulation.

#|
;;; Only one dimension

(define (Taylor-series-coefficients f x)
  (let ((dummy (generate-uninterned-symbol 'x)))
    ((series:elementwise (compose
			  simplify
			  (lambda (term)
			    (subst x dummy term))
			  simplify))
     (((exp D) f) dummy))))


;;; With one structured argument

(define (Taylor-series-coefficients f x)
  (let ((dummy (typical-object x)))
    ((series:elementwise 
      (compose
       simplify
       (lambda (term)
	 (let ((ans term))
	   (let walk ((x x) (dummy dummy))
	     (cond ((structure? x)
		    (let ((n (s:length x)))
		      (let lp ((i 0))
			(if (fix:= i n)
			    'done
			    (begin
			      (walk (s:ref x i) (s:ref dummy i))
			      (lp (fix:+ i 1)))))))
		   ((pair? x)
		    (for-each walk x y))		    
		   (else
		    (set! ans (subst x dummy ans)))))
	   ans))
       simplify))
     (((exp D) f) dummy))))
|#

;;; With many structured arguments

(define (Taylor-series-coefficients f . args)
  (assert (not (null? args)))
  (define (output result x dummy)
    ((series:elementwise 
      (compose
       simplify
       (lambda (term)
	 (let ((ans term))
	   (let walk ((x x) (dummy dummy))
	     (cond ((structure? x)
		    (let ((n (s:length x)))
		      (let lp ((i 0))
			(if (fix:= i n)
			    'done
			    (begin
			      (walk (s:ref x i) (s:ref dummy i))
			      (lp (fix:+ i 1)))))))
		   ((pair? x)
		    (for-each walk x dummy))		    
		   (else
		    (set! ans (subst x dummy ans)))))
	   ans))
       simplify))
     result))
  (if (null? (cdr args))
      (let* ((x (car args))
	     (dummy (typical-object x)))
	(output (((exp D) f) dummy) x dummy))
      (let* ((x args)
	     (dummies (map typical-object x)))
	(output (apply ((exp D) f) dummies) x dummies))))





#|
;;; Examples


(ref (Taylor-series-coefficients exp 0) 0)
#| 1 |#

(ref (Taylor-series-coefficients exp 0) 1)
#| 1 |#

(ref (Taylor-series-coefficients exp 0) 2)
#| 1/2 |#

(ref (Taylor-series-coefficients exp 0) 3)
#| 1/6 |#



(define (f v)
  (* (sin (* 3 (ref v 0)))
     (cos (* 4 (ref v 1)))))


(ref (Taylor-series-coefficients f (up 'a 'b)) 0)
#|
(* (cos (* 4 b)) (sin (* 3 a)))
|#

(ref (Taylor-series-coefficients f (up 'a 'b)) 1)
#|
(down (* 3 (cos (* 4 b)) (cos (* 3 a))) (* -4 (sin (* 3 a)) (sin (* 4 b))))
|#

(ref (Taylor-series-coefficients f (up 'a 'b)) 2)
#|
(down (down (* -9/2 (cos (* 4 b)) (sin (* 3 a))) (* -6 (sin (* 4 b)) (cos (* 3 a))))
      (down (* -6 (sin (* 4 b)) (cos (* 3 a))) (* -8 (cos (* 4 b)) (sin (* 3 a)))))
|#


(ref
 (Taylor-series-coefficients (literal-function 'G (-> (UP Real Real) Real))
			     (up 'a 'b))
 0)
#|
(G (up a b))
|#

(ref
 (Taylor-series-coefficients (literal-function 'G (-> (UP Real Real) Real))
			     (up 'a 'b))
 1)
#|
(down (((partial 0) G) (up a b)) (((partial 1) G) (up a b)))
|#

(ref
 (Taylor-series-coefficients (literal-function 'G (-> (UP Real Real) Real))
			     (up 'a 'b))
 2)
#|
(down
 (down (* 1/2 (((partial 0) ((partial 0) G)) (up a b)))
       (* 1/2 (((partial 0) ((partial 1) G)) (up a b))))
 (down (* 1/2 (((partial 0) ((partial 1) G)) (up a b)))
       (* 1/2 (((partial 1) ((partial 1) G)) (up a b)))))
|#




(ref
 (Taylor-series-coefficients (literal-function 'H (-> (X Real Real) Real))
			     'a 'b)
 0)
#|
(H a b)
|#

(ref
 (Taylor-series-coefficients (literal-function 'H (-> (X Real Real) Real))
			     'a 'b)
 3)
#|
(down
 (down
  (down (* 1/6 (((partial 0) ((partial 0) ((partial 0) H))) a b))
        (* 1/6 (((partial 0) ((partial 0) ((partial 1) H))) a b)))
  (down (* 1/6 (((partial 0) ((partial 0) ((partial 1) H))) a b))
        (* 1/6 (((partial 0) ((partial 1) ((partial 1) H))) a b))))
 (down
  (down (* 1/6 (((partial 0) ((partial 0) ((partial 1) H))) a b))
        (* 1/6 (((partial 0) ((partial 1) ((partial 1) H))) a b)))
  (down (* 1/6 (((partial 0) ((partial 1) ((partial 1) H))) a b))
        (* 1/6 (((partial 1) ((partial 1) ((partial 1) H))) a b)))))
|#
|#

;;;; Bug!
;;; This does not produce a function.  It is a 
;;; symbolic manipulation.

#|
(define (Taylor-series-coefficients f x)
  (let ((dummy (generate-uninterned-symbol 'x)))
    ((series:elementwise (compose
			  simplify
			  (lambda (term)
			    (subst x dummy term))
			  simplify))
     (((exp D) f) dummy))))


((D (lambda (y) 
      (ref (Taylor-series-coefficients (lambda (x) (* x y)) 0) 1)))
 'a)
#| 0 |#  ; Wrong

;;; Apparently due to simplify...

(define (Taylor-series-coefficients f x)
  (let ((dummy (generate-uninterned-symbol 'x)))
    ((series:elementwise (lambda (term)
			    (subst x dummy term)))
     (((exp D) f) dummy))))
#| Taylor-series-coefficients |#

((D (lambda (y) 
      (ref (Taylor-series-coefficients (lambda (x) (* x y)) 0) 1)))
 'a)
#| 1 |#  ; Right
|#
