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

;;;; Vandermonde solver, from Zippel, section 13.1

(declare (usual-integrations))

;;; Simple Vandermonde systems
;;; Given k_i and w_j to find x_i 
;;;  such that Sigma_i (expt k_j i) x_i = w_j

;;;  e.g.    x_0 + k_0*x_1 + k_0^2*x_2 + k_0^3*x_3 + ... = w_0
;;;          x_0 + k_1*x_1 + k_1^2*x_2 + k_1^3*x_3 + ... = w_1
;;;          ...

(define (solve-vandermonde-system ks ws succeed fail)
  (let ((n (length ws))
	(linear-terms
	 (map (lambda (k) (sparse-linear 1 0 k))
	      ks)))
    (assert (fix:= n (length ks)))
    (let ((Q
	   (fold-left sparse-multiply (sparse-one 1)
		      linear-terms))
	  (x (make-vector (length ws) 0)))
      (let lp ((i 0))
	(if (fix:= i n)
	    (succeed (vector->list x))
	    (let ((ki (list-ref ks i))
		  (x-ki (list-ref linear-terms i))
		  (except-ki (delete-nth i ks)))
	      (sparse-divide Q x-ki
		(lambda (Qi R)
		  (assert (null? R))
		  (let ((d
			 (sparse-constant-term 1
			    (roots->poly-value except-ki ki))))
		    (if (sparse-zero-term? d)
			(fail)
			(let ((Pi (sparse-normalize Qi d)))
			  (for-each (lambda (term)
				      (let ((j (car (sparse-exponents term)))
					    (c (sparse-coefficient term)))
					(vector-set! x j
						     (+ (* (list-ref ws i) c)
							(vector-ref x j)))))
				    Pi)
			  (lp (fix:+ i 1)))))))))))))

;;; Transposed Vandermonde systems
;;; Given k_i and w_j to find x_i 
;;;  such that Sigma_i (expt k_i j) x_i = w_j 

;;;  e.g.        x_0   +     x_1   +     x_2   +     x_3   + ... = w_0
;;;          k_0*x_0   + k_1*x_1   + k_2*x_2   + k_3*x_3   + ... = w_1
;;;          k_0^2*x_0 + k_1^2*x_1 + k_2^2*x_2 + k_3^2*x_3 + ... = w_2
;;;          ...

(define (solve-vandermonde-t-system ks ws succeed fail)
  (let ((n (length ws))
	(linear-terms
	 (map (lambda (k) (sparse-linear 1 0 k))
	      ks)))
    (assert (fix:= n (length ks)))
    (let ((Q
	   (fold-left sparse-multiply (sparse-one 1)
		      linear-terms))
	  (x (make-vector (length ws) 0)))
      (let lp ((i 0))
	(if (fix:= i n)
	    (succeed (vector->list x))
	    (let ((ki (list-ref ks i))
		  (x-ki (list-ref linear-terms i))
		  (except-ki (delete-nth i ks)))
	      (sparse-divide Q x-ki
		 (lambda (Qi R)
		   (assert (null? R))
		   (let ((d
			  (sparse-constant-term 1
			     (roots->poly-value except-ki ki))))
		     (if (sparse-zero-term? d)
			 (fail)
			 (let ((Pi (sparse-normalize Qi d)))
			   (for-each (lambda (term)
				       (let ((j (car (sparse-exponents term)))
					     (c (sparse-coefficient term)))
					 (vector-set! x i
						      (+ (* (list-ref ws j) c)
							 (vector-ref x i)))))
				     Pi)
			   (lp (fix:+ i 1)))))))))))))

;;; Variant transposed Vandermonde systems
;;; Given k_i and w_j to find x^i 
;;;  such that Sigma_i (expt k_i (+ j 1)) x^i = w^j 

;;;  e.g. 
;;;          k_0*x_0   + k_1*x_1   + k_2*x_2   + k_3*x_3   + ... = w_0
;;;          k_0^2*x_0 + k_1^2*x_1 + k_2^2*x_2 + k_3^2*x_3 + ... = w_1
;;;          ...

(define (solve-vandermonde-td-system ks ws succeed fail)
  (let ((n (length ws))
	(linear-terms
	 (map (lambda (k) (sparse-linear 1 0 k))
	      ks)))
    (assert (fix:= n (length ks)))
    (let ((Q
	   (fold-left sparse-multiply (sparse-one 1)
		      linear-terms))
	  (x (make-vector (length ws) 0)))
      (let lp ((i 0))
	(if (fix:= i n)
	    (succeed (vector->list x))
	    (let ((ki (list-ref ks i))
		  (x-ki (list-ref linear-terms i))
		  (except-ki (delete-nth i ks)))
	      (sparse-divide Q x-ki
		 (lambda (Qi R)
		   (assert (null? R))
		   (let ((d
			  (sparse-constant-term 1
			     (* ki (roots->poly-value except-ki ki)))))
		     (if (sparse-zero-term? d)
			 (fail)
			 (let ((Pi (sparse-normalize Qi d)))
			   (for-each (lambda (term)
				       (let ((j (car (sparse-exponents term)))
					     (c (sparse-coefficient term)))
					 (vector-set! x i
						      (+ (* (list-ref ws j) c)
							 (vector-ref x i)))))
					 Pi)
			   (lp (fix:+ i 1)))))))))))))

(define (roots->poly-value ks z)
  (fold-left *
	     1
	     (map (lambda (k)
		    (- z k))
		  ks)))

#|
(solve-vandermonde-system '(3 5 7) '(9 11 13)
			  (lambda (x) x) (lambda () 'foo))
;Value: (6 1 0)

(solve-vandermonde-t-system '(3 5 7) '(9 11 13)
			    (lambda (x) x) (lambda () 'foo))
;Value: (49/2 -23 15/2)

(solve-vandermonde-td-system '(3 5 7) '(9 11 13)
			     (lambda (x) x) (lambda () 'foo))
;Value: (49/6 -23/5 15/14)

(solve-vandermonde-td-system '(3 5 0) '(9 11 13)
			     (lambda (x) x) (lambda () 'foo))
;Value: foo
|#
