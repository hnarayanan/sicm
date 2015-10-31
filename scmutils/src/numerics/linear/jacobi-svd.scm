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

;;;;         Jacobi-Givens rotations... 

;;; Scheme version of SVD, using ideas from Golub and Van Loan,
;;;  by Gerald Jay Sussman

(define (svd-least-squares a b #!optional eps)
  (if (default-object? eps) (set! eps 1e-15))
  (slow-svd a
       (lambda (u sigma v w)
	 (let* ((maxw
		 (apply max (map magnitude (vector->list w))))
		(inverted-w
		 (let ((wmin
			(cond ((number? eps) (* eps maxw))
			      ((procedure? eps) (* (eps w) maxw))
			      (else
			       (error "Bad cutoff -- SVD" eps)))))
		   (make-initialized-vector (vector-length w)
		     (lambda (i) 
		       (let ((wi (vector-ref w i)))
			 (if (< (magnitude wi) wmin) 0 (/ 1 wi))))))))
	   (matrix*vector v
			  ((vector-elementwise *)
			   (matrix*vector (m:transpose u) b)
			   inverted-w))))))


(define (slow-svd A cont)
  ;;cont = (lambda (U S V)			
             ;; (assert (= A (* U S (m:transpose V))))
  ;;         ... )
  (let ((U (m:make-identity (m:num-rows A)))
	(V (m:make-identity (m:num-cols A)))
	(S (m:copy A))
	(maxn (m:maxnorm A)))
    (slow-svd! S U V (* svd-tolerance maxn *machine-epsilon*))
    (cont U S (m:transpose V)
	  (v:generate (fix:min (m:num-rows A)
			       (m:num-cols A))
		      (lambda (i)
			(matrix-ref S i i))))))

(define (slow-svd! A U V tolerance)
  (let lp ()
      (max-off-diagonal-element A
	(lambda (i j amax)
	  (cond ((> amax tolerance)
		 (force-to-zero! A i j U V)
		 (lp))
		(else 'done))))))

(define svd-tolerance 10)

;;; The following procedure forces A_ij = 0 by Jacobi rotation.
;;; If A is an mxn matrix then the mxm matrix U and the nxn matrix V
;;; are modified to record the rotations performed on A.

(define (force-to-zero! A i j U V)
  (cond ((fix:< i j)			; superdiagonal
	 (Jacobi-rotation (matrix-ref A i i) (matrix-ref A i j)
			  (lambda (c s)
			    (Right-Jacobi! A i j c s)
			    (Left-Jacobi! V i j c s))))
	((fix:> i j)			; subdiagonal
	 (Jacobi-rotation (matrix-ref A j j) (matrix-ref A i j)
			  (lambda (c s)
			    (Left-Jacobi! A j i c s)
			    (Right-Jacobi! U j i c s))))
	(else
	 (error "Diagonal element -- FORCE-TO-ZERO!" i))))

;;; Given scalars a and b to compute c and s so that 
;;;            T
;;;  |  c   s |     | a |   | r |
;;;  |        |  X  |   | = |   |
;;;  | -s   c |     | b |   | 0 |
;;;

(define (Jacobi-rotation a b cont)
  ;; cont = (lambda (c s) ...)
  (define (nonzeronumbers cont)
    (define (signit c s cont)
      (let ((nx (- (* c a) (* s b))))
	(if (real? nx)
	    (if (negative? nx)
		(cont (- c) (- s))
		(cont c s))
	    (if (negative? (real-part nx))
		(cont (- c) (- s))
		(cont c s)))))
    (if (> (magnitude b) (magnitude a))
	(let ((tau (- (/ a b))))
	  (let ((s (/ 1 (sqrt (+ 1 (square tau))))))
	    (signit (* s tau) s cont)))
	(let ((tau (- (/ b a))))
	  (let ((c (/ 1 (sqrt (+ 1 (square tau))))))
	    (signit c (* c tau) cont)))))
  (define (symbolic cont)
    (let ((l (sqrt (+ (square a) (square b)))))
      (cont (/ a l) (- (/ b l)))))
  (cond ((number? b)
	 (cond ((= b 0) (cont 1 0))
	       ((number? a)
		(if (= a 0)
		    (cont 0 1)
		    (nonzeronumbers cont)))
	       (else (symbolic cont))))
	((and (number? a) (= a 0))
	 (cont 0 1))
	(else (symbolic cont))))

;;; Given an mXn matrix A and an mXm rotation G(i, k, c, s), 
;;;    where c=cos(theta) and s=sin(theta), the
;;; following procedure modifies A to perform the rotation:
;;;        A <-- transpose(G(i, k, c, s)) * A

(define (Left-Jacobi! A i k c s)
  (let ((m (m:num-rows A))
	(n (m:num-cols A))
	(A (matrix->array A)))
    (assert (and (fix:<= 0 i) (fix:< i m)
		 (fix:<= 0 k) (fix:< k m)))
    (do-up 0 n
	   (lambda (j)
	     (let ((tau1 (array-ref A i j))
		   (tau2 (array-ref A k j)))
	       ;; Note typo in G&VL 5.1.9.
	       (array-set! A i j (- (* c tau1) (* s tau2)))
	       (array-set! A k j (+ (* s tau1) (* c tau2))))))))

;;; Given an mXn matrix A and an nXn rotation G(i, k, c, s), the
;;;    where c=cos(theta) and s=sin(theta), the
;;; following procedure modifies A to perform the rotation:
;;;        A <-- A * G(i, k, c, s)

(define (Right-Jacobi! A i k c s)
  (let ((m (m:num-rows A))
	(n (m:num-cols A))
	(A (matrix->array A)))
    (assert (and (fix:<= 0 i) (fix:< i n)
		 (fix:<= 0 k) (fix:< k n)))
    (do-up 0 m
	   (lambda (j)
	     (let ((tau1 (array-ref A j i))
		   (tau2 (array-ref A j k)))
	       (array-set! A j i (- (* c tau1) (* s tau2)))
	       (array-set! A j k (+ (* s tau1) (* c tau2))))))))

(define (max-off-diagonal-element A cont)
  ;; cont = (lambda (i j aij) ...)
  (let ((m (m:num-rows A))
	(n (m:num-cols A))
	(A (matrix->array A)))
    (let ((topi 0)
	  (topj 1)
	  (topmag (magnitude (array-ref A 0 1))))
      (let rowlp ((i 0))
	(if (fix:= i m)
	    (cont topi topj topmag)
	    (let ((row (vector-ref A i)))
	      (let collp ((j 0))
		(cond ((fix:= j n)
		       (rowlp (fix:+ i 1)))
		      ((fix:= i j)
		       (collp (fix:+ j 1)))
		      (else
		       (let ((ma (magnitude (vector-ref row j))))
			 (if (> ma topmag)
			     (begin (set! topi i)
				    (set! topj j)
				    (set! topmag ma)
				    (collp (fix:+ j 1)))
			     (collp (fix:+ j 1)))))))))))))

(define (m:copy A)
  (tag-matrix (m:num-rows A)
	      (m:num-cols A)
	      (array-copy (matrix->array A))))

(define (m:maxnorm A)
  (apply max (map maxnorm (vector->list (matrix->array A)))))

(define (test-svd A)
  (slow-svd A
	    (lambda (U S V sing)
	      (list (m:maxnorm (- (* (m:transpose U) A V)
				  S))
		    (m:maxnorm (- A (* U S (m:transpose V))))
		    sing))))
#|
(pe (test-svd
     (matrix-by-rows (list 1/3 1/3 2/3)
		     (list 2/3 2/3 4/3)
		     (list 1/3 2/3 3/3)
		     (list 2/5 2/5 4/5)
		     (list 3/5 1/5 4/5))))
(6.020152509023309e-16
 8.881784197001252e-16
 (up 2.5987215089389935 .36815133826556556 2.582797189886822e-17))

(pe (test-svd (matrix:generate 10 6
			       (lambda (i j)
				 (+ 1 (* i i j))))))
(1.9988923584309317e-13
 1.1368683772161603e-13
 (up 922.9930189039417
     2.981116332488982
     -2.0408046929126068e-16
     7.271245817610178e-16
     -1.8860055683679873e-15
     4.2582974233184604e-15))

;;; Example from Forsyth, Malcom, and Moler

(pe (test-svd
     (matrix-by-rows (list 1  6 11)
		     (list 2  7 12)
		     (list 3  8 13)
		     (list 4  9 14)
		     (list 5 10 15))))
(7.105427357601002e-15
 2.1316282072803006e-14
 (up 35.12722333357465 2.4653966969165184 1.1062054708352963e-15))

(4.440892098500626e-16
 7.771561172376096e-16
 (up 1.28766271605269
     .16273735109863258
     1.3248901623213105e-2
     7.980370980435422e-4
     3.589955622528404e-5
     1.1926446632613517e-6
     2.8444950944661096e-8
     4.611354262855067e-10
     4.553227914564646e-12
     2.067444395900821e-14))
|#