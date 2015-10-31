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

;;;;  Linear System Solver -- LU

(declare (usual-integrations))

;;; This file contains the following definitions:
;;;
;;; (lu-solve-linear-system A-matrix b-vector) => x-vector
;;;
;;; (lu-solve A b succeed fail)
;;;      succeed = (lambda (x) ...)
;;;      fail = (lambda (dismiss) ...)
;;;
;;; (lu-determinant A-matrix) ==> number
;;; (lu-invert A-matrix) ==> matrix
;;;
;;; (lu-null-space matrix) ==> (<vector> ... <vector>)
;;;
;;; (lu-decompose matrix succeed fail)
;;;    succeed = (lambda (lu-matrix lu-permutation sign) ...)
;;;    fail = (lambda (dismiss) ...)
;;;
;;; (lu-backsubstitute lu-matrix lu-permutation b-vector) => x-vector


;;; Solves an inhomogeneous system of linear equations, A*X=B, 
;;; returning the vector X.

(define (lu-solve-linear-system A b)
  (lu-solve-linear-system-internal (matrix->array A) b))

(define (lu-solve A b succeed fail)
  (lu-solve-internal (matrix->array A) b succeed fail))

(define (lu-determinant A)
  (lu-determinant-internal (matrix->array A)))

(define (lu-invert A)
  (array->matrix (lu-invert-internal (matrix->array A))))

(define (lu-decompose A succeed fail)
  (lu-decompose-internal (matrix->array A)
			 (lambda (lu-array permutation sign)
			   (succeed (array->matrix lu-array)
				    permutation
				    sign))
			 fail))

;;; The following procedure assumes that the matrix is in LU form.

(define (lu-backsubstitute lu-matrix lu-permutation b-vector)
  (lu-backsubstitute-internal (matrix->array lu-matrix) lu-permutation b-vector))

;;; The following programs work on arrays rather than matrices.

(define (lu-solve-linear-system-internal A b)
  (lu-decompose-internal A
     (lambda (lumatrix lupermutations sign)
       (lu-backsubstitute-internal lumatrix lupermutations b))
     barf-on-zero-pivot))

(define (lu-solve-internal A b succeed fail)
  (lu-decompose-internal A
     (lambda (lumatrix lupermutations sign)
       (succeed (lu-backsubstitute-internal lumatrix lupermutations b)))
     fail))

(define (lu-invert-internal A)
  (let ((n (num-rows a)))
    (lu-decompose-internal a
      (lambda (lumat luperm sign)
	(let ((m (make-initialized-vector n
		  (lambda (i)
		    (let ((e (v:make-basis-unit n i)))
		      (lu-backsubstitute-internal lumat luperm e))))))
	  (transpose-array m)))
      barf-on-zero-pivot)))
    
(define (lu-determinant-internal M)
  (let ((n (num-rows M)))
    (lu-decompose-internal M
      (lambda (A p s)
	(let loop ((i 0) (prod s))
	  (if (fix:= i n)
	      prod
	      (loop (fix:+ i 1)
		    (* prod (array-ref A i i))))))
      allow-zero-pivot)))

;;; LU decomposition -- uses Crout's algorithm (see Press, section 2.3).
;;; The LU decomposition returns a LIST of the resulting matrix, the
;;; permutation vector, and the sign (+1 or -1) of the permutation.
;;; The only currently-defined flag controls error-trapping on detection
;;; of a zero-pivot. This flag, if omitted, defaults to true.

(define (lu-decompose-internal m succeed singular-matrix)
  (let* ((n (num-rows m))
	 (perms (make-initialized-vector n (lambda (i) i))) ;row permutations
         (sign 1)			;1 for even permutations, -1 for odd
         ;; We must copy the matrix, since Crout's algorithm clobbers it.
         (m (array-copy m)))
    (let jloop ((j 0))
      (if (fix:< j n)
	  (begin
	    (let iloop ((i 0))		;compute elements above diagonal
	      (if (not (fix:> i j))
		  (begin (array-set! m i j (lu-upper-eqn i j m))
			 (iloop (fix:+ i 1)))))
	    (let iloop ((i (fix:+ j 1))) ;compute elements below diagonal
	      (if (fix:< i n)
		  (begin (array-set! m i j (lu-lower-eqn i j m))
			 (iloop (fix:+ i 1)))))
	    (let* ((pivot-info (lu-find-best-pivot m j n))
		   (pivot (car pivot-info))
		   (pivot-index (cdr pivot-info)))
	      (if (bad-pivot? pivot)
		  (singular-matrix (lambda () (jloop (fix:+ j 1))))
		  (let ((inverted-pivot (invert pivot)))
		    (lu-row-swap m j pivot-index perms)
		    (if (not (fix:= j pivot-index))
			(set! sign (fix:- 0 sign)))
		    (let iloop ((i (fix:+ j 1))) ;divide through by pivot
		      (if (fix:= i n)
			  'done
			  (begin (array-set! m i j
					     (* (array-ref m i j)
						inverted-pivot))
				 (iloop (fix:+ i 1)))))
		    (jloop (fix:+ j 1))))))
	  (succeed m perms sign)))))

(define tiny-pivot-bugger-factor
  ;; As a practical matter, the following theoretical factor need not
  ;;  be adjusted if conditions of STP do not precisely apply.
  (/ 1.0 6.0238e23))

(define (bad-pivot? pivot)
  (or (and (exact? pivot) (zero? pivot))
      (and (inexact? pivot)
	   (< (magnitude pivot) tiny-pivot-bugger-factor))))

;;; Note that we start looping at the jth row in searching for the
;;; next pivot.  We can make this somewhat better numerically by
;;; including scale factors for the columns, but i'm too lazy to do
;;; this now. -- HAL

(define (lu-find-best-pivot m j n)
  (let ((column (make-initialized-vector n (lambda (i) (array-ref m i j)))))
    (let iloop ((i (fix:+ j 1))
		(bestindex j)
		(bestpivot (vector-ref column j)))
      (if (fix:= i n)
	  (cons bestpivot bestindex)
	  (let* ((p (vector-ref column i)))
	    (if (better-pivot? p bestpivot)
		(iloop (fix:+ i 1) i p)
		(iloop (fix:+ i 1) bestindex bestpivot)))))))

(define (better-pivot? p1 p2)
  (> (magnitude p1) (magnitude p2)))


(define (lu-upper-eqn i j m)
  (if (fix:= i 0)
      (array-ref m i j)
      (let kloop ((k 0) (sum 0))
	(if (fix:= k i)
	    (- (array-ref m i j) sum)
	    (kloop (fix:+ k 1)
		   (+ sum (* (array-ref m i k) (array-ref m k j))))))))

(define (lu-lower-eqn i j m)
  (let kloop ((k 0) (sum 0))
    (if (fix:= k j)
	(- (array-ref m i j) sum)
	(kloop (fix:+ k 1)
	       (+ sum (* (array-ref m i k) (array-ref m k j)))))))

(define (lu-row-swap  m i1 i2 perms)
  (define (swap-elements vector i j)
    (let ((temp (vector-ref vector i)))
      (vector-set! vector i (vector-ref vector j))
      (vector-set! vector j temp)))
  (if (not (fix:= i1 i2))
      (begin (swap-elements perms i1 i2)
	     ;;uses fact that matrix is a vector of rows
	     (swap-elements m i1 i2))))

;;; Back substitution (see Press, page 32)

(define (lu-backsubstitute-internal m perm b)
  (let* ((n (vector-length b))
	 (top (fix:- n 1))
	 (y (make-vector n '()))
	 (x (make-vector n '())))
    (let fdloop ((i 0))
      (if (fix:< i n)
	  (begin
	    (vector-set! y i
	      (- (vector-ref b (vector-ref perm i))
		 (let jloop ((j 0) (sum 0))
		   (if (fix:= j i)
		       sum
		       (jloop (fix:+ j 1)
			      (+ sum
				 (* (vector-ref y j)
				    (array-ref m i j))))))))
	      (fdloop (fix:+ i 1)))))
    (let bkloop ((i top))
      (if (not (fix:< i 0))
	  (begin
	    (vector-set! x i
	      (/ (- (vector-ref y i)
		    (let jloop ((j (fix:+ i 1)) (sum 0))
		      (if (fix:= j n)
			  sum
			  (jloop (fix:+ j 1)
				 (+ sum
				    (* (vector-ref x j)
				       (array-ref m i j)))))))
		 (array-ref m i i)))
	    (bkloop (fix:- i 1)))))
    x))

;;; In the case of a homogeneous system we can solve if the matrix is 
;;;  singular.  

(define (lu-null-space A)
  (let* ((n (m:dimension A))
	 (AA (matrix->array A))
	 (maxel (vector-accumulate 
		 max
		 (lambda (row)
		   (vector-accumulate max g:magnitude :zero row))
		 :zero
		 AA)))
    (lu-decompose-internal AA
      (lambda (LU permutation sign)
	(let lp ((i 0))
	  (if (fix:= i n)
	      '()
	      (let ((v (lu-null-vector-internal LU i maxel)))
		(cond ((heuristic-zero-vector? v maxel) '())
		      ((heuristic-zero-vector? (matrix*vector A v) maxel)
		       (cons (v:make-unit v) (lp (fix:+ i 1))))
		      (else (lp (fix:+ i 1))))))))
      allow-zero-pivot)))


;;; For each non-negative integer index less than the nullity of the
;;; lu-matrix we can obtain an independent solution vector (the entire
;;; set spans the null space).
;;; In this case, the upper part of the matrix contains the U part of
;;; the LU decomposition.  The L part is irrelevant to this process
;;; since we are solving a homogeneous equation.  The algorithm is to
;;; work backwards, from n-1 to 0, solving the equations m[i,i]x[i] +
;;; m[i,i+1]x[i+1] + ... = 0.
;;; If m[i,i] is zero (to within the tolerance), then we can set x[i]
;;; to an arbitrary value.  We set the kth arbitary unknown to 1 and
;;; the rest to 0 (0<=k<nullity).  If a luser supplies a k>=nullity,
;;; he will get a zero vector.

(define (lu-null-vector-internal m k maxel)
  (let* ((n (num-rows m))
	 (top (fix:- n 1))
	 (x (make-vector n '())))
    (let bkloop ((i top) (acount 0))
      (if (not (fix:< i 0))
	  (let ((p (array-ref m i i)))
	    (if (heuristically-zero? p maxel)
                (begin
                  (if (fix:= acount k)
		      (vector-set! x i 1)
                      (vector-set! x i 0))
	          (bkloop (fix:- i 1) (fix:+ acount 1)))
		(let ((s (let jloop ((j (fix:+ i 1)) (sum 0))
			   (if (fix:= j n)
			       sum
			       (jloop (fix:+ j 1)
				      (+ sum
					 (* (vector-ref x j)
					    (array-ref m i j))))))))
		  (vector-set! x i (/ (- s) p))
		  (bkloop (fix:- i 1) acount))))))
    x))

(define heuristic-zero-test-bugger-factor
  ;; The following default number was assigned by HAL. -- GJS & MH.
  (* 1000 *machine-epsilon*))

(define (heuristically-zero? z m)
  (< (magnitude z) (* heuristic-zero-test-bugger-factor (+ m 1))))

(define (heuristic-zero-vector? v m)
  (for-all? (vector->list v) (lambda (x) (heuristically-zero? x m))))

;;; This is stuff to test the LU-decomposition

(define (hilbert n)
  (m:generate n n
    (lambda (i j) (/ 1.0 (+ i j 2)))))

(define (upper-matrix m)
  (let ((n (m:num-rows m)))
    (m:generate n n
      (lambda (i j)
	(if (fix:> i j)
	    0
	    (matrix-ref m i j))))))

(define (lower-matrix m)
  (let ((n (m:num-rows m)))
    (m:generate n n
      (lambda (i j)
	(cond ((fix:< i j) 0)
	      ((fix:= i j) 1)
	      (else (matrix-ref m i j)))))))


(define (lower*upper m)
  (matrix*matrix (lower-matrix m)
		 (upper-matrix m)))

(define (checklu M)
  (lu-decompose M
		(lambda (ans-mat ans-perm sign)
		  (let ((prod (lower*upper ans-mat)))
		    (let ((n (m:num-rows M)))
		      (m:generate n n
				  (lambda (i j)
				    (- (matrix-ref prod i j)
				       (matrix-ref M
						   (vector-ref ans-perm i)
						   j)))))))
		allow-zero-pivot))
