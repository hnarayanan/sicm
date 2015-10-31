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

;;;;  Linear System Solver -- Full-Pivot

;;; 13 August 1990 -- Full pivoting linear-equation solver -- GJS & Jacob

(declare (usual-integrations))

;;; This file contains the following definitions:
;;;
;;;   (full-pivot-solve-linear-system a-matrix b-vector) => x-vector
;;;
;;;   (full-pivot-solve a-matrix b-vector succeed fail)
;;;      succeed = (lambda (x) (assert A*x=b) ...)
;;;      fail = (lambda (dismiss) ...)

;;; There is also a version that works on arrays, rather than matrices
;;;   (full-pivot-solve-internal A b succeed fail)

(define (full-pivot-solve-linear-system A b)
  (full-pivot-solve-internal (matrix->array A) b (lambda (x) x) barf-on-zero-pivot))

(define (full-pivot-solve A b succeed fail)
  (full-pivot-solve-internal (matrix->array A) b succeed fail))

(define (full-pivot-solve-internal A b succeed fail)
  ;; succeed = (lambda (x) ... (assert A*x=b))
  ;; fail    = (lambda (dismiss) ... )
  (let ((n (num-rows A)))
    (if (fix:< n 2)
	(let ((a00 (array-ref A 0 0)))
	  (if (< (magnitude a00) *minimum-allowable-full-pivot*)
	      (fail singular-matrix-error)
	      (succeed (vector (/ (vector-ref b 0) a00)))))
	(full-pivot-find A n
		    (lambda (p ip jp)	; pivot p is in row ip, column jp
		      (let ((nm1 (fix:- n 1))
			    (pivot-row (nth-row A ip)))
			(let ((scaled-pivot-row
			       (make-initialized-vector nm1
				 (lambda (k)
				   (if (fix:< k jp)
				       (/ (vector-ref pivot-row k) p)
				       (/ (vector-ref pivot-row (fix:+ k 1)) p)))))
			      (pivot-column
			       (make-initialized-vector nm1
				 (lambda (k)
				   (if (fix:< k ip)
				       (array-ref A k jp)
				       (array-ref A (fix:+ k 1) jp)))))
			      (bp (/ (vector-ref b ip) p)))
			  (full-pivot-solve-internal
			   (generate-array nm1 nm1
			     (lambda (i j)
			       (let ((c (* (vector-ref pivot-column i)
					   (vector-ref scaled-pivot-row j))))
				 (if (fix:< i ip)
				     (if (fix:< j jp)
					 (- (array-ref A i j) c)
					 (- (array-ref A i (fix:+ j 1)) c))
				     (if (fix:< j jp)
					 (- (array-ref A (fix:+ i 1) j) c)
					 (- (array-ref A (fix:+ i 1) (fix:+ j 1)) c))))))
			   (make-initialized-vector nm1
			     (lambda (i)
			       (let ((c (* bp (vector-ref pivot-column i))))
				 (if (fix:< i ip)
				     (- (vector-ref b i) c)
				     (- (vector-ref b (fix:+ i 1)) c)))))
			   ;;Continuation of full-pivot-solve-internal
			   (lambda (x)
			     (let ((xip 
				    (let lp ((k 0) (sum 0))
				      (if (fix:= k nm1)
					  (- bp sum)
					  (lp (fix:+ k 1)
					      (+ sum
						 (* (vector-ref x k)
						    (vector-ref scaled-pivot-row k))))))))
			       (succeed
				(make-initialized-vector n
				  (lambda (i)
				    (cond ((fix:< i jp) (vector-ref x i))
					  ((fix:= i jp) xip)
					  ((fix:> i jp) (vector-ref x (fix:- i 1)))))))))
			   fail))))
		    fail))))
		
(define (full-pivot-find A n found-pivot fail)
  ;; found  = (lambda (pivot ip jp) ... )
  ;; fail   = (lambda (dismiss) ... )
  (let row-loop ((i 0) (maxabs -1) (maxpiv -1) (imax -1) (jmax -1))
    (if (fix:= i n)
	(if (< maxabs *minimum-allowable-full-pivot*)
	    (fail singular-matrix-error)
	    (found-pivot maxpiv imax jmax))
	(let col-loop ((j 0) (maxabs maxabs) (maxpiv maxpiv) (imax imax) (jmax jmax))
	  (if (fix:= j n)
	      (row-loop (fix:+ i 1) maxabs maxpiv imax jmax)
	      (let* ((newel (array-ref A i j))
		     (newabs (magnitude newel)))
		(if (> newabs maxabs)
		    (col-loop (fix:+ j 1) newabs newel i j)
		    (col-loop (fix:+ j 1) maxabs maxpiv imax jmax))))))))

(define *minimum-allowable-full-pivot* 1.0e-30)


