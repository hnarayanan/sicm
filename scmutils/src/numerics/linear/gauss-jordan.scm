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

;;;;  Linear System Solver -- Gauss-Jordan

(declare (usual-integrations))

;;; This file contains the following definitions:
;;;
;;;   (gauss-jordan-solve-linear-system A b) => x-vector
;;;
;;;   (gauss-jordan-invert-and-solve A b succeed fail)
;;;

;;;   (destructive-gauss-jordan-solve-linear-system A b succeed fail)
;;;      succeed = (lambda (x C) (assert A*C=I, A*x=b) ...)
;;;      fail = (lambda (dismiss) ...)

;;; The destructive version works on arrays, not matrices.

;;; Solves an inhomogeneous system of linear equations, A*X=B, returning
;;; the vector X.

(define (gauss-jordan-solve-linear-system A b)
  (let ((A (array-copy (matrix->array A)))	;routine clobbers A and b
	(b (vector-copy b)))
    (destructive-gauss-jordan-solve-linear-system
	   A
	   b
	   (lambda (x Ainv) x)
	   barf-on-zero-pivot)))


(define (gauss-jordan-solve A b succeed fail)
  (let ((A (array-copy (matrix->array A)))	;routine clobbers A and b
	(b (vector-copy b)))
    (destructive-gauss-jordan-solve-linear-system
	   A
	   b
	   (lambda (x Ainv) (succeed x))
	   fail)))

(define (gauss-jordan-invert-and-solve A b succeed fail)
  ;; succeed = (lambda (x C) where x = C*b, C*A = I)
  ;; fail    = (lambda (dismiss) ... )
  (let ((A (array-copy (matrix->array A)))	;routine clobbers A and b
	(b (vector-copy b)))
    (destructive-gauss-jordan-solve-linear-system
	   A
	   b
	   succeed
	   fail)))

(define *minimum-allowable-gj-pivot* 1.0e-30)

;;; Transliterated from Press, Fortran version, p.28., with prejudice.
;;;  Replaces A by A^-1, and b by solution.

(define (destructive-gauss-jordan-solve-linear-system A b succeed fail)
  (let* ((n (num-rows A))
	 (ipiv (make-vector n false))	;not a legitimate index
	 (indxr (make-vector n 0))
	 (indxc (make-vector n 0)))
    (if (not (fix:= n (num-cols A)))
	(error "Non-square matrix -- gj-solve-linear-system" n))
    (if (not (fix:= n (vector-length b)))
	(error "Incompatible sizes --  gj-solve-linear-system" n))

    (let iloop ((i 0))			;runs over columns
      (if (fix:= i n)
	  'done
	  (let ((big *minimum-allowable-gj-pivot*)
		(irow 0) (icol 0) (pivinv 0))

	    ;; Find the position of the largest (in absolute value) element 
	    ;;  in the matrix that is not in a column from which we
	    ;;  have already picked a pivot.  

	    (let jloop ((j 0))	                        ;runs over rows
	      (if (fix:= j n)
		  'done
		  (begin
		    (if (not (vector-ref ipiv j))	;row is free
			(let kloop ((k 0))              ;runs over columns 
			  (if (fix:= k n)
			      'done
			      (begin
				(if (not (vector-ref ipiv k))
				    (let ((ajk (magnitude (array-ref a j k))))
				      (if (> ajk big)
					  (begin (set! big ajk)
						 (set! irow j)
						 (set! icol k)))))
				(kloop (fix:+ k 1))))))
		    (jloop (fix:+ j 1)))))
	    ;(bkpt "gug")
	    (if (= *minimum-allowable-gj-pivot* big) (fail singular-matrix-error))
	    (vector-set! ipiv icol true)
	    ;; Output of jloop (above) is summarized in IROW, ICOL, IPIV
	    ;;(bkpt "pivot found")

	    ;; Pivot element must be on diagonal.
	    ;; The following swaps two rows unless they are already 
	    ;;   the same row.  It will work if the = test is removed.
	    (if (not (fix:= irow icol))
		(begin
		  (let lloop ((l 0))
		      (if (fix:= l n)
			  'done
			  (let ((dum (array-ref a irow l)))
			    (array-set! a irow l
					(array-ref a icol l))
			    (array-set! a icol l dum)
			    (lloop (fix:+ l 1)))))
		  ;;more generally, b can be a matrix
		  ;;if so,replace this loop by one similar to the
		  ;;one above
		  (let ((dum (vector-ref b irow)))
		    (vector-set! b irow (vector-ref b icol))
		    (vector-set! b icol dum))))

	    ;; We remember that we did this swap in information in INDXR and INDXC
	    (vector-set! indxr i irow)
	    (vector-set! indxc i icol)

	    ;;(bkpt "after swap")
	    ;; Scale the icol row by 1/pivot, and set the diag element to 1/pivot.
	    (let ((aii (array-ref a icol icol)))
	      (set! pivinv (invert aii))
	      (array-set! a icol icol 1))
	    (let lloop ((l 0))
	      (if (fix:= l n)
		  'done
		  (begin (array-set! a icol l
				     (* (array-ref a icol l) pivinv))
			 (lloop (fix:+ l 1)))))
	    ;;more generally, as above....
	    (vector-set! b icol (* (vector-ref b icol) pivinv))

	    ;;for each row, except the pivot row, do row reduction by
	    ;;subtracting the appropriate multiple of the pivot row.
	    (let llloop ((ll 0))
	      (if (fix:= ll n)
		  'done
		  (begin
		    (if (not (fix:= ll icol))
			(let ((dum (array-ref a ll icol)))
			  (array-set! a ll icol 0)
			  (let lloop ((l 0))
			    (if (fix:= l n)
				'done
				(begin
				  (array-set! a ll l
					      (- (array-ref a ll l)
						 (* (array-ref a icol l)
						    dum)))
				  (lloop (fix:+ l 1)))))
			  (vector-set! b ll
				       (- (vector-ref b ll)
					  (* (vector-ref b icol)
					     dum)))))
		    (llloop (fix:+ ll 1)))))
	    (iloop (fix:+ i 1)))))
    ;;end of matrix reduction

    ;;interchange the columns of the matrix, according to the
    ;;permutation specified by INDEXR and INDEXC
    (let lloop ((l (fix:- n 1)))
      (if (fix:< l 0)
	  'done
	  (let ((kswap (vector-ref indxr l))
		(cswap (vector-ref indxc l)))
	    (if (not (fix:= kswap cswap))
		(let kloop ((k 0))
		  (if (fix:= k n)
		      'done
		      (let ((dum (array-ref a k kswap)))
			(array-set! a k kswap
				    (array-ref a k cswap))
			(array-set! a k cswap dum)
			(kloop (fix:+ k 1))))))
	    (lloop (fix:- l 1))))))
  (succeed b a))
