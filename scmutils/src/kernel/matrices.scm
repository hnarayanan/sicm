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

;;;;               Matrices

(declare (usual-integrations))

(define (m:type m) matrix-type-tag)
(define (m:type-predicate m) matrix-quantity?)

;;; The matrix type is a structural type, providing a means of
;;; combining other objects.  In this system an explicit matrix is a
;;; tagged array.  An array is represented as a scheme vector of rows,
;;; where each row is a scheme vector of elements.

#|
  (*matrix* (nrows . ncols) 
	    #( #( a11 a12 ...)
	       #( a21 a22 ...)
	       ...))
|#

(define (tag-matrix nrows ncols array)
  (list matrix-type-tag (cons nrows ncols) array))

(define (m:num-rows matrix)
  (caadr matrix))

(define (m:num-cols matrix)
  (cdadr matrix))

(define (matrix->array matrix)
  (caddr matrix))

(define (array->matrix array)
  (assert (and (vector? array) (vector-forall vector? array))
	  "Not an array -- ARRAY->MATRIX" array)
  (let ((nrows (num-rows array)) (ncols (num-cols array)))
    (assert
     (vector-forall (lambda (row) (fix:= (vector-length row) ncols))
		    array)
     "Not all rows have same length -- ARRAY->MATRIX" array)
    (tag-matrix nrows ncols array)))

(define (m:dimension mat)
  (assert (matrix? mat) "Not a matrix -- DIMENSION" mat)
  (let ((d (m:num-rows mat)))
    (assert (fix:= d (m:num-cols mat))
	    "Not a square matrix -- DIMENSION" mat)
    d))

(define (matrix-size mat)
  (assert (matrix? mat) "Not a matrix -- SIZE" mat)
  (fix:* (m:num-rows mat) (m:num-cols mat)))

;;; Single columns or rows are often important.

(define (column-matrix? m)
  (and (matrix? m)
       (fix:= (m:num-cols m) 1)))

(define (row-matrix? m)
  (and (matrix? m)
       (fix:= (m:num-rows m) 1)))

;;; Sometimes Scheme vectors need to be coerced to matrices

(define (vector->row-matrix v)
  (assert (vector? v))
  (tag-matrix 1 (vector-length v) (vector v)))

(define (vector->column-matrix v)
  (assert (vector? v))
  (define (vector->column-array v)
    (make-initialized-vector (vector-length v)
			     (lambda (i)
			       (vector (vector-ref v i)))))
  (tag-matrix (vector-length v) 1 (vector->column-array v)))

(define (row-matrix . args)
  (vector->row-matrix (apply vector args)))

(define (column-matrix . args)
  (vector->column-matrix (apply vector args)))


;;; We may need to extract a scheme vector from a matrix

(define (column-matrix->vector m)
  (assert (column-matrix? m))
  (nth-col (matrix->array m) 0))

(define (row-matrix->vector m)
  (assert (row-matrix? m))
  (nth-row (matrix->array m) 0))

(define (m:nth-row m n)
  (nth-row (matrix->array m) n))

(define (m:nth-col m n)
  (nth-col (matrix->array m) n))

(define (m:diagonal m)
  (let ((rows (m:dimension m)))
    (make-initialized-vector rows
      (lambda (i) (matrix-ref m i i)))))

(define (literal-matrix name nrows ncols)
  (m:generate nrows ncols
	      (lambda (i j)
		(string->symbol
		 (string-append (symbol->string name)
				"^"
				(number->string i)
				"_"
				(number->string j))))))

#|
(literal-matrix 'A 2 3)
#|
(matrix-by-rows (list A^0_0 A^0_1 A^0_2)
		(list A^1_0 A^1_1 A^1_2))
|#
|#

(define (literal-column-matrix name nrows)
  (m:generate nrows 1
	      (lambda (i j)
		(string->symbol
		 (string-append (symbol->string name)
				"^"
				(number->string i))))))

(define (literal-row-matrix name ncols)
  (m:generate 1 ncols
	      (lambda (i j)
		(string->symbol
		 (string-append (symbol->string name)
				"_"
				(number->string j))))))

;;; We need to be able to enter matrices easily, in a variety of
;;; ways. 

(define (up->column-matrix v)
  (assert (up? v))
  (vector->column-matrix (up->vector v)))

(define (column-matrix->up m)
  (assert (column-matrix? m))
  (vector->up (nth-col (matrix->array m) 0)))

(define (down->row-matrix v)
  (assert (down? v))
  (vector->row-matrix (down->vector v)))

(define (row-matrix->down m)
  (assert (row-matrix? m))
  (vector->down (nth-row (matrix->array m) 0)))


(define (matrix-by-rows . rows)
  (matrix-by-row-list rows))

(define (matrix-by-row-list rows)
  (assert (and (not (null? rows)) (list? (car rows))))
  (let ((nrows (length rows)))
    (let ((ncols (length (car rows))))
      (assert (for-all? (cdr rows)
		(lambda (row)
		  (and (list? row) (fix:= ncols (length row))))))
      (m:generate nrows ncols
		  (lambda (i j)
		    (list-ref (list-ref rows i) j))))))


(define (matrix-by-cols . cols)
  (matrix-by-col-list cols))

(define (matrix-by-col-list cols)
  (assert (and (not (null? cols)) (list (car cols))))
  (let ((ncols (length cols)))
    (let ((nrows (length (car cols))))
      (assert (for-all? (cdr cols)
		(lambda (col)
		  (and (list? col) (fix:= nrows (length col))))))
      (m:generate nrows ncols
		  (lambda (i j)
		    (list-ref (list-ref cols j) i))))))


;;; Sometimes we need to make a modified matrix

(define (matrix-with-substituted-row A i V)
  (tag-matrix (m:num-rows A) (m:num-cols A) 
    (vector-with-substituted-coord (matrix->array A) i V)))

(define (matrix-ref m i j)
  (vector-ref (vector-ref (matrix->array m) i) j))

(define m:ref matrix-ref)

(define (m:generate nrows ncols proc)
  (tag-matrix nrows ncols (generate-array nrows ncols proc)))

(define matrix:generate m:generate)


(define (m:transpose m)
  (m:generate (m:num-cols m) (m:num-rows m)
    (lambda (i j) (matrix-ref m j i))))

(define ((m:elementwise f) . matrices)
  (assert (and (not (null? matrices))
	       (for-all? matrices matrix?)))
  (let ((nrows (m:num-rows (car matrices)))
	(ncols (m:num-cols (car matrices))))
    (assert (for-all? (cdr matrices)
	      (lambda (m)
		(and (fix:= (m:num-rows m) nrows)
		     (fix:= (m:num-cols m) ncols)))))
    (m:generate nrows ncols
		(lambda (i j)
		  (g:apply f
			   (map (lambda (m)
				  (matrix-ref m i j))
				matrices))))))

(define matrix:elementwise m:elementwise)

;;; Submatrices are often used -- here we extract one

(define (m:submatrix A lowrow hirow+1 lowcol hicol+1)
  (m:generate (fix:- hirow+1 lowrow) (fix:- hicol+1 lowcol)
    (lambda (i j)
      (matrix-ref A (fix:+ i lowrow) (fix:+ j lowcol)))))


;;; A minor is a submatrix obtained from a given matrix
;;;   by dropping a given row and column.

(define (m:minor m i j)
  (m:generate (fix:- (m:num-rows m) 1)
	      (fix:- (m:num-cols m) 1)
    (lambda (a b)
      (matrix-ref m
		  (if (fix:< a i)
		      a
		      (fix:+ a 1))
		  (if (fix:< b j)
		      b
		      (fix:+ b 1))))))

(define (m:zero? matrix)
  (assert (matrix? matrix) "Not a matrix -- ZERO?" matrix)
  (let ((m (m:num-rows matrix))
	(n (m:num-cols matrix))
	(mat (matrix->array matrix)))
    (let rowlp ((i 0))
      (if (fix:= i m)
	  #t
	  (let collp ((j 0))
	    (if (fix:= j n)
		(rowlp (fix:+ i 1))
		(if (g:zero? (array-ref mat i j))
		    (collp (fix:+ j 1))
		    #f)))))))

(define (m:make-zero n #!optional m)
  (let ((m (if (default-object? m) n m)))
    (m:generate n m (lambda (i j) :zero))))

(define (m:zero-like m)
  (let ((z (g:zero-like (matrix-ref m 0 0))))
    (m:generate (m:num-rows m) (m:num-cols m)
		(lambda (i j) z))))


(define (m:make-identity n)
  (m:generate n n
    (lambda (i j)
      (if (fix:= i j) :one :zero))))

(define (m:identity? matrix)
  (assert (matrix? matrix)
	  "Not a matrix -- IDENTITY?" matrix)
  (let ((dim (m:num-rows matrix))
	(mat (matrix->array matrix)))
    (and (fix:= dim (m:num-cols matrix))
	 (let rowlp ((i 0))
	   (if (fix:= i dim)
	       #t
	       (let collp ((j 0))
		 (if (fix:= j dim)
		     (rowlp (fix:+ i 1))
		     (if (fix:= i j)
			 (if (g:one? (array-ref mat i j))
			     (collp (fix:+ j 1))
			     #f)
			 (if (g:zero? (array-ref mat i j))
			     (collp (fix:+ j 1))
			     #f)))))))))

(define (m:one-like m)
  (m:make-identity (m:dimension m)))

(define (m:identity-like m)
  (m:make-identity (m:dimension m)))

(define (m:make-diagonal diag)
  ;; From a scheme vector DIAG.
  (let ((n (vector-length diag)))
    (m:generate n n
      (lambda (i j)
	(if (fix:= i j)
	    (vector-ref diag i)
	    :zero)))))

(define (diagonal? matrix)
  (assert (matrix? matrix) "Not a matrix -- DIAGONAL?" matrix)
  (let ((dim (m:num-rows matrix))
	(mat (matrix->array matrix)))
    (and (fix:= dim (m:num-cols matrix))
	 (let rowlp ((i 0))
	   (if (fix:= i dim)
	       #t
	       (let collp ((j 0))
		 (if (fix:= j dim)
		     (rowlp (fix:+ i 1))
		     (if (fix:= i j)
			 (collp (fix:+ j 1))
			 (if (g:zero? (array-ref mat i j))
			     (collp (fix:+ j 1))
			     #f)))))))))


(define (matrix=matrix m1 m2)
  (assert (and (matrix? m1) (matrix? m2)))
  (and (fix:= (m:num-rows m1) (m:num-rows m2))
       (fix:= (m:num-cols m1) (m:num-cols m2))
       (vector-forall
	 (lambda (row1 row2)
	   (vector-forall g:= row1 row2))
	 (matrix->array m1) (matrix->array m2))))

(define (matrix-binary-componentwise binop matrix1 matrix2)
  (assert (and (matrix? matrix1) (matrix? matrix2))
	  "Not a matrix -- addition" (list binop matrix1 matrix2))
  (let ((nrows (m:num-rows matrix1))
	(ncols (m:num-cols matrix1))
	(m1 (matrix->array matrix1))
	(m2 (matrix->array matrix2)))
    (assert (and (fix:= nrows (m:num-rows matrix2))
		 (fix:= ncols (m:num-cols matrix2)))
	    "Matrices of unequal size -- addition"
	    (list binop matrix1 matrix2))
    (tag-matrix nrows ncols
      (make-initialized-vector nrows
        (lambda (i)
	  (let ((m1row (vector-ref m1 i))
	        (m2row (vector-ref m2 i)))
	    (make-initialized-vector ncols
	      (lambda (j)
	        (binop (vector-ref m1row j)
		       (vector-ref m2row j))))))))))

(define (matrix+matrix matrix1 matrix2)
  (matrix-binary-componentwise g:+ matrix1 matrix2))

(define (matrix-matrix matrix1 matrix2)
  (matrix-binary-componentwise g:- matrix1 matrix2))


(define (matrix*matrix matrix1 matrix2)
  (assert (and (matrix? matrix1) (matrix? matrix2))
	  "Not a matrix -- *" (list matrix1 matrix2))
  (let ((m1r (m:num-rows matrix1))
	(m1c (m:num-cols matrix1))
	(m2r (m:num-rows matrix2))
	(m2c (m:num-cols matrix2))
	(m1 (matrix->array matrix1))
	(m2 (matrix->array matrix2)))
    (assert (fix:= m1c m2r)
	    "Matrix sizes do not match -- MATRIX*MATRIX"
	    (list m1 m2))
    (let ((m1cm1 (fix:- m1c 1)))
      (m:generate m1r m2c
        (lambda (i j)
	  (let ((r1i (vector-ref m1 i)))
	    (g:sigma (lambda (k)
		       (g:* (vector-ref r1i k)
			    (array-ref m2 k j)))
		     0
		     m1cm1)))))))

(define (m:square a)
  (matrix*matrix a a))

(define (m:expt M n)
  (assert (matrix? M) "Not a matrix -- EXPT")
  (cond ((or (not (integer? n)) (inexact? n))
	 (error "Only integer powers allowed -- M:EXPT"))
	((fix:< n 0) 
	 (m:expt (m:invert M) (fix:- 0 n)))
	((fix:zero? n)
	 (m:make-identity (m:num-rows M)))
	(else
	 (let loop ((count n))
	   (cond ((fix:= count 1) M)
		 ((even? count) 
		  (let ((a (loop (fix:quotient count 2))))
		    (matrix*matrix a a)))
		 (else
		  (matrix*matrix M
				 (loop (fix:- count 1)))))))))


(define (matrix*scalar matrix k)
  (assert (and (matrix? matrix) (scalar? k))
	  "Not matrix*scalar" (list matrix k))
  (let ((m (matrix->array matrix)))
    (m:generate (m:num-rows matrix) (m:num-cols matrix)
      (lambda (i j) (g:* (array-ref m i j) k)))))

(define (scalar*matrix k matrix)
  (assert (and (matrix? matrix) (scalar? k))
	  "Not matrix*scalar" (list matrix k))
  (let ((m (matrix->array matrix)))
    (m:generate (m:num-rows matrix) (m:num-cols matrix)
      (lambda (i j) (g:* k (array-ref m i j))))))

(define (m:scale k)
  (lambda (m) (scalar*matrix k m)))

(define (m:outer-product v1 v2)
  (assert (and (column-matrix? v1) (row-matrix? v2)))
  (matrix*matrix v1 v2))

(define (m:inner-product v1 v2)
  (assert (and (row-matrix? v1) (column-matrix? v2)))
  (matrix-ref (matrix*matrix v1 v2) 0 0))

(define (matrix/matrix m1 m2)
  (matrix*matrix m1 (m:invert m2)))

;;; Cleaning up some useful hacks

(define (matrix*up m v)
  (column-matrix->up
   (matrix*matrix m
		  (up->column-matrix v))))

(define (down*matrix v m)
  (row-matrix->down
   (matrix*matrix (down->row-matrix v)
		  m)))

#| ;;; Unnecessary, since up tuples are vectors
(define (matrix*vector m v)
  (column-matrix->vector
   (matrix*matrix m
		  (vector->column-matrix v))))
|#
(define (matrix*vector m v) (matrix*up m v))

;;; Dangerous, should not be generic.

(define (vector*matrix v m)
  (row-matrix->vector
   (matrix*matrix (vector->row-matrix v)
		  m)))

(define (matrix/scalar m k)
  (matrix*scalar m (g:invert k)))

(define (scalar/matrix k m)
  (scalar*matrix k (m:invert m)))

(define (matrix=scalar m c)
  (matrix=matrix m
    (scalar*matrix c
		   (m:make-identity (m:num-rows m)))))

(define (scalar=matrix c m)
  (matrix=matrix (scalar*matrix c
		      (m:make-identity (m:num-rows m)))
       m))

(define (matrix+scalar m c)
  (matrix+matrix m
       (scalar*matrix c
		      (m:make-identity (m:num-rows m)))))

(define (scalar+matrix c m)
  (matrix+matrix (scalar*matrix c
		      (m:make-identity (m:num-rows m)))
       m))


(define (matrix-scalar m c)
  (matrix-matrix m
       (scalar*matrix c
		      (m:make-identity (m:num-rows m)))))

(define (scalar-matrix c m)
  (matrix-matrix (scalar*matrix c
		      (m:make-identity (m:num-rows m)))
       m))

(define (m:trace matrix)
  (assert (matrix? matrix) "Not a matrix -- TRACE")
  (let ((rows (m:num-rows matrix))
	(m (matrix->array matrix)))
    (assert (fix:= rows (m:num-cols matrix))
	    "Not a square matrix -- TRACE" matrix)
    (g:sigma (lambda (j) (array-ref m j j))
	     0
	     (fix:- rows 1))))

(define (m:conjugate mat)
  ((m:elementwise g:conjugate) mat))

(define (m:negate mat)
  ((m:elementwise g:negate) mat))

(define (m:dot-product-row r1 r2)
  (v:dot-product (row-matrix->vector r1)
		 (row-matrix->vector r2)))

(define (m:dot-product-column c1 c2)
  (v:dot-product (column-matrix->vector c1)
		 (column-matrix->vector c2)))


(define (m:cross-product-row r1 r2)
  (vector->row-matrix
   (v:cross-product (row-matrix->vector r1)
		    (row-matrix->vector r2))))

(define (m:cross-product-column c1 c2)
  (vector->column-matrix
   (v:cross-product (column-matrix->vector c1)
		    (column-matrix->vector c2))))


(define (m:exp mat)
  (series:value exp-series (list mat)))

(define (m:sin mat)
  (series:value sin-series (list mat)))

(define (m:cos mat)
  (series:value cos-series (list mat)))

;;; Kleanthes Konaris determinant routine, slightly edited by GJS
;;; -------------------------------------------------------------

;;; (iota 4) --> (0 1 2 3), as in APL.

(define (general-determinant add sub mul easy-zero?)
  (let ((zero (add)))
    (define (det m)
      (let ((cache '()))
	(define (c-det row active-column-list)
	  (if (null? (cdr active-column-list)) ;one active column
	      (matrix-ref m row (car active-column-list))
	      (let ((value
		     (assoc (list row active-column-list) cache)))
		(if value
		    (cadr value)	; cache hit!
		    (let loop		; cache miss!
			((index 0)	
			 (remaining-columns active-column-list)
			 (answer zero))
		      (if (null? remaining-columns)
			  (begin (set! cache
				       (cons (list (list row
							 active-column-list)
						   answer)
					     cache))
				 answer)
			  (let ((term
				 (matrix-ref m row (car remaining-columns))))
			    (if (easy-zero? term)
				(loop (fix:+ index 1)
				      (cdr remaining-columns)
				      answer)
				(let ((contrib
				       (mul term
					    (c-det (fix:+ row 1)
						   (delete-nth index
							       active-column-list)))))
				  (if (even? index)
				      (loop (fix:+ index 1)
					    (cdr remaining-columns)
					    (add answer contrib))
				      (loop (fix:+ index 1)
					    (cdr remaining-columns)
					    (sub answer contrib))))))))))))
	(c-det 0 (iota (m:dimension m)))))
    det))

;;;; Linear equations solved by Cramer's rule.  
;;;   Solves an inhomogeneous system of linear equations, A*X=B,
;;;    where the matrix A and the column matrix B are given.
;;;    It returns the column matrix X.
;;;  Unlike LU decomposition, Cramer's rule generalizes to symbolic solutions.

(define (Cramers-rule add sub mul div zero?)
  (let ((det (general-determinant add sub mul zero?)))
    (define solve
      (lambda (A B)
	(assert (and (matrix? A)
		     (column-matrix? B)
		     (fix:= (m:dimension A) (m:num-rows B))))
	(let ((bv (m:nth-col B 0))
	      (d (det A))
	      (At (m:transpose A)))
	  (vector->column-matrix
	   (make-initialized-vector (vector-length bv)
	     (lambda (i)
	       (div (det (matrix-with-substituted-row At i bv))
		    d)))))))
    solve))


;;; The following implements the classical adjoint formula for the
;;; inverse of a matrix.  This may be useful for symbolic applications.

(define (classical-adjoint-formula zero one add sub mul div zero?)
  (let ((det (general-determinant add sub mul zero?)))
    (define (matinv A)
      (let ((dim (m:dimension A)))
	(if (fix:= dim 1)
	    (m:generate 1 1
	      (lambda (i j) (div one (matrix-ref A 0 0))))
	    (let* ((d (det A)) (-d (sub zero d)))
	      (m:generate dim dim
		(lambda (j i)
		  (if (even? (+ i j))
		      (div (det (m:minor A i j)) d)
		      (div (det (m:minor A i j)) -d))))))))
    matinv))

(define (easy-zero? x)
  (cond ((number? x) (zero? x))
	;; Perhaps some form of easy simplification here?
	;;  e.g. substitution of numbers for literals, 
        ;;  and testing for zero result.
	(else #f)))

(define matinv-general
  (classical-adjoint-formula :zero :one g:+ g:- g:* g:/ easy-zero?))

(define solve-general
  (Cramers-rule g:+ g:- g:* g:/ easy-zero?))
    
(define determinant-general
  (general-determinant g:+ g:- g:* easy-zero?))


;;;LU decomposer, etc, must use correct arithmetic

(define matinv-numerical)
(define solve-numerical)
(define determinant-numerical)
    
(define numerical? #f)
    
(define (m:invert A)
  (if numerical? (matinv-numerical A) (matinv-general A)))
(define (m:solve A b)
  (if numerical? (solve-numerical A b) (solve-general A b)))
(define (m:determinant A)
  (if numerical? (determinant-numerical A) (determinant-general A)))

(define (m:rsolve b A)
  (cond ((up? b)
	 (column-matrix->up
	  (m:solve A (up->column-matrix b))))
	((column-matrix? b) 
	 (m:solve A b))
	((down? b)
	 (row-matrix->down
	  (m:transpose
	   (m:solve (m:transpose A)
		    (m:transpose (down->row-matrix b))))))
        ((row-matrix? b)
	 (m:transpose
	   (m:solve (m:transpose A)
		    (m:transpose b))))
	(else (error "I don't know how to solve:" b A))))

(define (m:solve-linear A b)
  (m:rsolve b a))

(define (set-numerical! #!optional matinv solve determinant)
  (set! numerical? #t)
  (if (not (default-object? matinv)) (set! matinv-numerical matinv))
  (if (not (default-object? solve)) (set! solve-numerical solve))
  (if (not (default-object? determinant)) (set! determinant-numerical determinant))
  'thank-you)

(define (set-symbolic!)
  (set! numerical? #f)
  'thank-you)

(define (m:apply matrix args)
  (m:generate (m:num-rows matrix) (m:num-cols matrix)
	      (lambda (i j)
		(g:apply (matrix-ref matrix i j)
			 args))))
    
(define (m:arity mat)
  (let ((n (m:num-rows mat)) (m (m:num-cols mat)))
    (let rowlp ((i 0) (a *at-least-zero*))
      (if (fix:= i n)	      
	  a
	  (let collp ((j 0) (a a))
	    (if (fix:= j m)
		(rowlp (fix:+ i 1) a)
		(let ((b
		       (joint-arity a
				    (g:arity (matrix-ref mat i j)))))
		  (if b
		      (collp (fix:+ j 1) b)
		      #f))))))))

(define (m:partial-derivative matrix varspecs)
  ((m:elementwise
    (lambda (f)
      (generic:partial-derivative f varspecs)))
   matrix))

(define (m:inexact? m)
  (vector-exists (lambda (v)
		   (vector-exists g:inexact? v))
		 (matrix->array m)))

(assign-operation 'type             m:type             matrix?)
(assign-operation 'type-predicate   m:type-predicate   matrix?)
(assign-operation 'arity            m:arity            matrix?)
(assign-operation 'inexact?         m:inexact?         matrix?)
						     
(assign-operation 'zero-like        m:zero-like        matrix?)
(assign-operation 'one-like         m:one-like         matrix?)
(assign-operation 'identity-like    m:identity-like    matrix?)
						     
(assign-operation 'zero?            m:zero?            matrix?)
(assign-operation 'identity?        m:identity?        matrix?)
						     
(assign-operation 'negate           m:negate           matrix?)
(assign-operation 'invert           m:invert    square-matrix?)
						     
(assign-operation 'conjugate        m:conjugate        matrix?)
(assign-operation 'exp              m:exp       square-matrix?)
(assign-operation 'sin              m:sin       square-matrix?)
(assign-operation 'cos              m:cos       square-matrix?)


(assign-operation '=   matrix=matrix           matrix? matrix?)
(assign-operation '=   matrix=scalar    square-matrix? scalar?)
(assign-operation '=   scalar=matrix    scalar? square-matrix?)
		     
(assign-operation '+   matrix+matrix           matrix? matrix?)
(assign-operation '+   matrix+scalar    square-matrix? scalar?)
(assign-operation '+   scalar+matrix    scalar? square-matrix?)
		     
(assign-operation '-   matrix-matrix           matrix? matrix?)
(assign-operation '-   matrix-scalar    square-matrix? scalar?)
(assign-operation '-   scalar-matrix    scalar? square-matrix?)
		     
(assign-operation '*   matrix*matrix           matrix? matrix?)
(assign-operation '*   matrix*scalar           matrix? scalar?)
(assign-operation '*   scalar*matrix           scalar? matrix?)

(assign-operation '*   down*matrix           down? matrix?)
(assign-operation '*   matrix*up           matrix? up?)
		     
(assign-operation '/   matrix/scalar    matrix? scalar?)
(assign-operation '/   scalar/matrix    scalar? square-matrix?)
(assign-operation '/   m:rsolve         column-matrix? square-matrix?)
(assign-operation '/   m:rsolve         up? square-matrix?)
(assign-operation '/   m:rsolve         down? square-matrix?)
(assign-operation '/   m:rsolve         row-matrix? square-matrix?)
(assign-operation '/   matrix/matrix    matrix? square-matrix?)

(assign-operation 'dot-product m:dot-product-row row-matrix? row-matrix?)
(assign-operation 'dot-product m:dot-product-column column-matrix? column-matrix?)

(assign-operation 'outer-product m:outer-product column-matrix? row-matrix?)


(assign-operation 'cross-product m:cross-product-row row-matrix? row-matrix?)
(assign-operation 'cross-product m:cross-product-column column-matrix? column-matrix?)

		       
(assign-operation 'expt  m:expt  square-matrix? exact-integer?)

(assign-operation 'partial-derivative
		         m:partial-derivative
                                                  matrix? any?)

(assign-operation 'apply m:apply                  matrix? any?)

(assign-operation 'determinant m:determinant square-matrix?)
(assign-operation 'trace       m:trace       square-matrix?)
(assign-operation 'transpose   m:transpose   matrix?)
(assign-operation 'dimension   m:dimension   square-matrix?)
(assign-operation 'dimension   m:num-rows    column-matrix?)
(assign-operation 'dimension   m:num-cols    row-matrix?)

(assign-operation 'solve-linear m:solve-linear square-matrix? column-matrix?)
(assign-operation 'solve-linear m:solve-linear square-matrix? up?)
(assign-operation 'solve-linear m:solve-linear square-matrix? row-matrix?)
(assign-operation 'solve-linear m:solve-linear square-matrix? down?)

;;; Abstract matrices generalize matrix quantities.

(define (abstract-matrix symbol)
  (make-literal abstract-matrix-type-tag symbol))

(define (am:arity v)
  ;; Default is matrix of numbers.
  (get-property v 'arity *at-least-zero*))

(define (am:zero-like m)
  (let ((z (abstract-matrix (list 'zero-like m))))
    (add-property! z 'zero #t)
    z))

(define (am:one-like m)
  (let ((z (abstract-matrix (list 'one-like m))))
    (add-property! z 'one #t)
    z))

(define (am:id-like m)
  (let ((z (abstract-matrix (list 'identity-like m))))
    (add-property! z 'one #t)
    z))

(define (make-matrix-combination operator #!optional reverse?)
  (if (default-object? reverse?)
      (lambda operands 
	(make-combination abstract-matrix-type-tag
			  operator operands))
      (lambda operands 
	(make-combination abstract-matrix-type-tag
			  operator (reverse operands)))))

(assign-operation 'type            m:type             abstract-matrix?)
(assign-operation 'type-predicate  m:type-predicate   abstract-matrix?)
(assign-operation 'arity           am:arity           abstract-matrix?)

(assign-operation 'inexact?  (has-property? 'inexact) abstract-matrix?)

(assign-operation 'zero-like       am:zero-like       abstract-matrix?)

(assign-operation 'zero?     (has-property? 'zero)    abstract-matrix?)
(assign-operation 'one?      (has-property? 'one)     abstract-matrix?)
(assign-operation 'identity? (has-property? 'one)     abstract-matrix?)

(assign-operation
    'negate     (make-matrix-combination 'negate)     abstract-matrix?)
(assign-operation
    'invert  (make-matrix-combination 'invert) square-abstract-matrix?)

(assign-operation
    'conjugate  (make-matrix-combination 'conjugate)  abstract-matrix?)
(assign-operation
    'exp        (make-matrix-combination 'exp) square-abstract-matrix?)
(assign-operation
    'sin        (make-matrix-combination 'sin) square-abstract-matrix?)
(assign-operation
    'cos        (make-matrix-combination 'cos) square-abstract-matrix?)

;(assign-operation
;  '=          matrix=matrix         abstract-matrix? abstract-matrix?)

(assign-operation
   '+ (make-matrix-combination '+)    abstract-matrix? abstract-matrix?)
(assign-operation
   '+ (make-matrix-combination '+)    matrix?          abstract-matrix?)
(assign-operation
   '+ (make-matrix-combination '+ 'r) abstract-matrix? matrix?)         
(assign-operation
   '+ (make-matrix-combination '+)    scalar?   square-abstract-matrix?)
(assign-operation
   '+ (make-matrix-combination '+ 'r) square-abstract-matrix? scalar?)         

(assign-operation
   '- (make-matrix-combination '-)    abstract-matrix? abstract-matrix?)
(assign-operation
   '- (make-matrix-combination '-)    matrix?          abstract-matrix?)
(assign-operation
   '- (make-matrix-combination '-)    abstract-matrix? matrix?)
(assign-operation
   '- (make-matrix-combination '-)    scalar?   square-abstract-matrix?)
(assign-operation
   '- (make-matrix-combination '-)    square-abstract-matrix? scalar?)         

(assign-operation
   '* (make-matrix-combination '*)    abstract-matrix? abstract-matrix?)
(assign-operation
   '* (make-matrix-combination '*)    matrix?          abstract-matrix?)
(assign-operation
   '* (make-matrix-combination '*)    abstract-matrix? matrix?)
(assign-operation
   '* (make-matrix-combination '*)    scalar?          abstract-matrix?)
(assign-operation
   '* (make-matrix-combination '* 'r) abstract-matrix? scalar?)         

(assign-operation
   '/ (make-matrix-combination '/)    abstract-matrix? square-abstract-matrix?)
(assign-operation
   '/ (make-matrix-combination '/)    matrix?          square-abstract-matrix?)
(assign-operation
   '/ (make-matrix-combination '/)    abstract-matrix? square-matrix?)
(assign-operation
   '/ (make-matrix-combination '/)    scalar?          square-abstract-matrix?)
(assign-operation
   '/ (make-matrix-combination '/)    abstract-matrix? scalar?)         
(assign-operation
   '/ (make-matrix-combination '/)    vector-quantity? square-abstract-matrix?)

(assign-operation
  'expt (make-matrix-combination 'expt) square-abstract-matrix? exact-integer?)

(assign-operation
  'partial-derivative
  (make-matrix-combination 'partial-derivative)
  abstract-matrix? any?)

;(assign-operation 'apply   m:apply                abstract-matrix? any?)
