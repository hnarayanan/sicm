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

;;;; Linear System Solver -- SVD -- Singular-Value Decomposition

;;; This file contains the following definitions:
;;;
;;; (svd-solve-linear-system A-matrix b-vector) => x-vector
;;; (svd-matrix-inverse A b succeed fail)
;;;
;;; (svd A continue)
;;;      where continue = (lambda (U SIGMA V W)
;;;                          ;;A = U * SIGMA * (transpose V)
;;;                          ...)
;;; After execution the first n columns of U will contain 
;;; left singular vectors, W be a vector of singular values,
;;; and V will contain right singular vectors.  SIGMA will be
;;; a square matrix with the singular values on the diagonal.

;;; More precisely: 

;;; Columns, j, of U whose wj are nonzero form an orthonormal basis
;;;  for the range of A.

;;; Columns, j, of V whose qj are zero form an orthonormal basis for
;;;  the null space of A.
;;;    Note: These are solutions of the homogeneous equation A*x = 0

(define (svd a continue)
  (svd-internal (matrix->array a)
    (lambda (u sigma v w)
      (continue (array->matrix u)
		(array->matrix sigma)
		(array->matrix v)
		w))))

(define (svd-least-squares a b #!optional eps)
  (if (default-object? eps) (set! eps 1e-15))
  (svd a
       (lambda (u sigma v w)
	 (let ((inverted-w
		(let ((wmin
		       (cond ((number? eps)
			      (* eps (apply max (vector->list w))))
			     ((procedure? eps)
			      (* (eps w) (apply max (vector->list w))))
			     (else
			      (error "Bad cutoff -- SVD" eps)))))
		  (make-initialized-vector (vector-length w)
		    (lambda (i) 
		      (let ((wi (vector-ref w i)))
			(if (< wi wmin) 0 (/ 1 wi))))))))
	   (matrix*vector v
			  ((vector-elementwise *)
			   (matrix*vector (m:transpose u) b)
			   inverted-w))))))

(define svd-solve-linear-system svd-least-squares)


(define (svd-invert a #!optional eps)
  (if (default-object? eps) (set! eps 1e-15))
  (svd a
       (lambda (u sigma v w)
	 (let ((inverted-w
		(let ((wmin
		       (cond ((number? eps)
			      (* eps (apply max (vector->list w))))
			     ((procedure? eps)
			      (* (eps w) (apply max (vector->list w))))
			     (else
			      (error "Bad cutoff -- SVD" eps)))))
		  (make-initialized-vector (vector-length w)
		    (lambda (i) 
		      (let ((wi (vector-ref w i)))
			(if (< wi wmin) 0 (/ 1 wi))))))))
	   (matrix*matrix v
			  (matrix*matrix (m:make-diagonal inverted-w)
					 (m:transpose u)))))))

;;;              Singular-Value Decomposition

;;;    Blind, ugly translation of Fortran SVD algorithm given in
;;; Forsythe, Malcolm, and Moler, "Computer Methods For Mathematical
;;; Computations", Chapter 9, Section 9.5, p.227.

;;;    Obtained from RZ in Common Lisp 
;;;     and Transliterated to Scheme by GJS.

;;; Given m by n matrix A, compute the Singular Value Decomposition,
;;;     A = U * SIGMA * (transpose V).  A is not modified.  
;;; After execution the first n columns of *SVD.U* will contain 
;;; left singular vectors, *SVD.W* will contain singular values, and
;;; *SVD.V* will contain right singular vectors.

(define (svd-internal a continue #!optional matu matv)
  (if (default-object? matu) (set! matu true))
  (if (default-object? matv) (set! matv true))
  ;; continue = (lambda (u sigma v w) ...)
  (let* ((m (num-rows a)) (n (num-cols a))
	 ;;(nm (max n m))
	 (ierr 0)
	 (g) (scale) (anorm)
	 (l) (f) (h) (s)
	 (its) (flag1) (flag2) (l1) (c) (flag3) (k1)
	 (convergence)
	 (x) (y) (z)
	 (i)

	 (*svd.a* (array-copy a))
	 (*svd.u* (array-copy a))
	 (*svd.v* (generate-array n n (lambda (i j) 0)))
	 (*svd.w* (make-vector n 0))
	 (*svd.rv1* (make-vector n 0)))

    (set! g 0.0)
    (set! scale 0.0)
    (set! anorm 0.0)

    ;; Perform Householder bidiagonalization

    (do-up 0 n			;DO 300 I=1,N
      (lambda (i)
	(set! l (fix:+ i 1))
	;; Perform column Householder
	;; (write-line "column Householder")
	(vector-set! *svd.rv1* i (* scale g))
	(set! g 0.0)
	(set! s 0.0)
	(set! scale 0.0)
	(if (not (fix:> i (fix:- m 1)))
	    (begin
	     (do-up i m
	       (lambda (k)
		 (set! scale
		       (+ scale
			  (magnitude (array-ref *svd.u* k i))))))
	     (if (not (zero? scale))
		 (begin
		  (do-up i m
		    (lambda (k)
		      (array-set! *svd.u* k i
				   (/ (array-ref *svd.u* k i)
				      scale))
		      (set! s (+ s (square (array-ref *svd.u* k i))))))
		  (set! f (array-ref *svd.u* i i))  
		  (set! g (if (negative? f) (sqrt s) (- (sqrt s))))
		  (set! h (- (* f g) s))
		  (array-set! *svd.u* i i (- f g))
		  (if (not (fix:= i (fix:- n 1)))
		      (do-up l n
			(lambda (j)
			  (set! s 0.0)
			  (do-up i m
			    (lambda (k)
			      (set! s
				    (+ s
				       (* (array-ref *svd.u* k i)
					  (array-ref *svd.u* k j))))))
			  (set! f (/ s h))
			  (do-up i m
			    (lambda (k)
			      (array-set! *svd.u* k j
					   (+ (array-ref *svd.u* k j)
					      (* f (array-ref *svd.u* k i)))))))))
		  (do-up i m
		    (lambda (k)
		      (array-set! *svd.u* k i (* scale (array-ref *svd.u* k i)))))))))
	(vector-set! *svd.w* i (* scale g))

	;; Perform row Householder
	;; (write-line "row Householder")

	(set! g 0.0)			;S210 + 1
	(set! s 0.0)
	(set! scale 0.0)

	(if (not (or (fix:> i (fix:- m 1)) (fix:= i (fix:- n 1))))
	    (begin
	     (do-up l n
	       (lambda (k)
		 (set! scale (+ scale (magnitude (array-ref *svd.u* i k))))))

	     (if (not (zero? scale))
		 (begin
		  (do-up l n
		    (lambda (k)
		      (array-set! *svd.u* i k
				   (/ (array-ref *svd.u* i k)
				      scale))
		      (set! s (+ s (square (array-ref *svd.u* i k))))))
		  (set! f (array-ref *svd.u* i l)) 
		  (set! g (if (negative? f) (sqrt s) (- (sqrt s))))
		  (set! h (- (* f g) s)) 
		  (array-set! *svd.u* i l (- f g))
		  (do-up l n
		    (lambda (k)
		      (vector-set! *svd.rv1* k (/ (array-ref *svd.u* i k) h))))

		  (if (not (fix:= i (fix:- m 1)))
		      (do-up l m
		        (lambda (j)
			  (set! s 0.0)
			  (do-up l n
			    (lambda (k)
			      (set! s
				    (+ s
				       (* (array-ref *svd.u* j k)
					  (array-ref *svd.u* i k))))))
			  (do-up l n
			    (lambda (k)
			      (array-set! *svd.u* j k
					   (+ (array-ref *svd.u* j k)
					      (* s (vector-ref *svd.rv1* k)))))))))
		  (do-up l n
		    (lambda (k)
		      (array-set! *svd.u* i k (* scale (array-ref *svd.u* i k)))))))))

	(set! anorm
	      (max anorm
		   (+ (magnitude (vector-ref *svd.w* i))
		      (magnitude (vector-ref *svd.rv1* i)))))))
    ;;S300

    ;; Accumulation of right-hand transformations

    (if matv
	(do-down (fix:- n 1) -1
	  (lambda (i)
	    (if (not (fix:= i (fix:- n 1)))
		(begin
		 (if (not (= g 0.0))
		     (begin
		      (do-up l n
			(lambda (j)
			  (array-set! *svd.v* j i
				       (/ (/ (array-ref *svd.u* i j)
					     (array-ref *svd.u* i l))
					  g))))
		      (do-up l n
			(lambda (j)
			  (set! s 0.0)
			  (do-up l n
			    (lambda (k)
			      (set! s (+ s (* (array-ref *svd.u* i k)
					      (array-ref *svd.v* k j))))))
			  (do-up l n
			    (lambda (k)
			      (array-set! *svd.v* k j
					   (+ (array-ref *svd.v* k j)
					     (* s (array-ref *svd.v* k i))))))))))
		    (do-up l n
		      (lambda (j)
			(array-set! *svd.v* i j 0.0)
			(array-set! *svd.v* j i 0.0)))))
	    (array-set! *svd.v* i i 1.0)
	    (set! g (vector-ref *svd.rv1* i))
	    (set! l i))))

    ;; Accumulation of left-hand transformations

    (if matu
      (do-down (fix:- (if (fix:< m n) m n) 1) -1
	(lambda (i)
	  (set! l (fix:+ i 1))
	  (set! g (vector-ref *svd.w* i))
	  (if (not (fix:= i (fix:- n 1)))
	      (do-up l n
		(lambda (j)
		  (array-set! *svd.u* i j 0.0))))
	  (if (not (zero? g))
	      (begin
	       (if (not (fix:= i (fix:- (if (fix:< m n) m n) 1)))
		   (do-up l n
		     (lambda (j)
		       (set! s 0.0)
		       (do-up l m
			 (lambda (k)
			   (set! s (+ s (* (array-ref *svd.u* k i)
					   (array-ref *svd.u* k j))))))
		       (set! f (/ (/ s (array-ref *svd.u* i i)) g))
		       (do-up i m
			 (lambda (k)
			   (array-set! *svd.u* k j
					(+ (array-ref *svd.u* k j)
					   (* f (array-ref *svd.u* k i)))))))))
	       (do-up i m
		 (lambda (j)
		   (array-set! *svd.u* j i (/ (array-ref *svd.u* j i) g)))))
	    (do-up i m
	      (lambda (j)
		(array-set! *svd.u* j i 0.0))))
	  (array-set! *svd.u* i i (+ (array-ref *svd.u* i i) 1.0)))))

    ;; Bidiagonalization complete
    ;; (write-line "Bidiagonal form (diagonal followed by superdiagonal):")
    ;; (write-line *svd.w*) 
    ;; (write-line *svd.rv1*)


    ;; Diagonalization of bidiagonal form
    
    (do-down (- n 1) -1
      (lambda (k)
	(set! k1 (fix:- k 1))
	(set! its 0)
	(set! flag1 false)
	(set! flag2 false)
      
	(set! convergence false)
	(let lp ()
	  (if (not convergence)
	      (begin
	       (set! flag1 false)
	       (set! flag2 false)
	       (let ll-lp ((ll k))
		 (if (not (or (fix:< l 0) flag1 flag2))
		     (begin
		      (set! l ll)
		      (set! l1 (fix:- l 1))
		      (set! flag1 (= (+ (magnitude (vector-ref *svd.rv1* l)) anorm) anorm))
		      (if (not flag1)
			  (set! flag2
				(= (+ (magnitude (vector-ref *svd.w* l1))
				      anorm)
				   anorm)))
		      (ll-lp (fix:- ll 1)))))
	       (if (not flag1)
		   (begin
		    (set! c 0.0)
		    (set! s 1.0)
		    (set! flag3 false)
		    (let i-lp ((i l))
			 (if (not (or (fix:> i k) flag3))
			     (begin 
			      (set! f (* s (vector-ref *svd.rv1* i)))
			      (vector-set! *svd.rv1* i (* c (vector-ref *svd.rv1* i)))
			      (set! flag3 (= (+ (magnitude f) anorm) anorm))
			      (if (not flag3)
				  (begin
				   (set! g (vector-ref *svd.w* i))
				   (set! h (sqrt (+ (* f f) (* g g))))
				   (vector-set! *svd.w* i h)
				   (set! c (/ g h))
				   (set! s (/ (- f) h))
				   (if matu
				       (do ((j 0 (fix:+ j 1)))
					   ((fix:= j m))
					   (set! y (array-ref *svd.u* j l1))
					   (set! z (array-ref *svd.u* j i))
					   (array-set! *svd.u* j l1 (+ (* y c) (* z s)))
					   (array-set! *svd.u* j i (+ (* (- y) s) (* z c)))))))
			      (i-lp (fix:+ i 1)))))))

	       ;; test for convergence

	       (set! z (vector-ref *svd.w* k))
	       (cond ((fix:= l k)
		      (set! convergence true))
		     (else
		      (if (fix:= its 30)
			  (error "SVD: No convergence after 30 iterations."))
		      (set! its (1+ its))
		      (set! x (vector-ref *svd.w* l))
		      (set! y (vector-ref *svd.w* k1))
		      (set! g (vector-ref *svd.rv1* k1))
		      (set! h (vector-ref *svd.rv1* k))
		      (set! f (/ (+ (* (- y z) (+ y z)) (* (- g h) (+ g h)))
				 (* 2.0 h y)))
		      (set! g (sqrt (+ (square f) 1.0)))
		      (set! f (/ (+ (* (- x z) (+  x z))
				    (* h
				       (- 
					(/ y (+ f (if (negative? f) (- g) g)))
					h)))
				 x))
		      (set! c 1.0)
		      (set! s 1.0)
		      (do-up l (fix:+ k1 1)
			(lambda (i1)
			  (set! i (fix:+ i1 1))
			  (set! g (vector-ref *svd.rv1* i))
			  (set! y (vector-ref *svd.w* i))
			  (set! h (* s g))
			  (set! g (* c g))
			  (set! z (sqrt (+ (square f) (square h))))
			  (vector-set! *svd.rv1* i1 z)
			  (set! c (/ f z))
			  (set! s (/ h z))
			  (set! f (+ (* x c) (* g s)))
			  (set! g (+ (* (- x) s) (* g c)))
			  (set! h (* y s))
			  (set! y (* y c))
			  (if matv
			      (do-up 0 n
				(lambda (j)
				  (set! x (array-ref *svd.v* j i1))
				  (set! z (array-ref *svd.v* j i))
				  (array-set! *svd.v* j i1 (+ (* x c) (* z s)))
				  (array-set! *svd.v* j i (+ (* (- x) s) (* z c))))))
			  (set! z (sqrt (+ (square f) (square h))))
			  (vector-set! *svd.w* i1 z)
			  (if (not (zero? z))
			      (begin (set! c (/ f z))
				     (set! s (/ h z))))
			  (set! f (+ (* c g) (* s y)))
			  (set! x (+ (* (- s) g) (* c y)))
			  (if matu
			      (do-up 0 m
				(lambda (j)
				  (set! y (array-ref *svd.u* j i1))
				  (set! z (array-ref *svd.u* j i))
				  (array-set! *svd.u* j i1 (+ (* y c) (* z s)))
				  (array-set! *svd.u* j i (+ (* (- y) s) (* z c))))))))
	  
		      (vector-set! *svd.rv1* l 0.0)
		      (vector-set! *svd.rv1* k f)
		      (vector-set! *svd.w* k x)
		      (set! convergence false)))
	       (lp))))

	;; convergence
	(if (< z 0.0)
	    (begin (vector-set! *svd.w* k (- z))
		   (if matv
		       (do-up 0 n
			      (lambda (j)
				(array-set! *svd.v* j k (- (array-ref *svd.v* j k))))))))))

    ;; Set *svd.a* to be the matrix of singular values
    
    (set! *svd.a* (generate-array n n (lambda (i j) 0)))
    (do-up 0 n
	   (lambda (i)
	     (array-set! *svd.a* i i (vector-ref *svd.w* i))))

    (continue *svd.u* *svd.a* *svd.v* *svd.w*)))

#|;;; Test case from book.

(define a 
  #( #( 1  6 11 )
     #( 2  7 12 )
     #( 3  8 13 )
     #( 4  9 14 )
     #( 5 10 15 ) ))

(pp (svd a list))
(#(#(-.35455705703768087 -.6886866437682524 .5623498172874758)
   #(-.39869636999883223 -.3755545293958711 -.6404098216452332)
   #(-.4428356829599836 -.06242241502349071 4.3232194451219827e-4)
   #(-.48697499592113497 .2507096993488898 -.32903444810323323)
   #(-.5311143088822865 .5638418137212697 .40666213051647754))
 #(#(35.127223333574676 0 0)
   #(0 2.4653966969165197 0)
   #(0 0 2.839772892390358e-15))
 #(#(-.201664911192694 .8903171327830188 .4082482904638638)
   #(-.5168305013923045 .2573316268240516 -.816496580927726)
   #(-.8319960915919149 -.3756538791349179 .40824829046386263))
 #(35.127223333574676 2.4653966969165197 2.839772892390358e-15))
|#
#|
;;; From Golub and Reinsch in Wilkinson&Reinsch Handbook for Automatic
;;; Computation -- Linear Algebra Vol II, p.150.

(define a
  (m:generate 20 21
	      (lambda (i j)
		(cond ((> i j)  0.0)
		      ((= i j)  (- 20.0 i))
		      ((< i j) -1.0)))))

(pp (cadddr (svd a list)))
#(20.4939015319192   19.493588689617948 3.5665073441521055e-27
  18.49324200890693  17.492855684535876 16.492422502470617
  15.491933384829649 14.491376746189422 13.49073756323204
  12.489995996796784 11.489125293076066 10.48808848170151
  9.486832980505142  8.48528137423857   7.483314773547884
  3.4641016151377517 6.480740698407856  4.47213595499958
  5.477225575051661  2.4494897427831783 1.414213562373095)


;;; Singular values should be:

(define (sig j)
  (let ((k (- 21 j)))
    (sqrt (* k (+ k 1)))))

(sig 20)
;Value: 1.4142135623730951

(sig 1)
;Value: 20.493901531919196

(sig 2)
;Value: 19.493588689617926

(sig 3)
;Value: 18.49324200890693

(sig 4)
;Value: 17.4928556845359

(sig 20)
;Value: 1.4142135623730951

(sig 21)
;Value: 0



;;; From Golub and Reinsch in Wilkinson&Reinsch Handbook for Automatic
;;; Computation -- Linear Algebra Vol II, p.150.

(define a
  (m:generate 30 30
	      (lambda (i j)
		(cond ((> i j)  0.0)
		      ((= i j)  1.0)
		      ((< i j) -1.0)))))

(pp (cadddr (svd a list)))
#(18.202905557529274 6.2231965226042325    3.9134802033356157 2.976794502557797
  2.4904506296603617 2.7939677151340594e-9 2.2032075744799298 2.0191836540545935
  1.8943415476856935 1.8059191266123142    1.7411357677479578 1.6923565443952688
  1.654793027369345  1.6253208928779395    1.6018333566662764 1.5828695887137096
  1.5673921444800174 1.55464889010938      1.5440847140760587 1.5352835655449113
  1.5279295121603145 1.5217800390635037    1.5166474128367937 1.5123854738997016
  1.508880156801892  1.5060426207239777    1.5038042438126578 1.5002314347754444
  1.5021129767540111 1.5009307119770665)

|#



#|  ;;; Testing

(define (matnorm a)
  (apply max
	 (map abs
	      (apply append 
		     (map vector->list 
			  (vector->list (matrix->array a)))))))

(define (test n #!optional m)
  (if (default-object? m)
      (set! m 100))
  (let ((h (lu-hilbert n)))
    (write-line `(lu ,(matnorm
		       (matrix-matrix (matrix*matrix h (lu-invert h))
				      (m:make-identity n)))))
    (svd h
	 (lambda (u sigma v w)
	   (let elp ((eps 1e-10) (m m))
	     (if (fix:= m 0)
		 'done
		 (let ((inverted-w
			(let ((wmin (* eps (apply max (vector->list w)))))
			  (make-initialized-vector (vector-length w)
			    (lambda (i) 
			      (let ((wi (vector-ref w i)))
				(if (< wi wmin) 0 (/ 1 wi))))))))
		   (let ((inv
			  (matrix*matrix v
					 (matrix*matrix (m:make-diagonal inverted-w)
							(m:transpose u)))))
		   
		     (write-line `(svd ,eps
				       ,(matnorm
					 (matrix-matrix (matrix*matrix h inv)
							(m:make-identity n))))))
			     
		   (elp (/ eps 3) (fix:- m 1)))))))))


;;; Before 13 LU is better than SVD, but SVD eventually wins.

(test 13)
(lu .90625)
(svd .0000000001 .6675129055220168)
(svd 3.3333333333333335e-11 .6000871658325195)
(svd 1.1111111111111111e-11 .6000871658325195)
(svd 3.703703703703703e-12 .6000871658325195)
(svd 1.2345679012345677e-12 .5423380881547928)
(svd 4.115226337448559e-13 .5423380881547928)
(svd 1.371742112482853e-13 .5423380881547928)
(svd 4.572473708276176e-14 .5423380881547928)
(svd 1.5241579027587254e-14 .4376716613769531)
(svd 5.0805263425290845e-15 .4376716613769531)
(svd 1.6935087808430282e-15 .4376716613769531)
(svd 5.64502926947676e-16 .4376716613769531)
(svd 1.88167642315892e-16 .4376716613769531)

(svd 6.272254743863067e-17 .348388671875)

(svd 2.0907515812876892e-17 .348388671875)
(svd 6.96917193762563e-18 .348388671875)
(svd 2.3230573125418768e-18 2.515625)
(svd 7.74352437513959e-19 2.515625)
(svd 2.5811747917131964e-19 2.515625)
(svd 8.603915972377322e-20 2.515625)
(svd 2.867971990792441e-20 2.515625)
(svd 9.559906635974802e-21 2.515625)
(svd 3.186635545324934e-21 2.515625)
(svd 1.0622118484416447e-21 2.515625)
(svd 3.540706161472149e-22 2.515625)
(svd 1.180235387157383e-22 2.515625)
;Quit!


(test 19)
(lu 4.875)
(svd .0000000001 .7776952391723171)
(svd 3.3333333333333335e-11 .7395966090261936)
(svd 1.1111111111111111e-11 .7395966090261936)
(svd 3.703703703703703e-12 .7395966090261936)
(svd 1.2345679012345677e-12 .6962781026959419)
(svd 4.115226337448559e-13 .6962781026959419)
(svd 1.371742112482853e-13 .6962781026959419)
(svd 4.572473708276176e-14 .6577432155609131)
(svd 1.5241579027587254e-14 .6577432155609131)
(svd 5.0805263425290845e-15 .6577432155609131)
(svd 1.6935087808430282e-15 .6577432155609131)
(svd 5.64502926947676e-16 .5989990234375)
(svd 1.88167642315892e-16 .5989990234375)

(svd 6.272254743863067e-17 .5989990234375)

(svd 2.0907515812876892e-17 .90625)
(svd 6.96917193762563e-18 .90625)
(svd 2.3230573125418768e-18 22.3125)
(svd 7.74352437513959e-19 22.3125)
;Quit!


(test 20)
(lu 34.)
(svd .0000000001 .752550782635808)
(svd 3.3333333333333335e-11 .752550782635808)
(svd 1.1111111111111111e-11 .752550782635808)
(svd 3.703703703703703e-12 .752550782635808)
(svd 1.2345679012345677e-12 .7140587568283081)
(svd 4.115226337448559e-13 .7140587568283081)
(svd 1.371742112482853e-13 .7140587568283081)
(svd 4.572473708276176e-14 .6803770065307617)
(svd 1.5241579027587254e-14 .6803770065307617)
(svd 5.0805263425290845e-15 .6803770065307617)
(svd 1.6935087808430282e-15 .643280029296875)
(svd 5.64502926947676e-16 .643280029296875)
(svd 1.88167642315892e-16 .643280029296875)
(svd 6.272254743863067e-17 .643280029296875)

(svd 2.0907515812876892e-17 .59033203125)

(svd 6.96917193762563e-18 .59033203125)
(svd 2.3230573125418768e-18 13.)
(svd 7.74352437513959e-19 24.)
(svd 2.5811747917131964e-19 24.)
;Quit!


(test 30)
(lu 12.25)
(svd .0000000001 .8594314045330975)
(svd 3.3333333333333335e-11 .8390090614557266)
(svd 1.1111111111111111e-11 .8390090614557266)
(svd 3.703703703703703e-12 .8144026100635529)
(svd 1.2345679012345677e-12 .8144026100635529)
(svd 4.115226337448559e-13 .8144026100635529)
(svd 1.371742112482853e-13 .7979546785354614)
(svd 4.572473708276176e-14 .7979546785354614)
(svd 1.5241579027587254e-14 .7979546785354614)
(svd 5.0805263425290845e-15 .7774620056152344)
(svd 1.6935087808430282e-15 .7774620056152344)
(svd 5.64502926947676e-16 .7774620056152344)

(svd 1.88167642315892e-16 .7529296875)

(svd 6.272254743863067e-17 .7529296875)
(svd 2.0907515812876892e-17 .7529296875)
(svd 6.96917193762563e-18 5.703125)
(svd 2.3230573125418768e-18 13.203125)
(svd 7.74352437513959e-19 31.8125)
(svd 2.5811747917131964e-19 34.3359375)
;Quit!


;;; Singular values for (hilbert 30) are:

#(1.6046959983598275     .3344360125333556      .05091302233254397
  6.612384688874973e-3   7.527825195740986e-4   7.593680264534132e-5
  6.834096698733383e-6   5.513422736482628e-7   4.000480673281162e-8
  2.6164842785734506e-9  1.5446121110479932e-10 8.235086631801633e-12
  3.964803127460767e-13  1.7223602006838723e-14 6.6921268878625e-16
  2.4447558436546055e-17 9.074602910397975e-18  5.985016835530655e-18
  5.7110319490636254e-18 1.1209774921001327e-17 7.86180746543282e-18
  7.113462016773155e-18  5.4562440222629994e-18 3.964051053484464e-18
  4.899374346302087e-18  3.7838677932677645e-18 3.1519489253291782e-18
  2.852971370421691e-18  1.9386313723000905e-18 9.527265084040377e-19)

|#

