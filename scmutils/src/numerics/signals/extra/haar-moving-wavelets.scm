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

;;; We start with a signal sig[nsamples].
;;; We make an array A[nscales+1, nsamples] of pairwise averages
;;;  and an array B[nscales, nsamples] of pairwise differences,
;;;  which are the wavelet coefficients.
;;; We put nsamples samples of the supplied signal sig(j)
;;;  in A(0, j), where 0 <= j <= nsamples.
;;;  We then expand this signal into the Haar moving-wavelets 
;;;  transform:

(define (haar-moving-wavelets sig nscales)
  (let* ((sqrt2 (sqrt 2.0))
	 (nsamples (vector-length sig))
	 (A (make-array (fix:+ nscales 1) nsamples 0.0))
         (B (make-array nscales nsamples 0.0)))
    (define (haar-average x y)
      (/ (+ x y) sqrt2))
    (define (haar-difference x y)
      (/ (- x y) sqrt2))
    (let init ((j 0))
      (if (fix:= j nsamples)
	  'done
	  (begin
	    (vector-set! (vector-ref A 0)
			 j
			 (vector-ref sig j))
	    (init (fix:+ j 1)))))
    (let next-scale ((i 1))
      (if (fix:> i nscales)
	  'done
	  (let* ((i-1    (fix:- i 1))
		 (stride (expt 2 i-1))
		 (Ai-1   (array-row A i-1))
		 (Ai     (array-row A i))
		 (Bi-1   (array-row B i-1))
		 (maxj   (fix:- nsamples (fix:- (expt 2 i) 1))))
	    (let next-sample ((j 0))
	      (if (fix:= j maxj)
		  (next-scale (fix:+ i 1))
		  (begin
		    (vector-set! Ai j
		      (haar-average (vector-ref Ai-1 j)
				    (vector-ref Ai-1 (fix:+ j stride))))
		    (vector-set! Bi-1 j
		      (haar-difference (vector-ref Ai-1 j)
				       (vector-ref Ai-1 (fix:+ j stride))))
		    (next-sample (fix:+ j 1))))))))
    B))

(define (num-rows m) (vector-length m))
(define (num-cols m) (vector-length (vector-ref m 0)))

(define (array-row A i)
  (vector-ref A i))

(define (make-array n m filler)
  (let ((ans (make-vector n)))
    (let loop ((i 0))
      (if (fix:= i n)
	  ans
	  (begin (vector-set! ans i
                              (make-vector m filler))
		 (loop (fix:+ i 1)))))))

#|
(define test-sig1
  #(8 4 1 3 2 5 7 9 3 1 4))

(pp (haar-moving-wavelets test-sig1 4))
#(#(2.82842712474619    2.1213203435596424  -1.414213562373095 .7071067811865475
    -2.1213203435596424 -1.414213562373095  -1.414213562373095 4.242640687119285
    1.414213562373095   -2.1213203435596424 0)
  #(3.9999999999999996 0. -1.5              -3.499999999999999
    -4.499999999999999 0. 5.999999999999999 3.499999999999999
    1.9999999999999998 0  0)
  #(-2.474873734152917 -4.949747468305832 -3.181980515339463 0.
    6.717514421272201  8.485281374238568  7.071067811865474  0
    0                  0                  0)
  #(9.749999999999998 8.499999999999996 7.7499999999999964 0 0 0 0 0 0 0 0))

(define test-sig2
  #(1 0 -3 2 1 0 1 2))

(pp (haar-moving-wavelets test-sig2 3))
#(#(.7071067811865475 2.1213203435596424 -3.5355339059327373 .7071067811865475
    .7071067811865475 -.7071067811865475 -.7071067811865475  0)
  #(.9999999999999999  -2.9999999999999996 -.9999999999999999 .9999999999999999
    -.9999999999999999 .49999999999999994  0                  0)
  #(-1.4142135623730947 -.3535533905932737 0. 1.4142135623730947 0 0 0 0))
|#

;;; More generally, we can supply wavelet filter coefficients

(define ((moving-wavelets smoothing-filter detail-filter) sig nscales)
  (let* ((nsamples (vector-length sig))
	 (nstrides (fix:- (vector-length smoothing-filter) 1))
	 (A (make-array (fix:+ nscales 1) nsamples 0.0))
         (B (make-array nscales nsamples 0.0)))
    (let init ((j 0))
      (if (fix:= j nsamples)
	  'done
	  (begin
	    (vector-set! (vector-ref A 0)
			 j
			 (vector-ref sig j))
	    (init (fix:+ j 1)))))
    (let next-scale ((i 1) (nvalid (fix:- nsamples nstrides)))
      (if (fix:> i nscales)
	  'done
	  (let* ((i-1    (fix:- i 1))
		 (stride (expt 2 i-1))
		 (Ai-1   (array-row A i-1))
		 (Ai     (array-row A i))
		 (Bi-1   (array-row B i-1)))
	    (let next-sample ((j 0))
	      (if (fix:< j nvalid)
		  (begin
		    (vector-set! Ai j
		      (lincomb smoothing-filter Ai-1 j stride))
		    (vector-set! Bi-1 j
		      (lincomb detail-filter Ai-1 j stride))
		    (next-sample (fix:+ j 1)))
		  (next-scale (fix:+ i 1)
			      (fix:- nvalid
				     (fix:* nstrides
					    (expt 2 i)))))))))
    B))

(define (lincomb coeffs data start stride)
  (let ((n (vector-length coeffs)))
    (let lp ((i 0) (ans 0.0))
      (if (fix:= i n)
	  ans
	  (lp (fix:+ i 1)
	      (+ (* (vector-ref coeffs i)
		    (vector-ref data (fix:+ start (fix:* i stride))))
		 ans))))))

;;; Some Filters:

;;; Haar
(define haar-0 (/ 1 (sqrt 2)))
(define haar-1 (/ 1 (sqrt 2)))

(define haar-smooth (vector haar-0 haar-1))
(define haar-detail (vector haar-0 (- haar-1)))

(define haar-filters (list haar-smooth haar-detail))


;;; Daubechies DAUB4
(define daub4-0 (/ (+ 1 (sqrt 3)) (* 4 (sqrt 2))))
(define daub4-1 (/ (+ 3 (sqrt 3)) (* 4 (sqrt 2))))
(define daub4-2 (/ (- 3 (sqrt 3)) (* 4 (sqrt 2))))
(define daub4-3 (/ (- 1 (sqrt 3)) (* 4 (sqrt 2))))

(define daub4-smooth (vector daub4-0 daub4-1 daub4-2 daub4-3))
(define daub4-detail (vector daub4-3 (- daub4-2) daub4-1 (- daub4-0)))

(define daub4-filters (list daub4-smooth daub4-detail))
