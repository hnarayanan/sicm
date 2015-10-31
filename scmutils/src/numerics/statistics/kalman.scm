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

;;;;           Discrete Linear Kalman Filter
;;;   by Thanos Siapas, edited by Gerald Jay Sussman


;;; Load this into scmutils-base-environment.


;;; We deal here in Gaussian ellipsoids -- see ellipses.scm

(define make-ellipsoid list)
(define center-point car)
(define covariance-matrix cadr)

(define-package-maker kalman (coefficient-package)
  (let ((vector-package (vector-package-maker coefficient-package))
	(matrix-package (matrix-package-maker coefficient-package))
	(package-name `(kalman ,(coefficient-package 'package-name)))
	)
    (let ((v:dimension (vector-package 'dimension))
	  (v:+ (vector-package '+))
	  (v:- (vector-package '-))
	  (dot-product (vector-package 'dot-product))

	  (m:+ (matrix-package '+))
	  (m:- (matrix-package '-))
	  (m:* (matrix-package '*))
	  (matrix*vector (matrix-package 'matrix*vector))
	  (identity-matrix (matrix-package 'identity))
	  (invert (matrix-package 'invert))
	  (transpose (matrix-package 'transpose)))

;;; KALMAN-FILTER makes a Kalman filter for a dynamical system
;;;  characterized by the following functions of the state.

;;; (df x)    : transition matrix 
;;; (f x)     : evolution function (transition + forcing)
;;; (M x)     : measurement matrix
;;; (Q x)     : system noise covariance matrix


;;; The resulting Kalman filter takes an initial estimate ellipsoid and 
;;;  a data stream consisting of ellipsoids for the measurement data

;;; data      : measurement data stream
;;; estimates : estimates stream

;;; Currently parameterized at time i.  FIX

(define (kalman-filter f df M Q)
  (define (next-estimate estimate-ellipsoid data)
    (cons-stream estimate-ellipsoid
		 (next-estimate
		  (let ((measurement-ellipsoid (head data)))
		    (let ((x (center-point estimate-ellipsoid))
			  (Px (covariance-matrix estimate-ellipsoid))
			  (y (center-point measurement-ellipsoid))
			  (Py (covariance-matrix measurement-ellipsoid)))
		      (kalman-filter-iteration x
					       Px 
					       (f x)
					       (df x)
					       (M x)
					       (Q x)
					       y
					       Py)))
		  (tail data))))
  next-estimate)
  
;;;
;;; Discrete Linear Kalman Filter Iteration
;;;

;;; Inputs:

;;; xk      : state estimate at time t_k, given y_1,..., y_k
;;; Pk      : covariance matrix for the error in x_k
;;; Phi     : system transition matrix from time t_k to time t_(k+1)
;;; xk+1-   : state estimate at time t(k+1) given y_1,...,y_k
;;; M       : measurement matrix at time t_k
;;; Qk      : system noise covariance matrix at time t_k
;;; yk+1    : measurement at time t_(k+1)
;;; Rk+1    : measurement noise covariance matrix at time t_(k+1)


;;; Intermediate quantities:

;;; Pk+1-   : covariance matrix of error in xk+1-
;;; r       : predicted residual at time t_(k+1)
;;; Y       : predicted residual covariance matrix at time t_(k+1)
;;; K       : Kalman gain

;;; Outputs:

;;; xk+1    : state estimate at time t_(k+1) given y_1,....,y_(k+1)
;;; Pk+1    : covariance matrix for the error in x_(k+1)
	      
(define (kalman-filter-iteration xk Pk xk+1- Phi M Qk yk+1 Rk+1)
    (let* ((Pk+1- (m:+ (m:* Phi Pk (transpose Phi)) Qk))
	   (e (v:- yk+1 (matrix*vector M xk+1-))) ;measured-predicted
	   (B (m:* Pk+1- (transpose M)))
	   (Y (m:+ (m:* M B) Rk+1))
	   (K (m:* B (invert Y)))
	   (xk+1 (v:+ xk+1- (matrix*vector K e)))
	   (A (m:- (identity-matrix (v:dimension xk)) (m:* K M)))
	   (Pk+1 (m:+ (m:* A Pk+1- (transpose A))
		      (m:* K Rk+1 (transpose K)))))
      (make-ellipsoid xk+1 Pk+1)))

(make-arithmetic-package package-name
  `(kalman-filter ,kalman-filter)
  `(kalman-filter-iteration ,kalman-filter-iteration))
)))

#|
;;; For example,

(define kalman-package (kalman-package-maker scheme-number-package))

(define kalman-filter (kalman-package 'kalman-filter))


;;; Let's estimate the state in the discrete dynamical system 
;;;  x_(n+2) + a x_n = 0

;;; The state vector is [x_n, x_(n-1)]

;;; The dynamical evolution function is:

(define a 1)

(define (f v)
  (let ((xn (vector-ref v 0))
	(xn-1 (vector-ref v 1)))
    (vector xn-1			;new x_n
	    (* -1 a xn)			;new x_(n-1)
	    )))


;;; Thus, the transition matrix is the Jacobian of f.

(define (df v)
  (let ((xn (vector-ref v 0))
	(xn-1 (vector-ref v 1)))
    (vector (vector 0     1  )
	    (vector (- a) 0  ))))


;;; Let's measure x only, yielding a measurement matrix:

(define (m v)
  (let ((xn (vector-ref v 0))
	(xn-1 (vector-ref v 1)))
    (vector (vector 1 0))))

;;; We also need a system noise covariance matrix.
;;;   Assume a noisless evolution:

(define (q v)
  (let ((xn (vector-ref v 0))
	(xn-1 (vector-ref v 1)))
    (vector (vector 0 0)
	    (vector 0 0))))

(define initial-estimate
  (make-ellipsoid (vector -1 1)	;at t=0
		  (vector (vector .0001 0    )
			  (vector 0     .0001))))

(define (me sigma)
  (let ((ss (square sigma)))
    (make-ellipsoid ((add-noise sigma) (vector 1))
		    (vector (vector ss)))))

(define (mo sigma)
  (let ((ss (square sigma)))
    (make-ellipsoid ((add-noise sigma) (vector -1))
		    (vector (vector ss)))))


(define (measurements sigma)
  (cons-stream (me sigma)		 ;at t=1+4n
       (cons-stream (me sigma)		 ;at t=2+4n
	    (cons-stream (mo sigma)	 ;at t=3+4n
		 (cons-stream (mo sigma) ;at t=4+4n
		      (measurements sigma))))))

(stream:for-each pp
		 ((kalman-filter f df m q)
		  initial-estimate
		  (measurements .01))
		 200)
(#(-1 1) #(#(.0001 0) #(0 .0001)))
(#(.99470729778394 1) #(#(.00005 0) #(0 .0001)))
(#(.9920138466000679 -.99470729778394) #(#(.00005 0) #(0 .00005)))
(#(-.994122797167975 -.9920138466000679)
 #(#(3.333333333333334e-5 0) #(0 .00005)))
(#(-.9953900488991766 .994122797167975)
 #(#(3.333333333333334e-5 0) #(0 3.333333333333334e-5)))
(#(.9958474964344258 .9953900488991766)
 #(#(2.5000000000000005e-5 0) #(0 3.333333333333334e-5)))
  ... ... ... ... ... ... ... ... ... ... ... ... ... ... 
(#(-.9983766235570967 1.0001158544214666)
 #(#(1.0101010101010112e-6 0) #(0 1.0101010101010112e-6)))
(#(1.000080874500018 .9983766235570967)
 #(#(1.000000000000001e-6 0) #(0 1.0101010101010112e-6)))
(#(.998203386325823 -1.000080874500018)
 #(#(1.000000000000001e-6 0) #(0 1.000000000000001e-6)))
(#(-1.0001232792752752 -.998203386325823)
 #(#(9.90099009900991e-7 0) #(0 1.000000000000001e-6)))
;Value: ...
|#

#|
;;; For example,

(define kalman-package (kalman-package-maker scheme-number-package))

(define kalman-filter (kalman-package 'kalman-filter))


;;; Let's estimate the value of a in the discrete dynamical system 
;;;  x_(n+2) + a x_n = 0

;;; The state vector is [x_n, x_(n-1), a]

;;; The dynamical evolution function is:

(define (f v)
  (let ((xn (vector-ref v 0))
	(xn-1 (vector-ref v 1))
	(a (vector-ref v 2)))
    (vector xn-1			;new x_n
	    (* -1 a xn)			;new x_(n-1)
	    a)))


;;; Thus, the transition matrix is the Jacobian of f.

(define (df v)
  (let ((xn (vector-ref v 0))
	(xn-1 (vector-ref v 1))
	(a (vector-ref v 2)))
    (vector (vector 0     1     0  )
	    (vector (- a) 0     0  )
	    (vector 0     0     1  ))))


;;; Let's measure x and a only, yielding a measurement matrix:

(define (m v)
  (let ((xn (vector-ref v 0))
	(xn-1 (vector-ref v 1))
	(a (vector-ref v 2)))
    (vector (vector 1 0 0)
	    (vector 0 0 1))))

;;; We also need a system noise covariance matrix.
;;;   Assume a noisless evolution:

(define (q v)
  (let ((xn (vector-ref v 0))
	(xn-1 (vector-ref v 1))
	(a (vector-ref v 2)))
    (vector (vector 0 0 0)
	    (vector 0 0 0)
	    (vector 0 0 0))))

;;; We will certainly have measurement noise and noise 
;;;  in our initial state estimate.
|#

#|
(define initial-estimate
  (make-ellipsoid (vector -1 1 1)	;at t=0
		  (vector (vector 1 0 0)
			  (vector 0 1 0)
			  (vector 0 0 1))))

(define me
  (make-ellipsoid (vector 1 1)
		  (vector (vector 1 0)
			  (vector 0 1))))

(define mo
  (make-ellipsoid (vector -1 1)
		  (vector (vector 1 0)
			  (vector 0 1))))


(define measurements
  (cons-stream me						;at t=1+4n
	       (cons-stream me					;at t=2+4n
			    (cons-stream mo			;at t=3+4n
					 (cons-stream mo	;at t=4+4n
						      measurements)))))

(stream:for-each pp
		 ((kalman-filter f df m q) initial-estimate measurements)
		 20)

(#(-1 1 1) #(#(1 0 0) #(0 1 0) #(0 0 1)))
(#(1 1 1) #(#(1/2 0 0) #(0 1 0) #(0 0 1/2)))
(#(1 -1 1) #(#(1/2 0 0) #(0 1/2 0) #(0 0 1/3)))
(#(-1 -1 1) #(#(1/3 0 0) #(0 1/2 0) #(0 0 1/4)))
(#(-1 1 1) #(#(1/3 0 0) #(0 1/3 0) #(0 0 1/5)))
(#(1 1 1) #(#(1/4 0 0) #(0 1/3 0) #(0 0 1/6)))
(#(1 -1 1) #(#(1/4 0 0) #(0 1/4 0) #(0 0 1/7)))
(#(-1 -1 1) #(#(1/5 0 0) #(0 1/4 0) #(0 0 1/8)))
(#(-1 1 1) #(#(1/5 0 0) #(0 1/5 0) #(0 0 1/9)))
(#(1 1 1) #(#(1/6 0 0) #(0 1/5 0) #(0 0 1/10)))
(#(1 -1 1) #(#(1/6 0 0) #(0 1/6 0) #(0 0 1/11)))
(#(-1 -1 1) #(#(1/7 0 0) #(0 1/6 0) #(0 0 1/12)))
(#(-1 1 1) #(#(1/7 0 0) #(0 1/7 0) #(0 0 1/13)))
(#(1 1 1) #(#(1/8 0 0) #(0 1/7 0) #(0 0 1/14)))
(#(1 -1 1) #(#(1/8 0 0) #(0 1/8 0) #(0 0 1/15)))
(#(-1 -1 1) #(#(1/9 0 0) #(0 1/8 0) #(0 0 1/16)))
(#(-1 1 1) #(#(1/9 0 0) #(0 1/9 0) #(0 0 1/17)))
(#(1 1 1) #(#(1/10 0 0) #(0 1/9 0) #(0 0 1/18)))
(#(1 -1 1) #(#(1/10 0 0) #(0 1/10 0) #(0 0 1/19)))
(#(-1 -1 1) #(#(1/11 0 0) #(0 1/10 0) #(0 0 1/20)))
;Value: ...
|#

#| ;;; Need to load stat/gauss  

(define initial-estimate
  (make-ellipsoid (vector -1 1 1)	;at t=0
		  (vector (vector .0001 0     0)
			  (vector 0     .0001 0)
			  (vector 0     0     .0001))))

(define (me sigma)
  (let ((ss (square sigma)))
    (make-ellipsoid ((add-noise sigma) (vector 1 1))
		    (vector (vector ss 0)
			    (vector 0  ss)))))

(define (mo sigma)
  (let ((ss (square sigma)))
    (make-ellipsoid ((add-noise sigma) (vector -1 1))
		    (vector (vector ss 0)
			    (vector 0  ss)))))


(define (measurements sigma)
  (cons-stream (me sigma)		 ;at t=1+4n
       (cons-stream (me sigma)		 ;at t=2+4n
	    (cons-stream (mo sigma)	 ;at t=3+4n
		 (cons-stream (mo sigma) ;at t=4+4n
		      (measurements sigma))))))

(stream:for-each pp
		 ((kalman-filter f df m q)
		  initial-estimate
		  (measurements .01))
		 200)
(#(-1 1 1)
 #(#(.0001 0 0) #(0 .0001 0) #(0 0 .0001)))
(#(.9999376413032195 1 1.0057446249431141)
 #(#(.00005 0 0) #(0 .0001 0) #(0 0 .00005)))
(#(1.0057348593788396 -1.0056819080190087 1.0057677181183942)
 #(#(.00005 0 0) #(0 5.057611253009827e-5 0) #(0 0 3.333333333333334e-5)))
(#(-1.0085448724810275 -1.0115356545495795 1.0054978189836263)
 #(#(3.358840368520521e-5 0 0)
   #(0 5.057843514045408e-5 0)
   #(0 0 2.5000000000000005e-5)))
   ... ... ... ... ... ... ... ... ... ... ... ... ... ... ... 
(#(1.0288889309703222 1.0276658779334358 1.000074552045696)
 #(#(1.057257453641285e-6 0 0)
   #(0 1.0669956062602948e-6 0)
   #(0 0 5.050505050505055e-7)))
(#(1.0274665272335184 -1.02896563674492 1.0000953641970813)
 #(#(1.0557310028460001e-6 0 0)
   #(0 1.0574151009295225e-6 0)
   #(0 0 5.025125628140708e-7)))
(#(-1.0287870172396263 -1.027564510753916 1.0000437911325195)
 #(#(1.0463508292523073e-6 0 0)
   #(0 1.055932370326007e-6 0)
   #(0 0 5.000000000000004e-7)))
;Value: ...
|#
