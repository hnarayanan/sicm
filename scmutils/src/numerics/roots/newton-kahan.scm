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

;;; In simple cases, if one knows the function and its derivative,
;;;  Newton's method is a quick-and-dirty way to find a root.  
;;;  However, one must start close to the root to get it to converge.

(declare (usual-integrations))

(define (newton-search f&df x0 eps) 
  (define (newton-improve xn)
    (f&df xn
	  (lambda (fn dfn)
	    (- xn (/ fn dfn)))))
  (let lp ((xn x0))
    (let ((xn+1 (newton-improve xn)))
      (if (close-enuf? xn xn+1 eps)
	  (average xn xn+1)
	  (lp xn+1)))))
#|
(newton-search
 (lambda (x cont)
   (write-line x)
   (cont (cos x) (- (sin x))))
 1.0
 1e-15)
1.
1.6420926159343308
1.5706752771612507
1.5707963267954879
1.5707963267948966
;Value: 1.5707963267948966


;;; If the root is multiple, the convergence is much slower 
;;;  and much less accurate.

(newton-search
 (lambda (x cont)
   (write-line x)
   (cont (- 1.0 (sin x)) (- (cos x))))
 1
 1e-15)
1
1.2934079930260234
1.4329983666650792

;;; 28 iterations here

1.570796311871084
1.570796319310356
;Value: 1.570796319310356
|#

;;; Kahan's hack speeds up search for multiple roots, but costs
;;;  a bit for simple roots.

(define (newton-kahan-search f&df x0 x1 eps) 
  (define (kahan-trick x)
    (let ((z (round (abs x))))
      (if *kahan-wallp* (write-line `(kahan ,z)))
      z))
  (define (psi x) (f&df x /))
  (define (secant-improve xn psn xn-1 psn-1)
    (- xn 
       (* psn 
	  (kahan-trick (/ (- xn xn-1)
			  (- psn psn-1))))))
  (let lp ((xn x1) (psn (psi x1)) (xn-1 x0) (psn-1 (psi x0)))
    (if (close-enuf? xn xn-1 eps)
	(average xn xn-1)
	(let ((xn+1 (secant-improve xn psn xn-1 psn-1)))
	  (lp xn+1 (psi xn+1) xn psn)))))

(define *kahan-wallp* #f)
#|
;;; for example

(newton-kahan-search
 (lambda (x cont)
   (write-line x)
   (cont (cos x) (- (sin x))))
 1.0
 2.0
 1e-15)
1.
2.
1.5423424456397141
1.5708040082580965
1.5707963267948966
1.5707963267948966
;Value: 1.5707963267948966


;;; Kahan's method really speeds things up here, but it
;;;  doesn't make the result more accurate.

(newton-kahan-search
 (lambda (x cont)
   (write-line x)
   (cont (- 1.0 (sin x)) (- (cos x))))
 1.0
 2.0
 1e-15)
1.
2.
1.564083803078276
1.5707963519994068
1.5707963255702555
1.5707963255702555
;Value: 1.5707963255702555
|#
