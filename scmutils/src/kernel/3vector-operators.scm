#| -*-Scheme-*-

Copyright (C) 1986, 1987, 1988, 1989, 1990, 1991, 1992, 1993, 1994,
    1995, 1996, 1997, 1998, 1999, 2000, 2001, 2002, 2003, 2004, 2005,
    2006, 2007, 2008, 2009, 2010, 2011, 2012, 2013, 2014, 2015, 2016
    Massachusetts Institute of Technology

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

;;; Traditional Grad, Div, Curl, Lap in 3d rectangular coords

(define Grad
  (make-operator
   (lambda (f)				; function on 3d space
     (vector ((partial 0) f) ((partial 1) f) ((partial 2) f)))
   'Grad))

(define Div
  (make-operator
   (lambda (f3)				;3d vector of functions on 3d space
     (g:+ ((partial 0) (g:ref f3 0))
	  ((partial 1) (g:ref f3 1))
	  ((partial 2) (g:ref f3 2))))
   'Div))

(define Curl
  (make-operator
   (lambda (f3)				;3d vector of functions on 3d space
     (let ((Dx (partial 0)) (Dy (partial 1)) (Dz (partial 2))
	   (fx (g:ref f3 0)) (fy (g:ref f3 1)) (fz (g:ref f3 2)))
       (vector (g:- (Dy fz) (Dz fy))
	       (g:- (Dz fx) (Dx fz))
	       (g:- (Dx fy) (Dy fx)))) )
   'Curl))

(define Lap
  (make-operator
   (lambda (f)				; function on 3d space
     (g:+ ((expt (partial 0) 2) f)
	  ((expt (partial 1) 2) f)
	  ((expt (partial 2) 2) f)))
   'Lap))

#|
(define F (literal-function 'F (-> (UP Real Real Real) Real)))

(define A
  (literal-function 'A (-> (UP Real Real Real) (UP Real Real Real))))

((Grad F) (up 'x 'y 'z))
#|
(up (((partial 0) F) (up x y z))
    (((partial 1) F) (up x y z))
    (((partial 2) F) (up x y z)))
|#

((Div A) (up 'x 'y 'z))
#|
(+ (((partial 0) A^0) (up x y z))
   (((partial 1) A^1) (up x y z))
   (((partial 2) A^2) (up x y z)))
|#

((Curl A) (up 'x 'y 'z))
#|
(up
 (+ (((partial 1) A^2) (up x y z)) (* -1 (((partial 2) A^1) (up x y z))))
 (+ (((partial 2) A^0) (up x y z)) (* -1 (((partial 0) A^2) (up x y z))))
 (+ (((partial 0) A^1) (up x y z)) (* -1 (((partial 1) A^0) (up x y z)))))
|#

((Lap F) (up 'x 'y 'z))
#|
(+ (((partial 0) ((partial 0) F)) (up x y z))
   (((partial 1) ((partial 1) F)) (up x y z))
   (((partial 2) ((partial 2) F)) (up x y z)))
|#

;;; Vector version of Laplacian also works.
((Lap A) (up 'x 'y 'z))
#|
(up
 (+ (((partial 1) ((partial 1) A^0)) (up x y z))
    (((partial 0) ((partial 0) A^0)) (up x y z))
    (((partial 2) ((partial 2) A^0)) (up x y z)))
 (+ (((partial 2) ((partial 2) A^1)) (up x y z))
    (((partial 0) ((partial 0) A^1)) (up x y z))
    (((partial 1) ((partial 1) A^1)) (up x y z)))
 (+ (((partial 0) ((partial 0) A^2)) (up x y z))
    (((partial 1) ((partial 1) A^2)) (up x y z))
    (((partial 2) ((partial 2) A^2)) (up x y z))))
|#

;;; Identities

(define F (literal-function 'F (-> (UP Real Real Real) Real)))

(define G (literal-function 'G (-> (UP Real Real Real) Real)))

(define A
  (literal-function 'A (-> (UP Real Real Real) (UP Real Real Real))))


((Curl (Grad F)) (up 'x 'y 'z))
#| (up 0 0 0) |#


((Div (Curl A)) (up 'x 'y 'z))
#| 0 |#

((- (Div (Grad F))
    (Lap F))
 (up 'x 'y 'z))
#| 0 |#

((- (Curl (Curl A))
    (- (Grad (Div A)) (Lap A)))
 (up 'x 'y 'z))
#| (up 0 0 0) |#

((- (Div (* F (Grad G)))
    (+ (* F (Lap G))
       (dot-product (Grad F) (Grad G))))
 (up 'x 'y 'z))
#| 0 |#

|#

#|
;;; Alternative definitions that sometimes work:

(define Del
  (vector (partial 0) (partial 1) (partial 2)))

(define (One xyz) 1)

(define Div
  (make-operator
   (lambda (f3)
     ((dot-product Del f3) One))
   'Div))

(define Grad
  (make-operator
   (lambda (f)
     (Del f))
   'Grad))

(define Curl
  (make-operator
   (lambda (f3)				; function with three components
     ((cross-product Del f3) One))
   'Curl))

;;; Problem example:

((Curl (Grad G)) (up 'x 'y 'z))
#|
(up 0 0 0)
|#

((Div (Grad G)) (up 'x 'y 'z))
#|
(+ (((partial 0) ((partial 0) G)) (up x y z))
   (((partial 1) ((partial 1) G)) (up x y z))
   (((partial 2) ((partial 2) G)) (up x y z)))
|#

;;; But

((Curl (* F (Grad G))) (up 'x 'y 'z))
;Generic operator inapplicable

((Div (* F (Grad G))) (up 'x 'y 'z))
;Bad selectors -- DERIV:EUCLIDEAN-STRUCTURE

;;; Doesn't defer enough?
|#