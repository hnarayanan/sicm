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

(declare (usual-integrations))

;;; To find x such that f(x)=0, 
;;;  given x1, x2 where f(x1) and f(x2) have opposite sign.

(define (false-position-search f x1 x2 epsilon)
  (define (fpsl x1 fx1 x2 fx2)
    (let ((afx1 (abs fx1)) (afx2 (abs fx2)))
      (define (best-guess)
	(if (opposite-sign? fx1 fx2)
	    (average x1 x2)
	    (if (< afx1 afx2) x1 x2)))
      (if (< afx1 epsilon)
	  (if (< afx2 epsilon)
	      (best-guess)
	      x1)
	  (if (< afx2 epsilon)
	      x2
	      (if (close-enuf? x1 x2 epsilon)
		  (best-guess)
		  (let* ((x (/ (- (* x2 fx1) (* fx2 x1))
			       (- fx1 fx2)))
			 (fx (f x)))
		    (if (opposite-sign? fx1 fx)
			(fpsl x1 fx1 x fx)
			(fpsl x fx x2 fx2))))))))
  (fpsl x1 (f x1) x2 (f x2)))
#|
;;; For example

(define (kepler ecc m)
  (false-position-search
   (lambda (e)
     (write-line e)
     (- e (* ecc (sin e)) m))
   0.0
   2pi
   1e-15))

(kepler .99 .01)
6.283185307179586
0.
.01
.01988423649613729
2.9653394755776365e-2
3.9307245802801455e-2
.04884472750060591
;;; about 300 lines of iteration here
.3422703164917564
.34227031649175765
.3422703164917588
.3422703164917599
;Value: .3422703164917599
|#

;;; If we are provided with a derivative of f as well as f
;;;  we can try to converge faster with Newton's method.

(define (newton-with-false-position-search f&df x1 x2 epsilon)
  (define (fpsl x1 fx1 dfx1 x2 fx2 dfx2)
    (define (try x)
      (if (opposite-sign? (- x x1) (- x x2))
	  (begin ;;(write-line (list 'newton x1 fx1 dfx1 x2 fx2 dfx2 x))
		 (next x))
	  (begin ;;(write-line (list 'fp x1 fx1 x2 fx2))
		 (next (/ (- (* x2 fx1) (* fx2 x1)) (- fx1 fx2))))))
    (define (next x)
      (f&df x
	    (lambda (fx dfx)
	      (if (opposite-sign? fx1 fx)
		  (fpsl x1 fx1 dfx1 x fx dfx)
		  (fpsl x fx dfx x2 fx2 dfx2)))))
    (let ((afx1 (abs fx1)) (afx2 (abs fx2)))
      (define (best-guess)
	(if (opposite-sign? fx1 fx2)
	    (average x1 x2)
	    (if (< afx1 afx2) x1 x2)))
      (if (< afx1 epsilon)
	  (if (< afx2 epsilon)
	      (best-guess)
	      x1)
	  (if (< afx2 epsilon)
	      x2
	      (if (close-enuf? x1 x2 epsilon)
		  (best-guess)
		  (if (< afx1 afx2)
		      (try (- x1 (/ fx1 dfx1)))
		      (try (- x2 (/ fx2 dfx2)))))))))
  (f&df x1
	(lambda (fx1 dfx1)
	  (f&df x2
		(lambda (fx2 dfx2)
		  (fpsl x1 fx1 dfx1 x2 fx2 dfx2))))))
#|
;;; For example

(define (kepler ecc m)
  (newton-with-false-position-search
    (lambda (e cont)
      (write-line e)
      (cont (- e (* ecc (sin e)) m)
	    (- 1 (* ecc (cos e)))))
    0.0
    2pi
    1e-15))

(kepler .99 .01)
.9999999999999991
.05990042451486612
.8552375430328689
.12924052056093327
.586704215814149
.23257623307830544
.3854635290318111
.3463221490526433
.34231027307859746
.3422703204251508
.34227031649177553
;Value 35: .34227031649177553
|#

(define (average x y)
  (/ (+ x y) 2.0))

(define (opposite-sign? x y)
  (< (* x y) 0.0))
