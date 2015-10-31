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

;;;;        PPA:  Piecewise polynomial approximations

;;; Edited by GJS 10Jan09
;;; Smooth versions of PPA added 5/19/89 (mh)
;;; bug fix 1/13/88

(declare (usual-integrations))

;;;   To make a piecewise polynomial approximation of a function, f,
;;;   we specify the range, [low, high], the maximum order of polynomial 
;;;   that fits may be made with, and the accuracy required.

(define (make-ppa f low high max-order accuracy)
  (let* ((c (/ (+ low high) 2))
         (d (/ (- high low) 2))
         (g (lambda (x) (f (+ x c))))
         (result (get-poly-and-errors g (- d) d max-order))
         (p (car result))
         (eps (cadr result)))
    (if (< eps accuracy)
        (ppa-make-from-poly low high
			    (cheb-econ p (- d) d (- accuracy eps)))
        (ppa-adjoin (make-ppa f low c max-order accuracy)
		    (make-ppa f c high max-order accuracy)))))


;;; PPA-VALUE will evaluate a PPA at any given point, x.

(define (ppa-value ppa x)
  (define (ppa-search low high body)
    (cond ((ppa-terminal? body)
           (poly:value (ppa-poly body) (- x (/ (+ low high) 2))))
          ((ppa-split? body)
           (let ((s (ppa-split body)))
             (if (< x s)
                 (ppa-search low s (ppa-low-side body))
                 (ppa-search s high (ppa-high-side body)))))
          (else (error "Bad body -- PPA-SEARCH"))))
  (let ((low (ppa-low-bound ppa))
        (high (ppa-high-bound ppa)))
    (if (and (<= low x) (<= x high))
        (ppa-search low high (ppa-body ppa))
        (error "Out of bounds -- PPA-VALUE"))))


;;; We may use PPAs to memoize functions.

(define (ppa-memo f low high max-order accuracy)
  (let ((ppa (make-ppa f low high max-order accuracy)))
    (lambda (x) (ppa-value ppa x))))

;;; When derivatives of a numerical procedure are available, they may
;;; be used to increase the accuracy easily achievable in the piecewise
;;; approximating process. The first argument is a list of the numerical
;;; procedure f and as many of its successive derivative procedures as
;;; we care to use. For example, 
;;; 		(make-smooth-ppa (list sin cos) 0 pi/2 1e-7)
;;; will use Hermite fitting with first derivative contact, employing
;;; piecewise cubics for the process.

(define (make-smooth-ppa flist low high accuracy)
  (let* ((n (length flist))
         (f (car flist))
         (herm (make-hermite-interpolator (- n 1))))
    (let loop ((a low) (b high))
      (let* ((c (/ (+ a b) 2))
             (d (/ (- b a) 2))
             (-d (- d))
	     (avals (map (lambda (f) (f a)) flist))
             (bvals (map (lambda (f) (f b)) flist))
             (p (herm (cons -d avals) (cons d bvals)))
             (g (lambda (x) (f (+ c x))))
             (erf (lambda (t) (abs (- (g t) (poly:value p t)))))
             (eps (cadr (gsmax erf -d d 'function-tol .01))))
        (if (< eps accuracy)
            (ppa-make-from-poly a b p)
            (let ((mid (/ (+ a b) 2)))
              (ppa-adjoin (loop a mid)
                          (loop mid b))))))))

(define (smooth-ppa-memo flist low high accuracy)
  (let ((ppa (make-smooth-ppa flist low high accuracy)))
    (lambda (x) (ppa-value ppa x))))


;;; Implementation of PPA data structures

(define (ppa-make-from-poly low high poly)
  (cons (cons low high) 
        (cons 'ppa-terminal poly)))

(define (ppa-adjoin ppalow ppahigh)
  (if (= (cdar ppalow) (caar ppahigh))
      (cons (cons (caar ppalow) (cdar ppahigh))
            (cons 'ppa-split
                  (cons (cdar ppalow)
                        (cons (cdr ppalow) (cdr ppahigh)))))
      (error "PPAs not adjacent -- PPA-ADJOIN")))

(define ppa-low-bound caar)
(define ppa-high-bound cdar)

(define ppa-body cdr)

(define (ppa-terminal? b)
  (eq? (car b) 'ppa-terminal))
(define ppa-poly cdr)


(define (ppa-split? b)
  (eq? (car b) 'ppa-split))

(define ppa-split cadr)
(define ppa-low-side caddr)
(define ppa-high-side cdddr)

#|
(define win (frame 0 pi/2 -0.0001 +0.0001))

(define s0
  (let ((p (make-ppa sin 0 pi/2 5 .0001)))
    (lambda (x) (ppa-value p x))))

(plot-function win (- s0 sin) 0 (- pi/2 .01) .001)
;;; S0 has a nasty discontinuity at the ppa split.

(define s1
  (let ((p (make-smooth-ppa (list sin) 0 pi/2 .0001)))
    (lambda (x) (ppa-value p x))))

(plot-function win (- s1 sin) 0 (- pi/2 .01) .001)
;;; s1 is continuous, but otherwise horrible!

(define s2
  (let ((p (make-smooth-ppa (list sin cos) 0 pi/2 .0001)))
    (lambda (x) (ppa-value p x))))

(plot-function win (- s2 sin) 0 (- pi/2 .01) .001)
;;; s2 is very oscillatory and not very accurate, but it is smooth.

(define s3
  (let ((p (make-smooth-ppa (list sin cos (- sin)) 0 pi/2 .0001)))
    (lambda (x) (ppa-value p x))))

(plot-function win (- s3 sin) 0 (- pi/2 .01) .001)
;;; S3 is really better!  More smooth, accurate, and only one split.

(graphics-clear win)
(graphics-close win)
|#