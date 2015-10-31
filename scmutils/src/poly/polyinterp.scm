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

;;;; Numerical construction of Polynomial interpolations

;;; Edited by GJS 10Jan09

(declare (usual-integrations))

;;; Alter the coefficients of polynomial P so that its domain [a,b] is
;;;  mapped onto the canonical domain [-1,1].

(define (poly-domain->canonical p a b)
  (if (<= b a)
      (error "bad interval: must have a < b in POLY-DOMAIN->CANONICAL"))
  (let ((c (/ (+ a b) 2)) (d (/ (- b a) 2)))
    ;; p(x) [a,b] --> p(y+c) = q(y) [-d,d] --> q(d*z) = r(z) [-1,1]
    (poly:arg-scale (poly:arg-shift p (list c)) (list d))))


;;; Alter the coefficients of polynomial P so that its domain [-1,1]
;;;  is mapped onto [a,b].  This is the inverse operation to
;;;  POLY-DOMAIN->CANONICAL.

(define (poly-domain->general p a b)
  (if (<= b a)
      (error "bad interval: must have a < b in POLY-DOMAIN->GENERAL"))
  (let ((c (/ (+ a b) 2)) (d (/ (- b a) 2)))
    (poly:arg-shift (poly:arg-scale p (list (/ 1 d))) (list (- c)))))


;;; Given a list of distinct abscissas xs = (x1 x2 ... xn) and a list
;;; of ordinates ys = (y1 y2 ... yn), return the Lagrange interpolation
;;; polynomial through the points (x1, y1), (x2, y2), ... (xn, yn).

(define (make-interp-poly ys xs)
  ;; given a point list, return a poly that evaluates to 1 at the 
  ;; first point and 0 at the others.
  (define (roots->poly roots)
    (a-reduce poly:*
	      (map (lambda (r) (poly:- poly:identity r))
		   roots)))
  (define (unity-at-first-point point-list)
    (let* ((x (car point-list))
           (px (apply * (map (lambda (u) (- x u)) 
			     (cdr point-list)))))
      (if (zero? px)
          (error "MAKE-INTERP-POLY: abscissas not distinct"))
          (poly:scale (roots->poly (cdr point-list)) (/ 1 px))))
  (let loop ((p poly:zero) (points xs) (values ys))
    (if (null? values)
        p
        (let ((q (unity-at-first-point points)))
          (loop (poly:+ p (poly:scale q (car values)))
                (left-circular-shift points)
                (cdr values))))))

;;; Given a function F, an interval [a, b], and a specified N > 0: we 
;;; generate a polynomial P of order N (and degree N-1) that interpolates 
;;; F at the "Chebyshev" points mapped onto [a, b].  We assume that the 
;;; absolute error function E(x) = |F(x) - P(x)| is unimodal between
;;; adjacent interpolation points.  Thus E(x) has altogether N+1 "bumps"; 
;;; the largest of these has height Bmax and the smallest has height Bmin. 
;;; Of course Bmax is an error bound for the approximation of F by P on 
;;; [a, b]; but Bmin has heuristic significance as a lower-bound for the 
;;; reduced error that might be obtained by tuning the interpolation points 
;;; to meet the equiripple criterion. We return the list (P Bmax Bmin).

(define (get-poly-and-errors f a b n)
  (let* ((c (/ (+ a b) 2))
         (d (/ (- b a) 2))
         (imap (lambda (x) (+ c (* d x)))) ;map [-1, 1] -> [a, b]
         (points (map imap (cheb-root-list n)))
         (p (make-interp-poly (map f points) points))
         (abserr (lambda (x) (abs (- (f x) (poly:value p x)))))
         (abserr-a (abserr a))
         (abserr-b (abserr b))
         (max-and-min-bumps
           (let loop ((pts points)
                      (bmax (max abserr-a abserr-b))
                      (bmin (min abserr-a abserr-b)))
             (if (< (length pts) 2)
                 (list bmax bmin)
                 (let ((x0 (car pts)) (x1 (cadr pts)))
                   (let ((bump (cadr (brent-max abserr x0 x1 1e-6))))
                     (cond ((> bump bmax) (loop (cdr pts) bump bmin))
                           ((< bump bmin) (loop (cdr pts) bmax bump))
                           (else (loop (cdr pts) bmax bmin)))))))))
    (list p (car max-and-min-bumps) (cadr max-and-min-bumps))))


;;; Often we want a function that computes the value of a polynomial
;;; at given points.

(define ((polynomial-function poly) x)
  (assert (pcf? poly) "Not a polynomial")
  (poly/horner-univariate p x))

