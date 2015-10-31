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

;;;; Hermite interpolation 6/1/89 (mh)

;;; Edited by GJS 10Jan09

(declare (usual-integrations))

;;; This is currently configured to work for polynomials with only
;;; numerical coefficients. (gjs)

;;; Return the cubic polynomial that matches specified value and
;;; slope at each of two points.

(define make-cubic-interpolant
  ;; create the basic Hermite interpolants
  (let ((h0 (poly:dense-> '(1 0 -3  2))) ;h(0)=1,  h(1)=h'(0)=h'(1)=0
        (h1 (poly:dense-> '(0 0  3 -2))) ;h(1)=1,  h(0)=h'(0)=h'(1)=0
        (h2 (poly:dense-> '(0 1 -2  1))) ;h'(0)=1, h(0)=h(1)=h'(1)=0
        (h3 (poly:dense-> '(0 0 -1  1))));h'(1)=1, h(0)=h(1)=h'(0)=0
    (define (cubic-interpolant a fa fpa b fb fpb)
      ;; make the polynomial on [0,1] and then scale.
      ;; Note: the derivative must be scaled initially
      (let ((s (- b a)))
        (let ((p (poly:+ (poly:+ (poly:scale h0 fa)
				 (poly:scale h1 fb))
			 (poly:+ (poly:scale h2 (* s fpa))
				 (poly:scale h3 (* s fpb))))))
          (poly:arg-shift (poly:arg-scale p (list (/ 1 s)))
			  (list (- a))))))
    cubic-interpolant))


;;; As above, but return a 5th-degree polynomial that matches value
;;; and first two derivatives at each of two points.

(define make-quintic-interpolant
  (let ((m
	 (m:invert (matrix-by-rows '(1  0  0  0  0  0)
				   '(0  1  0  0  0  0)
				   '(0  0  2  0  0  0)
				   '(1  1  1  1  1  1)
				   '(0  1  2  3  4  5)
				   '(0  0  2  6 12 20)))))
    (define (quintic-interpolant a fa fpa fppa b fb fpb fppb)
      (let* ((s (- b a))
             (v (vector fa (* fpa s) (* fppa s s)
                        fb (* fpb s) (* fppb s s)))
             (p (poly:dense-> (vector->list (matrix*vector m v)))))
        (poly:arg-shift (poly:arg-scale p (list (/ 1 s)))
			(list (- a)))))
    quintic-interpolant))


;;; Given an order of contact n, return a procedure that generates
;;; an Hermite interpolating polynomial of degree 2n+1, which matches 
;;; function value along with first n derivatives at two selected 
;;; points. The returned procedure expects two arguments, each a list 
;;; of length n+2:
;;;    (a f(a) f'(a) f"(a) ... ),   (b f(b) f'(b) f"(b) ... )

(define (make-hermite-interpolator n)
  (let* ((m (fix:+ n 1))
         (2m (fix:* 2 m))
         (! (lambda (n k) ;k*(k+1)*(k+2)*...*(k+n-1)
              (let loop ((i 0) (prod 1))
                (if (fix:= i n)
                    prod
                    (loop (fix:+ i 1)
			  (fix:* prod (fix:+ k i)))))))
         (term (lambda (i j)
                 (if (fix:< i m)
                     (if (fix:= j i) (! i 1) 0)
                     (let ((i (fix:- i m)))
                       (if (fix:< j i)
                           0
                           (! i (fix:- j (fix:+ i -1))))))))
         (mat (m:invert (m:generate 2m 2m term))))
    (define (hermite-interpolator avals bvals)
      (let* ((a (car avals)) (b (car bvals))
             (s (- b a))
             (*s (lambda (x) (* s x)))
             (scale (letrec 
                      ((scale 
                         (lambda (L)
			   (if (null? L)
			       '()
			       (cons (car L)
				     (scale (map *s (cdr L))))))))
                      scale))
             (v (list->vector (append (scale (cdr avals)) (scale (cdr bvals)))))
             (p (poly:dense-> (vector->list (matrix*vector mat v)))))
        (poly:arg-shift (poly:arg-scale p (list (/ 1 s)))
			(list (- a)))))
    hermite-interpolator))

