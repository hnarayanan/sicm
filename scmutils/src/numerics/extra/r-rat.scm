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

;;;; EXACT-RATIONAL-APPROXIMATION, HEURISTIC-ROUNDER

(if-mit (declare (usual-integrations)))

;;; 10 December 1988.  
;;;  GJS fixed this to work with rational arithmetic in place.

;;; This file defines EXACT-RATIONAL-APPROXIMATION and uses it to 
;;; make a heuristic number rounder.

(define rational-approximation-wallp false)

(define (exact-rational-approximation x . optionals)
  (define *default-maxden* 6e23)
  (let ((epsilon
	 (if (not (null? optionals))
	     (car optionals)
	     (* 10 *machine-epsilon*)))
	(maxden
	 (if (and (not (null? optionals))
		  (not (null? (cdr optionals))))
	     (cadr optionals)
	     *default-maxden*)))
    (define (rat1 x cont)		;cont = (lambda (num den) ...)
      (let ((ix (inexact->exact (truncate x))))
	(let loop ((num ix) (den 1) (onum 1) (oden 0) (xx x) (a ix))
	  (if rational-approximation-wallp
	      (pp `((num= ,num) (den= ,den) (xx= ,xx))))				
	  (cond ((> den maxden) false)
		((and (not (= den 0))
		      (< (abs (/ (- x (/ num den)) x))
			 epsilon))
		 (cont num den))
		(else
		 (let* ((y (/ 1 (- xx a)))
			(iy (inexact->exact (truncate y))))
		   (loop (+ (* iy num) onum)
			 (+ (* iy den) oden)
			 num
			 den
			 y
			 iy)))))))
    (cond ((exact? x) x)
	  ((< (abs x) epsilon) 0)
	  ((< x 0)
	   (rat1 (abs x)
		 (lambda (num den)
		   (/ (- num) den))))
	  (else
	   (rat1 x /)))))

;;; Some processes, such as finding the roots of a polynomial, can
;;; benefit by heuristic rounding of results (to a nearby rational).

;;; Heuristic rounding will occur to a rational within 
(define heuristic-rounding-tolerance 1.0e-9)
;;; that is expressible with a denominator less than the 
(define heuristic-rounding-denominator 100)
;;; if such a rational exists.

(define (heuristic-round-real x)
  (let ((r (exact-rational-approximation x
					 heuristic-rounding-tolerance
					 heuristic-rounding-denominator)))
    (if r (exact->inexact r) x)))

(define (heuristic-round-complex z)
  (if (real? z)
      (heuristic-round-real z)
      (let ((r (real-part z)) (i (imag-part z)))
	(let ((ar (abs r)) (ai (abs i)))
	  (cond ((and (< ar heuristic-rounding-tiny)
		      (< ai heuristic-rounding-tiny))
		 0.)
		((< ai (* heuristic-rounding-irrelevant ar))
		 (heuristic-round-real r))
		((< ar (* heuristic-rounding-irrelevant ai))
		 (make-rectangular 0 (heuristic-round-real i)))
		(else
		 (let* ((ag (/ (angle z) pi))
			(af (heuristic-round-real ag)))
		   (if (= ag af)
		       (make-rectangular (heuristic-round-real r)
					 (heuristic-round-real i))
		       (make-polar (heuristic-round-real (magnitude z))
				   (* pi af))))))))))

(define heuristic-rounding-irrelevant 1.0e-9)
(define heuristic-rounding-tiny 1.0e-15)


(define (heuristic-round-angle a)
  (let* ((ag (/ a pi))
	 (af (heuristic-round-real ag)))
    (if (= ag af)
	a
	(* pi af))))
