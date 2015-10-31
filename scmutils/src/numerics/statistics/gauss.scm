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

;;; UNIFORM-RANDOM produces an inexact number x,    0 <= x < 1

#|
(define uniform-random
  (let* ((random-max (expt 2 23))
	 (frandom-max (exact->inexact random-max)))
    (lambda ()
      (/ (random random-max)
	 frandom-max))))
|#

(define (uniform-random) (random 1.))

(define (nonzero-uniform-random)
  (let ((x (uniform-random)))
    (if (= x 0.)
	(nonzero-uniform-random)
	x)))

;;; Given uniform random numbers, we can produce pairs of
;;; gaussian-distributed numbers, with zero mean and unit
;;; standard deviation, by the following trick:

(define (gaussian-random-pair #!optional continue)
  ;; continue = (lambda (y1 y2) ...)
  (let ((continue (if (default-object? continue) cons continue))
	(x1 (uniform-random))
	(x2 (uniform-random)))
    (let ((r (sqrt (* -2.0 (log x1)))))
      (continue (* r (cos (* 2pi x2)))
		(* r (sin (* 2pi x2)))))))

(define (gaussian-random)
  (gaussian-random-pair (lambda (x y) x)))

(define (gaussian-random-list d)
  (let lp ((j d) (t '()))
    (if (fix:= j 0)
	t
	(gaussian-random-pair
	 (lambda (x1 x2)
	   (if (fix:= j 1)
	       (cons x1 t)
	       (lp (fix:- j 2) (cons x1 (cons x2 t)))))))))


;;; Makes a list of n 2-vectors of gaussian-distributed random numbers  

(define (gaussian-random-pairs n)
  (if (fix:= n 0) 
      '()
      (cons (gaussian-random-pair vector)
	    (gaussian-random-pairs (fix:- n 1)))))


;;; Makes a list of n d-vectors of gaussian-distributed random numbers  

(define (gaussian-random-tuples d n)
  (if (fix:= n 0) 
      '()
      (cons (list->vector (gaussian-random-list d))
	    (gaussian-random-tuples d (fix:- n 1)))))


;;; For adding zero-mean noise with a given standard deviation to a vector.

(define ((add-noise sigma) v)
  (list->vector (map (lambda (signal noise)
		       (+ signal (* sigma noise)))
		     (vector->list v)
		     (gaussian-random-list (vector-length v)))))