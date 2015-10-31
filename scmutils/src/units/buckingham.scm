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

;;; f(W1, ..., Wn) = 0
;;; W1, ..., Wn are symbolic quantities with-units

(define (->dimensionless #!rest Ws)
  (assert (> (length Ws) 1))
  (assert (for-all? Ws with-units?))
  (let* ((a (unit-exponents (u:units (car Ws))))
	 (m (vector-length a))		; fundamental dimensions
	 (n (length Ws))		; input quantities
	 (b (map (lambda (W)
		   (let ((uw (u:units W)))
		     (if (eq? uw &unitless)
			 (make-vector m 0)
			 (unit-exponents uw))))
		 Ws))
	 (xs				; exponents of Ws to make dimensionless quantities
	  (map (lambda (i) (symbol 'x i))
	       (iota n)))
	 (homogeneous-equations
	  (map (lambda (j)		;for each fundamental dimension
		 (make-equation
		  (symb:add:n
		   (map (lambda (x w)
			  (symb:mul x (ref w j)))
			xs
			b))
		  (list j)))
	       (iota m)))
	 (homogeneous-solutions
	  (solve-incremental homogeneous-equations (reverse xs)))
	 (kr (length (residual-variables homogeneous-solutions)))
	 (kh (length (hopeless-variables homogeneous-solutions)))
	 (k (+ kr kh)) ;n-rank(B))
	 (xvals
	  (let ((s (substitutions homogeneous-solutions)))
	    (map (lambda (x)
		   (or (find (lambda (substitution)
			       (eq? x (substitution-variable substitution)))
			     s)
		       x))
		 xs))))
    (pp homogeneous-solutions)
    (pp `(Need ,kr exponents for repeating variables
	       and ,kh exponents for other variables.
	       There are ,k PIs))
    (lambda assign
      (assert (= (length assign) k))
      (let* ((dict
	      (pair-up (list-head xvals kr) (list-head assign kr)
		       (pair-up (hopeless-variables homogeneous-solutions)
				(list-tail assign kr)
				'())))
	     (exponents
	      (map (lambda (xval)
		     (let* ((expr
			     (if (symbol? xval)
				 xval
				 (substitution-expression xval)))
			    (e
			     (solve-simplifier
			      (substitute-multiple expr dict))))
		       (assert (number? e))
		       e))
		   xvals)))
	(apply g:* (map g:expt Ws exponents))))))

#|
(define speed-example
   (->dimensionless (& 'd &meter) (& 't &second) (& 'v (/ &meter &second))))
(() (x0) (((= x1 (* -1 x0)) (2 0)) ((= x2 (* -1 x0)) (0))) ())
(Need 1 exponents for repeating variables
      and 0 exponents for other variables.
      There are 1 PIs)
#| speed-example |#

(speed-example 1)
#|
(/ d (* t v))
|#

(define pendulum-example
   (->dimensionless (& 't &second)
		    (& 'm &kilogram)
		    (& 'l &meter)
		    (& 'g (/ &meter (square &second)))))
(() (x0) (((= x1 0) (1)) ((= x2 (* -1/2 x0)) (2 0)) ((= x3 (* 1/2 x0)) (2 0))) ())
(Need 1 exponents for repeating variables
      and 0 exponents for other variables.
      There are 1 PIs)

(pendulum-example 2)
#|
(/ (* g (expt t 2)) l)
|#


(define explosion
  (->dimensionless (& 'E &joule)
		   (& 't &second)
		   (& 'rho_0 (/ &kilogram (cube &meter)))
		   (& 'P_0 (/ &newton (square &meter)))))
(()
 (x0)
 (((= x1 (* -3 x0)) (2 1 0))
  ((= x2 (* 3/2 x0)) (1 0))
  ((= x3 (* -5/2 x0)) (1 0)))
 ())
(Need 1 exponents for repeating variables
      and 0 exponents for other variables.
      There are 1 PIs)

(explosion -2)
#|
(/ (* (expt P_0 5) (expt t 6)) (* (expt E 2) (expt rho_0 3)))
|#
|#

#|
(define heat-example
  (->dimensionless (& 'rho (/ &kilogram (cube &meter)))
		   (& 'c (/ (square &meter) (* (square &second) &kelvin)))
		   (& 'K (/ (* &meter &kilogram) (* &kelvin (cube &second))))
		   (& 'Q (/ &kilogram (square &second)))
		   (& 'x &meter)
		   (& 't &second)))
(()
 (x1 x0)
 (((= x2 (* -1 x1)) (4))
  ((= x3 (+ (* -1 x0) x1)) (1 4))
  ((= x4 (+ (* 3 x0) (* -1 x1))) (0 4))
  ((= x5 (+ (* -2 x0) x1)) (2 1 4)))
 ())
(Need 2 exponents for repeating variables
      and 0 exponents for other variables.
      There are 2 PIs)
#| heat-example |#

(heat-example 1 2)
#|
(/ (* Q (expt c 2) x rho) (expt K 2))
|#

(heat-example 1 3)
#|
(/ (* (expt Q 2) (expt c 3) t rho) (expt K 3))
|#
|#

#|
((->dimensionless (& 'D &meter)
		 (& 'v (/ &meter &second))
		 (& 'rho (/ &kilogram (cube &meter)))
		 (& 'Delta_p (/ &kilogram (* &meter (square &second))))
		 (& 'mu  (/ &kilogram (* &meter &second)))
		 (& 'l  &meter)
		 ;(& 'eta &unitless)
		 )
 1 0 0)
(will need 3 numbers for each of 3 PIs)
#|
(/ D l)
|#

((->dimensionless (& 'D &meter)
		 (& 'v (/ &meter &second))
		 (& 'rho (/ &kilogram (cube &meter)))
		 (& 'Delta_p (/ &kilogram (* &meter (square &second))))
		 (& 'mu  (/ &kilogram (* &meter &second)))
		 (& 'l  &meter)
		 ;(& 'eta &unitless)
		 )
 0 1 0)
(will need 3 numbers for each of 3 PIs)
#|
(/ (* mu v) (* l Delta_p))
|#

((->dimensionless (& 'D &meter)
		 (& 'v (/ &meter &second))
		 (& 'rho (/ &kilogram (cube &meter)))
		 (& 'Delta_p (/ &kilogram (* &meter (square &second))))
		 (& 'mu  (/ &kilogram (* &meter &second)))
		 (& 'l  &meter)
		 ;(& 'eta &unitless)
		 )
 0 0 1)
(will need 3 numbers for each of 3 PIs)
#|
(/ (* (expt l 2) Delta_p rho) (expt mu 2))
|#
|#

