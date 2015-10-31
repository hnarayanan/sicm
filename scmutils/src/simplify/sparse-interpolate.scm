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

;;;;         Sparse Multivariate Polynomial Interpolation 
;;;         a probabilistic method based on Richard Zippel's
;;;          "Interpolating Polynomials From Their Values"
;;;   TR89-963, Department of CS, Cornell University, January 1989
;;;      coded and debugged by Gerald Jay Sussman and Dan Zuras  
;;;                        June 1998                  

;;; This code differs from Zippel's in that it does not use modular
;;; arithmetic.  This makes the idea stand out in stark contrast
;;; without the confusing complications introduced by those
;;; optimizations.

(declare (usual-integrations))

;;; Given a polynomial function f of arity n and maximum degree d, to
;;; find a representation for the terms of the polynomial.

(define (sparse-interpolate f n d)
  (let* ((rargs0 (generate-list (fix:- n 1) interpolate-random))
	 (f1 (lambda (x) (apply f x rargs0)))
	 (p1 (univariate-interpolate f1 d)))
    (let stagelp			;p has k vars interpolated
	((k 1) (p p1) (rargs rargs0))
      (if (fix:= k n)
	  p
	  (let* ((fk
		  (lambda (xk+1)
		    (lambda x1-xk
		      (apply f (append x1-xk (list xk+1) (cdr rargs))))))
		 (xk+1s
		  (generate-list (fix:+ d 1) interpolate-random))
		 (ps
		  (map (lambda (xk+1)
			 (interpolate-skeleton (fk xk+1) p))
		       xk+1s))
		 (css
		  (list-transpose
		   (map (lambda (p)
			  (map sparse-coefficient p))
			ps))))
	    (let ((cps
		   (let clp ((css css))
		     (if (null? css)
			 '()
			 (univariate-interpolate-values xk+1s (car css)
			  (lambda (cp) (cons cp (clp (cdr css))))
			  (lambda () (stagelp k p rargs)))))))
	      (stagelp (fix:+ k 1) (expand-poly p cps) (cdr rargs))))))))

#|
(sparse-interpolate
 (lambda (x y z) (+ (* 3 (square x) (cube y)) (* x y z) (* 4 z) 1))
 3
 4)
;Value: (((2 3 0) . 3) ((1 1 1) . 1) ((0 0 1) . 4) ((0 0 0) . 1))
|#

(define *interpolate-skeleton-using-vandermonde* #t)

(define (interpolate-skeleton f skeleton-polynomial)
  (let ((skeleton (map sparse-exponents skeleton-polynomial))
	(arity (length (sparse-exponents (car skeleton-polynomial)))))
    (if *interpolate-skeleton-using-vandermonde*
        (let try-again ((args (generate-list arity interpolate-random)))
	  (let* ((ones (make-list arity 1))
		 (ks
		  (map (lambda (exponent-list)
			 (apply * (map expt args exponent-list)))
		       skeleton))
		 (ws
		  (let lp ((i (length ks)) (argl ones) (fs '()))
		    (if (fix:= i 0)
			(reverse! fs)
			(lp (fix:- i 1)
			    (map * argl args)
			    (cons (apply f argl) fs))))))
	    (solve-vandermonde-t-system ks ws
	      (lambda (coefficients)
		(filter (lambda (term)
			  (not (zero? (sparse-coefficient term))))
			(map (lambda (exponent-list coefficient)
			       (sparse-term exponent-list coefficient))
			     skeleton
			     coefficients)))
	      (lambda ()
		(try-again (generate-list arity interpolate-random))))))
	(let ((new-args
	       (lambda ()
		 (generate-list (length skeleton-polynomial)
				(lambda (i)
				  (generate-list arity interpolate-random))))))
	  (let try-again ((trial-arglists (new-args)))
	    (let ((matrix
		   (matrix-by-row-list
		    (map (lambda (argument-list)
			   (map (lambda (exponent-list)
				  (apply * (map expt argument-list exponent-list)))
				skeleton))
			 trial-arglists)))
		  (values
		   (map (lambda (argl) (apply f argl))
			trial-arglists)))
	      (lu-solve matrix
			(list->vector values)
			(lambda (coefficients)
			  (filter (lambda (term)
				    (not (zero? (sparse-coefficient term))))
				  (map (lambda (exponent-list coefficient)
					 (sparse-term exponent-list coefficient))
				       skeleton
				       (vector->list coefficients))))
			(lambda (ignore) (try-again (new-args))))))))))

#|
(interpolate-skeleton
 (lambda (x) (+ (* 3 (expt x 5)) (expt x 2) x 4))
 '(((5) . 1) ((2) . 1) ((1) . 1) ((0) . 1)))
;Value: (((5) . 3) ((2) . 1) ((1) . 1) ((0) . 4))
|#

(define (expand-poly p cps)
  (sort
   (apply append
	  (map (lambda (skel-term cp)
		 (let ((old-exponents (sparse-exponents skel-term)))
		   (map (lambda (coeff-term)
			  (sparse-term
			   (append old-exponents (sparse-exponents coeff-term))
			   (sparse-coefficient coeff-term)))
			cp)))
	       p cps))
   sparse-term->))

#|
(pp (expand-poly '(((5) . 3) ((2) . 1) ((1) . 1) ((0) . 4))
		 '( (((1) . 1) ((0) . 3))
		    (((1) . 1))
		    (((3) . 2) ((0) . 4))
		    (((1) . 2) ((0) . 5)) )))
(((5 1) . 1) ((5 0) . 3) ((1 3) . 2) ((2 1) . 1) ((1 0) . 4) ((0 1) . 2) ((0 0) . 5))
|#


;;; f is a univariate polynomial function.  
;;; d+1 is the number of unknown coefficients.
;;; (usually d is the degree of the polynomial)

(define (univariate-interpolate f d)
  (let* ((xs (generate-list (+ d 1) interpolate-random))
	 (fs (map f xs)))
    (univariate-interpolate-values
     xs
     fs
     (lambda (poly) poly)
     (lambda () (univariate-interpolate f d)))))

(define (univariate-interpolate-values xs fs succeed fail)
  (solve-vandermonde-system xs fs
    (lambda (coefficients)
      (succeed (reverse
		(filter (lambda (term)
			  (not (zero? (sparse-coefficient term))))
			(map (lambda (exponent coefficient)
			       (sparse-term (list exponent)
					    coefficient))
			     (iota (length xs))
			     coefficients)))))
    fail))


(define *interpolate-size* 10000)

(define (interpolate-random i)
  (+ (random *interpolate-size*) 1))

#|
(univariate-interpolate
 (lambda (x) (+ (* 3 (expt x 5)) (expt x 2) x 4))
 6)
;Value: (((5) . 3) ((2) . 1) ((1) . 1) ((0) . 4))
|#

#|
(define (old-univariate-interpolate-values xs fs succeed fail)
  (let ((n (length xs)))
    (assert (fix:= n (length fs)))
    (let* ((exponents (iota n))
	   (matrix
	    (matrix-by-row-list
	     (map (lambda (x)
		    (map (lambda (e) (expt x e))
			 exponents))
		  xs))))
      (lu-solve matrix
		(list->vector fs)
		(lambda (coefficients)
		  (succeed (reverse
			    (filter (lambda (term)
				      (not (zero? (sparse-coefficient term))))
				    (map (lambda (exponent coefficient)
					   (sparse-term (list exponent)
							coefficient))
					 exponents
					 (vector->list coefficients))))))
		(lambda (ignore) (fail))))))

;;; Check that the new algorithm is equivalent to the old one, and faster
(define (check m)
  (let ((xs (generate-list m interpolate-random))
	(fs (generate-list m interpolate-random)))
    (let ((t0 (runtime)))
      (univariate-interpolate-values xs fs
        (lambda (new-result)
	  (let ((t1 (runtime)))
	    (old-univariate-interpolate-values xs fs
	      (lambda (old-result)
		(let ((t2 (runtime)) (e (equal? old-result new-result)))
		  ;;(pp (list '+ (- t1 t0) (- t2 t1)))
		  (assert e)))
	      (lambda ()
		(pp (list 'old-failed-new-won xs fs new-result))))))
	(lambda ()
	  (let ((t1 (runtime)))
	    (old-univariate-interpolate-values xs fs
	      (lambda (old-result)
		(pp (list 'new-failed-old-won xs fs old-result)))
	      (lambda ()
		(let ((t2 (runtime)))
		  ;;(pp (list '* (- t1 t0) (- t2 t1)))
		  )
		'both-failed))))))))

(let lp ((i 100000))
  (if (fix:= i 0)
      'done
      (begin (check 30)
	     (lp (fix:- i 1)))))

;;; Increase failure rate to about 40% for size 30 problems.
(set! *interpolate-size* 1000)
|#
