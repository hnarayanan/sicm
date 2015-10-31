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

;;;; Top-level optimization defaults

(declare (usual-integrations))

(define (minimize f lowx highx)
  (brent-min f lowx highx brent-error))
  
(define brent-error 1.0e-5)

;;; f is a function of the parameters.

(define nelder-start-step .01)
(define nelder-epsilon 1.0e-10)
(define nelder-maxiter 1000)

#|
;;; Simple, flat case 

(define (multidimensional-minimize f parameters)
  (let ((f (compose f vector->list)))
    (let ((result
	   (nelder-mead f
			(list->vector parameters)
			nelder-start-step
			nelder-epsilon
			nelder-maxiter)))
      (if (eq? 'ok (car result))
	  (vector->list (caadr result))
	  (error "Minimizer did not converge")))))
|#


(define (multidimensional-minimize f parameters)
  (let ((f (compose f (vector->parameters parameters))))
    (let ((result
	   (nelder-mead f
			(parameters->vector parameters)
			nelder-start-step
			nelder-epsilon
			nelder-maxiter)))
      (if (eq? 'ok (car result))
	  ((vector->parameters parameters)
	   (caadr result))
	  (error "Minimizer did not converge")))))

(define (parameters->vector p)
  (define (flatten x)
    (cond ((number? x) (list x))
	  ((structure? x)
	   (let ((lst (vector->list (s:->vector x))))
	     (if (for-all? lst number?)
		 lst
		 (append-map flatten lst))))
	  ((list? x)
	   (if (for-all? x number?)
	       x
	       (append-map flatten x)))
	  (else
	   (error "Non-numerical data in optimizer" p x))))
  (if (for-all? p number?)
      (list->vector p)
      (list->vector (flatten p))))

#|
(parameters->vector
 (list (vector 1 2.3 4)
       (up 3.5 (down 5 1.3) 6)))
;Value: #(1 2.3 4 3.5 5 1.3 6)
|#


(define ((vector->parameters prototype) vect)
  (let ((cur 0))
    (let plp ((proto prototype))
      (cond ((structure? proto)
	     (s:generate (s:length proto)
			 (s:same proto)
			 (lambda (i)
			   (plp (s:ref proto i)))))
	    ((list? proto)
	     (let llp ((proto proto))
	       (if (null? proto)
		   '()
		   (let ((first (plp (car proto))))
		     (cons first (llp (cdr proto)))))))
	    (else
	     (let ((el (vector-ref vect cur)))
	       (set! cur (fix:+ cur 1))
	       el))))))

#|
((vector->parameters
  (list (vector 'a 'b 'c) (up 'd (down 'e 'f) 'g)))
 #(1 2.3 4 3.5 5 1.3 6))
;Value: (#(1 2.3 4) #(3.5 (*down* #(5 1.3)) 6))
|#

#| ;;; Historical nonsense
(define (multidimensional-minimize f x0 cont)
  ;; cont=(lambda (status minimum-point minimum-value) ...)
  (let* ((bundle?
	  (cond ((vector? x0) #f)
		((list-of-vectors? x0) #t)
		(else
		 (error "Bad initial point -- MINIMIZE"
			x0))))
	 (result
	  (nelder-mead (if bundle?
			   (compose f (bundle-vectors (length x0)))
			   f)
		       (if bundle?
			   (flatten-list-of-vectors x0)
			   x0)
		       nelder-start-step
		       nelder-epsilon
		       nelder-maxiter)))
    (cont (eq? 'OK (car result))
	  (if bundle?
	      ((bundle-vectors (length x0)) (caadr result))
	      (caadr result))
	  (cdadr result))))


(define ((bundle-vectors n) qs)
  (let ((dimension (quotient (vector-length qs) n)))
    (let lp ((i 0) (ans '()))
      (if (fix:= i n)
	  (reverse ans)
	  (lp (fix:+ i 1)
	      (cons (subvector qs
			       (fix:* i dimension)
			       (fix:* (fix:+ i 1) dimension))
		    ans))))))

(define (flatten-list-of-vectors l)
  (list->vector (apply append (map vector->list l))))

  (define (list-of-vectors? l)
    (or (null? l)
	(and (pair? l)
	     (vector? (car l))
	     (list-of-vectors? (cdr l)))))
|#

