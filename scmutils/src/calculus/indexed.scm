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

;;;;    Minimal support for Indexed Objects
;;; e.g. the components of tensors relative to a basis.

(declare (usual-integrations + *))

;;; A minimal interface for multi-index stuff.

(define (argument-types proc)
  (eq-get proc 'argument-types))

(define has-argument-types? argument-types)

(define (declare-argument-types! proc argument-types)
  (assert (procedure? proc))
  (eq-put! proc 'argument-types argument-types))

;;; argument-types are, for example 
;;;    (list 1form-field? vector-field? vector-field?), 
;;; for a Christoffel-2: it takes one 1form field and two vector fields.

(define (index-types proc)
  (eq-get proc 'index-types))

(define has-index-types? index-types)

(define (declare-index-types! proc index-types)
  (assert (procedure? proc))
  (eq-put! proc 'index-types index-types))

;;; argument-types are, for example 
;;;    (list up down down), 
;;; for a Christoffel-2: it takes one 1form field and two vector fields.

;;; An argument-typed function of type (n . m) takes n 1form fields
;;; and m vector-fields, in that order, and produces a function on a
;;; manifold point.  An indexed function of type (n . m) takes n+m
;;; indices and gives a function on a manifold point.

;;; For each argument-typed function and basis there is an indexed
;;; function that gives the function resulting from applying the
;;; argument-typed function to the basis elements specified by the
;;; corresponding indices.

(define (typed->indexed function basis)
  (let ((arg-types (argument-types function))
	(vector-basis (basis->vector-basis basis))
	(1form-basis (basis->1form-basis basis)))
    (assert (and arg-types
		 (every (lambda (arg-type)
			  (or (eq? arg-type 1form-field?)
			      (eq? arg-type vector-field?)))
			arg-types)
		 (not (any (lambda (arg-type) ;forms then vectors.
			     (eq? arg-type 1form-field?))
			   (or (memq vector-field? arg-types)
			       '()))))
	    "Bad arg types")
    (define (indexed indices)
      (assert (fix:= (length indices) (length arg-types)))
      (apply function
	     (map (lambda (arg-type arg-index)
		    (cond ((eq? arg-type vector-field?)
			   (g:ref vector-basis arg-index))
			  ((eq? arg-type 1form-field?)
			   (g:ref 1form-basis arg-index))))
		  arg-types indices)))
    (declare-index-types! indexed
      (map (lambda (arg-type)
	     (cond ((eq? arg-type vector-field?)
		    down)
		   ((eq? arg-type 1form-field?)
		    up)))
	   arg-types))
    indexed))

(define (indexed->typed indexed basis)
  (let ((index-types (index-types indexed))
	(vector-basis (basis->vector-basis basis))
	(1form-basis (basis->1form-basis basis))
	(n (basis->dimension basis)))
    (assert (and index-types
		 (every (lambda (index-type)
			  (or (eq? index-type up) (eq? index-type down)))
			index-types)
		 (not (any (lambda (index-type)	;ups before downs
			     (eq? index-type up))
			   (or (memq down index-types)
			       '()))))
	    "Bad index types")
    (define (function . args)
      (assert (fix:= (length index-types) (length args)))
      (assert (every (lambda (index-type arg)
		       (or (and (eq? index-type up) (1form-field? arg))
			   (and (eq? index-type down) (vector-field? arg))))
		     index-types args)
	      "Args do not match indices")
      (let ((sum 0))			;Ugh! an accumulator...
	(let aloop ((args args) (term 1) (indices '()))
	  (if (null? args)
	      (set! sum (g:+ (g:* (indexed (reverse indices)) term) sum))
	      (let ((arg (car args)))
		(let dloop ((i 0))
		  (if (fix:< i n)
		      (begin
			(aloop (cdr args)
			       (g:* (cond ((vector-field? arg)
					   ((g:ref 1form-basis i) arg))
					  ((1form-field? arg)
					   (arg (g:ref vector-basis i))))
				    term)
			       (cons i indices))
			(dloop (fix:+ i 1))))))))
	sum))
    (declare-argument-types! function
      (map (lambda (index-type)
	     (cond ((eq? index-type up) 1form-field?)
		   ((eq? index-type down) vector-field?)))
	   index-types))
    function))

#|
(define-coordinates (up x y) R2-rect)

(define (T w1 w2 v1)
  (+ (* 'a (dx v1) (w1 d/dx) (w2 d/dy))
     (* 'b (dy v1) (w1 d/dy) (w2 d/dx))
     (* 'c (dy v1) (w1 d/dy) (w2 d/dy))))

(declare-argument-types! T
  (list 1form-field? 1form-field? vector-field?))

(((indexed->typed
   (typed->indexed T (coordinate-system->basis R2-rect))
   (coordinate-system->basis R2-rect))
  (literal-1form-field 'w1 R2-rect)
  (literal-1form-field 'w2 R2-rect)
  (literal-vector-field 'v1 R2-rect))
 ((point R2-rect) (up 'x 'y)))
#|
(+ (* a (w2_1 (up x y)) (w1_0 (up x y)) (v1^0 (up x y)))
   (* b (w2_0 (up x y)) (w1_1 (up x y)) (v1^1 (up x y)))
   (* c (w2_1 (up x y)) (w1_1 (up x y)) (v1^1 (up x y))))
|#
;;; Seems to work!
|#

(define (i:outer-product T1 T2)
  (let ((i1 (index-types T1)) (i2 (index-types T2)))
    (assert i1 "T1 not index typed")
    (assert i2 "T2 not index typed")
    (let ((nu1 (count-occurrences up i1)) (nd1 (count-occurrences down i1))
          (nu2 (count-occurrences up i2)) (nd2 (count-occurrences down i2)))
      (let ((nup (fix:+ nu1 nu2)) (ndp (fix:+ nd1 nd2)))
	(let ((np (fix:+ nup ndp)) (n1 (fix:+ nup nd1)))    
	  (define (product args)
	    (assert (fix:= (length args) np)
		    "Wrong number of args to i:outer-product")
	    (g:* (T1 (append (sublist args 0 nu1)
			     (sublist args nup n1)))
		 (T2 (append (sublist args nu1 nup)
			     (sublist args n1 np)))))
	  (declare-index-types! product
	    (append (make-list nup up) (make-list ndp down)))
	  product)))))

(define (count-occurrences element list)
  (count-elements (lambda (x) (eq? element x)) list))

#|
(define-coordinates (up x y) R2-rect)

(define (T1 w1 w2 v1)
  (+ (* 'a (dx v1) (w1 d/dx) (w2 d/dy))
     (* 'b (dy v1) (w1 d/dy) (w2 d/dx))
     (* 'c (dy v1) (w1 d/dy) (w2 d/dy))))

(declare-argument-types! T1
  (list 1form-field? 1form-field? vector-field?))

(define iT1
  (typed->indexed T1 (coordinate-system->basis R2-rect)))

(define (T2 w1 w2)
  (+ (* (w1 d/dx) (w2 d/dx))
     (* (w1 d/dy) (w2 d/dy))
     (* (w1 d/dy) (w2 d/dx))))

(declare-argument-types! T2
  (list 1form-field? 1form-field?))

(define iT2 
  (typed->indexed T2 (coordinate-system->basis R2-rect)))

(define iT3 (i:outer-product iT1 iT2))

(pe (((indexed->typed iT3 (coordinate-system->basis R2-rect))
      (literal-1form-field 'w1 R2-rect)
      (literal-1form-field 'w2 R2-rect)
      (literal-1form-field 'w3 R2-rect)
      (literal-1form-field 'w4 R2-rect)
      (literal-vector-field 'v1 R2-rect))
     ((point R2-rect) (up 'x 'y))))
#|
(+ (* a (w1_0 (up x y)) (v1^0 (up x y)) (w2_1 (up x y)) (w3_0 (up x y)) (w4_0 (up x y)))
   (* a (w1_0 (up x y)) (v1^0 (up x y)) (w2_1 (up x y)) (w4_1 (up x y)) (w3_1 (up x y)))
   (* a (w1_0 (up x y)) (v1^0 (up x y)) (w2_1 (up x y)) (w4_0 (up x y)) (w3_1 (up x y)))
   (* b (w2_0 (up x y)) (w1_1 (up x y)) (v1^1 (up x y)) (w3_0 (up x y)) (w4_0 (up x y)))
   (* b (w2_0 (up x y)) (w1_1 (up x y)) (v1^1 (up x y)) (w4_1 (up x y)) (w3_1 (up x y)))
   (* b (w2_0 (up x y)) (w1_1 (up x y)) (v1^1 (up x y)) (w4_0 (up x y)) (w3_1 (up x y)))
   (* c (w2_1 (up x y)) (w1_1 (up x y)) (v1^1 (up x y)) (w3_0 (up x y)) (w4_0 (up x y)))
   (* c (w2_1 (up x y)) (w1_1 (up x y)) (v1^1 (up x y)) (w4_1 (up x y)) (w3_1 (up x y)))
   (* c (w2_1 (up x y)) (w1_1 (up x y)) (v1^1 (up x y)) (w4_0 (up x y)) (w3_1 (up x y))))
|#
|#

(define (i:contract T u d n)
  (let ((i-types (index-types T)))
    (assert i-types "T not index typed")
    (let ((nu (count-occurrences up i-types))
	  (nd (count-occurrences down i-types)))
      (assert (and (fix:<= 0 u) (fix:< u nu)
		   (fix:<= 0 d) (fix:< d nd))
	      "Contraction indices not in range")
      (let ((nuc (fix:- nu 1)) (ndc (fix:- nd 1)))
	(define (contraction args)
	  (sigma (lambda (i)
		   (T (append
		       (list-with-inserted-coord (list-head args nuc) u i)
		       (list-with-inserted-coord (list-tail args nuc) d i))))
		 0 (fix:- n 1)))
	(declare-index-types! contraction
          (append (make-list nuc up) (make-list ndc down)))
	contraction))))

(define (list-with-inserted-coord list index coord)
  (append (list-head list index) (cons coord (list-tail list index))))

#|
(((indexed->typed (i:contract iT1 0 0 2)
       (coordinate-system->basis R2-rect))
      (literal-1form-field 'w1 R2-rect))
     ((point R2-rect) (up 'x 'y)))
#|
(+ (* a (w1_1 (up x y)))
   (* b (w1_0 (up x y)))
   (* c (w1_1 (up x y))))
|#

(((indexed->typed (i:contract iT1 1 0 2)
   (coordinate-system->basis R2-rect))
  (literal-1form-field 'w1 R2-rect))
 ((point R2-rect) (up 'x 'y)))
#| (* c (w1_1 (up x y))) |#

(((indexed->typed (i:contract iT3 1 0 0)
   (coordinate-system->basis R2-rect))
  (literal-1form-field 'w1 R2-rect)
  (literal-1form-field 'w2 R2-rect)
  (literal-1form-field 'w3 R2-rect))
 ((point R2-rect) (up 'x 'y)))
#| 0 |#
|#

(define (typed->structure T basis)
  (let ((vector-basis (basis->vector-basis basis))
	(1form-basis (basis->1form-basis basis)))
    (define (iterate arg-types argl)
      (if (null? arg-types)
	  (apply T (reverse argl))
	  (s:map/r (lambda (e) (iterate (cdr arg-types) (cons e argl)))
		   (cond ((eq? (car arg-types) vector-field?) vector-basis)
			 ((eq? (car arg-types) 1form-field?) 1form-basis)
			 (else (error "Bad arg-type"))))))
    (iterate (argument-types T) '())))

(define (structure->typed coeff-functions basis)
  (let ((vector-basis (basis->vector-basis basis))
	(1form-basis (basis->1form-basis basis))
	(arg-types
	 (let lp ((cf coeff-functions))
	   (if (structure? cf)
	       (cons (let ((shape (s:opposite cf)))
		       (cond ((eq? shape 'up) vector-field?)
			     ((eq? shape 'down) 1form-field?)
			     (else (error "Bad Shape"))))
		     (lp (ref cf 0)))
	       '())))
	(coeff-functions
	 (maybe-simplify-coeff-functions coeff-functions basis)))
    (define (indexed-function . args)
      (assert (fix:= (length args) (length arg-types)))
      (for-each (lambda (arg-type arg) (assert (arg-type arg)))
		arg-types args)
      (g:* (let lp ((args args) (arg-types arg-types))
	     (if (null? args)
		 one-manifold-function
		 (let ((arg (car args)) (arg-type (car arg-types)))
		   (cond ((eq? arg-type vector-field?)
			  (s:map/r (lambda (etilde)
				     (g:* (etilde arg)
					  (lp (cdr args)
					      (cdr arg-types))))
				   1form-basis))
			 ((eq? arg-type 1form-field?)
			  (s:map/r (lambda (e)
				     (g:* (arg e)
					  (lp (cdr args)
					      (cdr arg-types))))
				   vector-basis))))))
	   coeff-functions))
    (declare-argument-types! indexed-function arg-types)
    indexed-function))

#|
(define-coordinates (up x y) R2-rect)

(define (T v1 w1 w2)
  (+ (* 'a (dx v1) (w1 d/dx) (w2 d/dy))
     (* 'b (dy v1) (w1 d/dy) (w2 d/dx))
     (* 'c (dy v1) (w1 d/dy) (w2 d/dy))))

(declare-argument-types! T
  (list vector-field? 1form-field? 1form-field?))


((typed->structure T (coordinate-system->basis R2-rect))
 ((point R2-rect) (up 'x 'y)))
#|
(down (up (up 0 a) (up 0 0)) (up (up 0 0) (up b c)))
|#
;;; Outer index is first argument.  Inner index is last argument.

(((structure->typed
   (typed->structure T (coordinate-system->basis R2-rect))
   (coordinate-system->basis R2-rect))
  (literal-vector-field 'v1 R2-rect)
  (literal-1form-field 'w1 R2-rect)
  (literal-1form-field 'w2 R2-rect))
 ((point R2-rect) (up 'x 'y)))
#|
(+ (* a (w2_1 (up x y)) (w1_0 (up x y)) (v1^0 (up x y)))
   (* b (w2_0 (up x y)) (w1_1 (up x y)) (v1^1 (up x y)))
   (* c (w2_1 (up x y)) (w1_1 (up x y)) (v1^1 (up x y))))
|#

((typed->structure
  (structure->typed
   (typed->structure T (coordinate-system->basis R2-rect))
   (coordinate-system->basis R2-rect))
  (coordinate-system->basis R2-rect))
 ((point R2-rect) (up 'x 'y)))
#|
(down (up (up 0 a) (up 0 0)) (up (up 0 0) (up b c)))
|#
;;; Seems to work!
|#

(define (maybe-simplify-coeff-functions coeff-functions basis)
  (if (and simplify-coeff-functions? (coordinate-basis? basis))
      (s:map/r (simplify-coeff-function
		(typical-point (basis->coordinate-system basis)))
	       coeff-functions)
      coeff-functions))

(define ((simplify-coeff-function m) f)
  ;; a stub.  see einstein/speedup.scm for ideas
  f)

(define simplify-coeff-functions? #t)


;;; The following are universally ok.

(define (zero-manifold-function? f)
  (eq? f zero-manifold-function))

(define (one-manifold-function? f)
  (eq? f one-manifold-function))

(define (manifold-function-cofunction? x)
  (or (function-quantity? x)
      (numerical-quantity? x)))

(defhandler '* (lambda (x y) zero-manifold-function)
  zero-manifold-function? manifold-function-cofunction?)
(defhandler '* (lambda (x y) zero-manifold-function)
  manifold-function-cofunction? zero-manifold-function?)

(defhandler '* (lambda (x y) y)
  one-manifold-function? manifold-function-cofunction?)
(defhandler '* (lambda (x y) x)
  manifold-function-cofunction? one-manifold-function?)

(defhandler '+ (lambda (x y) y)
  zero-manifold-function? manifold-function-cofunction?)
(defhandler '+ (lambda (x y) x)
  manifold-function-cofunction? zero-manifold-function?)

