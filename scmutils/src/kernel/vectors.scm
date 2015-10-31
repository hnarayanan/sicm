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

;;;;            Vectors

(declare (usual-integrations))

(define (v:type v) vector-type-tag)
(define (v:type-predicate v) vector-quantity?)

;;; This file makes the identification of the Scheme VECTOR data
;;; type with mathematical n-dimensional vectors.  These are 
;;; interpreted as column vectors by the matrix package.

;;; Thus we inherit the constructors VECTOR and MAKE-VECTOR,
;;; the selectors VECTOR-LENGTH and VECTOR-REF,
;;; the mutator VECTOR-SET!, and zero-based indexing.  
;;; We also get the iterator MAKE-INITIALIZED-VECTOR, 
;;; and the predicate VECTOR?

(define-integrable v:generate make-initialized-vector)
(define-integrable vector:generate make-initialized-vector)
(define-integrable v:dimension vector-length)

(define ((v:elementwise f) . vectors)
  (assert (and (not (null? vectors))
	       (for-all? vectors vector?)))
  (let ((n (v:dimension (car vectors))))
    (assert (for-all? (cdr vectors)
	      (lambda (m)
		(fix:= (v:dimension m) n))))
    (v:generate
     (vector-length (car vectors))
     (lambda (i)
       (g:apply f
		(map (lambda (v) (vector-ref v i))
		     vectors))))))

(define vector:elementwise v:elementwise)


(define (v:zero? v)
  (vector-forall g:zero? v))

(define (v:make-zero n)
  (make-vector n :zero))

(define (v:zero-like v)
  (v:generate (vector-length v)
	      (lambda (i)
		(g:zero-like (vector-ref v i)))))

(define (literal-vector name dimension)
  (v:generate dimension
	      (lambda (i)
		(string->symbol
		 (string-append (symbol->string name)
				"^"
				(number->string i))))))

(define (v:make-basis-unit n i)	; #(0 0 ... 1 ... 0) n long, 1 in ith position
  (v:generate n (lambda (j) (if (fix:= j i) :one :zero))))

(define (v:basis-unit? v)
  (let ((n (vector-length v)))
    (let lp ((i 0) (ans #f))
      (cond ((fix:= i n) ans)
	    ((g:zero? (vector-ref v i))
	     (lp (fix:+ i 1) ans))
	    ((and (g:one? (vector-ref v i)) (not ans))
	     (lp (fix:+ i 1) i))
	    (else #f)))))


(define (vector=vector v1 v2)
  (vector-forall g:= v1 v2))

(define (vector+vector v1 v2)
  (v:generate (vector-length v1)
    (lambda (i)
      (g:+ (vector-ref v1 i)
	   (vector-ref v2 i)))))
    
(define (vector-vector v1 v2)
  (v:generate (vector-length v1)
    (lambda (i)
      (g:- (vector-ref v1 i)
	   (vector-ref v2 i)))))

(define (v:negate v)
  (v:generate (vector-length v)
    (lambda (i)
      (g:negate (vector-ref v i)))))

(define (v:scale s)
  (lambda (v)
    (v:generate (vector-length v)
      (lambda (i)
	(g:* s (vector-ref v i))))))

(define (scalar*vector s v)
  (v:generate (vector-length v)
    (lambda (i)
      (g:* s (vector-ref v i)))))

(define (vector*scalar v s)
  (v:generate (vector-length v)
    (lambda (i)
      (g:* (vector-ref v i) s))))

(define (vector/scalar v s)
  (v:generate (vector-length v)
    (lambda (i)
      (g:/ (vector-ref v i) s))))

(define (v:inner-product v1 v2)
  (assert (and (vector? v1) (vector? v2))
	  "Not vectors -- INNER-PRODUCT" (list v1 v2))
  (let ((n (v:dimension v1)))
    (assert (fix:= n (v:dimension v2))
	    "Not same dimension -- INNER-PRODUCT" (list v1 v2))
    (let lp ((i 0) (ans :zero))
      (if (fix:= i n)
	  ans
	  (lp (fix:+ i 1)
	      (g:+ ans
		   (g:* (g:conjugate (vector-ref v1 i))
			(vector-ref v2 i))))))))
    
(define (v:dot-product v1 v2)
  (assert (and (vector? v1) (vector? v2))
	  "Not vectors -- V:DOT-PRODUCT" (list v1 v2))
  (let ((n (v:dimension v1)))
    (assert (fix:= n (v:dimension v2))
	    "Not same dimension -- V:DOT-PRODUCT" (list v1 v2))
    (let lp ((i 0) (ans :zero))
      (if (fix:= i n)
	  ans
	  (lp (fix:+ i 1)
	      (g:+ ans
		   (g:* (vector-ref v1 i)
			(vector-ref v2 i))))))))


(define (v:square v)
  (v:dot-product v v))

(define (v:cube v)
  (scalar*vector (v:dot-product v v) v))

(define (euclidean-norm v)
  (g:sqrt (v:dot-product v v)))

(define (complex-norm v)
  (g:sqrt (v:inner-product v v)))

(define (maxnorm v)
  (vector-accumulate max g:magnitude :zero v))


(define (v:make-unit v)
  (scalar*vector (g:invert (euclidean-norm v))
		 v))

(define (v:unit? v)
  (g:one? (v:dot-product v v)))


(define (v:conjugate v)
  ((v:elementwise g:conjugate) v))

(define (v:cross-product v w)
  (assert (and (fix:= (vector-length v) 3)
	       (fix:= (vector-length w) 3))
	  "Cross product of non-3-dimensional vectors?"
	  (list v w))
  (let ((v0 (vector-ref v 0))
	(v1 (vector-ref v 1))
	(v2 (vector-ref v 2))
	(w0 (vector-ref w 0))
	(w1 (vector-ref w 1))
	(w2 (vector-ref w 2)))
    (vector (g:- (g:* v1 w2) (g:* v2 w1))
	    (g:- (g:* v2 w0) (g:* v0 w2))
	    (g:- (g:* v0 w1) (g:* v1 w0)))))

(define (general-inner-product addition multiplication :zero)
  (define (ip v1 v2)
    (let ((n (vector-length v1)))
      (if (not (fix:= n (vector-length v2)))
	  (error "Unequal dimensions -- INNER-PRODUCT" v1 v2))
      (if (fix:= n 0)
	  :zero
	  (let loop ((i 1)
		     (ans (multiplication (vector-ref v1 0)
					  (vector-ref v2 0))))
	    (if (fix:= i n)
		ans
		(loop (fix:+ i 1)
		      (addition ans
				(multiplication (vector-ref v1 i)
						(vector-ref v2 i)))))))))
  ip)

(define (v:apply vec args)
  (v:generate (vector-length vec)
    (lambda (i)
      (g:apply (vector-ref vec i) args))))

(define (v:arity v)
  (let ((n (vector-length v)))
    (cond ((fix:= n 0)
	   (error "I don't know the arity of the empty vector"))
	  (else
	   (let lp ((i 1) (a (g:arity (vector-ref v 0))))
	     (if (fix:= i n)
		 a
		 (let ((b (joint-arity a (g:arity (vector-ref v i)))))
		   (if b
		       (lp (+ i 1) b)
		       #f))))))))

(define (v:partial-derivative vector varspecs)
  ((v:elementwise
    (lambda (f)
      (generic:partial-derivative f varspecs)))
   vector))

(define (v:inexact? v)
  (vector-exists g:inexact? v))


(assign-operation 'type                v:type            vector?)
(assign-operation 'type-predicate      v:type-predicate  vector?)
(assign-operation 'arity               v:arity           vector?)
(assign-operation 'inexact?            v:inexact?        vector?)

(assign-operation 'zero-like           v:zero-like       vector?)

(assign-operation 'zero?               v:zero?           vector?)
(assign-operation 'negate              v:negate          vector?)
(assign-operation 'magnitude           complex-norm    vector?)
(assign-operation 'abs                 euclidean-norm  vector?)
(assign-operation 'conjugate           v:conjugate       vector?)

(assign-operation '=           vector=vector       vector? vector?)
(assign-operation '+           vector+vector       vector? vector?)
(assign-operation '-           vector-vector       vector? vector?)

(assign-operation '*           scalar*vector       scalar? vector?)
(assign-operation '*           vector*scalar       vector? scalar?)

(assign-operation '/           vector/scalar       vector? scalar?)

;;; subsumed by s:dot-product
;;; (assign-operation 'dot-product v:dot-product   vector? vector?)

(assign-operation 'cross-product v:cross-product   vector? vector?)

(assign-operation 'dimension v:dimension  vector?)


#| ;;; Should be subsumed by deriv:pd in deriv.scm.
(assign-operation 'partial-derivative
		  v:partial-derivative
		  vector? any?)
|#

#| ;;; Should be subsumed by s:apply in structs.scm.
(assign-operation 'apply       v:apply           vector? any?)
|#

;;; Abstract vectors generalize vector quantities.

(define (abstract-vector symbol)
  (make-literal vector-type-tag symbol))

(define (av:arity v)
  ;; Default is vector of numbers.
  (get-property v 'arity *at-least-zero*))

(define (av:zero-like v)
  (let ((z (abstract-vector (list 'zero-like v))))
    (add-property! z 'zero #t)
    z))

(define (make-vector-combination operator #!optional reverse?)
  (if (default-object? reverse?)
      (lambda operands 
	(make-combination vector-type-tag
			  operator operands))
      (lambda operands 
	(make-combination vector-type-tag
			  operator (reverse operands)))))

(assign-operation 'type           v:type             abstract-vector?)
(assign-operation 'type-predicate v:type-predicate   abstract-vector?)
(assign-operation 'arity          av:arity           abstract-vector?)

(assign-operation 'inexact? (has-property? 'inexact) abstract-vector?)

(assign-operation 'zero-like      av:zero-like       abstract-vector?)

(assign-operation 'zero?    (has-property? 'zero)    abstract-vector?)

(assign-operation
   'negate     (make-vector-combination 'negate)     abstract-vector?)
(assign-operation
   'magnitude  (make-vector-combination 'magnitude)  abstract-vector?)
(assign-operation
   'abs        (make-vector-combination 'abs)        abstract-vector?)
(assign-operation
   'conjugate  (make-vector-combination 'conjugate)  abstract-vector?)
#|
(assign-operation
   'derivative (make-vector-combination 'derivative) abstract-vector?)
|#

;(assign-operation '= vector=vector abstract-vector? abstract-vector?)
(assign-operation
   '+  (make-vector-combination '+) abstract-vector? abstract-vector?)
(assign-operation
   '+  (make-vector-combination '+) vector?          abstract-vector?)
(assign-operation
   '+  (make-vector-combination '+ 'r)       abstract-vector? vector?)

(assign-operation
   '-  (make-vector-combination '-) abstract-vector? abstract-vector?)
(assign-operation
   '-  (make-vector-combination '-) vector?          abstract-vector?)
(assign-operation
   '-  (make-vector-combination '-) abstract-vector? vector?)
		     
(assign-operation
   '*  (make-numerical-combination '*)    abstract-vector? abstract-vector?)
(assign-operation
   '*  (make-numerical-combination '*)    vector?          abstract-vector?)
(assign-operation
   '*  (make-numerical-combination '* 'r) abstract-vector? vector?)
(assign-operation
   '*  (make-vector-combination '*)       scalar?          abstract-vector?)
(assign-operation
   '*  (make-vector-combination '* 'r)    abstract-vector? scalar?)
		     
(assign-operation
   '/  (make-vector-combination '/)       abstract-vector? scalar?)


(assign-operation
   'dot-product (make-vector-combination 'dot-product)
   abstract-vector? abstract-vector?)

(assign-operation 'partial-derivative
		  (make-vector-combination 'partial-derivative)
		  abstract-vector? any?)

;;; I don't know what to do here -- GJS.  This is part of the literal
;;; function problem.

;(assign-operation 'apply  v:apply              abstract-vector? any?)
