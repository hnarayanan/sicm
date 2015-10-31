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

;;;; Power-series arithmetic using infinite streams.

(declare (usual-integrations))

(define (series:type ps) series-type-tag)
(define (series:type-predicate ps) series?)

(define (make-series arity stream)
  (cons series-type-tag (cons arity stream)))

(define (series:arity series) (cadr series))

(define (series:promote-arity series)
  (assert (equal? (series:arity series) *exactly-zero*))
  (make-series *exactly-one* (series->stream series)))

(define (series->stream series)
  (if (not (series? series))
      (error "Not a series" series))
  (cddr series))

(define (series:same-arity series-list)
  (if (null? series-list)
      *at-least-zero*
      (a-reduce joint-arity
		(map series:arity series-list))))

(define ((series-wrapper stream-function) . series-list)
  (let ((arity (series:same-arity series-list)))
    (make-series arity
		 (apply stream-function
			(map series->stream
			     series-list)))))

(define (series:generate p #!optional arity)
  (make-series (if (default-object? arity)
		   *exactly-one*
		   arity)
	       (let lp ((i 0))
		 (cons-stream (p i) (lp (+ i 1))))))

(define (series:for-each proc series . optionals)
  (apply stream:for-each
	 proc
	 (series->stream series)
	 optionals))

(define ((series:elementwise proc) . series-list)
  (let ((arity (series:same-arity series-list)))
    (make-series arity
		 (apply map-streams
			proc
			(map series->stream series-list)))))

(define (series:print s . optionals)
  (apply series:for-each
	 print-expression
	 s
	 optionals))

(define (series:ref series index)
  (stream-ref (series->stream series) index))

;;; The following procedure takes a finite list and makes an infinite
;;; series that has the finite list as the initial segment of the
;;; coefficients.

(define (series . args)
  (make-series *exactly-zero*
    (let lp ((a args))
      (if (null? a)
	  (infinite-stream-of (g:zero-like (car args)))
	  (cons-stream (car a) (lp (cdr a)))))))

(define (power-series . args)
  (make-series *exactly-one*
    (let lp ((a args))
      (if (null? a)
	  (infinite-stream-of (g:zero-like (car args)))
	  (cons-stream (car a) (lp (cdr a)))))))

(define series:zero 
  (make-series *exactly-one* zero-stream))

(define series:one 
  (make-series *exactly-one*
	       (cons-stream :one zero-stream)))

(define series:identity
  (make-series *exactly-one*
	       (cons-stream :zero
			    (cons-stream :one zero-stream))))


(define (constant-series c #!optional arity)
  (make-series (if (default-object? arity)
		   *exactly-one*
		   arity)
	       (cons-stream c zero-stream)))
    

;;; The following procedures provide a set of capabilities for
;;;  manipulating series.

(define (coefficient+series c series)
  (let ((s (series->stream series)))
    (make-series (series:arity series)
      (cons-stream (g:+ c (head s)) (tail s)))))

(define (series+coefficient series c)
  (let ((s (series->stream series)))
    (make-series (series:arity series)
      (cons-stream (g:+ (head s) c) (tail s)))))

(define (coefficient-series c series)
  (let ((s (series->stream series)))
    (make-series (series:arity series)
      (cons-stream (g:- c (head s))
		   (negate-stream (tail s))))))

(define (series-coefficient series c)
  (let ((s (series->stream series)))
    (make-series (series:arity series)
      (cons-stream (g:- (head s) c) (tail s)))))

;;; c*(a0 + a1*x + a2*x^2 + a3*x^3 + ...)
;;;  = c*a0 + c*a1*x + c*a2*x^2 + c*a3*x^3 + ...

(define (coefficient*series c s)
  (make-series (series:arity s)
    (map-stream (lambda (x) (g:* c x))
		(series->stream s))))

(define (series*coefficient s c)
  (make-series (series:arity s)
    (map-stream (lambda (x) (g:* x c))
		(series->stream s))))


(define (series/coefficient s c)
  (make-series (series:arity s)
    (map-stream (lambda (x) (g:/ x c))
		(series->stream s))))
    
(define (coefficient/series c s)
  (series:mul (constant-series c (series:arity s))
	      (series:invert s)))

;;; (a0 + a1*x + a2*x^2 + ...) + (b0 + b1*x + b2*x^2 + ...)
;;;   = (a0+b0) + (a1+b1)*x + (a2+b2)*x^2 + ...

(define add-series (combiner-padded-streams g:+ :zero))

(define series:add (series-wrapper add-series))

(define sub-series (combiner-padded-streams g:- :zero))

(define series:sub (series-wrapper sub-series))

(define (negate-stream s) (map-stream g:negate s))

(define series:negate (series-wrapper negate-stream))

;;; (a0 + a1*x + a2*x^2 + ...) * (b0 + b1*x + b2*x^2 + ...)
;;;   = a0*b0 + (a0*b1+a1*b0)*x + (a0*b2+a1*b1+a2*b0)*x^2 + ...
;;; Each coefficient of the result is formed by reversing an initial
;;;  segment of one series, multiplying it by the coefficients of an
;;;  initial segment of the other series, and accumulating the
;;;  products.

(define (stream:c*s c s)
  (map-stream (lambda (x) (g:* c x)) s))

(define (stream:s/c s c)
  (map-stream (lambda (x) (g:/ x c)) s))

(define (mul-series s1 s2)
  (cons-stream (g:* (head s1) (head s2))
	       (add-series (stream:c*s (head s1) (tail s2))
		    (mul-series (tail s1) s2))))

(define series:mul (series-wrapper mul-series))

;;; what's this?
(define mul$series
  (accumulation series:mul series:one))

(define (invert-series s)
  (let ((s0 (g:/ :one (head s))))
    (define inverted
      (cons-stream s0
        (mul-series (stream:c*s (g:negate s0) (tail s)) inverted)))
    inverted))

(define series:invert (series-wrapper invert-series))

(define (series:div s1 s2)
  (series:mul s1 (series:invert s2)))

(define div$series
  (inverse-accumulation series:div
			series:mul
			series:invert
			series:one))  

(define (series:expt s e)
  (letrec ((square (lambda (s) (mul-series s s)))
	   (series:one
	    (cons-stream :one zero-stream))
	   (zuras
	    (lambda (t e k)
	      (cons-stream :one
		(stream:c*s (div-coeff e k)
			    (mul-series t
					(zuras t
					       (sub-coeff e 1)
					       (fix:+ k 1)))))))
	   (iexpt
	    (lambda (s e)
	      (cond ((fix:< e 0)
		     (invert-series (iexpt s (fix:negate e))))
		    ((fix:= e 0) :one)
		    ((fix:= e 1) s)
		    ((even? e)
		     (square
		      (iexpt s (fix:quotient e 2))))
		    (else
		     (mul-series s
		       (square
			(iexpt s
			       (fix:quotient (fix:- e 1)
					     2))))))))
	   (expt
	    (lambda (s e)
	      (if (exact-integer? e)
		  (iexpt s e)
		  (stream:c*s (expt-coeff (head s) e)
		    (zuras (stream:s/c (tail s) (head s)) e 1))))))
    (make-series (series:arity s)
		 (expt (series->stream s) e))))

(define series:derivative
  (let ()
    (define (deriv-iter s n)
      (if (null? s)
	  '()
	  (cons-stream (g:* n (head s))
		       (deriv-iter (tail s) (fix:+ n 1)))))
    (define (derivative s varnums)
      (cond ((equal? (series:arity s) *exactly-zero*)
	     ((series:elementwise
	       (lambda (term)
		 (generic:partial-derivative term varnums)))
	      s))
	    ((equal? (series:arity s) *exactly-one*)
	     (if (not (null? varnums))
		 (error "Cannot yet take partial derivatives of a series"
			s varnums))
	     (make-series *exactly-one*
			  (deriv-iter (tail (series->stream s)) 1)))
	    (else
	     (error "Cannot take derivative of non arity=1 series"
		    s varnums))))
    derivative))

;;; The integral of a series
;;;           a0 + a1*x + a2*x^2 + a3*x^3 + ...
;;;  is       c + a0*x + a1*x^2/2 + a2*x^3/3 + ...
;;;  and is returned by the procedure *INTEGRATE-SERIES which
;;;  takes the "initial condition" c as a required argument. 
;;;  For technical reasons, we are unable to use *INTEGRATE-SERIES 
;;;  with mutual-recursion as in
;;;
;;;    (define cos-series (*integrate-series (series:negate sin-series) 1)) ;DOESN'T
;;;    (define sin-series (*integrate-series cos-series 0))                 ;WORK!
;;;
;;;  However, we can achieve the desired effect by postponing the
;;;  attachment of the constant term, as follows. We use the procedure
;;;  INTEGRAL-SERIES-TAIL which returns the indefinite integral
;;;  part of the integrated series, i.e., {a0 a1/2 a2/3 a3/4 ...}.
;;;  Now, the mutual-recursion above can be made to work:
;;;    (define cos-series
;;;      (make-series 1 (cons-stream 1
;;;                      (series:negate (integral-series-tail sin-series)))))
;;;    (define sin-series
;;;      (make-series 1 (cons-stream 0 (integral-series-tail cos-series))))
;;;
;;;  We have a special form, INTEGRATE-SERIES, to encapsulate this ugly mess.  Look
;;;   in the file fundamental-series.scm for examples.

(define integrate-helper
  (lambda (s n)
    (cons-stream (g:/ (head s) n)
		 (integrate-helper (tail s) (fix:+ n 1)))))

(define (*integrate-series series constant-term)
  (make-series (series:arity series)
	       (cons-stream constant-term
			    (integrate-helper (series->stream series)
					      1))))

(define integral-series-tail
  (lambda (series)
    (integrate-helper (series->stream series) 1)))

;;;  A series of arity zero may be summed to yield a value.
;;;  Given a stream that represents such a series, the
;;;  following procedure will produce a stream of partial sums.
;;;  Note that this sequence is a stream, not a series.

(define (partial-sums series)
  (if (not (equal? (series:arity series) *exactly-zero*))
      (error "Cannot sum non arity=0 series" series))
  (let ((stream (series->stream series)))
    (partial-sums-stream (head stream) (tail stream))))

(define (partial-sums-stream value s)
  (cons-stream value
	       (partial-sums-stream (g:+ value (head s))
				    (tail s))))

(define (series:sum series order)
  (g:ref (partial-sums series) order))

;;; This procedure produces the result of substituting the argument
;;; for the indeterminate in the given power series.  

;;; Note, if the argument is an OPERATOR, the resulting series may be
;;; an operator too, as the series is an implicit summation.

(define (series:value series arguments)
  (define (collect stream-of-procs)
    (let ((first-result (g:apply (head stream-of-procs) arguments)))
      (if (series? first-result)
	  (let ((fr (series->stream first-result)))
	    (cons-stream (head fr)
			 (stream:+ (tail fr)
				   (collect (tail stream-of-procs)))))
	  (cons-stream first-result
		       (collect (tail stream-of-procs))))))
  (cond ((equal? (series:arity series) *exactly-one*)
	 (cond ((fix:= (length arguments) 1)
		(make-series *exactly-zero*
		 (map-streams g:*
			      (series->stream series)
			      (stream-of-powers (car arguments)
						(g:one-like
						 (car arguments))))))
	       (else
		(error "Wrong number of args to series" series arguments))))
	((equal? (series:arity series) *exactly-zero*)
	 (make-series *exactly-zero*
	  (collect (series->stream series))))
	(else
	 (error "Bad arity series" series arguments))))

(define (series:->function series)
  (cond ((equal? (series:arity series) *exactly-zero*)
	 (series:promote-arity series))
	((equal? (series:arity series) *exactly-one*)
	 series)
	(else
	 (error "Wrong arity SERIES:->FUNCTION" series))))


;;; To go the other way we need Taylor's theorem to give us a power series:

(define (series:function-> f . opt)
  (let ((x0 (if (null? opt) :zero (car opt))))
    (make-series *exactly-one*
		 (let lp ((i 1) (fn f) (factn 1))
		   (cons-stream (g:/ (fn x0) factn)
				(lp (fix:1+ i)
				    (derivative fn)
				    (* factn i)))))))


;;; To expand a series in a power of the argument

(define (series:inflate series exponent)
  (assert (and (integer? exponent) (positive? exponent) (series? series)))
  (make-series (series:arity series)
	       (stream:inflate (series->stream series)
			       (fix:- exponent 1))))

(define (series:zero-like x)  series:zero)
(define (series:one-like x)   series:one)


(assign-operation 'type             series:type             series?)
(assign-operation 'type-predicate   series:type-predicate   series?)
(assign-operation 'arity      series:arity                  series?)
#|
(assign-operation 'one        series:one           series?)
(assign-operation 'zero       series:zero          series?)
(assign-operation 'identity   series:identity      series?)
|#
(assign-operation 'zero-like  series:zero-like     series?)
(assign-operation 'one-like   series:one-like      series?)

(assign-operation 'negate     series:negate        series?)
(assign-operation 'invert     series:invert        series?)

(assign-operation '+          series:add           series?        series?)
(assign-operation '+          coefficient+series   not-series?    series?)
(assign-operation '+          series+coefficient   series?        not-series?)

(assign-operation '-          series:sub           series?        series?)
(assign-operation '-          coefficient-series   not-series?    series?)
(assign-operation '-          series-coefficient   series?        not-series?)

(assign-operation '*          series:mul           series?        series?)
(assign-operation '*          coefficient*series   not-series?    series?)
(assign-operation '*          series*coefficient   series?        not-series?)

(assign-operation '/          series:div           series?        series?)
(assign-operation '/          coefficient/series   not-series?    series?)
(assign-operation '/          series/coefficient   series?        not-series?)


(assign-operation 'expt       series:expt          series?        exact-integer?)

;(assign-operation 'exp       exp-series           operator?)

(assign-operation 'partial-derivative series:derivative    series? any?)
(assign-operation 'apply              series:value         series? any?)



#| what to do here ???

		     `(integrate ,*integrate-series)
		     `(integrate-tail ,integral-series-tail)
		     `(partial-sums ,partial-sums)
		     `(->function ,->function)
		     `(function-> ,function->)

|#

;;; The coefficients of (1+x)^a
(define (binomial-series a)
  (define (binomial-helper a n c)
    (if (g:= a 0)
	(cons-stream c zero)
	(cons-stream c
	  (binomial-helper (g:- a 1) (g:+ n 1) (g:/ (g:* c a) n)))))
  (make-series *exactly-one* (binomial-helper a 1 1)))


;;; without macros

(define cos-series
  (make-series *exactly-one*
    (cons-stream 1
		 (negate-stream (integral-series-tail sin-series)))))

(define sin-series
  (make-series *exactly-one*
	       (cons-stream 0 (integral-series-tail cos-series))))

(define exp-series
  (make-series *exactly-one*
	       (cons-stream 1 (integral-series-tail exp-series))))

(define cosh-series
  (make-series *exactly-one*
	       (cons-stream 1 (integral-series-tail sinh-series))))

(define sinh-series
  (make-series *exactly-one*
	       (cons-stream 0 (integral-series-tail cosh-series))))

(define tan-series
  (series:div sin-series cos-series))

(define atan-series  
  (let ()
    (define (atan-helper n s)
      (if (even? n) 
	  (cons-stream 0 (atan-helper (+ n 1) s))
	  (cons-stream (* s (/ 1 n))
		       (atan-helper (+ n 1) (- s)))))
    (make-series *exactly-one*
		 (atan-helper 0 1))))

