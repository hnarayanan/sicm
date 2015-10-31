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

;;;            Calculus of Infinitesimals

(declare (usual-integrations))

;;; The idea is that we compute derivatives by passing special
;;; "differential objects" [x,dx] through functions.  A first
;;; approximation to the idea is as follows:

;;;               f
;;;      [x,dx] |---> [f(x), Df(x)*dx]

;;; Note that the derivative of f at the point x, DF(x), is the
;;; coefficient of dx in the result.  If we then pass this result
;;; through another function, we obtain the chain-rule answer we would
;;; hope for.

;;;                         g
;;;      [f(x), Df(x)*dx] |---> [g(f(x)), DG(f(x))*DF(x)*dx]

;;; Thus, we can find the derivative of a composition by this process.
;;; We need only define how each of the primitives act on these
;;; "differentials" and then we can use ordinary Scheme compositions
;;; of these to do the job.  See the procedure diff:derivative near
;;; the bottom to understand how derivatives are computed given this
;;; differential algebra.  This idea was discovered by Dan Zuras and
;;; Gerald Jay Sussman in 1992.

;;; To expand this idea to work for multiple derivatives of functions
;;; of several variables we define an algebra in "infinitesimal
;;; space".  The objects are multivariate power series in which no
;;; incremental has exponent greater than 1.  This was worked out in
;;; detail by Hal Abelson around 1994, and painfully redone in 1997 by
;;; Sussman with the help of Hardy Mayer and Jack Wisdom.

;;;                Data Structure
;;; A differential quantity is a typed list of differential terms,
;;; representing the power series alluded to earlier.  The terms are
;;; kept in a sorted order, in ascending order. (Order is the number
;;; of incrementals.  So dx*dy is higher order than dx or dy.)

(define (make-differential-quantity differential-term-list)
  (cons differential-type-tag differential-term-list))

(define (differential-term-list diff)
  (assert (differential? diff))
  (cdr diff))



(define (differential->terms diff)
  (if (differential? diff)
      (differential-term-list diff)
      (list
       (make-differential-term '() diff))))

(define (terms->differential terms)
  (cond ((null? terms) :zero)
	((and (null? (cdr terms))
	      (null? (differential-tags (car terms))))
	 (differential-coefficient (car terms)))
	(else
	 (make-differential-quantity terms))))


;;; Each differential term has a list of tags.  The tags represent the
;;; incrementals.  Roughly, "dx" and "dy" are tags in the terms: 3*dx,
;;; 4*dy, 2*dx*dy.  There is a tag created for each derivative that is
;;; in progress.  Since the only use of a tag is to distinguish
;;; unnamed incrementals we use positive integers for the tags.

(define (make-differential-term tags coefficient)
  (list tags coefficient))

(define (differential-tags dterm)
  (car dterm))

(define (differential-coefficient dterm)
  (cadr dterm))

(define (differential-of x)
  (let lp ((x x))
    (if (differential? x)
	(lp (differential-coefficient
	     (car (differential-term-list x))))
	x)))

(define (diff:arity x)
  (let lp ((x x))
    (if (differential? x)
	(lp (differential-coefficient
	     (car (differential-term-list x))))
	(g:arity x))))

(define (diff:apply diff args)
  (terms->differential
   (map (lambda (dterm)
	  (make-differential-term (differential-tags dterm)
				  (g:apply (differential-coefficient dterm)
					   args)))
	(differential->terms diff))))

;;; Differential tags lists are ordinary lists of positive integers,
;;; so we can use Scheme list-manipulation procedures on them.
;;; Differential tag lists are also kept in sorted order, so they can
;;; be compared with EQUAL? and can be used to sort the terms.

;;; To be sorted, and to make it possible to collect like terms, the
;;; terms need to be compared using their tag lists.

(define (same-differential-tags? dterm1 dterm2)
  (equal? (differential-tags dterm1)
	  (differential-tags dterm2)))

(define (<differential-tags? dterm1 dterm2)
  (let ((dts1 (differential-tags dterm1))
	(dts2 (differential-tags dterm2)))
    (let ((l1 (length dts1))
	  (l2 (length dts2)))
      (or (fix:< l1 l2)
	  (and (fix:= l1 l2)
	       (<dts dts1 dts2))))))

(define (<dts dts1 dts2)
  (cond ((null? dts1) #f)
	((<dt (car dts1) (car dts2)) #t)
	(else (<dts (cdr dts1) (cdr dts2)))))


;;; Each tag is represented by a small integer, and a new one is made
;;; by calling MAKE-DIFFERENTIAL-TAG.

(define differential-tag-count 0)

(define (make-differential-tag)
  (set! differential-tag-count
	(+ differential-tag-count 1))
  differential-tag-count)

(define (<dt dt1 dt2)
  (< dt1 dt2))

(define (=dt dt1 dt2)
  (= dt1 dt2))

;;; Set operations on differential tag lists preserve order:

(define (union-differential-tags set1 set2)
  (cond ((null? set1) set2)
	((null? set2) set1)
	((=dt (car set1) (car set2))
	 (cons (car set1)
	       (union-differential-tags (cdr set1)
					(cdr set2))))
	((<dt (car set1) (car set2))
	 (cons (car set1)
	       (union-differential-tags (cdr set1)
					set2)))
	(else
	 (cons (car set2)
	       (union-differential-tags set1
					(cdr set2))))))

(define (intersect-differential-tags set1 set2)
  (cond ((null? set1) '())
	((null? set2) '())
	((=dt (car set1) (car set2))
	 (cons (car set1)
	       (intersect-differential-tags (cdr set1)
					    (cdr set2))))
	((<dt (car set1) (car set2))
	 (intersect-differential-tags (cdr set1)
				      set2))
	(else
	 (intersect-differential-tags set1
				      (cdr set2)))))

;;; Differential term lists represent a kind of power series, so they
;;; can be added and multiplied.  It is important to note that when
;;; terms are multiplied, no contribution is made if the terms being
;;; multiplied have a differential tag in common.  Thus dx^2 = zero.

(define (dtl:+ xlist ylist)
  (cond ((null? xlist) ylist)
	((null? ylist) xlist)
	((same-differential-tags? (car xlist) (car ylist))
	 (let ((ncoeff
		(g:+ (differential-coefficient (car xlist))
		     (differential-coefficient (car ylist)))))
	   (if (g:zero? ncoeff)                 ;;(exact-zero? ncoeff)
	       (dtl:+ (cdr xlist) (cdr ylist))
	       (cons (make-differential-term
		      (differential-tags (car xlist))
		      ncoeff)
		     (dtl:+ (cdr xlist) (cdr ylist))))))
	((<differential-tags? (car xlist) (car ylist))
	 (cons (car xlist) (dtl:+ (cdr xlist) ylist)))
	(else
	 (cons (car ylist) (dtl:+ xlist (cdr ylist))))))

(define (dtl:* xlist ylist)
  (if (null? xlist)
      '()
      (dtl:+ (tdtl:* (car xlist) ylist)
	     (dtl:* (cdr xlist) ylist))))

(define (tdtl:* term terms)
  (let ((tags (differential-tags term))
	(coeff (differential-coefficient term)))
    (let lp ((terms terms))
      (if (null? terms)
	  '()
	  (let ((tags1 (differential-tags (car terms))))
	    (if (null? (intersect-differential-tags tags tags1))
		(cons (make-differential-term
		       (union-differential-tags tags tags1)
		       (g:* coeff
			    (differential-coefficient (car terms))))
		      (lp (cdr terms)))
		(lp (cdr terms))))))))

;;; Here we have the primitive addition and multiplication that
;;; everything else is built on.

(define (d:+ u v)
  (terms->differential
   (dtl:+ (differential->terms u)
	  (differential->terms v))))

(define (d:* u v)
  (terms->differential
   (dtl:* (differential->terms u)
	  (differential->terms v))))

;;; To turn a unary function into one that operates on differentials
;;; we must supply the derivative.  This is the essential chain rule.

(define (diff:unary-op f df/dx)
  (define (uop x)
      (let ((lox (finite-part x)))
	(d:+ (f lox)
	     (d:* (df/dx lox)
		  (infinitesimal-part x)))))
  (diff-memoize-1arg uop))

;;; The finite-part is all terms except for terms containing the
;;; highest numbered differential tag in a term of highest order, and
;;; infinitesimal-part is the remaining terms, all of which contain
;;; that differential tag.  So:

;;;                           f
;;;    x + dx + dy + dx*dy |----> f(x+dx) + Df(x+dx)*(dy+dx*dy)

;;; Alternatively, we might have computed the following, but we think
;;; that the ultimate values of derivatives don't care, because mixed
;;; partials of R^2 --> R commute.

;;;    x + dx + dy + dx*dy |----> f(x+dy) + Df(x+dy)*(dx+dx*dy)

;;; We see in the following code that we have made the wrong choice of
;;; order in the sort of the terms and the tags.  Probably this
;;; doesn't matter, but it is ugly.

(define (finite-part x)
  (if (differential? x)
      (let ((dts (differential->terms x)))
	(let ((keytag
	       (car (last-pair (differential-tags (car (last-pair dts)))))))
	  (terms->differential-collapse
	   (filter (lambda (term)
		     (not (memv keytag (differential-tags term))))
		   dts))))
      x))

(define (infinitesimal-part x)
  (if (differential? x)
      (let ((dts (differential->terms x)))
	(let ((keytag
	       (car (last-pair (differential-tags (car (last-pair dts)))))))
	  (terms->differential-collapse
	   (filter (lambda (term)
		     (memv keytag (differential-tags term)))
		   dts))))
      :zero))

(define (terms->differential-collapse terms)
  (terms->differential
   (reduce dtl:+ '() (map list terms))))


;;; To turn a binary function into one that operates on differentials
;;;  we must supply the partial derivatives with respect to each
;;;  argument.

#|
;;; This is the basic idea, but it often does too much work.

(define (diff:binary-op f df/dx df/dy)
  (define (bop x y)
      (let ((mt (max-order-tag x y)))
	(let ((dx (with-tag x mt))
	      (dy (with-tag y mt))
	      (xe (without-tag x mt))
	      (ye (without-tag y mt)))
	  (d:+ (f xe ye)
	       (d:+ (d:* dx (df/dx xe ye))
		    (d:* (df/dy xe ye) dy))))))
  (diff-memoize-2arg bop))
|#

;;; Here, we only compute a partial derivative if the increment in
;;; that direction is not known to be zero.

(define (diff:binary-op f df/dx df/dy)
  (define (bop x y)
      (let ((mt (max-order-tag x y)))
	(let ((dx (with-tag x mt))
	      (dy (with-tag y mt))
	      (xe (without-tag x mt))
	      (ye (without-tag y mt)))
	  (let ((a (f xe ye)))
	    (let ((b
		   (if (and (number? dx) (zero? dx))
		       a
		       (d:+ a (d:* dx (df/dx xe ye))))))
	      (let ((c
		     (if (and (number? dy) (zero? dy))
			 b
			 (d:+ b (d:* (df/dy xe ye) dy)))))
		c))))))
  (diff-memoize-2arg bop))

;;; For multivariate functions we must choose the finite-part and the
;;; infinitesimal-part of each input to be consistent with respect to
;;; the differential tag, we do this as follows:

(define (max-order-tag . args)
  (car (last-pair
	(a-reduce union-differential-tags
		  (map (lambda (arg)
			 (differential-tags
			  (car (last-pair (differential->terms arg)))))
		       args)))))

(define (without-tag x keytag)
  (if (differential? x)
      (let ((dts (differential->terms x)))
	(terms->differential-collapse
	 (filter (lambda (term)
		   (not (memv keytag (differential-tags term))))
		 dts)))
      x))

(define (with-tag x keytag)
  (if (differential? x)
      (let ((dts (differential->terms x)))
	(terms->differential-collapse
	 (filter (lambda (term)
		   (memv keytag (differential-tags term)))
		 dts)))
      :zero))


#|
;;; More generally, but not used in this file:

(define (diff:nary-op f partials)
  (define (nop . args)
    (diff:nary f partials args))
  (diff-memoize nop))

(define (diff:nary f partials args)
  (let ((mt (apply max-order-tag args)))
    (let ((es (map (lambda (arg) (without-tag arg mt)) args))
	  (ds (map (lambda (arg) (with-tag arg mt)) args)))
      (d:+ (g:apply f es)
	   (a-reduce d:+
		     (map (lambda (p d)
			    (d:* (g:apply p es) d))
			  partials
			  ds))))))
|#

(define diff:+
  (diff:binary-op g:+
		  (lambda (x y) 1)
		  (lambda (x y) 1)))


(define diff:-
  (diff:binary-op g:-
		  (lambda (x y) 1)
		  (lambda (x y) -1)))


(define diff:*
  (diff:binary-op g:*
		  (lambda (x y) y)
		  (lambda (x y) x)))


(define diff:/
  (diff:binary-op g:/
		  (lambda (x y)
		    (g:/ 1 y))
		  (lambda (x y)
		    (g:* -1 (g:/ x (g:square y))))))


(define diff:negate
  (diff:unary-op (lambda (x)
		   (g:* -1 x))
		 (lambda (x)
		   -1)))

(define diff:invert
  (diff:unary-op (lambda (x)
		   (g:/ 1 x))
		 (lambda (x)
		   (g:/ -1 (g:square x)))))


(define diff:sqrt
  (diff:unary-op g:sqrt
		 (lambda (x)
		   (g:/ 1 (g:* 2 (g:sqrt x))))))


;;; Breaking off the simple (lambda (x) (expt x n)) case:

(define diff:power
  (diff:binary-op g:expt
		  (lambda (x y)
		    (g:* y (g:expt x (g:- y 1))))
		  (lambda (x y)
		    (error "Should not get here: DIFF:POWER" x y))))

(define diff:expt
  (diff:binary-op g:expt
		  (lambda (x y)
		    (g:* y (g:expt x (g:- y 1))))
		  (lambda (x y)
		    (if (and (number? x) (zero? x))
			(if (number? y)
			    (if (positive? y)
				:zero
				(error "Derivative undefined: EXPT" x y))
			    :zero) ;But what if y is negative later?
			(g:* (g:log x) (g:expt x y))))))


(define diff:exp
  (diff:unary-op g:exp g:exp))

(define diff:log
  (diff:unary-op g:log (lambda (x) (g:/ 1 x))))

(define diff:sin
  (diff:unary-op g:sin g:cos))

(define diff:cos
  (diff:unary-op g:cos
		 (lambda (x)
		   (g:* -1 (g:sin x)))))

(define diff:asin
  (diff:unary-op g:asin
		 (lambda (x)
		   (g:/ 1
			(g:sqrt
			 (g:- 1 (g:square x)))))))

(define diff:acos
  (diff:unary-op g:acos
		 (lambda (x)
		   (g:* -1
			(g:/ 1
			     (g:sqrt
			      (g:- 1 (g:square x))))))))


(define diff:atan1
  (diff:unary-op g:atan1
		 (lambda (x)
		   (g:/ 1
			(g:+ 1 (g:square x))))))

(define diff:atan2 
  (diff:binary-op g:atan2
		  (lambda (y x)
		    (g:/ x
			 (g:+ (g:square x)
			      (g:square y))))
		  (lambda (y x)
		    (g:/ (g:* -1 y)
			 (g:+ (g:square x)
			      (g:square y))))))


(define diff:sinh
  (diff:unary-op g:sinh g:cosh))

(define diff:cosh
  (diff:unary-op g:cosh g:sinh))


(define (diff:abs x)
  (let ((f (finite-part x)))
    ((cond ((g:< f 0)
	    (diff:unary-op (lambda (x) x) (lambda (x) -1)))
	   ((g:= f 0)
	    (error "Derivative of ABS undefined at zero"))
	   ((g:> f 0)
	    (diff:unary-op (lambda (x) x) (lambda (x) +1)))
	   (else
	    (error "Derivative of ABS at" x)))
     x)))

;;; Funny functions -- needs singularity distributions?
;;; known-real? means provably real.  e.g. a polynomial in *known-reals*

(define (diff:conjugate z)
  #| ;; This cannot really work.
  (terms->differential
   (map (lambda (term)
	  (make-differential-term (differential-tags term)
				  (g:conjugate (differential-coefficient term))))
	(differential->terms z)))
  |#
  (if (not (known-real? (finite-part z)))
      (error "Not real -- DIFF:CONJUGATE" z))
  ((diff:unary-op (lambda (x) x) (lambda (x) 1)) z))


(define (diff:real-part z)
  (if (not (known-real? (finite-part z)))
      (error "Not real -- DIFF:REAL-PART" z))
  ((diff:unary-op (lambda (x) x) (lambda (x) 1)) z))

(define (diff:imag-part z)
  (if (not (known-real? (finite-part z)))
      (error "Not real -- DIFF:IMAG-PART" z))
  ((diff:unary-op (lambda (x) 0) (lambda (x) 0)) z))

(define (diff:magnitude z)
  #|
  (if (not (known-real? (finite-part z)))
      (error "Not real -- DIFF:MAGNITUDE" z))
  ;; Could be z or -z
  (error "Unimplemented -- DIFF:MAGNITUDE" z)
  |#
  (diff:abs z))

(define (diff:angle z)
  #|
  (if (not (known-real? (finite-part z)))
      (error "Not real -- DIFF:ANGLE" z))
  ;; Could be 1 or -1
  (error "Unimplemented -- DIFF:ANGLE" z)
  |#
  (let ((f (finite-part z)))
    ((cond ((g:< f 0)
	    (diff:unary-op (lambda (x) x) (lambda (x) :zero)))
	   ((g:= f 0)
	    (error "Derivative of ABS undefined at zero"))
	   ((g:> f 0)
	    (diff:unary-op (lambda (x) x) (lambda (x) :zero)))
	   (else
	    (error "Derivative of ABS at" z)))
     z)))

(define (diff:type x) differential-type-tag)
(define (diff:type-predicate x) differential?)

(define (diff:zero-like n) :zero)
(define (diff:one-like n) :one)

(assign-operation 'type            diff:type             differential?)
(assign-operation 'type-predicate  diff:type-predicate   differential?)
(assign-operation 'arity           diff:arity            differential?)
(assign-operation 'apply           diff:apply            differential? any?)


;;; arity?, inexact?

(assign-operation 'zero-like       diff:zero-like        differential?)
(assign-operation 'one-like        diff:one-like         differential?)

;;; zero?, one?
(assign-operation 'negate          diff:negate           differential?)
(assign-operation 'invert          diff:invert           differential?)

(assign-operation 'sqrt            diff:sqrt             differential?)

(assign-operation 'exp             diff:exp              differential?)
(assign-operation 'log             diff:log              differential?)

(assign-operation 'sin             diff:sin              differential?)
(assign-operation 'cos             diff:cos              differential?)
(assign-operation 'asin            diff:asin             differential?)
(assign-operation 'acos            diff:acos             differential?)
(assign-operation 'atan1           diff:atan1            differential?)
(assign-operation 'sinh            diff:sinh             differential?)
(assign-operation 'cosh            diff:cosh             differential?)

(assign-operation '+               diff:+                differential? not-compound?)
(assign-operation '+               diff:+                not-compound? differential?)
(assign-operation '-               diff:-                differential? not-compound?)
(assign-operation '-               diff:-                not-compound? differential?)
(assign-operation '*               diff:*                differential? not-compound?)
(assign-operation '*               diff:*                not-compound? differential?)
(assign-operation '/               diff:/                differential? not-compound?)
(assign-operation '/               diff:/                not-compound? differential?)

(assign-operation 'dot-product     diff:*                differential? not-compound?)

(assign-operation 'expt     diff:power  differential? (negation differential?))
(assign-operation 'expt     diff:expt   not-compound?          differential?)

(assign-operation 'atan2           diff:atan2            differential? not-compound?)
(assign-operation 'atan2           diff:atan2            not-compound? differential?)

(assign-operation 'abs             diff:abs              differential?)

;;; The following are not completely thought out!  Needs more work.

(assign-operation 'conjugate       diff:conjugate        differential?)
(assign-operation 'real-part       diff:real-part        differential?)
(assign-operation 'imag-part       diff:imag-part        differential?)
(assign-operation 'magnitude       diff:magnitude        differential?)
(assign-operation 'angle           diff:angle            differential?)

;;; This stuff allows derivatives to work in code where there are
;;; conditionals if the finite parts are numerical.


(define (diff:zero? x)
  (assert (differential? x))
  (for-all? (differential-term-list x)
    (lambda (term)
      (let ((c (differential-coefficient term)))
	(g:zero? c)))))

(assign-operation 'zero? diff:zero? differential?)

(define (diff:one? x)
  (assert (differential? x))
  (and (g:one? (finite-part x))
       (diff:zero? (infinitesimal-part x))))

(assign-operation 'one? diff:one? differential?)

#|
;;; this does not slow down derivatives!
(define (diff:zero? x) #f)

(assign-operation 'zero? diff:zero? differential?)


(define (diff:one? x) #f)

(assign-operation 'one? diff:one? differential?)
|#


(define (diff:binary-comparator p)
  (define (bp? x y)
    (let ((xe (finite-part x))
	  (ye (finite-part y)))
      (p xe ye)))
  bp?)


(assign-operation '= (diff:binary-comparator g:=) differential? not-compound?)
(assign-operation '= (diff:binary-comparator g:=) not-compound? differential?)

(assign-operation '< (diff:binary-comparator g:<) differential? not-compound?)
(assign-operation '< (diff:binary-comparator g:<) not-compound? differential?)

(assign-operation '<= (diff:binary-comparator g:<=) differential? not-compound?)
(assign-operation '<= (diff:binary-comparator g:<=) not-compound? differential?)

(assign-operation '> (diff:binary-comparator g:>) differential? not-compound?)
(assign-operation '> (diff:binary-comparator g:>) not-compound? differential?)

(assign-operation '>= (diff:binary-comparator g:>=) differential? not-compound?)
(assign-operation '>= (diff:binary-comparator g:>=) not-compound? differential?)

;;; The derivative of a univariate function over R is a value
;;;  ((derivative (lambda (x) (expt x 5))) 'a)
;;;    ==> (* 5 (expt a 4))

(define (diff:derivative f)
  (define (the-derivative x)
    (simple-derivative-internal f x))
  the-derivative)


;;; SIMPLE-DERIVATIVE-INTERNAL represents the essential computation.
;;; To compute the derivative of function f at point x, make a new
;;; differential object with incremental part 1 (the coefficient of
;;; the new differential tag, dx) and with constant part x.  We pass
;;; this through the function f, and then extract the terms which
;;; contain the the differential tag dx, removing that tag.  This
;;; leaves the derivative.

;;;                           f
;;;                 x + dx |----> f(x) + Df(x)*dx
;;;                  \                  /
;;;                   \                /
;;;                    \     Df       /
;;;                      x |----> Df(x)


(define (simple-derivative-internal f x)
  (let ((dx (make-differential-tag)))
    (extract-dx-part dx (f (make-x+dx x dx)))))

(define (make-x+dx x dx)
  (d:+ x
       (make-differential-quantity
	(list (make-differential-term (list dx) :one))))) ;worry!!!


;;; For debugging, sometimes we need a differential object.

(define (differential-object x)
  (let ((dx (make-differential-tag)))
    (list (make-x+dx x dx) dx)))

;;; This is not quite right... It is only good for testing R-->R stuff.
;;; Now disabled, see DERIV.SCM.

;(assign-operation 'derivative      diff:derivative       function?)

#|
;;; See the long story below: Alexey's Amazing Bug!

(define (extract-dx-part dx obj)
  (define (extract obj)
    (if (differential? obj)
	(terms->differential-collapse
	 (append-map
	  (lambda (term)
	    (let ((tags (differential-tags term)))
	      (if (memv dx tags)
		  (list (make-differential-term (delv dx tags)
			  (differential-coefficient term)))
		  '())))
	  (differential-term-list obj)))
	:zero))
  (define (dist obj)
    (cond ((structure? obj)
	   (s:map/r dist obj))
	  ((matrix? obj)
	   ((m:elementwise dist) obj))
	  ((function? obj)
	   (compose dist obj))
	  ((operator? obj)
	   (g:* (make-operator dist 'extract (operator-subtype obj))
		obj))
	  ((series? obj)
	   (make-series (g:arity obj)
			(map-stream dist (series->stream obj))))
	  (else (extract obj))))
  (dist obj))
|#

#|
;;; The following hairy code was introduced to fix the "Amazing Bug"
;;; discovered by Alexey Radul from a suggestion by Barak Perlmutter
;;; (June 2011)!  Here is the bug.

(define (((f x) g) y)
  (g (+ x y)))
#| f |#

(define f-hat ((D f) 3))
#| f-hat |#

(((f-hat exp) 5)
#| 2980.9579870417283 |#

((f-hat (f-hat exp)) 5)
#| 0 |#			;WRONG!
|#

#|
;;; BTW.
;;; Alexey Radul, Jeff Siskind, and Oleksandr Manzyuk give different
;;; names to the procedures.  They write:

; derivative :: (R -> a) -> R -> a
(define (derivative f)
  (let ((epsilon (gensym)))
    (lambda (x)
      (tangent epsilon (f (make-bundle epsilon x 1))))))

;;; They think of what I call a differential object as an element of
;;; the tangent bundle and thus use "make-bundle" for my "make-x+dx".
;;; Of course, it is a bound tangent vector (bound to the point x).
;;; We generalize the functions (f) to operate on such bound vectors
;;; and then extract the free tangent vector from the resulting bound
;;; vector.  To extract the free vector they use the name "tangent"
;;; where I use "extract-dx-part".
|#

(define (extract-dx-part dx obj)
  (define (extract obj)
    (if (differential? obj)
	(terms->differential-collapse
	 (append-map
	  (lambda (term)
	    (let ((tags (differential-tags term)))
	      (if (memv dx tags)
		  (list
		   (make-differential-term (delv dx tags)
					   (differential-coefficient term)))
		  '())))
	  (differential-term-list obj)))
	:zero))
  (define (dist obj)
    (cond ((structure? obj)
	   (s:map/r dist obj))
	  ((matrix? obj)
	   ((m:elementwise dist) obj))
	  ((quaternion? obj)
	   (quaternion
	    (dist (quaternion-ref obj 0))
	    (dist (quaternion-ref obj 1))
	    (dist (quaternion-ref obj 2))
	    (dist (quaternion-ref obj 3))))
	  ((function? obj)
	   (hide-tag-in-procedure dx (compose dist obj)))
	  ((operator? obj)
	   (hide-tag-in-procedure dx
	      (g:* (make-operator dist 'extract (operator-subtype obj))
		   obj)))
	  ((series? obj)
	   (make-series (g:arity obj)
			(map-stream dist (series->stream obj))))
	  (else (extract obj))))
  (dist obj))

(define (hide-tag-in-object tag object)
  (cond ((procedure? object)
	 (hide-tag-in-procedure tag object))
	(else object)))

(define ((hide-tag-in-procedure external-tag procedure) . args)
  (let ((internal-tag (make-differential-tag)))
    (hide-tag-in-object internal-tag
      (apply (wrap-procedure-differential-tags external-tag internal-tag procedure) args))))

(define (wrap-procedure-differential-tags external-tag internal-tag procedure)
  (let ((new
	 (lambda args
	   ((replace-differential-tag internal-tag external-tag)
	    (apply procedure
		   (map (replace-differential-tag external-tag internal-tag)
			args))))))
    (cond ((function? procedure)
	   new)
	  ((operator? procedure)
	   (make-operator new 'composition (operator-subtype procedure)))
	  (else
	   (error "Unknown procedure type -- WRAP-PROCEDURE-DIFFERENTIAL-TAGS:"
		  external-tag internal-tag procedure)))))

(define ((replace-differential-tag oldtag newtag) object)
  (cond ((differential? object)
	 (terms->differential
	  (map (lambda (term)
		 (if (memv oldtag (differential-tags term))
		     (make-differential-term
		      (insert-differential-tag newtag
                       (remove-differential-tag oldtag
                        (differential-tags term)))
		      (differential-coefficient term))
		     term))
	       (differential-term-list object))))
	((procedure? object)
	 (wrap-procedure-differential-tags oldtag newtag object))
	((structure? object)
	 (s:map/r (replace-differential-tag oldtag newtag) object))
	((matrix? object)
	 ((m:elementwise (replace-differential-tag oldtag newtag)) object))
	((quaternion? object)
	 (let ((r (replace-differential-tag oldtag newtag)))
	   (quaternion
	    (r (quaternion-ref object 0))
	    (r (quaternion-ref object 1))
	    (r (quaternion-ref object 2))
	    (r (quaternion-ref object 3)))))
	((series? object)
	 (make-series (g:arity object)
		      (map-stream (replace-differential-tag oldtag newtag)
				  (series->stream object))))
	(else object)))

(define (remove-differential-tag tag tags)
  (delv tag tags))

(define (insert-differential-tag tag tags)
  (cond ((null? tags) (list tag))
	((<dt tag (car tags)) (cons tag tags))
	((=dt tag (car tags))
	 (error "INSERT-DIFFERENTIAL-TAGS:" tag tags))
	(else
	 (cons (car tags)
	       (insert-differential-tag tag (cdr tags))))))

#|     
;;; 2011 buggy version: did not handle derivative functions with multiple args
;;; (define ((f x) y z) (* x y z))
;;; (((D f) 2) 3 4) should return 12, but gets error.

(define ((hide-tag-in-procedure external-tag procedure) x)
  (let ((internal-tag (make-differential-tag)))
    (hide-tag-in-object internal-tag
      (((replace-differential-tag external-tag internal-tag) procedure) x))))

(define ((replace-differential-tag oldtag newtag) object)
  (cond ((differential? object)
	 (terms->differential
	  (map (lambda (term)
		 (if (memv oldtag (differential-tags term))
		     (make-differential-term
		      (insert-differential-tag newtag
                       (remove-differential-tag oldtag
                        (differential-tags term)))
		      (differential-coefficient term))
		     term))
	       (differential-term-list object))))
	((procedure? object)
	 (let ((new
		(compose (replace-differential-tag newtag oldtag)
			 object
			 (replace-differential-tag oldtag newtag))))
	   (cond ((function? object)
		  new)
		 ((operator? object)
		  (make-operator new 'composition (operator-subtype object)))
		 (else
		  (error "Unknown procedure type --REPLACE-DIFFERENTIAL-TAGS:"
			 oldtag newtag object)))))
	((structure? object)
	 (s:map/r (replace-differential-tag oldtag newtag) object))
	((matrix? object)
	 ((m:elementwise (replace-differential-tag oldtag newtag)) object))
	((series? object)
	 (make-series (g:arity object)
		      (map-stream (replace-differential-tag oldtag newtag)
				  (series->stream object))))
	(else object)))
|#
