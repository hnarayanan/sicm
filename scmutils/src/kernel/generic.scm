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

;;;; Primitive Generic Operation Declarations 

(declare (usual-integrations))

;;; Unary Operators 

(define g:type (make-generic-operator 1 'type))

(define g:type-predicate (make-generic-operator 1 'type-predicate))

(define g:arity (make-generic-operator 1 'arity (lambda (x) #f)))


(define g:inexact?
  (make-generic-operator 1 'inexact?))

(define g:zero-like
  (make-generic-operator 1 'zero-like (lambda (x) :zero)))

(define g:one-like
  (make-generic-operator 1 'one-like (lambda (x) :one)))

(define g:identity-like
  (make-generic-operator 1 'identity-like (lambda (x) g:identity)))


;;; Generic tests are conservative.  
;;; They will return #f unless the answer is known true.

(define generic:zero?
  (make-generic-operator 1 'zero? (lambda (x) #f)))

(define (g:zero? x)
    (if (number? x) (exact-zero? x) (generic:zero? x)))


(define generic:one? (make-generic-operator 1 'one? (lambda (x) #f)))

(define (g:one? x)
    (if (number? x) (exact-one? x) (generic:one? x)))


(define g:identity? (make-generic-operator 1 'identity? (lambda (x) #f)))

(define g:negate (make-generic-operator 1 'negate))

(define g:invert (make-generic-operator 1 'invert))

(define g:square (make-generic-operator 1 'square (lambda (x) (g:* x x))))

(define g:sqrt (make-generic-operator 1 'sqrt))

(define g:exp (make-generic-operator 1 'exp))

(define g:log (make-generic-operator 1 'log))

(define g:sin (make-generic-operator 1 'sin))

(define g:cos (make-generic-operator 1 'cos))

(define g:asin (make-generic-operator 1 'asin))

(define g:acos (make-generic-operator 1 'acos))

(define g:sinh (make-generic-operator 1 'sinh))

(define g:cosh (make-generic-operator 1 'cosh))

(define g:abs (make-generic-operator 1 'abs))

(define g:determinant (make-generic-operator 1 'determinant))

(define g:trace
  (make-generic-operator 1
			 'trace 
			 ;;overlays system trace procedure trace-both
			 trace-both))

(define (g:transpose thing #!optional shape)
  (if (default-object? shape)
      (g:transpose-1-arg thing)
      (s:transpose1 thing shape)))


(define g:transpose-1-arg
  (make-generic-operator 1 'transpose))

(define g:dimension
  (make-generic-operator 1
			 'dimension
			 (lambda (x)
			   ;;definition in calculus/manifold.scm
			   (coordinate-system-dimension x))))

(define g:solve-linear
  (make-generic-operator 2 'solve-linear))

;;; Duplicate of text in OPERATOR.SCM, except that the explicit type
;;; tag is here rather than the variable operator-type-tag.  This is
;;; necessary because of a problem of load order.

(define (make-operator p #!optional name subtype arity #!rest opts)
  (if (default-object? name) (set! name #f))
  (if (default-object? subtype) (set! subtype #f))
  (if (default-object? arity) (set! arity (procedure-arity p)))
  (make-apply-hook p `(*operator* ,subtype ,name ,arity ,@opts)))


(define generic:partial-derivative
  (make-generic-operator 2 'partial-derivative))

(define g:derivative
  (make-operator
   (lambda (f)
     (generic:partial-derivative f '()))
   'derivative))

(define (g:partial-derivative f . varspecs)
  (generic:partial-derivative f varspecs))

(define (g:partial . varspecs)
  (make-operator
   (lambda (f)
     (generic:partial-derivative f varspecs))
   `(partial ,@varspecs)))

;;; Binary Operators

(define generic:= (make-generic-operator 2 '= (lambda (x y) #f)))

(define (g:=:bin x y)
  (if (and (number? x) (number? y)) (= x y) (generic:= x y)))


(define generic:< (make-generic-operator 2 '< (lambda (x y) #f)))

(define (g:<:bin x y)
  (if (and (number? x) (number? y)) (< x y) (generic:< x y)))
 

(define generic:<= (make-generic-operator 2 '<= (lambda (x y) #f)))

(define (g:<=:bin x y)
  (if (and (number? x) (number? y)) (<= x y) (generic:<= x y)))


(define generic:> (make-generic-operator 2 '> (lambda (x y) #f)))

(define (g:>:bin x y)
  (if (and (number? x) (number? y)) (> x y) (generic:> x y)))


(define generic:>= (make-generic-operator 2 '>= (lambda (x y) #f)))

(define (g:>=:bin x y)
  (if (and (number? x) (number? y)) (>= x y) (generic:>= x y)))

(define generic:+ (make-generic-operator 2 '+))

(define (g:+:bin x y)
  (cond ((and (number? x) (number? y)) (+ x y))
	((g:zero? x) y)
	((g:zero? y) x)
	(else (generic:+ x y))))


(define generic:- (make-generic-operator 2 '-))

(define (g:-:bin x y)
  (cond ((and (number? x) (number? y)) (- x y))
	((g:zero? y) x)
	((g:zero? x) (g:negate y))
	(else (generic:- x y))))


(define generic:* (make-generic-operator 2 '*))

(define (g:*:bin x y)
  (cond ((and (number? x) (number? y)) (* x y))
	((exact-zero? x) (g:zero-like y))
	((exact-zero? y) (g:zero-like x))
	((g:one? x) y)
	((g:one? y) x)
	(else (generic:* x y))))

;;; In g:*:bin we test for exact (numerical) zero 
;;; because it is possible to produce a wrong-type 
;;; zero here, as follows:

;;;		  |0|             |0|
;;;	  |a b c| |0|   |0|       |0|
;;;	  |d e f| |0| = |0|, not  |0|

;;; We are less worried about the zero? below,
;;; because any invertible matrix is square.


(define generic:/ (make-generic-operator 2 '/))

(define (g:/:bin x y)
  (cond ((and (number? x) (number? y)) (/ x y))
	;; ((g:zero? x) (g:zero-like y))  ; Ancient bug!  No consequence.
	;; ((g:zero? x) x)
	((g:one? y) x)
	(else (generic:/ x y))))

(define generic:expt (make-generic-operator 2 'expt))

(define (g:expt x y)
  (cond ((and (number? x) (number? y)) (n:expt x y))
	;;((g:zero? x) x) ;No! consider 0^{-1}
	((g:one? x) x)
	((g:zero? y) (g:one-like x))
	((g:one? y) x)
	(else (generic:expt x y))))


(define g:gcd:bin (make-generic-operator 2 'gcd))

(define g:dot-product (make-generic-operator 2 'dot-product))
(define g:cross-product (make-generic-operator 2 'cross-product))

(define g:outer-product (make-generic-operator 2 'outer-product))


;;; Complex Operators

(define g:make-rectangular (make-generic-operator 2 'make-rectangular))
(define g:make-polar (make-generic-operator 2 'make-polar))

(define g:real-part (make-generic-operator 1 'real-part))
(define g:imag-part (make-generic-operator 1 'imag-part))

(define g:magnitude (make-generic-operator 1 'magnitude))
(define g:angle (make-generic-operator 1 'angle))

(define g:conjugate (make-generic-operator 1 'conjugate))


;;; Weird operators

(define (g:atan y #!optional x)
  (if (default-object? x) (g:atan1 y) (g:atan2 y x)))

(define g:atan1 (make-generic-operator 1 'atan1))
(define g:atan2 (make-generic-operator 2 'atan2))

(define generic:apply (make-generic-operator 2 'apply))

(define (g:apply f . apply-args)
  (define (collapse l)
    (if (null? (cdr l))
	(car l)
	(cons (car l)
	      (collapse (cdr l)))))
  (if (null? apply-args)
      (error "No argument list for G:APPLY")
      (let ((args (collapse apply-args)))
	(cond ((procedure? f)
	       (apply f args))
	      ((applicable-literal? f)
	       (apply
		(literal-function f
		  (permissive-function-type (length args)))
		args))
	      #|
	      ((eq? f second)
	       (apply (access second system-global-environment)
		      args))
	      |#
	      (else
	       (generic:apply f args))))))

(define (applicable-literal? f)
  (and (symbol? f) *enable-literal-apply*))

;;; *enable-literal-apply* is modulated by with-literal-apply-enabled.  
;;; This procedure is defined in extapply.scm.
;;; This feature is used explicitly in ode/interface.scm.

;;; N-ary Operator extensions

(define (g:= . args)
  (g:=:n args))

(define (g:=:n args)
  (cond ((null? args) #t)
	((null? (cdr args)) #t)
	(else
	 (let lp ((args (cddr args))
		  (larg (cadr args))
		  (ans (g:=:bin (car args) (cadr args))))
	   (if (null? args)
	       ans
	       (lp (cdr args)
		   (car args)
		   (and ans (g:=:bin larg (car args)))))))))


(define (g:< . args)
  (g:<:n args))

(define (g:<:n args)
  (cond ((null? args) #t)
	((null? (cdr args)) #t)
	(else
	 (let lp ((args (cddr args))
		  (larg (cadr args))
		  (ans (g:<:bin (car args) (cadr args))))
	   (if (null? args)
	       ans
	       (lp (cdr args)
		   (car args)
		   (and ans (g:<:bin larg (car args)))))))))

(define (g:<= . args)
  (g:<=:n args))

(define (g:<=:n args)
  (cond ((null? args) #t)
	((null? (cdr args)) #t)
	(else
	 (let lp ((args (cddr args))
		  (larg (cadr args))
		  (ans (g:<=:bin (car args) (cadr args))))
	   (if (null? args)
	       ans
	       (lp (cdr args)
		   (car args)
		   (and ans (g:<=:bin larg (car args)))))))))

(define (g:> . args)
  (g:>:n args))

(define (g:>:n args)
  (cond ((null? args) #t)
	((null? (cdr args)) #t)
	(else
	 (let lp ((args (cddr args))
		  (larg (cadr args))
		  (ans (g:>:bin (car args) (cadr args))))
	   (if (null? args)
	       ans
	       (lp (cdr args)
		   (car args)
		   (and ans (g:>:bin larg (car args)))))))))


(define (g:>= . args)
  (g:>=:n args))

(define (g:>=:n args)
  (cond ((null? args) #t)
	((null? (cdr args)) #t)
	(else
	 (let lp ((args (cddr args))
		  (larg (cadr args))
		  (ans (g:>=:bin (car args) (cadr args))))
	   (if (null? args)
	       ans
	       (lp (cdr args)
		   (car args)
		   (and ans (g:>=:bin larg (car args)))))))))

(define (g:+ . args)
  (g:+:n args))

(define (g:+:n args)
  (cond ((null? args) :zero)
	((null? (cdr args)) (car args))
	(else
	 (let lp ((args (cddr args))
		  (ans (g:+:bin (car args) (cadr args))))
	   (if (null? args)
	       ans
	       (lp (cdr args)
		   (g:+:bin ans (car args))))))))

(define (g:* . args)
  (g:*:n args))

(define (g:*:n args)
  (cond ((null? args) :one)
	((null? (cdr args)) (car args))
	(else
	 (let lp ((args (cddr args))
		  (ans (g:*:bin (car args) (cadr args))))
	   (if (null? args)
	       ans
	       (lp (cdr args)
		   (g:*:bin ans (car args))))))))

(define (g:- . args)
  (g:-:n args))

(define (g:-:n args)
  (cond ((null? args) :zero)
	((null? (cdr args)) (g:negate (car args)))
	(else
	 (g:-:bin (car args)
		  (g:+:n (cdr args))))))

(define (g:/ . args)
  (g:/:n args))

(define (g:/:n args)
  (cond ((null? args) :one)
	((null? (cdr args)) (g:invert (car args)))
	(else
	 (g:/:bin (car args)
		  (g:*:n (cdr args))))))

(define (g:gcd . args)
  (g:gcd:n args))

(define (g:gcd:n args)
  (cond ((null? args) :zero)
	((null? (cdr args)) (car args))
	(else
	 (let lp
	     ((as (cddr args))
	      (ans (g:gcd:bin (car args) (cadr args))))
	   (cond ((null? as) ans)
		 ((g:one? ans) ans)
		 (else
		  (lp (cdr as) (g:gcd:bin ans (car as)))))))))

