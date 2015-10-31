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

;;;;              NUMSYMB.SCM

(declare (usual-integrations))

;;; Algebraic constructors for symbolic experiments.

;;; Enable simplification on construction -- wastes time?
(define enable-constructor-simplifications? #t)

(define (enable-constructor-simplifications doit?)
  (assert (boolean? doit?) "argument must be a boolean.")
  (clear-memoizer-tables)
  (set! enable-constructor-simplifications? doit?))

;;; Disable intermediate simplification -- wastes time.
(define incremental-simplifier #f)

(define symbolic-operator-table (make-eq-hash-table))

(define (make-numsymb-expression operator-symbol operands)
  (let ((operand-expressions (map numerical-expression operands)))
    (let ((v (hash-table/get symbolic-operator-table operator-symbol #f)))
      (if v
	  (let ((newexp (apply v operand-expressions)))
	    (make-literal number-type-tag
			  (if incremental-simplifier
			      (incremental-simplifier newexp)
			      newexp)))
	  (make-combination number-type-tag operator-symbol operands)))))

(define (addto-symbolic-operator-table operator procedure)
  (hash-table/put! symbolic-operator-table operator procedure))


;;; currently disabled -- see heuristic.scm
(define heuristic-number-canonicalizer #f)

#|
;;; From general/canonicalizer.scm
(define numerical-expression-canonicalizer
  (make-expression-canonicalizer))
|#

(define numerical-expression-canonicalizer #f)

(define (numerical-expression expr)
  (cond ((number? expr)
	 (if (and (inexact? expr) heuristic-number-canonicalizer)
	     (heuristic-number-canonicalizer expr)
	     expr))
	((symbol? expr) expr)
	((literal-number? expr)
	 (if numerical-expression-canonicalizer
	     (numerical-expression-canonicalizer (expression-of expr))
	     (expression-of expr))
	 (expression-of expr))
	((pair? expr)
	 (cond ((memq (car expr) type-tags) expr)
	       (numerical-expression-canonicalizer
		(numerical-expression-canonicalizer expr))
	       (else expr)))
	(else expr)))

(define (make-rectangular? exp)
  (and (pair? exp) (eq? (car exp) 'make-rectangular)))

(define (symb:make-rectangular r i)
  (cond ((exact-zero? i) r)
	((and (real? r) (real? i))
	 (make-rectangular r i))
	(else
	 (symb:add r (symb:mul +i i)))))
(addto-symbolic-operator-table 'make-rectangular symb:make-rectangular)


(define (make-polar? exp)
  (and (pair? exp) (eq? (car exp) 'make-polar)))

(define (symb:make-polar m a)
  (cond ((exact-zero? m) m)
	((exact-zero? a) m)
	((and (real? m) (real? a))
	 (make-polar m a))
	(else
	 (symb:mul m
		   (symb:add (symb:cos a)
			     (symb:mul +i (symb:sin a)))))))
(addto-symbolic-operator-table 'make-polar symb:make-polar)


(define (real-part? exp)
  (and (pair? exp) (eq? (car exp) 'real-part)))

(define (symb:real-part z)
  (cond ((complex? z) (real-part z))
	;;((symbol? z) `(real-part ,z))
	(else
	 (symb:mul 1/2
		   (symb:add z (symb:conjugate z))))))
(addto-symbolic-operator-table 'real-part symb:real-part)


(define (imag-part? exp)
  (and (pair? exp) (eq? (car exp) 'imag-part)))

(define (symb:imag-part z)
  (cond ((complex? z) (imag-part z))
	;;((symbol? z) `(imag-part ,z))
	(else
	 (symb:mul 1/2
		   (symb:mul -i
			     (symb:- z (symb:conjugate z)))))))
(addto-symbolic-operator-table 'imag-part symb:imag-part)

(define (magnitude? exp)
  (and (pair? exp) (eq? (car exp) 'magnitude)))

(define (symb:magnitude z)
  (if (number? z)
      (if (exact? z)
	  (let ((m (magnitude z)))
	    (if (exact? m)
		m
		(symb:magexpr z)))
	  (magnitude z))
      (symb:magexpr z)))

(define (symb:magexpr z)
  (symb:sqrt (symb:mul (symb:conjugate z) z)))
(addto-symbolic-operator-table 'magnitude symb:magnitude)

(define (angle? exp)
  (and (pair? exp) (eq? (car exp) 'angle)))

(define (symb:angle z)
  (if (number? z)
      (if (exact? z)
	  (let ((a (angle z)))
	    (if (exact? a)
		a
		(symb:anglexpr z)))
	  (angle z))
      (symb:anglexpr z)))

(define (symb:anglexpr z)
  (symb:atan (symb:imag-part z) (symb:real-part z)))
(addto-symbolic-operator-table 'angle symb:angle)

(define (conjugate? exp)
  (and (pair? exp) (eq? (car exp) 'conjugate)))

(define (symb:conjugate z)
  (cond ((complex? z) (conjugate z))
        ((known-real? z) z)
        ((and (pair? z)
              (memq (operator z)
                    *conjugate-transparent-operators*))
         (cons (operator z)
               (map symb:conjugate (operands z))))
        (else
         `(conjugate ,z))))
(addto-symbolic-operator-table 'conjugate symb:conjugate)

(define *conjugate-transparent-operators*
  '(negate invert square cube
    sqrt exp log exp2 exp10 log2 log10
    sin cos tan sec csc
    asin acos atan
    sinh cosh tanh sech csch
    + - * / expt up down))

(define (symb:& numexp u1 #!optional u2)
  (if (default-object? u2)
      `(& ,numexp ,u1)
      `(& ,numexp ,u1 ,u2)))

(addto-symbolic-operator-table '& symb:&)


(define (equality? x)
  (and (pair? x) (eq? (car x) '=)))

(define (symb:=:bin a1 a2)
  (if (number? a1)
      (if (number? a2)
	  (and (exact? a1) (exact? a2) (= a1 a2))
	  #f)
      (if (number? a2)
	  #f
	  (if (equal? a1 a2)
	      #t
	      `(= ,a1 ,a2)))))

(define (symb:= . args)
  (cond ((null? args) #t)
	((null? (cdr args)) #t)
	(else
	 (let lp ((args (cddr args))
		  (larg (cadr args))
		  (ans (symb:=:bin (car args) (cadr args))))
	   (if (null? args)
	       ans
	       (lp (cdr args)
		   (car args)
		   (and ans (symb:=:bin larg (car args)))))))))

(addto-symbolic-operator-table '= symb:=)

(define (symb:zero? x)
  (cond ((number? x) (zero? x))
	(else `(= ,:zero ,x))))
(addto-symbolic-operator-table 'zero? symb:zero?)

(define (symb:one? x)
  (cond ((number? x) (one? x))
	(else `(= ,:one ,x))))
(addto-symbolic-operator-table 'one? symb:one?)

;;; Support for addition and subtraction

(define (sum? x)
  (and (pair? x) (eq? (car x) '+)))

(define (symb:addends expr) (cdr expr))

(define (symb:+ a1 a2)
  (cond ((and (number? a1) (number? a2)) (+ a1 a2))
        ((number? a1)
	 (cond ((zero? a1) a2)
	       ((sum? a2)
		`(+ ,a1 ,@(operands a2)))
	       (else `(+ ,a1 ,a2))))
        ((number? a2)
	 (cond ((zero? a2) a1)
	       ((sum? a1)
		`(+ ,a2 ,@(operands a1)))
	       (else `(+ ,a2 ,a1))))
	((sum? a1)
	 (cond ((sum? a2)
		`(+ ,@(operands a1) ,@(operands a2)))
	       (else `(+ ,@(operands a1) ,a2))))
	((sum? a2)
	 `(+ ,a1 ,@(operands a2)))
        (else `(+ ,a1 ,a2))))

(define (symb:add x y)
  (if enable-constructor-simplifications?
      (symb1:+ x y)
      (symb:+ x y)))

(define (symb:add:n args)
  (cond ((null? args) :zero)
	((null? (cdr args)) (car args))
	(else
	 (let lp ((args (cddr args))
		  (ans (symb:add (car args) (cadr args))))
	   (if (null? args)
	       ans
	       (lp (cdr args)
		   (symb:add ans (car args))))))))

(define (symb:sum . args)
  (symb:add:n args))
    
(addto-symbolic-operator-table '+ symb:sum)
;;; (addto-symbolic-operator-table '+ symb:add)
;;; (addto-symbolic-operator-table '+ symb:+)

(define (product? x)
  (and (pair? x) (eq? (car x) '*)))

(define (symb:multiplicands expr) (cdr expr))

(define (symb:* m1 m2)
  (cond ((and (number? m1) (number? m2)) (* m1 m2))
        ((number? m1)
         (cond ((zero? m1) m1)
               ((one? m1) m2)
	       ((product? m2)
		`(* ,m1 ,@(operands m2)))
               (else `(* ,m1 ,m2))))
        ((number? m2)
         (cond ((zero? m2) m2)
               ((one? m2) m1)
	       ((product? m1)
		`(* ,m2 ,@(operands m1)))
               (else `(* ,m2 ,m1))))
	((product? m1)
	 (cond ((product? m2)
		`(* ,@(operands m1) ,@(operands m2)))
	       (else `(* ,@(operands m1) ,m2))))
	((product? m2)
	 `(* ,m1 ,@(operands m2)))
        (else `(* ,m1 ,m2))))

(define (symb:mul x y)
  (if enable-constructor-simplifications?
      (symb1:* x y)
      (symb:* x y)))

(define (symb:mul:n args)
  (cond ((null? args) :one)
	((null? (cdr args)) (car args))
	(else
	 (let lp ((args (cddr args))
		  (ans (symb:mul (car args) (cadr args))))
	   (if (null? args)
	       ans
	       (lp (cdr args)
		   (symb:mul ans (car args))))))))

(define (symb:product . args)
  (symb:mul:n args))

(addto-symbolic-operator-table '* symb:product)
;;;(addto-symbolic-operator-table '* symb:mul)
;;;(addto-symbolic-operator-table '* symb:*)

(define (difference? expr)
  (and (pair? expr) (eq? (car expr) '-)))

(define (symb:minuend expr) (cadr expr))
(define (symb:subtrahend expr)
  (if (null? (cdddr expr))
      (caddr expr)
      `(* ,@(cddr expr))))

(define (symb:- a1 a2)
  (cond ((and (number? a1) (number? a2)) (- a1 a2))
        ((number? a1) (if (zero? a1) `(- ,a2) `(- ,a1 ,a2)))
        ((number? a2) (if (zero? a2) a1 `(- ,a1 ,a2)))
	((and (difference? a1) allow-nary-difference-quotient)
	 (cond ((sum? a2)
		`(- ,@(operands a1) ,@(operands a2)))
	       (else `(- ,@(operands a1) ,a2))))
	((and (sum? a2) allow-nary-difference-quotient)
	 `(- ,a1 ,@(operands a2)))
        (else `(- ,a1 ,a2))))

(define (symb:dif x y)
  (if enable-constructor-simplifications?
      (symb1:- x y)
      (symb:- x y)))

(define (symb:dif:n args)
  (cond ((null? args) :zero)
	((null? (cdr args))
         (symb:dif :zero (car args)))
	(else
	 (symb:dif (car args)
                   (symb:add:n (cdr args))))))

(define (symb:difference . args)
  (symb:dif:n args))
    
(addto-symbolic-operator-table '- symb:difference)
;;;(addto-symbolic-operator-table '- symb:dif)
;;;(addto-symbolic-operator-table '- symb:-)

(define (quotient? expr)
  (and (pair? expr) (eq? (car expr) '/)))

(define (symb:numerator expr) (cadr expr))
(define (symb:denominator expr)
  (if (null? (cdddr expr))
      (caddr expr)
      `(* ,@(cddr expr))))

(define symb:dividend symb:numerator)
(define symb:divisor symb:denominator)

(define (symb:/ m1 m2)
  (cond ((and (number? m1) (number? m2))
	 (/ m1 m2))
        ((number? m1)
         (cond ((zero? m1) m1)
               (else `(/ ,m1 ,m2))))
        ((number? m2)
         (cond ((zero? m2)
                (error "Divide by zero -- MAKE-QUO"))
               ((one? m2) m1)
               (else `(/ ,m1 ,m2))))
	((and (quotient? m1) allow-nary-difference-quotient)
	 (cond ((product? m2)
		`(/ ,@(operands m1) ,@(operands m2)))
	       (else `(/ ,@(operands m1) ,m2))))
	((and (product? m2) allow-nary-difference-quotient)
	 `(/ ,m1 ,@(operands m2)))
        (else `(/ ,m1 ,m2))))

(define (symb:quo x y)
  (if enable-constructor-simplifications?
      (symb1:/ x y)
      (symb:/ x y)))

(define (symb:quo:n args)
  (cond ((null? args) :one)
	((null? (cdr args))
         (symb:quo :one (car args)))
	(else
	 (symb:quo (car args)
                   (symb:mul:n (cdr args))))))

(define (symb:quotient . args)
  (symb:quo:n args))

(addto-symbolic-operator-table '/ symb:quotient)
;;;(addto-symbolic-operator-table '/ symb:quo)
;;;(addto-symbolic-operator-table '/ symb:/)

(define allow-nary-difference-quotient #f)

(define (abs? exp)
  (and (pair? exp) (eq? (car exp) 'abs)))

(define (symb:abs x)
  (cond ((number? x) (abs x))
	(else `(abs ,x))))
(addto-symbolic-operator-table 'abs symb:abs)


(define (expt? exp)
  (and (pair? exp) (eq? (car exp) 'expt)))

(define (symb:expt b e)
  (cond ((and (number? b) (number? e))
	 (expt b e))
	((number? b)
	 (cond ;;((zero? b) :zero) ;No! consider 0^{-1}
	       ((one? b) :one)
	       (else `(expt ,b ,e))))
	((number? e)
	 (cond ((zero? e) :one)
	       ((one? e) b)
	       ((and (integer? e) (even? e) (sqrt? b))
		(symb:expt (car (operands b)) (quotient e 2)))
	       ((and (expt? b)
		     (number? (cadr (operands b)))
		     (integer? (* (cadr (operands b)) e)))
	        (symb:expt (car (operands b))
			   (* (cadr (operands b)) e)))
	       ((negative? e)
		(symb:/ :one (symb:expt b (- e))))
	       (else `(expt ,b ,e))))
	(else `(expt ,b ,e))))
(addto-symbolic-operator-table 'expt symb:expt)


(define (square? exp)
  (and (pair? exp) (eq? (car exp) 'square)))

(define (symb:square exp) (symb:expt exp 2))
(addto-symbolic-operator-table 'square symb:square)


(define (cube? exp)
  (and (pair? exp) (eq? (car exp) 'cube)))

(define (symb:cube exp) (symb:expt exp 3))
(addto-symbolic-operator-table 'cube symb:cube)

(define (negate? exp)
  (and (pair? exp) (eq? (car exp) 'negate)))

(define (symb:negate exp) (symb:- :zero exp))
(addto-symbolic-operator-table 'negate symb:negate)

(define (invert? exp)
  (and (pair? exp) (eq? (car exp) 'invert)))

(define (symb:invert exp) (symb:/ :one exp))
(addto-symbolic-operator-table 'invert symb:invert)


(define (sqrt? exp)
  (and (pair? exp) (eq? (car exp) 'sqrt)))

(define (symb:sqrt exp)
  (if (and sqrt-expt-simplify
	   (number? exp))
      (if (inexact? exp)
	  (sqrt exp)
	  (cond ((zero? exp) exp)
		((one? exp) :one)
		(else
		 (let ((s (n:sqrt exp)))
		   (if (exact? s)
		       s
		       `(sqrt ,exp))))))
      `(sqrt ,exp)))
(addto-symbolic-operator-table 'sqrt symb:sqrt)

(define (exp? exp)
  (and (pair? exp) (eq? (car exp) 'exp)))

(define (symb:exp x)
  (if (number? x)
      (if (inexact? x)
	  (exp x)
	  (if (zero? x)
	      :one
	      `(exp ,x)))
      `(exp ,x)))
(addto-symbolic-operator-table 'exp symb:exp)


(define (log? expression)
  (and (pair? expression) (eq? (car expression) 'log)))

(define (symb:log x)
  (if (number? x)
      (if (inexact? x)
	  (log x)
	  (if (one? x)
	      :zero
	      `(log ,x)))
      `(log ,x)))
(addto-symbolic-operator-table 'log symb:log)

(define heuristic-sin-cos-simplify true)
(define relative-integer-tolerance (* 100 *machine-epsilon*))
(define absolute-integer-tolerance 1e-20)
(define n:pi/4 (atan 1 1))
(define n:pi (* 4 n:pi/4))
(define n:2pi (* 2 n:pi))
(define n:pi/2 (* 2 n:pi/4))
(define n:pi/3 (/ n:pi 3))
(define n:pi/6 (/ n:pi/2 3))

(define (almost-integer? x)
  (or (integer? x)
      (and heuristic-sin-cos-simplify
	   (real? x)
	   (let ((z (round x)))
	     (if (zero? z)
		 (< (abs x) absolute-integer-tolerance)
		 (< (abs (/ (- x z) z)) relative-integer-tolerance))))))

(define (n:zero-mod-pi? x) (almost-integer? (/ x n:pi)))
(define (symb:zero-mod-pi? x) (memq x '(:-pi :pi :+pi :-2pi :2pi)))

(define (n:pi/2-mod-2pi? x) (almost-integer? (/ (- x n:pi/2) n:2pi)))
(define (symb:pi/2-mod-2pi? x) (memq x '(:pi/2 :+pi/2)))

(define (n:-pi/2-mod-2pi? x) (almost-integer? (/ (+ x n:pi/2) n:2pi)))
(define (symb:-pi/2-mod-2pi? x) (memq x '(:-pi/2)))

(define (n:pi/2-mod-pi? x) (almost-integer? (/ (- x n:pi/2) n:pi)))
(define (symb:pi/2-mod-pi? x) (memq x '(:-pi/2 :pi/2 :+pi/2)))

(define (n:zero-mod-2pi? x) (almost-integer? (/ x n:2pi)))
(define (symb:zero-mod-2pi? x) (memq x '(:-2pi :2pi :+2pi)))

(define (n:pi-mod-2pi? x) (almost-integer? (/ (- x n:pi) n:2pi)))
(define (symb:pi-mod-2pi? x) (memq x '(:-pi :pi :+pi)))

(define (n:pi/4-mod-pi? x) (almost-integer? (/ (- x n:pi/4) n:pi)))
(define (symb:pi/4-mod-pi? x) (memq x '(:pi/4 :+pi/4)))

(define (n:-pi/4-mod-pi? x) (almost-integer? (/ (+ x n:pi/4) :pi)))
(define (symb:-pi/4-mod-pi? x) (memq x '(:-pi/4)))

(define (sin? exp)
  (and (pair? exp) (eq? (car exp) 'sin)))

(define (symb:sin x)
  (cond ((number? x)
      	 (if (exact? x)
	     (if (zero? x) 0 `(sin ,x))
	     (cond ((n:zero-mod-pi? x) 0.)
		   ((n:pi/2-mod-2pi? x) +1.)
		   ((n:-pi/2-mod-2pi? x) -1.)
		   (else (sin x)))))
	((symbol? x)
	 (cond ((symb:zero-mod-pi? x) 0)
	       ((symb:pi/2-mod-2pi? x) +1)
	       ((symb:-pi/2-mod-2pi? x) -1)
	       (else `(sin ,x))))
	(else `(sin ,x))))

(addto-symbolic-operator-table 'sin symb:sin)


(define (cos? exp)
  (and (pair? exp) (eq? (car exp) 'cos)))

(define (symb:cos x)
  (cond ((number? x)
	 (if (exact? x)
	     (if (zero? x) 1 `(cos ,x))
	     (cond ((n:pi/2-mod-pi? x) 0.)
		   ((n:zero-mod-2pi? x) +1.)
		   ((n:pi-mod-2pi? x) -1.)
		   (else (cos x)))))
	((symbol? x)
	 (cond ((symb:pi/2-mod-pi? x) 0)
	       ((symb:zero-mod-2pi? x) +1)
	       ((symb:pi-mod-2pi? x) -1)
	       (else `(cos ,x))))
	(else `(cos ,x))))

(addto-symbolic-operator-table 'cos symb:cos)

(define (tan? exp)
  (and (pair? exp) (eq? (car exp) 'tan)))

(define (symb:tan x)
  (cond ((number? x)
	 (if (exact? x)
	     (if (zero? x) 0 `(tan ,x))
	     (cond ((n:zero-mod-pi? x) 0.)
		   ((n:pi/4-mod-pi? x) 1.)
		   ((n:-pi/4-mod-pi? x) -1.)
		   ((n:pi/2-mod-pi? x)
		    (error "Undefined -- TAN" x))
		   (else (tan x)))))
	((symbol? x)
	 (cond ((symb:zero-mod-pi? x) 0)
	       ((symb:pi/4-mod-pi? x) 1)
	       ((symb:-pi/4-mod-pi? x) -1)
	       ((symb:pi/2-mod-pi? x)
		(error "Undefined -- TAN" x))
	       (else `(tan ,x))))
	(else `(tan ,x))))

(addto-symbolic-operator-table 'tan symb:tan)


(define (csc? exp)
  (and (pair? exp) (eq? (car exp) 'csc)))

(define (symb:csc x)
  (if (number? x)
      (if (inexact? x)
	  (csc x)
	  (if (zero? x)
	      (error "Zero argument -- CSC" x)
	      `(/ 1 ,(symb:sin x))))
      `(/ 1 ,(symb:sin x))))
(addto-symbolic-operator-table 'csc symb:csc)


(define (sec? exp)
  (and (pair? exp) (eq? (car exp) 'sec)))

(define (symb:sec x)
  (if (number? x)
      (if (inexact? x)
	  (sec x)
	  (if (zero? x)
	      :one
	      `(/ 1 ,(symb:cos x))))
      `(/ 1 ,(symb:cos x))))
(addto-symbolic-operator-table 'sec symb:sec)

(define (atan? exp)
  (and (pair? exp) (eq? (car exp) 'atan)))

(define (symb:atan x . opts)
  (if (null? opts)
      (if (number? x)
	  (if (inexact? x)
	      (atan x)
	      (if (zero? x)
		  :zero
		  `(atan ,x)))
	  `(atan ,x))
      (let ((y x) (x (car opts)))
	(if (number? y)
	    (if (exact? y)
		(if (zero? y)
		    :zero		;check x=0?
		    (if (number? x)
			(if (exact? x)
			    `(atan ,y ,x)
			    (atan y x))
			`(atan ,y ,x)))
		(if (number? x)
		    (atan y x)
		    `(atan ,y ,x)))
	    `(atan ,y ,x)))))
(addto-symbolic-operator-table 'atan symb:atan)


(define (asin? exp)
  (and (pair? exp) (eq? (car exp) 'asin)))

(define (symb:asin x)
  (if (number? x)
      (if (inexact? x)
	  (asin x)
	  (if (zero? x)
	      :zero
	      `(asin ,x)))
      `(asin ,x)))
(addto-symbolic-operator-table 'asin symb:asin)


(define (acos? exp)
  (and (pair? exp) (eq? (car exp) 'acos)))

(define (symb:acos x)
  (if (number? x)
      (if (inexact? x)
	  (acos x)
	  (if (one? x)
	      :zero
	      `(acos ,x)))
      `(acos ,x)))
(addto-symbolic-operator-table 'acos symb:acos)

(define (cosh? exp)
  (and (pair? exp) (eq? (car exp) 'cosh)))

(define (symb:cosh x)
  (if (number? x)
      (if (inexact? x)
	  (cosh x)
	  (if (zero? x)
	      :one
	      `(cosh ,x)))
      `(cosh ,x)))
(addto-symbolic-operator-table 'cosh symb:cosh)


(define (sinh? exp)
  (and (pair? exp) (eq? (car exp) 'sinh)))

(define (symb:sinh x)
  (if (number? x)
      (if (inexact? x)
	  (sinh x)
	  (if (zero? x)
	      :zero
	      `(sinh ,x)))
      `(sinh ,x)))
(addto-symbolic-operator-table 'sinh symb:sinh)

(define (max? exp)
  (and (pair? exp) (eq? (car exp) 'max)))

(define (symb:max . l)
  (if (for-all? l number?)
      (apply max l)
      `(max ,@l)))
(addto-symbolic-operator-table 'max symb:max)


(define (min? exp)
  (and (pair? exp) (eq? (car exp) 'min)))  

(define (symb:min . l)
  (if (for-all? l number?)
      (apply min l)
      `(min ,@l)))
(addto-symbolic-operator-table 'min symb:min)


(define (derivative? exp)
  (and (pair? exp) (eq? (car exp) 'derivative)))

(define (symb:derivative f)
  (cond ((derivative? f)
	 `(,(symb:expt 'derivative 2) ,(car (operands f))))
	((ederivative? f)
	 `(,(symb:expt 'derivative
		       (fix:+ (cadr (operands (operator f)))
			      1))
	   ,(car (operands f))))
	(else
	 `(derivative ,f))))

(define (ederivative? f)
  (and (pair? f)
       (pair? (operator f))
       (expt? (operator f))
       (eq? 'derivative (car (operands (operator f))))))



;;; The following makes (derivative f) into a numerical expression.  
;;;  It is a function
;;;(addto-symbolic-operator-table 'derivative symb:derivative)

;;; The following are more hairy construction simplifications for the
;;; most important functions.

(define (symb1:+ a1 a2)
  (cond ((sum? a1)
	 (cond ((sum? a2)
		(addup-args (append (operands a1) (operands a2)) '()))
	       ((difference? a2)
		(if (null? (cdr (operands a2)))
		    (addup-args (operands a1) (operands a2))
		    (addup-args (append (operands a1)
                                        (list (car (operands a2))))
				(cdr (operands a2)))))
	       (else (addup-args (append (operands a1)
                                         (list a2)) '()))))
	((difference? a1)
	 (if (null? (cdr (operands a1)))
	     (cond ((sum? a2) (addup-args (operands a2) (operands a1)))
		   ((difference? a2)
		    (if (null? (cdr (operands a2)))
			(addup-args '() (append (operands a1) (operands a2)))
			(addup-args (list (car (operands a2)))
				    (append (operands a1) (cdr (operands a2))))))
		   (else (addup-args (list a2) (operands a1))))
	     (cond ((sum? a2)
		    (addup-args (append (list (car (operands a1))) (operands a2))
				(cdr (operands a1))))
		   ((difference? a2)
		    (if (null? (cdr (operands a2)))
			(addup-args (list (car (operands a1)))
				    (append (cdr (operands a1))
                                            (operands a2)))
			(addup-args (list (car (operands a1))
                                          (car (operands a2)))
				    (append (cdr (operands a1))
                                            (cdr (operands a2))))))
		   (else (addup-args (list (car (operands a1)) a2)
				     (cdr (operands a1)))))))
	(else
	 (cond ((sum? a2)
		(addup-args (append (list a1) (operands a2)) '()))
	       ((difference? a2)
		(if (null? (cdr (operands a2)))
		    (addup-args (list a1) (operands a2))
		    (addup-args (append (list a1) (list (car (operands a2))))
				(cdr (operands a2)))))
	       (else (addup-args (list a1 a2) '()))))))

(define (addup-args pos neg)
  (define (make-answer sum pos neg)
    (if (zero? sum)
	(if (null? pos)
	    (if (null? neg)
		:zero
		(if (null? (cdr neg))
		    `(- ,(car neg))
		    `(- (+ ,@neg))))
	    (if (null? neg)
		(if (null? (cdr pos))
		    (car pos)
		    `(+ ,@pos))
		(if (null? (cdr pos))
		    (if (null? (cdr neg))
                        (if (equal? (car pos) (car neg))
                            :zero
                            `(- ,(car pos) ,(car neg)))
			`(- ,(car pos) (+ ,@neg)))
		    (if (null? (cdr neg))
			`(- (+ ,@pos) ,(car neg))
                        (if (equal? pos neg)
                            :zero
                            `(- (+ ,@pos) (+ ,@neg)))))))
	(if (null? pos)
	    (if (null? neg)
		sum
		(if (null? (cdr neg))
		    `(- ,sum ,(car neg))
		    `(- ,sum (+ ,@neg))))
	    (if (null? neg)
		`(+ ,sum ,@pos)
		(if (null? (cdr neg))
		    `(- (+ ,sum ,@pos) ,(car neg))
		    `(- (+ ,sum ,@pos) (+ ,@neg)))))))
  (let plp ((p pos) (sum :zero) (respos '()))
    (cond ((null? p)
	   (let nlp ((n neg) (sum sum) (resneg '()))
	     (cond ((null? n)
		    (make-answer sum
				 (reverse respos)
				 (reverse resneg)))
		   ((number? (car n))
		    (nlp (cdr n) (- sum (car n)) resneg))
		   (else
		    (nlp (cdr n) sum (cons (car n) resneg))))))
	  ((number? (car p))
	   (plp (cdr p) (+ sum (car p)) respos))
	  (else
	   (plp (cdr p) sum (cons (car p) respos))))))

(define (symb1:* a1 a2)
  (cond ((product? a1)
	 (cond ((product? a2)
		(mulup-args (append (operands a1) (operands a2)) '()))
	       ((quotient? a2)
		(if (null? (cdr (operands a2)))
		    (mulup-args (operands a1) (operands a2))
		    (mulup-args (append (operands a1)
                                        (list (car (operands a2))))
				(cdr (operands a2)))))
	       (else (mulup-args (append (operands a1) (list a2)) '()))))
	((quotient? a1)
	 (if (null? (cdr (operands a1)))
	     (cond ((product? a2) (mulup-args (operands a2) (operands a1)))
		   ((quotient? a2)
		    (if (null? (cdr (operands a2)))
			(mulup-args '() (append (operands a1) (operands a2)))
			(mulup-args (list (car (operands a2)))
				    (append (operands a1) (cdr (operands a2))))))
		   (else (mulup-args (list a2) (operands a1))))
	     (cond ((product? a2)
		    (mulup-args (append (list (car (operands a1))) (operands a2))
				(cdr (operands a1))))
		   ((quotient? a2)
		    (if (null? (cdr (operands a2)))
			(mulup-args (list (car (operands a1)))
				    (append (cdr (operands a1))
                                            (operands a2)))
			(mulup-args (list (car (operands a1))
                                          (car (operands a2)))
				    (append (cdr (operands a1))
                                            (cdr (operands a2))))))
		   (else (mulup-args (list (car (operands a1)) a2)
				     (cdr (operands a1)))))))
	(else
	 (cond ((product? a2)
		(mulup-args (append (list a1) (operands a2)) '()))
	       ((quotient? a2)
		(if (null? (cdr (operands a2)))
		    (mulup-args (list a1) (operands a2))
		    (mulup-args (append (list a1) (list (car (operands a2))))
				(cdr (operands a2)))))
	       (else (mulup-args (list a1 a2) '()))))))

(define (mulup-args pos neg)
  (define (make-product numfact factors)
    (if (null? factors)
        numfact
        (if (one? numfact)
            (if (null? (cdr factors))
                (car factors)
                `(* ,@factors))
            (if (null? (cdr factors))
                `(* ,numfact ,(car factors))
                `(* ,numfact ,@factors)))))
  (define (make-answer pfactor pos nfactor neg)
    (let ((num (make-product pfactor pos))
          (den (make-product nfactor neg)))
      (cond ((and (number? den) (zero? den))
             (error "zero divide in mulup-args"))
            ((and (number? num) (zero? num))
             :zero)
            ((and (number? den) (one? den))
             num)
            ((and (number? num) (number? den))
             (/ num den))
            ((equal? num den)
             :one)
            (else
             `(/ ,num ,den)))))
  (let plp ((p pos) (pfactor :one) (respos '()))
    (cond ((null? p)
	   (let nlp ((n neg) (nfactor :one) (resneg '()))
	     (cond ((null? n)
		    (make-answer pfactor
				 (reverse respos)
                                 nfactor
				 (reverse resneg)))
		   ((number? (car n))
		    (nlp (cdr n) (* nfactor (car n)) resneg))
		   (else
		    (nlp (cdr n) nfactor (cons (car n) resneg))))))
	  ((number? (car p))
	   (plp (cdr p) (* pfactor (car p)) respos))
	  (else
	   (plp (cdr p) pfactor (cons (car p) respos))))))
	   
(define (symb1:- a1 a2)
  (cond ((sum? a1)
	 (cond ((sum? a2)
		(addup-args (operands a1) (operands a2)))
	       ((difference? a2)
		(if (null? (cdr (operands a2)))
		    (addup-args (append (operands a1) (operands a2)) '())
		    (addup-args (append (operands a1) (cdr (operands a2)))
				(list (car (operands a2))))))
	       (else (addup-args (operands a1) (list a2)))))
	((difference? a1)
	 (if (null? (cdr (operands a1)))
	     (cond ((sum? a2)
                    (addup-args '() (append (operands a1) (operands a2))))
		   ((difference? a2)
		    (if (null? (cdr (operands a2)))
			(addup-args (operands a2) (operands a1))
			(addup-args (cdr (operands a2))
				    (append (operands a1)
					    (list (car (operands a2)))))))
		   (else (addup-args '() (append (operands a1) (list a2)))))
	     (cond ((sum? a2)
		    (addup-args (list (car (operands a1)))
				(append (cdr (operands a1)) (operands a2))))
		   ((difference? a2)
		    (if (null? (cdr (operands a2)))
			(addup-args (append (list (car (operands a1)))
                                            (operands a2))
				    (cdr (operands a1)))
			(addup-args (cons (car (operands a1))
                                          (cdr (operands a2)))
				    (append (cdr (operands a1))
					    (list (car (operands a2)))))))
		   (else (addup-args (list (car (operands a1)))
				     (append (cdr (operands a1)) (list a2)))))))
	(else
	 (cond ((sum? a2)
		(addup-args (list a1) (operands a2)))
	       ((difference? a2)
		(if (null? (cdr (operands a2)))
		    (addup-args (append (list a1) (operands a2)) '())
		    (addup-args (append (list a1) (cdr (operands a2)))
				(list (car (operands a2))))))
	       (else (addup-args (list a1) (list a2)))))))

(define (symb1:/ a1 a2)
  (cond ((product? a1)
	 (cond ((product? a2)
		(mulup-args (operands a1) (operands a2)))
	       ((quotient? a2)
		(if (null? (cdr (operands a2)))
		    (mulup-args (append (operands a1) (operands a2)) '())
		    (mulup-args (append (operands a1) (cdr (operands a2)))
				(list (car (operands a2))))))
	       (else (mulup-args (operands a1) (list a2)))))
	((quotient? a1)
	 (if (null? (cdr (operands a1)))
	     (cond ((product? a2)
                    (mulup-args '() (append (operands a1) (operands a2))))
		   ((quotient? a2)
		    (if (null? (cdr (operands a2)))
			(mulup-args (operands a2) (operands a1))
			(mulup-args (cdr (operands a2))
				    (append (operands a1)
					    (list (car (operands a2)))))))
		   (else (mulup-args '() (append (operands a1) (list a2)))))
	     (cond ((product? a2)
		    (mulup-args (list (car (operands a1)))
				(append (cdr (operands a1)) (operands a2))))
		   ((quotient? a2)
		    (if (null? (cdr (operands a2)))
			(mulup-args (append (list (car (operands a1)))
                                            (operands a2))
				    (cdr (operands a1)))
			(mulup-args (cons (car (operands a1))
                                          (cdr (operands a2)))
				    (append (cdr (operands a1))
					    (list (car (operands a2)))))))
		   (else (mulup-args (list (car (operands a1)))
				     (append (cdr (operands a1)) (list a2)))))))
	(else
	 (cond ((product? a2)
		(mulup-args (list a1) (operands a2)))
	       ((quotient? a2)
		(if (null? (cdr (operands a2)))
		    (mulup-args (append (list a1) (operands a2)) '())
		    (mulup-args (append (list a1) (cdr (operands a2)))
				(list (car (operands a2))))))
	       (else (mulup-args (list a1) (list a2)))))))

;;; answers #t if can prove that access chain is terminal in args.

(define (symb:elementary-access? access-chain args)
  (define (sea chain thing)
    (cond ((and (not (null? chain))
		(pair? thing)
		(or (eq? (car thing) 'up)
		    (eq? (car thing) 'down)))
	   (sea (cdr chain)
		(list-ref (cdr thing) (car chain))))
	  ((null? chain)
	   (or (not (pair? thing))
	       (not (or (eq? (car thing) 'up)
			(eq? (car thing) 'down)))))
	  (else #f)))
  (if (= (length args) 1)
      (sea access-chain (car args))
      (sea (cdr access-chain)
	   (list-ref args (car access-chain)))))
