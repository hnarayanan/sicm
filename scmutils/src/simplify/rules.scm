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

;;;; Rule systems for simplification.
;;;  Written by GJS in the 1980s, edited by Mira Wilczek in summer 2002.
;;;  Edited for a new version in january 2005.

(declare (usual-integrations))

;;; Default is simplifier lives dangerously.

;;; allows (log (exp x)) => x 
;;;  can confuse x=(x0+n*2pi)i with x0
(define log-exp-simplify? true)

;;; If x is real then (sqrt (square x)) = (abs x)
;;;   This is hard to work with, but we usually want to 
;;;   allow (sqrt (square x)) => x, but this is
;;;   not necessarily good if x is negative.
(define sqrt-expt-simplify? true)

;;; If x, y are real non-negative then
;;; (* (sqrt x) (sqrt y)) = (sqrt (* x y))
;;; but this is not true for negative factors.
(define sqrt-factor-simplify? true)

;;; allows (atan y x) => (atan (/ y d) (/ x d)) where d=(gcd x y)
;;; OK if d is a number (gcd is always positive) but may
;;; lose quadrant if gcd can be negative for some values of
;;; its variables.
(define aggressive-atan-simplify? true)

;;; allows (asin (sin x)) => x, etc
;;;  loses multivalue info, as in log-exp
(define inverse-simplify? true)

;;; Allows reduction of sin, cos of rational multiples of :pi
(define sin-cos-simplify? true)

;;; Allow half-angle reductions.  Sign of result is hairy!
(define half-angle-simplify? true)

;;; wierd case: ((d magnitude) (square x)) => 1
(define ignore-zero? true)

;;; allows commutation of partial derivatives.
;;;  only ok if components selected by partials are unstructured 
;;;  (e.g. real)
(define commute-partials? true)

;;; allows division through by numbers
;;; e.g. (/ (+ (* 4 x) 5) 3) => (+ (* 4/3 x) 5/3)
(define divide-numbers-through-simplify? true)

;;; Transforms products of trig functions into functions of sums 
;;;  of angles
;;; e.g. (* (sin x) (cos y)) 
;;;        ==> (+ (* 1/2 (sin (+ x y))) (* 1/2 (sin (+ x (* -1 y)))) )
(define trig-product-to-sum-simplify? false)

;;; however, we have control over the defaults

(define (log-exp-simplify doit?)
  (assert (boolean? doit?) "argument must be a boolean.")
  (clear-memoizer-tables)
  (set! log-exp-simplify? doit?))

(define (sqrt-expt-simplify doit?)
  (assert (boolean? doit?) "argument must be a boolean.")
  (clear-memoizer-tables)
  (set! sqrt-expt-simplify? doit?))

(define (sqrt-factor-simplify doit?)
  (assert (boolean? doit?) "argument must be a boolean.")
  (clear-memoizer-tables)
  (set! sqrt-factor-simplify? doit?))

(define (aggressive-atan-simplify doit?)
  (assert (boolean? doit?) "argument must be a boolean.")
  (clear-memoizer-tables)
  (set! aggressive-atan-simplify doit?))

(define (inverse-simplify doit?)
  (assert (boolean? doit?) "argument must be a boolean.")
  (clear-memoizer-tables)
  (set! inverse-simplify? doit?))

(define (sin-cos-simplify doit?)
  (assert (boolean? doit?) "argument must be a boolean.")
  (clear-memoizer-tables)
  (set! sin-cos-simplify? doit?))

(define (half-angle-simplify doit?)
  (assert (boolean? doit?) "argument must be a boolean.")
  (clear-memoizer-tables)
  (set! half-angle-simplify? doit?))

(define (ignore-zero-simplify doit?)
  (assert (boolean? doit?) "argument must be a boolean.")
  (clear-memoizer-tables)
  (set! ignore-zero? doit?))

(define (commute-partials-simplify doit?)
  (assert (boolean? doit?) "argument must be a boolean.")
  (clear-memoizer-tables)
  (set! commute-partials? doit?))

(define (divide-numbers-through-simplify doit?)
  (assert (boolean? doit?) "argument must be a boolean.")
  (clear-memoizer-tables)
  (set! divide-numbers-through-simplify? doit?))

(define (trig-product-to-sum-simplify doit?)
  (assert (boolean? doit?) "argument must be a boolean.")
  (clear-memoizer-tables)
  (set! trig-product-to-sum-simplify? doit?))

;;; The following predicates are used in trig rules.

(define (negative-number? x)
  (and (number? x) (real? x) (negative? x)))

(define (complex-number? z)
  (and (complex? z)
       (not (n:zero? (n:real-part z)))
       (not (n:zero? (n:imag-part z)))))

(define (imaginary-number? z)
  (and (complex? z)
       (not (n:zero? z))
       (n:zero? (n:real-part z))))

(define (imaginary-integer? z)
  (and (complex? z)
       (not (n:zero? z))
       (n:zero? (n:real-part z))
       (exact-integer? (n:imag-part z))))


(define (non-integer? x)
  (not (integer? x)))

(define (even-integer? x)
  (and (integer? x) (even? x) (fix:> x 1)))

(define (odd-integer? x)
  (and (integer? x) (odd? x) (fix:> x 1)))

(define (universal-reductions exp)
  (let ((vars (variables-in exp)))
    (let ((logexp? (occurs-in? '(log exp) vars))
	  (sincos? (occurs-in? '(sin cos) vars))
	  (invtrig? (occurs-in? '(asin acos atan) vars))
	  (sqrt? (memq 'sqrt vars))
	  (mag? (memq 'magnitude vars))
          )
      (let* ((e0 (miscsimp exp))
	     (e1 (if logexp? (logexp e0) e0))
	     (e2 (if mag? (magsimp e1) e1))
	     (e3 (if
		  (and sincos? (symbol? :pi) sin-cos-simplify?)
		  (special-trig e2)
		  e2)))
	(cond ((and sincos? invtrig?)
	       (simsqrt (triginv e3)))
	      (sqrt? (simsqrt e3))
	      (else e3))))))

(define logexp
  (rule-system

   ( (exp (* (? n integer?) (log (? x))))
     none
     (expt (: x) (: n)) )
     
   ( (exp (log (? x))) none (: x) )
   ( (log (exp (? x)))
     (and log-exp-simplify?
	  (let ((xs (rcf:simplify x)))
            (assume! `(= (exp (log ,xs)) ,xs) 'logexp1)))
     (: x) )

   ( (sqrt (exp (? x)))
     (and sqrt-expt-simplify?
          (let ((xs (rcf:simplify x)))
	     (assume! `(= (sqrt (exp ,xs)) (exp (/ ,xs 2)))
                      'logexp2)))
     (exp (/ (: x) 2)) )

   ( (log (sqrt (? x)))
     none
     (* 1/2 (log (: x))) )
   ))

(define magsimp
  (rule-system

   ( (magnitude (* (? x) (? y) (?? ys)))
     none
     (* (magnitude (: x)) (magnitude (* (: y) (:: ys)))) )

   ( (magnitude (expt (? x) (? n even-integer?)))
     none
     (expt (: x) (: n)) )

   ;; where does this nonsense come from?
   ( ((derivative magnitude) (expt (? x) (? n even-integer?)))
     (lambda (x)
       (error "Who ordered this?" x)
       ignore-zero?)
     1 )

   ))

(define miscsimp  
  (rule-system
   
   ( (expt (? x) 0) none 1 )

   ( (expt (? x) 1) none (: x) )

   ( (expt (? x) 1/2)
     none
     (sqrt (: x)) )

   ( (expt (expt (? x) (? p/q)) (? m*q))
     (integer? (rcf:simplify (symb:* p/q m*q)))
     (expt (: x) (: (symb:* p/q m*q))) )

   ( (* (?? fs1)			; a rare, expensive luxury
	(expt (? x) (? y1))
	(?? fs2)
	(expt (? x) (? y2))
	(?? fs3))
     none
     (* (:: fs1)
	(expt (: x) (+ (: y1) (: y2)))
	(:: fs2)
	(:: fs3)) )
   ))

(define simsqrt
  (rule-system
   
   ( (expt (sqrt (? x)) (? n even-integer?))
     none
     (expt (: x) (: (quotient n 2))) )

   ( (sqrt (expt (? x) (? n even-integer?)))
     (and sqrt-expt-simplify?
	  (let ((xs (rcf:simplify x)))
	     (assume! `(= (sqrt (expt ,xs ,n))
                          (expt ,xs ,(quotient n 2)))
                      'simsqrt)))
     (expt (: x) (: (quotient n 2))) )

   ;; Restrict for |n|>2
   ( (sqrt (expt (? x) (? n odd-integer?)))
     none
     (* (sqrt (: x)) (expt (: x) (: (quotient (fix:- n 1) 2)))) )

   ( (expt (sqrt (? x)) (? n odd-integer?))
     none
     (* (sqrt (: x)) (expt (: x) (: (quotient (fix:- n 1) 2)))) )
   
   ( (/ (? x) (sqrt (? x)))
     none
     (sqrt (: x)) )

   ( (/ (sqrt (? x)) (? x))
     none
     (/ 1 (sqrt (: x))) )

   ( (/ (* (?? u) (? x) (?? v)) (sqrt (? x)))
     none
     (* (:: u) (sqrt (: x)) (:: v)) )

   ( (/ (* (?? u) (sqrt (? x)) (?? v)) (? x))
     none
     (/ (* (:: u) (:: v)) (sqrt (: x))) )

   ( (/ (? x) (* (?? u) (sqrt (? x)) (?? v)))
     none
     (/  (sqrt (: x)) (* (:: u) (:: v))) )

   ( (/ (sqrt (? x)) (* (?? u) (? x) (?? v)))
     none
     (/ 1 (* (:: u) (sqrt (: x)) (:: v))) )

   ( (/ (* (?? p) (? x) (?? q))
	(* (?? u) (sqrt (? x)) (?? v)))
     none
     (/ (* (:: p) (sqrt (: x)) (:: q))
	(* (:: u) (:: v))) )

   ( (/ (* (?? p) (sqrt (? x)) (?? q))
	(* (?? u) (? x) (?? v)))
     none
     (/ (* (:: p) (:: q))
	(* (:: u) (sqrt (: x)) (:: v))) )
   ))

(define (non-negative-factors x y id)
  (let ((xs (rcf:simplify x))
	(ys (rcf:simplify y)))
    (define (if-false) #f)
    (and (assume! `(non-negative? ,xs) id if-false)
         (assume! `(non-negative? ,ys) id if-false))))


(define sqrt-expand
  (rule-system
		     
   ( (sqrt (* (? x) (? y)))
     (and sqrt-factor-simplify?
	  (non-negative-factors x y 'e1))
     (* (sqrt (: x)) (sqrt (: y))) )

   ( (sqrt (* (? x) (? y) (?? ys)))
     (and sqrt-factor-simplify?
	  (non-negative-factors x y 'e2))
     (* (sqrt (: x)) (sqrt (* (: y) (:: ys)))) )

   ( (sqrt (/ (? x) (? y)))
     (and sqrt-factor-simplify?
	  (non-negative-factors x y 'e3))
     (/ (sqrt (: x)) (sqrt (: y))) )

   ( (sqrt (/ (? x) (? y) (?? ys)))
     (and sqrt-factor-simplify?
	  (non-negative-factors x y 'e4))
     (/ (sqrt (: x)) (sqrt (* (: y) (:: ys)))) )
   ))

(define sqrt-contract
  (rule-system

   ( (* (?? a) (sqrt (? x)) (?? b) (sqrt (? y)) (?? c))
     (and sqrt-factor-simplify?
          (let ((xs (rcf:simplify x)) (ys (rcf:simplify y)))
            (define (if-false) #f)
            (if (equal? xs ys)
                (and (assume! `(non-negative? ,xs) 'c1 if-false)
                     `(* ,@a ,xs ,@b ,@c))
                (and (assume! `(non-negative? ,xs) 'c1 if-false)
                     (assume! `(non-negative? ,ys) 'c1 if-false)
                     `(* ,@a ,@b ,@c (sqrt (* ,xs ,ys))))))) )

   ( (/ (sqrt (? x)) (sqrt (? y)))
     (and sqrt-factor-simplify?
          (let ((xs (rcf:simplify x)) (ys (rcf:simplify y)))
            (define (if-false) #f)
            (if (equal? xs ys)
                (and (assume! `(non-negative? ,xs) 'c2 if-false)
                     1)
                (and (assume! `(non-negative? ,xs) 'c2 if-false)
                     (assume! `(non-negative? ,ys) 'c2 if-false)
                     `(sqrt (/ ,xs ,ys)))))) )

   ( (/ (* (?? a) (sqrt (? x)) (?? b)) (sqrt (? y)))
     (and sqrt-factor-simplify?
          (let ((xs (rcf:simplify x)) (ys (rcf:simplify y)))
            (define (if-false) #f)
            (if (equal? xs ys)
                (and (assume! `(non-negative? ,xs) 'c3 if-false)
                     `(* ,@a ,@b))
                (and (assume! `(non-negative? ,xs) 'c3 if-false)
                     (assume! `(non-negative? ,ys) 'c3 if-false)
                     `(* ,@a ,@b (sqrt (/ ,xs ,ys))))))) )

   ( (/ (sqrt (? x)) (* (?? a) (sqrt (? y)) (?? b)))
     (and sqrt-factor-simplify?
          (let ((xs (rcf:simplify x)) (ys (rcf:simplify y)))
            (define (if-false) #f)
            (if (equal? xs ys)
                (and (assume! `(non-negative? ,xs) 'c4 if-false)
                     `(/ 1 (* ,@a ,@b)))
                (and (assume! `(non-negative? ,xs) 'c4 if-false)
                     (assume! `(non-negative? ,ys) 'c4 if-false)
                     `(/ (sqrt (/ ,xs ,ys)) (* ,@a ,@b)))))) )

   ( (/ (* (?? a) (sqrt (? x)) (?? b))
	(* (?? c) (sqrt (? y)) (?? d)))
     (and sqrt-factor-simplify?
          (let ((xs (rcf:simplify x)) (ys (rcf:simplify y)))
            (define (if-false) #f)
            (if (equal? xs ys)
                (and (assume! `(non-negative? ,xs) 'c5 if-false)
                     `(/ (* ,@a ,@b)  (* ,@c ,@d)))
                (and (assume! `(non-negative? ,xs) 'c5 if-false)
                     (assume! `(non-negative? ,ys) 'c5 if-false)
                     `(/ (* ,@a ,@b (sqrt (/ ,xs ,ys))) (* ,@c ,@d)))))) )
   ))

(define specfun->logexp
  (rule-system
   ( (sqrt (? x)) none (exp (* 1/2 (log (: x)))) )

   ( (atan (? z))
     none
     (/ (- (log (+ 1 (* +i (: z)))) (log (- 1 (* +i (: z))))) +2i) )

   ( (asin (? z))
     none
     (* -i (log (+ (* +i (: z)) (sqrt (- 1 (expt (: z) 2)))))) )

   ( (acos (? z))
     none
     (* -i (log (+ (: z) (* +i (sqrt (- 1 (expt (: z) 2))))))) )

   ( (sinh (? u)) none (/ (- (exp (: u)) (exp (* -1 (: u)))) 2) )

   ( (cosh (? u)) none (/ (+ (exp (: u)) (exp (* -1 (: u)))) 2) )

   ( (expt (? x) (? y non-integer?)) none (exp (* (: y) (log (: x)))) )
   ))

(define logexp->specfun
  (rule-system
     ( (exp (* -1 (log (? x)))) none (expt (: x) -1) )

     ( (exp (* 1/2 (log (? x1)))) none (sqrt (: x1)) )

     ( (exp (* -1/2 (log (? x1)))) none (/ 1 (sqrt (: x1))) )

     ( (exp (* 3/2 (log (? x1)))) none (expt (sqrt (: x1)) 3) )

     ( (exp (* -3/2 (log (? x1)))) none (expt (sqrt (: x1)) -3) )

     ( (exp (* (?? n1) (log (? x)) (?? n2)))
       none
       (expt (: x) (* (:: n1) (:: n2))) )
     ))

(define log-contract
  (rule-system
   ( (+ (?? x1) (log (? x2)) (?? x3) (log (? x4)) (?? x5))
     none
     (+ (:: x1) (:: x3) (:: x5) (log (* (: x2) (: x4)))) )

   ( (* (? n integer?) (?? f1) (log (? x)) (?? f2))
     none
     (* (:: f1) (log (expt (: x) (: n))) (:: f2)) )

   ( (+ (?? x1)
	(* (?? f1) (log (? x)) (?? f2))
	(?? x2)
	(* (?? f3) (log (? y)) (?? f4))
	(?? x3))
     (let ((s1 (rcf:simplify `(* ,@f1 ,@f2)))
	   (s2 (rcf:simplify `(* ,@f3 ,@f4))))
       (if (exact-zero? (rcf:simplify `(- ,s1 ,s2)))
	   s1
	   #f))
     (+ (* (log (* (: x) (: y))) (: predicate-value))
	(:: x1)
	(:: x2)
	(:: x3)) )
   ))

(define log-expand
  (rule-system
   ( (log (* (? x1) (? x2) (?? xs)))
     none
     (+ (log (: x1)) (log (* (: x2) (:: xs)))) )

   ( (log (expt (? x) (? e))) none (* (: e) (log (: x))) )
   ))


(define (list< l1 l2)
  (cond ((null? l1) (not (null? l2)))
	((null? l2) #f)
	((< (car l1) (car l2)) #t)
	((> (car l1) (car l2)) #f)
	(else (list< (cdr l1) (cdr l2)))))

(define reals?
  (let ((s (string->symbol "Real")))
    (lambda (r) (eq? r s))))
  
(define canonicalize-partials
  (rule-system

   ( (((partial (?? i))
       ((partial (?? j))
	(? f)))
      (?? args))

     (and commute-partials?
	  (symbol? f)
	  (list< j i)
	  (symb:elementary-access? i args)
	  (symb:elementary-access? j args))

     (((partial (:: j))
       ((partial (:: i))
	(: f)))
      (:: args)) )

   #|
   ( ((partial (?? i))
      (literal-function
       (quote ((partial (?? j))
	       (literal-function (quote (? f))
				 (-> (? fsjd) (? fsjr reals?)))))
       (-> (? fsid) (? fsir reals?))))
     (and commute-partials? (list< j i))
     ( (partial (:: j))
       (literal-function
	(quote ((partial (:: i))
		(literal-function (quote (: f))
				  (-> (: fsid) (: fsir)))))
	(-> (: fsjd) (: fsjr))) ))
   |#
   ))

;;;; trigonometry

;;; the following rules are used to convert all trig expressions to
;;; ones involving only sin and cos functions, and to make 1-arg atan
;;; into 2-arg atan.

(define trig->sincos
  (rule-system

   ( (tan (? x)) none (/ (sin (: x)) (cos (: x))) )

   ( (cot (? x)) none (/ (cos (: x)) (sin (: x))) )

   ( (sec (? x)) none (/ 1 (cos (: x))) )

   ( (csc (? x)) none (/ 1 (sin (: x))) )

   ( (atan (/ (? y) (? x))) none (atan (: y) (: x)) )

   ( (atan (? y)) none (atan (: y) 1) )

   ))


;;; sometimes we want to express combinations of sin and cos in terms
;;; of other functions.

(define sincos->trig
  (rule-system
   ( (/ (sin (? x)) (cos (? x))) none (tan (: x)) )

   ( (/ (* (?? n1) (sin (? x)) (?? n2)) (cos (? x)))
     none
     (* (:: n1) (tan (: x)) (:: n2)) )

     
   ( (/ (sin (? x)) (* (?? d1) (cos (? x)) (?? d2)))
     none
     (/ (tan (: x)) (* (:: d1) (:: d2))) )
     

   ( (/ (* (?? n1) (sin (? x)) (?? n2))
	(* (?? d1) (cos (? x)) (?? d2)))
     none
     (/ (* (:: n1) (tan (: x)) (:: n2))
	(* (:: d1) (:: d2))) )

;   ( (/ (cos (? x)) (sin (? x))) none (cot (: x)) )

;   ( (/ (* (?? n1) (cos (? x)) (?? n2)) (sin (? x)))
;     none
;     (* (:: n1) (cot (: x)) (:: n2)) )

     
;   ( (/ (cos (? x)) (* (?? d1) (sin (? x)) (?? d2)))
;     none
;     (/ (cot (: x)) (* (:: d1) (:: d2))) )
     
;   ( (/ (* (?? n1) (cos (? x)) (?? n2))
;	(* (?? d1) (sin (? x)) (?? d2)))
;     none
;     (/ (* (:: n1) (cot (: x)) (:: n2))
;	(* (:: d1) (:: d2))) )
   ))

(define triginv
  (rule-system

   ( (atan (? y) (? x))
     (and aggressive-atan-simplify?
          (let ((ys (rcf:simplify y)) (xs (rcf:simplify x)))
            (if (equal? ys xs)
                (if (number? ys)
                    (if (negative? ys)
                        '(- (/ (* 3 :pi) 4))
                        '(/ :pi 4))
                    (let ((note `(assuming (positive? ,xs))))
                      (eq-adjoin! note 'rules 'aggressive-atan-1)
                      (note-that! note)
                      '(/ :pi 4)))
                (if (and (number? ys) (number? xs))
                    (atan ys xs)
                    (let ((s (rcf:simplify `(gcd ,ys ,xs))))
                      (if (equal? s 1)
                          #f            ;do nothing
                          (let ((note `(assuming (positive? ,s)))
                                (yv (rcf:simplify `(/ ,ys ,s)))
                                (xv (rcf:simplify `(/ ,xs ,s))))
                            (eq-adjoin! note 'rules 'aggressive-atan-2)
                            (note-that! note)
                            `(atan ,yv ,xv)))))))) )

   ( (sin (asin (? x))) none (: x) )
   ( (asin (sin (? x))) 
     (and inverse-simplify?
          (let ((xs (rcf:simplify x)))
            (assume! `(= (asin (sin ,xs)) ,xs) 'asin-sin)))
     (: x) )

   ( (cos (acos (? x))) none (: x) )
   ( (acos (cos (? x)))
     (and inverse-simplify?
          (let ((xs (rcf:simplify x)))
            (assume! `(= (acos (cos ,xs)) ,xs) 'acos-cos)))
     (: x) )

   ( (tan (atan (? x))) none (: x) )
   ( (atan (tan (? x)))
     (and inverse-simplify?
          (let ((xs (rcf:simplify x)))
            (assume! `(= (atan (tan ,xs)) ,xs) 'atan-tan)))
     (: x) )

   ( (sin (acos (? x))) none (sqrt (- 1 (expt (: x) 2))) )
   ( (cos (asin (? y))) none (sqrt (- 1 (expt (: y) 2))) )
   ( (tan (asin (? y))) none (/ (: y) (sqrt (- 1 (expt (: y) 2)))) )
   ( (tan (acos (? x))) none (/ (sqrt (- 1 (expt (: x) 2))) (: x)) )

   ( (atan (sin (? x)) (cos (? x)))
     (and inverse-simplify?
          (let ((xs (rcf:simplify x)))
            (assume! `(= (atan (sin ,xs) (cos ,xs)) ,xs) `atan-sin-cos)))
     (: x) )

   ( (asin (cos (? x)))
     (and inverse-simplify?
          (let ((xs (rcf:simplify x)))
            (assume! `(= (asin (cos ,xs)) (- (* 1/2 :pi) ,xs)) 'asin-cos)))
     (- (* 1/2 :pi) (: x)) )
   ( (acos (sin (? x)))
     (and inverse-simplify?
          (let ((xs (rcf:simplify x)))
            (assume! `(= (acos (sin ,xs)) (- (* 1/2 :pi) ,xs)) 'acos-sin)))
     (- (* 1/2 :pi) (: x)) )

   ( (sin (atan (? a) (? b)))
     none
     (/ (: a) (sqrt (+ (expt (: a) 2) (expt (: b) 2)))) )

   ( (cos (atan (? a) (? b)))
     none
     (/ (: b) (sqrt (+ (expt (: a) 2) (expt (: b) 2)))) )

    ))

;;; Rules when :pi is symbolic.

(define (zero-mod-pi? x)
  (integer? (rcf:simplify (symb:/ x :pi))))

(define (pi/2-mod-2pi? x)
  (integer?
   (rcf:simplify (symb:/ (symb:- x (symb:/ :pi 2)) (symb:* 2 :pi)))))

(define (-pi/2-mod-2pi? x)
  (integer?
   (rcf:simplify (symb:/ (symb:+ x (symb:/ :pi 2)) (symb:* 2 :pi)))))

(define (pi/2-mod-pi? x)
  (integer? (rcf:simplify (symb:/ (symb:- x (symb:/ :pi 2)) :pi))))

(define (zero-mod-2pi? x)
  (integer? (rcf:simplify (symb:/ x (symb:* 2 :pi)))))

(define (pi-mod-2pi? x)
  (integer? (rcf:simplify (symb:/ (symb:- x :pi) (symb:* 2 :pi)))))

(define (pi/4-mod-pi? x)
  (integer? (rcf:simplify (symb:/ (symb:- x (symb:/ :pi 4)) :pi))))

(define (-pi/4-mod-pi? x)
  (integer? (rcf:simplify (symb:/ (symb:+ x (symb:/ :pi 4)) :pi))))


(define special-trig
  (rule-system

   ( (sin (? x zero-mod-pi?))   none  0 )
   ( (sin (? x pi/2-mod-2pi?))  none +1 )
   ( (sin (? x -pi/2-mod-2pi?)) none -1 )

   ( (cos (? x pi/2-mod-pi?))   none  0 )
   ( (cos (? x zero-mod-2pi?))  none +1 )
   ( (cos (? x pi-mod-2pi?))    none -1 )

   ( (tan (? x zero-mod-pi?))   none  0 )
   ( (tan (? x pi/4-mod-pi?))   none +1 )
   ( (tan (? x -pi/4-mod-pi?))  none -1 )

   ))

;;; sin is odd, and cos is even.  we canonicalize by moving the sign
;;; out of the first term of the argument.

(define angular-parity
  (rule-system
   ( (cos (? n negative-number?))
     none
     (cos (: (- n))) )

   ( (cos (* (? n negative-number?) (?? x)))
     none
     (cos (* (: (- n)) (:: x))) )

   ( (cos (+ (* (? n negative-number?) (?? x)) (?? y)))
     none
     (cos (- (* (: (- n)) (:: x)) (:: y))) )

   ( (sin (? n negative-number?))
     none
     (- (sin (: (- n)))) )

   ( (sin (* (? n negative-number?) (?? x)))
     none
     (- (sin (* (: (- n)) (:: x)))) )

   ( (sin (+ (* (? n negative-number?) (?? x)) (?? y)))
     none
     (- (sin (- (* (: (- n)) (:: x)) (:: y)))) )
   ))

(define (exact-integer>3? x)
  (and (exact-integer? x) (> x 3)))

(define expand-multiangle
  (rule-system
   ( (sin (* 2 (? x) (?? y)))
     none
     (* 2 (sin (* (: x) (:: y))) (cos (* (: x) (:: y)))) )

   ( (cos (* 2 (? x) (?? y)))
     none
     (- (* 2 (expt (cos (* (: x) (:: y))) 2)) 1) )

   ( (sin (* 3 (? x) (?? y)))
     none
     (+ (* 3 (sin (* (: x) (:: y)))) (* -4 (expt (sin (* (: x) (:: y))) 3))) )

   ( (cos (* 3 (? x) (?? y)))
     none
     (+ (* 4 (expt (cos (* (: x) (:: y))) 3)) (* -3 (cos (* (: x) (:: y))))) )

   ( (sin (* (? n exact-integer>3?) (? f) (?? fs))) ;at least one f
     (> n 1)
     (+ (* (sin (* (: (- n 1)) (: f) (:: fs))) (cos (* (: f) (:: fs))))
	(* (cos (* (: (- n 1)) (: f) (:: fs))) (sin (* (: f) (:: fs))))) )

   ( (sin (+ (? x) (? y) (?? ys)))	;at least one y
     none
     (+ (* (sin (: x)) (cos (+ (: y) (:: ys))))
	(* (cos (: x)) (sin (+ (: y) (:: ys))))) )

   ( (cos (* (? n exact-integer>3?) (? f) (?? fs))) ;at least one f
     (> n 1)
     (- (* (cos (* (: (- n 1)) (: f) (:: fs))) (cos (* (: f) (:: fs))))
	(* (sin (* (: (- n 1)) (: f) (:: fs))) (sin (* (: f) (:: fs))))) )

   ( (cos (+ (? x) (? y) (?? ys)))	;at least one y
     none
     (- (* (cos (: x)) (cos (+ (: y) (:: ys))))
	(* (sin (: x)) (sin (+ (: y) (:: ys))))) )
   ))

(define trig-sum-to-product
  (rule-system
   ( (+ (?? a) (sin (? x)) (?? b) (sin (? y)) (?? c) )
     none
     (+ (* 2 (sin (/ (+ (: x) (: y)) 2)) (cos (/ (- (: x) (: y)) 2))) (:: a) (:: b) (:: c)) )
   
   ( (+ (?? a) (sin (? x)) (?? b) (* -1 (sin (? y))) (?? c) )
     none
     (+ (* 2 (sin (/ (- (: x) (: y)) 2)) (cos (/ (+ (: x) (: y)) 2))) (:: a) (:: b) (:: c)) )

   ( (+ (?? a) (* -1 (sin (? y))) (?? b) (sin (? x)) (?? c) )
     none
     (+ (* 2 (sin (/ (- (: x) (: y)) 2)) (cos (/ (+ (: x) (: y)) 2))) (:: a) (:: b) (:: c)) )

   ( (+ (?? a) (cos (? x)) (?? b) (cos (? y)) (?? c) )
     none
     (+ (* 2 (cos (/ (+ (: x) (: y)) 2)) (cos (/ (- (: x) (: y)) 2))) (:: a) (:: b) (:: c)) )

   ( (+ (?? a) (cos (? x)) (?? b) (* -1 (cos (? y))) (?? c) )
     none
     (+ (* -2 (sin (/ (+ (: x) (: y)) 2)) (sin (/ (- (: x) (: y)) 2))) (:: a) (:: b) (:: c)) )
   
   ( (+ (?? a) (* -1 (cos (? y))) (?? b) (cos (? x)) (?? c) )
     none
     (+ (* -2 (sin (/ (+ (: x) (: y)) 2)) (sin (/ (- (: x) (: y)) 2))) (:: a) (:: b) (:: c)) )
   ))

(define trig-product-to-sum
  (rule-system
   ( (* (?? u) (sin (? x)) (?? v) (sin (? y)) (?? w))
     none
     (* 1/2 (- (cos (- (: x) (: y))) (cos (+ (: x) (: y)))) (:: u) (:: v) (:: w)) )

   ( (* (?? u) (cos (? x)) (?? v) (cos (? y)) (?? w))
     none
     (* 1/2 (+ (cos (- (: x) (: y))) (cos (+ (: x) (: y)))) (:: u) (:: v) (:: w)) )

   ( (* (?? u) (sin (? x)) (?? v) (cos (? y)) (?? w))
     none
     (* 1/2 (+ (sin (+ (: x) (: y))) (sin (- (: x) (: y)))) (:: u) (:: v) (:: w)) )

   ( (* (?? u) (cos (? y)) (?? v) (sin (? x)) (?? w))
     none
     (* 1/2 (+ (sin (+ (: x) (: y))) (sin (- (: x) (: y)))) (:: u) (:: v) (:: w)) )
   ))

(define contract-expt-trig
  (rule-system
   ( (expt (sin (? x)) (? n exact-integer?))
     (> n 1)
     (* 1/2 (expt (sin (: x)) (: (- n 2))) (- 1 (cos (* 2 (: x))))))

   ( (expt (cos (? x)) (? n exact-integer?))
     (> n 1)
     (* 1/2 (expt (cos (: x)) (: (- n 2))) (+ 1 (cos (* 2 (: x))))))
   ))

(define (sin-half-angle-formula theta)
  (let ((thetas (rcf:simplify theta)))
    (assume!
     `(non-negative?
       (+ (* 2 :pi)
          (* -1 ,thetas)
          (* 4 :pi (floor (/ ,thetas (* 4 :pi))))))
     'sin-half-angle-formula)
    `(sqrt (/ (- 1 (cos ,thetas)) 2))))

(define (cos-half-angle-formula theta)
  (let ((thetas (rcf:simplify theta)))
    (assume!
     `(non-negative?
       (+ :pi
          ,thetas
          (* 4 :pi
             (floor (/ (- :pi ,thetas) (* 4 :pi))))))
     'cos-half-angle-formula)
    `(sqrt (/ (+ 1 (cos ,theta)) 2))))

(define half-angle
  (rule-system
   ( (sin (* 1/2 (? x) (?? y)))
     (and half-angle-simplify?
          (sin-half-angle-formula `(* ,x ,@y))) )

   ( (sin (/ (? x) 2))
     (and half-angle-simplify?
          (sin-half-angle-formula x)) )
          
   ( (cos (* 1/2 (? x) (?? y)))
     (and half-angle-simplify?
          (cos-half-angle-formula `(* ,x ,@y))) )

   ( (cos (/ (? x) 2))
     (and half-angle-simplify?
          (cos-half-angle-formula x)) )
   ))

(define (at-least-two? n)
  (and (number? n) (>= n 2)))


(define sin^2->cos^2
  (rule-system
   ( (expt (sin (? x)) (? n at-least-two?))
     none
     (* (expt (sin (: x)) (: (- n 2)))
	(- 1 (expt (cos (: x)) 2))) )
   ))


(define cos^2->sin^2
  (rule-system
   ( (expt (cos (? x)) (? n at-least-two?))
     none
     (* (expt (cos (: x)) (: (- n 2)))
	(- 1 (expt (sin (: x)) 2))) )
   ))

(define (sincos-flush-ones expression)
  ;; Order here is essential, to put sines before cosines.
  (flush-obvious-ones
   (split-high-degree-sines
    (split-high-degree-cosines expression))))

(define (more-than-two? n)
  (and (number? n) (> n 2)))

(define split-high-degree-cosines
  (rule-system 

   ( (* (?? f1)
	(expt (cos (? x)) (? n more-than-two?))
	(?? f2))
     none
     (* (expt (cos (: x)) 2)
	(expt (cos (: x)) (: (- n 2)))
	(:: f1)
	(:: f2)) )
   
   ( (+ (?? a1)
	(expt (cos (? x)) (? n more-than-two?))
	(?? a2))
     none
     (+ (* (expt (cos (: x)) 2)
	   (expt (cos (: x)) (: (- n 2))))
	(:: a1)
	(:: a2)) )
   ))

(define split-high-degree-sines
  (rule-system
   
   ( (* (?? f1)
	(expt (sin (? x)) (? n more-than-two?))
	(?? f2))
     none
     (* (expt (sin (: x)) 2)
	(expt (sin (: x)) (: (- n 2)))
	(:: f1)
	(:: f2)) )
   
   ( (+ (?? a1)
	(expt (sin (? x)) (? n more-than-two?))
	(?? a2))
     none
     (+ (* (expt (sin (: x)) 2)
	   (expt (sin (: x)) (: (- n 2))))
	(:: a1)
	(:: a2)) )
   ))

(define flush-obvious-ones
  (rule-system
   
   ( (+ (?? a1)
	(expt (sin (? x)) 2)
	(?? a2)
	(expt (cos (? x)) 2)
	(?? a3))
     none
     (+ 1 (:: a1) (:: a2) (:: a3)) )

#|;; Sines are before cosines (see note above)
   ( (+ (?? a1)
	(expt (cos (? x)) 2)
	(?? a2)
	(expt (sin (? x)) 2)
	(?? a3))
     none
     (+ (:: a1) (:: a2) (:: a3) 1) )
|#
   
   ( (+ (?? a1)
	(* (expt (sin (? x)) 2) (?? f1))
	(?? a2)
	(* (expt (cos (? x)) 2) (?? f2))
	(?? a3))
     (let ((s1 (rcf:simplify `(* ,@f1)))
	   (s2 (rcf:simplify `(* ,@f2))))
       (if (exact-zero? (rcf:simplify `(- ,s1 ,s2)))
	   s1
	   #f))
     (+ (:: a1) (:: a2) (:: a3) (: predicate-value)) )

#|;; Sines are before cosines (see note above)
   ( (+ (?? a1)
	(* (expt (cos (? x)) 2) (?? f1))
	(?? a2)
	(* (expt (sin (? x)) 2) (?? f2))
	(?? a3))
     (let ((s1 (rcf:simplify `(* ,@f1)))
	   (s2 (rcf:simplify `(* ,@f2))))
       (if (exact-zero? (rcf:simplify `(- ,s1 ,s2)))
	   s1
	   #f))
     (+ (:: a1) (:: a2) (:: a3) (: predicate-value)) )
|#
   ))

;;; here are some residual rules.

(define sincos-random
  (rule-system

   ( (+ (?? a1) (? a) (?? a2) (expt (cos (? x)) (? n at-least-two?)) (?? a3))
     (exact-zero? (rcf:simplify `(+ ,a (expt (cos ,x) ,(- n 2)))))
     (+ (:: a1) (:: a2) (:: a3) (* (expt (sin (: x)) 2) (: a))) )

   ( (+ (?? a1) (expt (cos (? x)) (? n at-least-two?)) (?? a2) (? a) (?? a3))
     (exact-zero? (rcf:simplify `(+ ,a (expt (cos ,x) ,(- n 2)))))
     (+ (:: a1) (:: a2) (:: a3) (* (expt (sin (: x)) 2) (: a))) )

   ( (+ (?? a1) (? a) (?? a2) (expt (sin (? x)) (? n at-least-two?)) (?? a3))
     (exact-zero? (rcf:simplify `(+ ,a (expt (sin ,x) ,(- n 2)))))
     (+ (:: a1) (:: a2) (:: a3) (* (expt (cos (: x)) 2) (: a))) )

   ( (+ (?? a1) (expt (sin (? x)) (? n at-least-two?)) (?? a2) (? a) (?? a3))
     (exact-zero? (rcf:simplify `(+ ,a (expt (sin ,x) ,(- n 2)))))
     (+ (:: a1) (:: a2) (:: a3) (* (expt (cos (: x)) 2) (: a))) )

   ( (+ (?? a1)
	(? a)
	(?? a2)
	(* (?? b1) (expt (cos (? x)) (? n at-least-two?)) (?? b2))
	(?? a3))
     (exact-zero? (rcf:simplify `(+ (* ,@b1 ,@b2 (expt (cos ,x) ,(- n 2))) ,a))) 
     (+ (:: a1) (:: a2) (:: a3) (* (: a) (expt (sin (: x)) 2))) )
     
   ( (+ (?? a1)
	(? a)
	(?? a2)
	(* (?? b1) (expt (sin (? x)) (? n at-least-two?)) (?? b2))
	(?? a3))
     (exact-zero? (rcf:simplify `(+ (* ,@b1 ,@b2 (expt (sin ,x) ,(- n 2))) ,a))) 
     (+ (:: a1) (:: a2) (:: a3) (* (: a) (expt (cos (: x)) 2))) )


   ( (+ (?? a1)
	(* (?? b1) (expt (cos (? x)) (? n at-least-two?)) (?? b2))
	(?? a2)
	(? a)
	(?? a3))
     (exact-zero? (rcf:simplify `(+ (* ,@b1 ,@b2 (expt (cos ,x) ,(- n 2))) ,a)))
     (+ (:: a1) (:: a2) (:: a3) (* (: a) (expt (sin (: x)) 2))) )
     
   ( (+ (?? a1)
	(* (?? b1) (expt (sin (? x)) (? n at-least-two?)) (?? b2))
	(?? a2)
	(? a)
	(?? a3))
     (exact-zero? (rcf:simplify `(+ (* ,@b1 ,@b2 (expt (sin ,x) ,(- n 2))) ,a)))
     (+ (:: a1) (:: a2) (:: a3) (* (: a) (expt (cos (: x)) 2))) )

   ))

;;; we can eliminate sin and cos in favor of complex exponentials 

(define sincos->exp1
  (rule-system
   ( (sin (? x)) 
     none
     (/ (- (exp (* +i (: x))) (exp (* -i (: x))))
	+2i) )
     
   ( (cos (? x)) 
     none
     (/ (+ (exp (* +i (: x))) (exp (* -i (: x))))
	2) )
   ))

(define sincos->exp2
  (rule-system
   ( (sin (? x)) 
     none
     (/ (- (exp (* +i (: x))) (/ 1 (exp (* +i (: x)))))
	+2i) )
     
   ( (cos (? x)) 
     none
     (/ (+ (exp (* +i (: x))) (/ 1 (exp (* +i (: x)))))
	2) )
   ))

;;; under favorable conditions, we can replace the trig functions.

(define exp->sincos
  (rule-system
   ( (exp (? c1 imaginary-number?))
     (positive? (n:imag-part c1))
     (+ (cos (: (n:imag-part c1)))
	(* +i (sin (: (n:imag-part c1))))) )

   ( (exp (? c1 imaginary-number?))
     (negative? (n:imag-part c1))
     (+ (cos (: (- (n:imag-part c1))))
	(* -i (sin (: (- (n:imag-part c1)))))) )

   ( (exp (* (? c1 imaginary-number?) (?? f)))
     (positive? (n:imag-part c1))
     (+ (cos (* (: (n:imag-part c1)) (:: f)))
	(* +i (sin (* (: (n:imag-part c1)) (:: f))))) )

   ( (exp (* (? c1 imaginary-number?) (?? f)))
     (negative? (n:imag-part c1))
     (* (exp (: (n:real-part c1)))
	(+ (cos (* (: (- (n:imag-part c1))) (:: f)))
	   (* -i (sin (* (: (- (n:imag-part c1))) (:: f)))))) )

   ( (exp (? c1 complex-number?))
     (positive? (n:imag-part c1))
     (* (exp (: (n:real-part c1)))
	(+ (cos (: (n:imag-part c1)))
	   (* +i (sin (: (n:imag-part c1)))))) )

   ( (exp (? c1 complex-number?))
     (negative? (n:imag-part c1))
     (* (exp (: (n:real-part c1)))
	(+ (cos (: (- (n:imag-part c1))))
	   (* -i (sin (: (- (n:imag-part c1))))))) )

   ( (exp (* (? c1 complex-number?) (?? f)))
     (positive? (n:imag-part c1))
     (* (exp (: (n:real-part c1)))
	(+ (cos (* (: (n:imag-part c1)) (:: f)))
	   (* +i (sin (* (: (n:imag-part c1)) (:: f)))))) )

   ( (exp (* (? c1 complex-number?) (?? f)))
     (negative? (n:imag-part c1))
     (* (exp (: (n:real-part c1)))
	(+ (cos (* (: (- (n:imag-part c1))) (:: f)))
	   (* -i (sin (* (: (- (n:imag-part c1))) (:: f)))))) )
   ))
	    
(define exp-contract
  (rule-system
   ( (* (?? x1) (exp (? x2)) (?? x3) (exp (? x4)) (?? x5))
     none
     (* (:: x1) (:: x3) (:: x5) (exp (+ (: x2) (: x4)))) )

   ( (expt (exp (? x)) (? p)) none (exp (* (: p) (: x))) )

   ( (/ (exp (? x)) (exp (? y))) none (exp (- (: x) (: y))) )

   ( (/ (* (?? x1) (exp (? x)) (?? x2)) (exp (? y)))
     none
     (* (:: x1) (:: x2) (exp (- (: x) (: y)))) )

   ( (/ (exp (? x)) (* (?? y1) (exp (? y)) (?? y2)))
     none
     (/ (exp (- (: x) (: y))) (* (:: y1) (:: y2))) )

   ( (/ (* (?? x1) (exp (? x)) (?? x2))
	(* (?? y1) (exp (? y)) (?? y2)))
     none
     (/ (* (:: x1) (:: x2) (exp (- (: x) (: y))))
	(* (:: y1) (:: y2))) )
   ))

(define exp-expand
  (rule-system
   ( (exp (- (? x1)))
     none
     (/ 1 (exp (: x1))) )

   ( (exp (- (? x1) (? x2)))
     none
     (/ (exp (: x1)) (exp (: x2))) )

   ( (exp (+ (? x1) (? x2) (?? xs)))
     none
     (* (exp (: x1)) (exp (+ (: x2) (:: xs)))) )

   ( (exp (* (? x imaginary-integer?) (?? factors)))
     (> (n:imag-part x) 1)
     (expt (exp (* +i (:: factors))) (: (n:imag-part x))) )

   ( (exp (* (? x imaginary-integer?) (?? factors)))
     (< (n:imag-part x) -1)
     (expt (exp (* -i (:: factors))) (: (- (n:imag-part x)))) )

   ( (exp (* (? n exact-integer?) (?? factors)))
     (> n 1)
     (expt (exp (* (:: factors))) (: n)) )

   ( (exp (* (? n exact-integer?) (?? factors)))
     (< n -1)
     (expt (exp (* -1 (:: factors))) (: (- n))) )

   ( (exp (? x complex-number?))
     none
     (* (exp (: (n:real-part x)))
	(exp (: (n:* (n:imag-part x) +i)))) )

   ( (exp (* (? x complex-number?) (?? factors)))
     none
     (* (exp (* (: (n:real-part x)) (:: factors)))
	(exp (* (: (n:* (n:imag-part x) +i)) (:: factors)))) )
   ))

(define complex-rules
  (rule-system
   ( (make-rectangular (cos (? theta)) (sin (? theta)))
     none
     (exp (* +i (: theta))) )

   ( (real-part (make-rectangular (? x) (? y)))
     none
     (: x) )
   ( (imag-part (make-rectangular (? x) (? y)))
     none
     (: x) )

   ( (magnitude (make-rectangular (? x) (? y)))
     none
     (sqrt (+ (expt (: x) 2) (expt (: y) 2))) )
   ( (angle (make-rectangular (? x) (? y)))
     none
     (atan (: y) (: x)) )


   ( (real-part (make-polar (? m) (? a)))
     none
     (* (: m) (cos (: a))) )
   ( (imag-part (make-polar (? m) (? a)))
     none
     (* (: m) (sin (: a))) )

   ( (magnitude (make-polar (? m) (? a)))
     none
     (: m) )
   ( (angle (make-polar (? m) (? a)))
     none
     (: a) )
   ))

(define divide-numbers-through
  (rule-system
   ( (* 1 (? factor))
     none
     (: factor) )
   
   ( (* 1 (?? factors))
     none
     (* (:: factors)) )

   ( (/ (? n number?) (? d number?))
     none
     (: (/ n d)) )

   ( (/ (+ (?? terms)) (? d number?))
     none
     (+ (:: (map (lambda (term) `(/ ,term ,d))
		 terms))) )

   ( (/ (* (? n number?) (?? factors)) (? d number?))
     none
     (* (: (/ n d)) (:: factors)) )

   ( (/ (* (?? factors)) (? d number?))
     none
     (* (: (n:invert d)) (:: factors)) )


   ( (/ (? n) (* (? d number?) (? factor)))
     none
     (/ (/ (: n) (: d)) (: factor)) )

   ( (/ (? n) (* (? d number?) (?? factors)))
     none
     (/ (/ (: n) (: d)) (* (:: factors))) )


   ( (/ (? n) (? d number?))
     none
     (* (: (n:invert d)) (: n)) )

   ))

;;;; simplifiers defined using these rule sets

;;; assuming that expression comes in canonical it goes out canonical

#|
(define (simplify-until-stable rule-simplify canonicalize)
  (define (simp exp)
    (let ((newexp (rule-simplify exp)))
      (if (equal? exp newexp)
	  exp
	  (simp (canonicalize newexp)))))
  simp)


(define (simplify-until-stable rule-simplify canonicalize)
  (define (simp exp)
    (let ((newexp (rule-simplify exp)))
      (if (equal? exp newexp)
	  exp
	  (let ((cexp (canonicalize newexp)))
	    (if (equal? exp cexp)
		exp
		(simp cexp))))))
  simp)
|#

(define (simplify-until-stable rule-simplify canonicalize)
  (define (simp exp)
    (let ((newexp (rule-simplify exp)))
      (if (equal? exp newexp)
	  exp
	  (let ((cexp (canonicalize newexp)))
	    (cond ((equal? cexp exp) exp)
		  ((exact-zero? (fpf:simplify `(- ,exp ,cexp))) cexp)
		  (else (simp cexp)))))))
  simp)


;;; Once around.

(define (simplify-and-canonicalize rule-simplify canonicalize)
  (define (simp exp)
    (let ((newexp (rule-simplify exp)))
      (if (equal? exp newexp)
	  exp
	  (canonicalize newexp))))
  simp)


;;; the usual canonicalizer is

(define simplify-and-flatten
  (compose fpf:simplify rcf:simplify))



(define (only-if p? do)
  (lambda (exp)
    (if (p? exp) (do exp) exp)))

(define ->poisson-form
  (compose simplify-and-flatten
	   angular-parity
	   (simplify-until-stable
	    (compose trig-product-to-sum simplify-and-flatten contract-expt-trig)
	    simplify-and-flatten)
	   simplify-and-flatten
	   trig->sincos))

(define (trigexpand exp)
  ((compose simplify-and-flatten
	    sincos->trig

	    simplify-and-flatten
	    sincos-flush-ones

	    simplify-and-flatten
	    exp->sincos
	    (simplify-until-stable exp-expand simplify-and-flatten)	
	    (simplify-until-stable exp-contract simplify-and-flatten)
	    (simplify-until-stable exp-expand simplify-and-flatten)
	    simplify-and-flatten
	    sincos->exp1
	    trig->sincos)
   exp))

(define (trigcontract exp)
  ((compose simplify-and-flatten
	    sincos->trig
	    (simplify-until-stable sincos-flush-ones simplify-and-flatten)
	    simplify-and-flatten
	    exp->sincos
	    (simplify-until-stable exp-expand simplify-and-flatten)
	    simplify-and-flatten
	    sincos-flush-ones
	    simplify-and-flatten
	    exp->sincos
	    (simplify-until-stable exp-contract simplify-and-flatten)
	    (simplify-until-stable exp-expand simplify-and-flatten)
	    simplify-and-flatten
	    sincos->exp1
	    trig->sincos)
   exp))

(define (full-simplify exp)
  ((compose rcf:simplify

	    (simplify-until-stable universal-reductions
				   rcf:simplify)
;	    (simplify-until-stable sqrt-contract
;				   rcf:simplify)
	    (simplify-until-stable sqrt-expand
				   rcf:simplify)
	    (simplify-until-stable sqrt-contract
				   rcf:simplify)
	    rcf:simplify
	    logexp->specfun
	    sincos->trig	    

	    simplify-and-flatten
	    sincos-flush-ones

	    rcf:simplify
	    exp->sincos
	    (simplify-until-stable (compose log-expand exp-expand)
				   rcf:simplify)	
	    (simplify-until-stable (compose log-contract exp-contract)
				   rcf:simplify)
	    (simplify-until-stable (compose log-expand exp-expand)
				   rcf:simplify)
	    rcf:simplify
	    sincos->exp1
	    trig->sincos
	    specfun->logexp
	    (simplify-until-stable (compose universal-reductions sqrt-expand)
				   rcf:simplify)
	    rcf:simplify
	    )
   exp))

(define (oe-simplify exp)
  ((compose (simplify-until-stable universal-reductions
				   simplify-and-flatten)
	    (simplify-until-stable sqrt-expand
				   simplify-and-flatten)
	    (simplify-until-stable sqrt-contract
				   simplify-and-flatten)
	    simplify-and-flatten
	    sincos->trig	   
	    (simplify-until-stable sincos-random
				   simplify-and-flatten)


	    simplify-and-flatten
	    sin^2->cos^2

	    simplify-and-flatten
	    sincos-flush-ones

	    (simplify-until-stable (compose log-expand exp-expand)
				   simplify-and-flatten)	
	    (simplify-until-stable (compose log-contract exp-contract)
				   simplify-and-flatten)
	    (simplify-until-stable (compose log-expand exp-expand)
				   simplify-and-flatten)
	    (simplify-until-stable angular-parity
				   simplify-and-flatten)
	    (simplify-until-stable (compose universal-reductions sqrt-expand)
				   simplify-and-flatten)
	    simplify-and-flatten
	    trig->sincos
	    canonicalize-partials
	    )
   exp))

(define (easy-simplify exp)
  ((compose (simplify-until-stable (compose universal-reductions sqrt-expand)
				   simplify-and-flatten)
	    simplify-and-flatten
	    root-out-squares
	    (simplify-until-stable sqrt-contract
				   simplify-and-flatten)

	    sincos->trig
	    (simplify-until-stable sincos-random
				   simplify-and-flatten)
	    simplify-and-flatten
	    sin^2->cos^2

	    simplify-and-flatten
	    sincos-flush-ones

	    (simplify-until-stable (compose log-expand exp-expand)
				   simplify-and-flatten)	
	    (simplify-until-stable (compose log-contract exp-contract)
				   simplify-and-flatten)

	    (simplify-until-stable (compose universal-reductions
					    angular-parity
					    log-expand
					    exp-expand
					    sqrt-expand)
				   simplify-and-flatten)
	    simplify-and-flatten
	    trig->sincos
	    canonicalize-partials
	    )
   exp))

(define (clear-square-roots-of-perfect-squares expr)
  (if sqrt-expt-simplify?
      ((simplify-and-canonicalize (compose universal-reductions
					   root-out-squares)
				  simplify-and-flatten)
       expr)
      expr))

(define (new-simplify exp)
  (define (sqrt? exp)
    (occurs-in? '(sqrt) exp))
  (define (full-sqrt? exp)
    (and sqrt-factor-simplify?
         (occurs-in? '(sqrt) exp)))
  (define (logexp? exp)
    (occurs-in? '(log exp) exp))
  (define (sincos? exp)
    (occurs-in? '(sin cos) exp))
  (define (partials? exp)
    (occurs-in? '(partial) exp))
  ((compose (only-if (lambda (exp) divide-numbers-through-simplify?)
		     divide-numbers-through)
	    (only-if sqrt? clear-square-roots-of-perfect-squares)
	    (only-if full-sqrt?
		     (compose
		      (simplify-until-stable (compose universal-reductions
						      sqrt-expand)
					     simplify-and-flatten)
		      clear-square-roots-of-perfect-squares
		      (simplify-until-stable sqrt-contract
					     simplify-and-flatten)))
	    (only-if sincos?
		     (compose (simplify-and-canonicalize
			       (compose universal-reductions sincos->trig)
			       simplify-and-flatten)
			      (simplify-and-canonicalize angular-parity
							 simplify-and-flatten)
			      (simplify-until-stable sincos-random
						     simplify-and-flatten)
			      (simplify-and-canonicalize sin^2->cos^2
							 simplify-and-flatten)
			      (simplify-and-canonicalize sincos-flush-ones
							 simplify-and-flatten)
			      (if trig-product-to-sum-simplify?
				  (simplify-and-canonicalize trig-product-to-sum
							     simplify-and-flatten)
				  (lambda (x) x))
			      (simplify-and-canonicalize universal-reductions
							 simplify-and-flatten)
			      (simplify-until-stable sincos-random
						     simplify-and-flatten)
			      (simplify-and-canonicalize sin^2->cos^2
							 simplify-and-flatten)
			      (simplify-and-canonicalize sincos-flush-ones
							 simplify-and-flatten)))
	    (only-if logexp?
		     (compose
		      (simplify-and-canonicalize universal-reductions
						 simplify-and-flatten)
		      (simplify-until-stable (compose log-expand exp-expand)
					     simplify-and-flatten)	
		      (simplify-until-stable (compose log-contract exp-contract)
					     simplify-and-flatten)))
	    (simplify-until-stable (compose universal-reductions
					    (only-if logexp?
						     (compose log-expand
							      exp-expand))
					    (only-if sqrt? sqrt-expand))
				   simplify-and-flatten)
	    (only-if sincos?
		     (simplify-and-canonicalize angular-parity
						simplify-and-flatten))
	    (simplify-and-canonicalize trig->sincos simplify-and-flatten)
	    (only-if partials?
		     (simplify-and-canonicalize canonicalize-partials
						simplify-and-flatten))
	    simplify-and-flatten
	    )
   exp))





