#| -*-Scheme-*-

Copyright (C) 1986, 1987, 1988, 1989, 1990, 1991, 1992, 1993, 1994,
    1995, 1996, 1997, 1998, 1999, 2000, 2001, 2002, 2003, 2004, 2005,
    2006, 2007, 2008, 2009, 2010, 2011, 2012, 2013, 2014, 2015, 2016
    Massachusetts Institute of Technology

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

;;;;               Simple Infix parser
;;; Original version from Aaron Graham-Horowitz, Spring 2014
;;; Cleaned up and extended by GJS, Spring 2014
;;;   added
;;;      Multi-argument functions, args separated by commas.
;;;      Whitespace is now ignored, except as stops.
;;;      Quotation of symbols and expressions.  (But not lists!)
;;; Spring 2015 -- GJS
;;;   added
;;;      unary "+"
;;;      full floating-point numbers
;;; Spring 2016 -- Extended by Eli B. Davis to parse a language
;;;  with Lambda, if-then-else, sequences, and definitions.

(declare (usual-integrations))

;;; Tokenizes a string of infix expressions and then parses tokens
;;; using Shunting-yard algorithm.  Can evaluate arbitrary functions
;;; of several arguments using parentheses for application and commas
;;; for separating arguments.

(define (infix-string->combination infix-string)
  (analyze-tokens
   (shunting-yard
    (parse-structure
     (tokenize infix-string)))))

#|
;;; Tests

;;; Definition tests
(infix-string->combination "(x:=5;x*3)")
;Value: (begin (define x 5) (* x 3))

(infix-string->combination "(x:=9*(3-4);x*3)")
;Value: (begin (define x (* 9 (- 3 4))) (* x 3))

;;; If statement tests
(pp (infix-string->combination 
      "if x>2 then (3*4 + 5) else 3-5"))
#|
(if (> x 2)
    (+ (* 3 4) 5)
    (- 3 5))
|#

(pp (infix-string->combination 
      "if x>2 
          then if 3<4 
                  then 5*6 
                  else 9 
          else 3-5"))
#|
(if (> x 2)
    (if (< 3 4)
        (* 5 6)
        9)
    (- 3 5))
|#

(pp (infix-string->combination 
      "if x>2 
          then if 3<4 
                  then 5*6 
                  else 9
          else if e1
                  then c1 
                  else if e2
                          then c2 
                          else e3"))
#|
(if (> x 2)
    (if (< 3 4)
        (* 5 6)
        9)
    (if e1
        c1
        (if e2
            c2
            e3)))
|#

;;; Lambda Tests
(infix-string->combination
 "(lambda x: if x < 0 then -x else x)(-3)")
;Value: ((lambda (x) (if (< x 0) (- x) x)) -3)

(infix-string->combination
 "(lambda x: ((lambda a:a*a)(x)))(2)")
;Value: ((lambda (x) ((lambda (a) (* a a)) x)) 2)

(infix-string->combination "(lambda x: (1 + x))(2)")
;Value ((lambda (x) (+ 1 x)) 2) 

(infix-string->combination "(lambda a:a*a)(2)")
;Value ((lambda (a) (* a a)) 2)

;;; Function definition tests
(pp (infix-string->combination 
      "fact := lambda n:
                if n == 0 
                   then 1
                   else n*fact(n-1);
       fact(4)"))
#|
(begin 
  (define fact 
    (lambda (n) 
      (if (= n 0) 
	  1 
	  (* n (fact (- n 1))))))
  (fact 4))
|#

(pp (infix-string->combination 
     "fib := lambda n: 
              if n == 0 
                 then 0
                 else if n == 1 
                         then 1 
                         else fib(n-1) + fib(n-2);
      fib(20)"))
#|
(begin
 (define fib
   (lambda (n)
     (if (= n 0)
         0
         (if (= n 1)
             1
             (+ (fib (- n 1)) (fib (- n 2)))))))
 (fib 20))
|#

(pp (infix-string->combination
     "square := lambda x: x*x;
      dist := lambda x1, y1, x2, y2: 
                sqrt(square(x1-x2) + square(y1-y2));
      dist(1,0,1,0)"))
#|
(begin
  (begin
    (define square
      (lambda (x)
	(* x x)))
    (define dist
      (lambda (x1 y1 x2 y2)
	(sqrt (+ (square (- x1 x2)) (square (- y1 y2)))))))
  (dist 1 0 1 0))
|#

(infix-string->combination "x:=4; fact(x)")
;Value: (begin (define x 4) (fact x))


;;; Computed function tests

(infix-string->combination "compose := lambda f,g: lambda x: f(g(x))")
;Value: (define compose (lambda (f g) (lambda (x) (f (g x)))))

(infix-string->combination "compose(sin, cos)(3)")
;Value: ((compose sin cos) 3)

(infix-string->combination "(if 1>0 then cos else sin)(3)")
;Value: ((if (> 1 0) cos sin) 3)

(infix-string->combination "(if 0>1 then cos else sin)(3)")
;Value: ((if (> 0 1) cos sin) 3)

(infix-string->combination "(compose((if 0>1 then cos else sin),square))(3)")
;Value: ((compose (if (> 0 1) cos sin) square) 3)

(infix-string->combination "(compose(if 0>1 then cos else sin,square))(3)")
;Value: ((compose (if (> 0 1) cos sin) square) 3)

;;; Arithmetic tests

(infix-string->combination "3*foo(bar)^2")
;Value: (* 3 (expt (foo bar) 2))

(infix-string->combination "3*foo(bar^2)")
;Value: (* 3 (foo (expt bar 2)))

(infix-string->combination "-3*foo(bar^2)")
;Value: (* -3 (foo (expt bar 2)))

(infix-string->combination "3*foo(bar^(-2))")
;Value: (* 3 (foo (expt bar -2)))

;;; But... Ugh... If unary minus had higher
;;; precedence than ^ this would produce 
;;; 3*foo(bar^(-2)) as one would expect
;;; but that change would cause -x^2 to be
;;; interpreted as (-x)^2.  No good choice!
(infix-string->combination "3*foo(bar^-2)")
;Value: (* 3 (foo (- (expt bar 2))))

(infix-string->combination "+3*foo(bar^2)")
;Value: (* 3 (foo (expt bar 2)))

(infix-string->combination "a*x^2+b*x+c")
;Value: (+ (+ (* a (expt x 2)) (* b x)) c)

(infix-string->combination "x^2+b/a*x+c/a")
;Value: (+ (+ (expt x 2) (* (/ b a) x)) (/ c a))

(infix-string->combination "x^2+b/(a*x)+c/a")
;Value: (+ (+ (expt x 2) (/ b (* a x))) (/ c a))

(infix-string->combination "-x^2")
;Value (- (expt x 2))

(infix-string->combination "(-x)^2")
;Value: (expt (- x) 2)

(infix-string->combination "-2.4e20 * foo(bar^2)")
;Value: (* -2.4e20 (foo (expt bar 2)))

(infix-string->combination "-2.4e-20 * foo(bar^2)")
;Value: (* -2.4e-20 (foo (expt bar 2)))

(infix-string->combination "-2*foo(bar^2, 'bletch, mum)")
;Value: (* -2 (foo (expt bar 2) (quote bletch) mum))

;;; Leaving out a comma is disasterous
(infix-string->combination "-2*foo(bar^2, 'bletch mum)")
;Value: (* -2 (foo (expt bar 2) (quote bletch)))

;;; Indeed, those commas are very essential!
(infix-string->combination "-2*foo(bar^2 'bletch, mum)")
;Value: (* -2 (foo (quote (expt bar 2)) bletch))

;;; Internals examples

(pp (tokenize "3*foo(bar)^2"))
#|
((number 2) 
 (binop 5 expt)
 (rparen)
 (var bar)
 (lparen)
 (var foo)
 (binop 3 *)
 (number 3))
|#

(pp (tokenize "3*foo(bar^2)"))
#|
((rparen) 
 (number 2)
 (binop 5 expt)
 (var bar)
 (lparen)
 (var foo)
 (binop 3 *)
 (number 3))

|#

(pp (parse-structure
     (tokenize
      "(lambda n:(lambda r:(3;if 4 then (3) else (6;3)));3) 2")))

#|
((number 2)
 (rparen)
 (number 3)
 (binop -1000 begin)
 (expr
  (rparen)
  (expr (rparen)
        (expr (rparen) (number 3) (binop -1000 begin) (number 6) (lparen))
        (binop 1000 else)
        (expr (rparen) (number 3) (lparen))
        (binop 1000 then)
        (expr (number 4))
        (unop 1000 if)
        (binop -1000 begin)
        (number 3)
        (lparen))
  (unop 1000 (lambda (r)))
  (lparen))
 (unop 1000 (lambda (n)))
 (lparen))
|#
|#
