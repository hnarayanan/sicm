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

;;;; Split a polynomial into factors of various multiplicities.
;;;    Original code by Mira Wilczek, June 2002.
;;;    Redone by GJS, January 2011.

(define (gcd-Dp p)
  ;; Compute the gcd of the all the partial derivatives of p
  (let ((n (poly:arity p)))
    (if (fix:= n 0)
	poly:one
	(let lp ((i 1) (ans (poly:partial-derivative p (list 0))))
	  (if (or (fix:= i n) (poly:one? ans))
	      ans
	      (lp (fix:+ i 1)
		  (poly:gcd (poly:partial-derivative p (list i))
			    ans)))))))
    

(define (split-polynomial p)
  (define (answer tracker const)
    (if (number? (car tracker))
	(cons (car tracker)
	      (cdr (reverse! (cons poly:one (cdr tracker)))))
	(cons const (cdr (reverse! tracker)))))
  (let lp ((m poly:zero) (h p) (tracker '()) (old-s p) (old-m poly:one) )
    (if (poly:one? m)
	(answer tracker h)
	(let* ((gg  (gcd-Dp h))
	       (new-s (poly:quotient h (poly:gcd h gg)))
	       (new-m (poly:gcd gg new-s))
	       (facts (poly:quotient old-s new-s))
	       ;; facts gets all the factors that were completely
	       ;; removed last step, i.e. all those that were to
	       ;; the 1 or 2 power.  The first loop through will
	       ;; get a totally wrong facts, but its gcd with the
	       ;; initial old-m=1 will be 1, so it won't result in
	       ;; incorrect doublefacts or singlefacts.

	       (doublefacts (poly:gcd facts old-m))
	       ;; doublefacts gets all the factors which were to 
	       ;; the power x>1, x<=2, (ergo x=2), in the last step.

	       (singlefacts (poly:quotient new-s new-m))
	       ;; takes out p = all factors only to the 1st power.
	       )
	  (lp new-m
	      ;; the followinghas all factors to the 1 or 2 power
	      ;; completely removed, others now to the power-2.
	      (poly:quotient h (poly:* new-m new-s))
	      ;; tracker of the form
	      ;;   h(vi) = (* (exponent (list-ref tracker k) k))
	      (cons singlefacts (cons doublefacts tracker))
	      new-s
	      new-m)))))

;;; Reconstruction

(define (actual-factors factors)
  (filter (lambda (f)
	    (or (not (number? f))
		(not (= f 1))))
	  (cons (car factors)
		(map (lambda (f n)
		       (symb:expt f (fix:+ n 1)))
		     (cdr factors)
		     (iota (fix:- (length factors) 1))))))

(define (split-polynomial->expression P)
  (let ((factors (factor-polynomial-expression P)))
    (cons '* (actual-factors factors))))

(define (factor-polynomial-expression P)
  (pcf:expression-> (expression P)
		     (lambda (p v)
		       (map (lambda (factor)
			      (g:simplify (pcf:->expression factor v)))
			    (split-polynomial p)))))

#| ;;; Simple test cases.
(pp (split-polynomial->expression
     (* (square (- 'x 'y)) (cube (+ 'x 'y)))))
;Value: (* (expt (+ x (* -1 y)) 2) (expt (+ x y) 3))

(pp (factor-polynomial-expression
     (* (square (- 'x 'y)) (cube (+ 'x 'y)))))
;Value: (1 1 (+ x (* -1 y)) (+ x y))

(pp (factor-polynomial-expression (square (- 'x 'y))))
;Value: (1 1 (+ x (* -1 y)) 1)

(pp (factor-polynomial-expression (* 3 (cube 'z) (+ (square 'x) 'y))))
;Value: (3 (+ (expt x 2) y) 1 z)

(pp (factor-polynomial-expression (* 3 (square 'z) (+ (square 'x) 'y))))
;Value: (3 (+ (expt x 2) y) z 1)
|#

;;; Recursive generalization

(define (pcf:->factors p v)
  (let ((factors (map (lambda (factor)
			(pcf:->expression factor v))
		      (split-polynomial p))))
    (let ((ff (actual-factors factors)))
      (cond ((null? ff) 1)
	    ((null? (cdr ff)) (car ff))
	    (else
	     (cons '*
		   (let lp ((args ff))
		     (cond ((null? args) '())
			   ((product? (car args))
			    (append (operands (car args))
				    (lp (cdr args))))
			   (else
			    (cons (car args)
				  (lp (cdr args))))))))))))


(define poly:factor-analyzer
  (make-analyzer pcf:->factors
		 pcf:expression->
		 pcf:operators-known))

(define poly:factor
  (default-simplifier poly:factor-analyzer))


#|
(define test-poly
  (let ((x 'x) (y 'y))
    (let ((z (square (+ x (* x (expt y 2))))))
      (simplify
       (expression
	(* (expt (+ (cos z) y) 2)
	   (expt (- (cos z) y) 3)))))))

(pp test-poly)
(+ (* -1 (expt y 5))
   (* (expt y 4)
      (cos (+ (* (expt x 2) (expt y 4)) (* 2 (expt x 2) (expt y 2)) (expt x 2))))
   (* 2
      (expt y 3)
      (expt (cos (+ (* (expt x 2) (expt y 4)) (* 2 (expt x 2) (expt y 2)) (expt x 2))) 2))
   (* -2
      (expt y 2)
      (expt (cos (+ (* (expt x 2) (expt y 4)) (* 2 (expt x 2) (expt y 2)) (expt x 2))) 3))
   (* -1
      y
      (expt (cos (+ (* (expt x 2) (expt y 4)) (* 2 (expt x 2) (expt y 2)) (expt x 2))) 4))
   (expt (cos (+ (* (expt x 2) (expt y 4)) (* 2 (expt x 2) (expt y 2)) (expt x 2))) 5))


(pp (poly:factor test-poly))
(* -1
   (expt (+ y (cos (expt (* (+ 1 (expt y 2)) x) 2))) 2)
   (expt (+ y (* -1 (cos (expt (* (+ 1 (expt y 2)) x) 2)))) 3))
|#

;;; Take perfect squares out of square roots.

#|
;;; Serious bug here -- GJS
(define (root-out-squares expression)
  (define (walk expr)
    (if (pair? expr)
	(if (eq? (operator expr) 'sqrt)
	    (process-sqrt expr)
	    (cons (walk (car expr))
		  (walk (cdr expr))))
	expr))
  (define (process-sqrt expr)
    (let ((fact-exp (poly:factor (car (operands expr)))))
      (let lp ((factors (if (product? fact-exp)
			    (operands fact-exp)
			    (list fact-exp)))
	       (odds 1)
	       (evens 1))
	(cond ((null? factors)
	       (symb:* (symb:sqrt odds) evens))
	      ((expt? (car factors))
	       (let ((b (car (operands (car factors))))
		     (e (cadr (operands (car factors)))))
		 (lp (cdr factors)
		     (if (odd? e) (symb:* odds b) odds)
		     (let ((power (quotient e 2)))
		       (cond ((fix:> power 1)
			      (symb:* evens (symb:expt b power)))
			     ((fix:= power 1)
			      (symb:* evens b))
			     (else evens))))))
	      (else
	       (lp (cdr factors)
		   (symb:* (car factors) odds)
		   evens))))))
  (walk expression))
|#

;;; Bug fixed here -- GJS

(define (root-out-squares expression)
  (define (walk expr)
    (if (pair? expr)
	(if (eq? (operator expr) 'sqrt)
	    (process-sqrt expr)
	    (cons (walk (car expr))
		  (walk (cdr expr))))
	expr))
  (define (process-sqrt expr)
    (let ((fact-exp (poly:factor (car (operands expr)))))
      (let lp ((factors (if (product? fact-exp)
			    (operands fact-exp)
			    (list fact-exp)))
	       (odds 1)
	       (evens 1))
	(cond ((null? factors)
               (if (not (and (number? evens) (= evens 1)))
                   (assume! `(non-negative? ,evens) 'root-out-squares))
	       (symb:* (symb:sqrt odds) evens))
	      ((expt? (car factors))
	       (let ((b (car (operands (car factors))))
		     (e (cadr (operands (car factors)))))
		 (if (and (exact-integer? e) (even? e))
		     (lp (cdr factors)
			 odds
			 (let ((power (quotient e 2)))
			   (cond ((fix:> power 1)
				  (symb:* evens (symb:expt b power)))
				 ((fix:= power 1)
				  (symb:* evens b))
				 (else evens))))
		     (lp (cdr factors)
			 (symb:* (car factors) odds)
			 evens))))
	      (else
	       (lp (cdr factors)
		   (symb:* (car factors) odds)
		   evens))))))
  (walk expression))

#|
(pe (root-out-squares
     (sqrt (* (square (+ 'x 'y)) (cube (- 'x 'y))))))
(+ (* (expt x 2) (sqrt (+ x (* -1 y)))) (* -1 (expt y 2) (sqrt (+ x (* -1 y)))))


(pe
 (root-out-squares
  (simplify
   '(/ (+ (* -1
	     (expt R 2)
	     (((partial 0) f)
	      (up (* R (cos phi) (sin theta))
		  (* R (sin phi) (sin theta))
		  (* R (cos theta))))
	     (cos phi)
	     (expt (cos theta) 3)
	     (sin theta))
	  (* -1
	     (expt R 2)
	     (((partial 1) f)
	      (up (* R (cos phi) (sin theta))
		  (* R (sin phi) (sin theta))
		  (* R (cos theta))))
	     (expt (cos theta) 3)
	     (sin phi)
	     (sin theta))
	  (* (((partial 2) f)
	      (up (* R (cos phi) (sin theta))
		  (* R (sin phi) (sin theta))
		  (* R (cos theta))))
	     (sqrt
	      (+ (* (expt R 4) (expt (cos theta) 4))
		 (* -2 (expt R 4) (expt (cos theta) 2))
		 (expt R 4)))
	     (expt (cos theta) 2)))
       (* R (sin theta))))))
(+ (* -1
      R
      (((partial 0) f)
       (up (* R (cos phi) (sin theta))
	   (* R (sin phi) (sin theta))
	   (* R (cos theta))))
      (cos phi)
      (expt (cos theta) 3))
   (* -1
      R
      (((partial 1) f)
       (up (* R (cos phi) (sin theta))
	   (* R (sin phi) (sin theta))
	   (* R (cos theta))))
      (sin phi)
      (expt (cos theta) 3))
   (* -1
      R
      (((partial 2) f)
       (up (* R (cos phi) (sin theta))
	   (* R (sin phi) (sin theta))
	   (* R (cos theta))))
      (expt (cos theta) 2)
      (sin theta)))

;;; Win! Yields the hand-simplified result!
|#
