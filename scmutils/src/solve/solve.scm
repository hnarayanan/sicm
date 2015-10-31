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

#| -*-Scheme-*-

$Id$

Copyright 2006 Massachusetts Institute of Technology

This program is free software; you can redistribute it and/or modify
it under the terms of the GNU General Public License as published by
the Free Software Foundation; either version 2 of the License, or (at
your option) any later version.

This program is distributed in the hope that it will be useful, but
WITHOUT ANY WARRANTY; without even the implied warranty of
MERCHANTABILITY or FITNESS FOR A PARTICULAR PURPOSE.  See the GNU
General Public License for more details.

You should have received a copy of the GNU General Public License
along with this program; if not, write to the Free Software
Foundation, Inc., 51 Franklin St, Fifth Floor, Boston, MA 02110-1301,
USA.

|#

;;;; Simple catch-as-catch-can solver #42.  
;;;  By GJS, November 2003.  For use with SCMUTILS.
;;;  Updated by GJS, 7 April 2008.
;;;  Updated by GJS, 8 April 2009, with suggestions by AXCH.
;;;  Improved by GJS, 16 December 2009.
;;;  Improved by GJS, 10 April 2014: if progress, hopeless->residual.

(declare (usual-integrations))

;;; Must be loaded before handler.scm if used in constraint propagator.
;;; Assumes standardized equations, as produced below.

#|
(define (solve-incremental equations variables) ...)

;;; Variables list gives variables to be eliminated in the preferred
;;; order of elimination.

;;; To access the parts of a solution returned by solve-incremental

(define (residual-equations solution) ...)
(define (residual-variables solution) ...)
(define (substitutions solution) ...)
(define (hopeless-variables solution) ...)


;;; To access substitutions produced by solve-incremental

(define (substitution-variable subst) ...)
(define (substitution-expression subst) ...)
(define (substitution-justifications subst) ...)


;;; To access residual equations of a solution

(define (equation-expression eqn) ...)
(define (equation-justifications eqn) ...)
|#

#|
We are looking to accumulate a substitution for
each variable, and use it to eliminate that
variable from the resulting equations and
previously acquired substitutions.

The general strategy is:

1. Choose a variable to eliminate.

2. Look for an equation for which that variable
   can be isolated.

3. Isolate and make a substitution for that
   variable.

4. Use the substitution to eliminate the variable
   from the remaining equations.

5. Use the substitution to eliminate the variable
   from the accumulated substitutions.

6. If more variables and more equations,
     go to step #1.
|#

(define solve-simplifier g:simplify)

(define (residual-equations solution) (car solution))
(define (residual-variables solution) (cadr solution))
(define (substitutions solution) (caddr solution))
(define (hopeless-variables solution) (cadddr solution))

(define (solve-incremental equations variables #!optional substitutions hopeless)
  (if (default-object? substitutions) (set! substitutions '()))
  (if (default-object? hopeless) (set! hopeless '()))
  (let lp ((residual-eqs
	    (flush-tautologies
	     (map (lambda (equation)
		    (apply-substitutions-to-equation equation substitutions))
		  equations)))
	   (residual-vars     variables)
	   (substitutions     substitutions)
	   (hopeless-vars     hopeless)
	   (progress          #f))
    (define (return)
      (list residual-eqs residual-vars substitutions hopeless-vars))
    (cond ((null? residual-eqs)		; Done
	   (return))
	  ((null? residual-vars)
	   (cond ((null? hopeless-vars) (return))
		 (progress
		  (lp residual-eqs hopeless-vars substitutions '() #f))
		 (else (return))))
	  (else
	   (let ((var (car residual-vars)))
	     (define (hopeless)
	       (lp residual-eqs
		   (cdr residual-vars)
		   substitutions
		   (cons var hopeless-vars)
		   progress))
	     (let ((eqs
		    (filter (lambda (eq)
			      (member var (equation-variables eq)))
			    residual-eqs)))
	       (if (null? eqs)
		   (hopeless)
		   (isolate-var var (sort eqs fewer-variables?)
		    (lambda (new-substitution equation-used)	
		      (lp (flush-tautologies
			   (next-equations new-substitution
			     (delete equation-used residual-eqs)))
			  (append (cdr residual-vars) hopeless-vars)
			  (cons new-substitution
				(next-substitutions new-substitution
						    substitutions))
			  '()
			  #t))
		    hopeless))))))))

(define (fewer-variables? eqn1 eqn2)
  (< (length (equation-variables eqn1))
     (length (equation-variables eqn2))))

(define (flush-tautologies equations)
  (filter (lambda (eqn) 
	    (let ((expr
		   (equation-expression eqn)))
	      (not (and (number? expr)
			(= expr 0)))))
	  equations))

(define (next-equations substitution equations)
  (map (lambda (equation)
	 (backsubstitute-equation substitution
				  equation))
       equations))

(define (next-substitutions new-substitution
			    substitutions)
  (map (lambda (substitution)
	 (backsubstitute-substitution new-substitution
				      substitution))
       substitutions))


;;; To isolate a variable, given a set of
;;; equations we go through the equations and try
;;; to isolate the variable from each one.  Note
;;; that in the following code we find the first
;;; reference to justifications.

(define (isolate-var  var eqs succeed fail)
  ;; succeed = (lambda (new-substitution equation-used) ...)
  ;; fail    = (lambda () ...)
  (let lp ((eqs-to-scan eqs))
    (cond ((null? eqs-to-scan) (fail))
	  ((member var (equation-variables (car eqs-to-scan)))
	   (isolatable? var (car eqs-to-scan)
	     (lambda (value)
	       (succeed
		(make-substitution var value
		   (equation-justifications (car eqs-to-scan)))
		(car eqs-to-scan)))
	     (lambda ()
	       (lp (cdr eqs-to-scan)))))
	  (else (lp (cdr eqs-to-scan))))))

	   
(define (occurs? var expr)
  (or (equal? var expr)
      (and (pair? expr)
	   (or (occurs? var (car expr))
	       (occurs? var (cdr expr))))))
	
;;; Isolatable is a kludge.  It should have been
;;; written as a pattern-matcher, but it wasn't
;;; when I wrote it.  Sorry...

(define (isolatable? var eqn succeed fail)
  (let lp ((expr (equation-expression eqn)))
    (cond ((equal? var expr) (succeed 0))
	  ((positive-power? expr)
	   (lp (car (operands expr))))
	  ((product? expr)
	   ;; If var^n in operands then zero.
	   (var-in-product var expr succeed fail))
	  ((sum? expr)
	   (var-in-sum var expr succeed fail))
	  ((quotient? expr)
	   (isolatable? var
			(car (operands expr))
			succeed
			fail))
	  (else (fail)))))

(define (positive-power? expr)
  (and (expt? expr)
       (number? (cadr (operands expr)))
       (> (cadr (operands expr)) 0)))

(define (var-in-product var expr succeed fail)
  (let lp ((factors (operands expr)))
    (if (pair? factors)
	(let ((ff (car factors)))
	  (cond ((and (symbol? ff)
		      (equal? var ff))
		 (succeed 0))
		((and
		  (positive-power? ff)
		  (equal? var
			  (car (operands ff))))
		 (succeed 0))
		(else
		 (lp (cdr factors)))))
	(fail))))

(define (var-in-sum var expr succeed fail)
  ;; Split addends into those with var^1 and those without...
  (let lp ((addends (operands expr))
	   (with '())
	   (without '()))
    (cond ((null? addends)
	   (if (null? without)
	       (succeed 0)
	       (succeed
		(symb:quo (symb:negate (symb:add:n without))
			  (symb:add:n with)))))
	  ((occurs? var (car addends))
	   (let ((addend (car addends)))
	     (cond ((equal? var addend)
		    (lp (cdr addends)
			(cons 1 with)
			without))
		   ((product? addend)
		    (let ((factors (operands addend)))
		      (if (member var factors)
			  (lp (cdr addends)
			      (cons
			       (symb:mul:n (delete var factors))
			       with)
			      without)
			  ;; occurs more painfully
			  (fail))))
		   (else (fail)))))
	  (else
	   (lp (cdr addends)
	       with
	       (cons (car addends) without))))))

(define (backsubstitute-substitution new-substitution substitution)
  (if (occurs? (substitution-variable new-substitution)
	       (substitution-expression substitution))
      (make-substitution
       (substitution-variable substitution)
       (substitute (substitution-expression new-substitution)
		   (substitution-variable new-substitution)
		   (substitution-expression substitution))
       (list-union (substitution-justifications substitution)
		   (substitution-justifications new-substitution)))
      substitution))

(define (backsubstitute-equation substitution equation)
  (if (occurs? (substitution-variable substitution)
	       (equation-expression equation))
      (make-equation
       (substitute (substitution-expression substitution)
		   (substitution-variable substitution)
		   (equation-expression equation))
       (list-union (equation-justifications equation)
		   (substitution-justifications substitution)))
      equation))
(define (substs->equations substs)
  (map subst->equation substs))

(define (subst->equation subst)
  (make-equation
   (symb:- (substitution-variable subst)
	   (substitution-expression subst))
   (substitution-justifications subst)))

(define (apply-substitutions expression substitutions)
  (let loop ((expression expression)
	     (substs substitutions)
	     (justs '()))
    (cond  ((null? substs) (cons expression justs))
	   ((occurs? (substitution-variable (car substs)) expression)
	    (loop (solve-simplifier
		   (substitute (substitution-expression (car substs))
			       (substitution-variable (car substs))
			       expression))
		  (cdr substs)
		  (list-union justs
			      (substitution-justifications (car substs)))))
	   (else (loop expression (cdr substs) justs)))))

(define (apply-substitutions-to-equation equation substitutions)
  (let ((result
	 (apply-substitutions (equation-expression equation)
			      substitutions)))
    (let ((expression (car result)) (justs (cdr result)))
      (make-equation expression
		     (list-union justs
				 (equation-justifications equation))))))

(define (make-substitution var value justs)
  (list (list '= var (solve-simplifier value)) justs))

(define (substitution-variable subst) (cadar subst))
(define (substitution-expression subst) (caddar subst))
(define (substitution-justifications subst) (cadr subst))



(define (make-equation expr justs)
  (let* ((specs (standardize-equation expr '() '() #f))
	 (pexpr (car specs))
	 (vspecs (cadr specs)))
    (if (and (number? pexpr) (not (= pexpr 0)))
	(begin (if *solve:contradiction-wallp*
		   (write-line `(contradiction ,justs)))
	       (list pexpr justs vspecs))
	(list pexpr justs vspecs))))

(define (equation-expression eqn) (car eqn))
(define (equation-justifications eqn) (cadr eqn))
(define (equation-variables eqn) (caddr eqn))

(define *solve:contradiction-wallp* #t)


(define (contradictory-eqn? eqn)
  (let ((expr (equation-expression eqn)))
    (and (number? expr) (not (= expr 0)))))

(define (eqn-contradiction? solution)
  (any contradictory-eqn? (residual-equations solution)))

;;; This stuff is in support of standardize-equation, below.

(define *zero-threshold* 1e-15)		;for small numbers

(define (differential-operator? expression)
  (or (D? expression) (Dn? expression)))

(define (D? x)
  (and (pair? x)
       (eq? (car x) 'D)))

(define (Dn? x)
  (and (pair? x) 
       (expt? (car x))
       (eq? (car (operands (car x))) 'D)))


;;; The procedure standardize-equation is a wierd device that performs
;;; several functions.  It walks the residual, finding the variables
;;; that one might want to solve for and adds them to the given
;;; variables.  This is the new-map.  Given an independent variable,
;;; say t, it finds expression that are functions of t, such as (f t),
;;; ((D f) t), (((expt D 2) f) t) and adds them to the given functions
;;; and the map.  This is useful for hacking differential equations,
;;; by extracting the time functions from the algebraic skeleton.

;;; This code also finds very small numbers and makes them zero, to
;;; improve simplification -- this is questionable.

#|
;;; For example...
(pp (standardize-equation '(- (* 3 ((D f) t))
			      (+ (* (sqrt x) z (f t))
				 (g t)
				 (((expt D 2) g) t)
				 (square y)))
			  '() '() 't))
((+ (* -1 z (f t) (sqrt x))
    (* -1 (expt y 2))
    (* 3 ((D f) t))
    (* -1 (g t))
    (* -1 (((expt D 2) g) t)))
 ((((expt D 2) g) t) (g t) ((D f) t) y x (f t) z)
 (((expt D 2) g) g (D f) f))
|#

(define (standardize-equation residual variables functions variable)
  ;; returns list = (new-residual new-map functions)
  (let ((redo #f))	; True if an inexact number becomes exact
    (define (walk-expression expression map functions continue)
      (cond ((pair? expression)
	     (let ((rator (operator expression))
		   (rands (operands expression)))
	       (cond ((and (= (length rands) 1) (eq? (car rands) variable))
		      (continue expression
				(list-adjoin expression map)
				(list-adjoin rator functions)))
		     ((differential-operator? expression)
		      (continue expression
				map
				(list-adjoin expression functions)))
		     (else
		      (walk-expression rator map functions
			(lambda (rator-result rator-map rator-functions)
			  (walk-list rands rator-map rator-functions
			     (lambda (rands-result rands-map rands-functions)
			       (continue (cons rator-result rands-result)
					 rands-map
					 rands-functions)))))))))
	    ((number? expression)
	     (continue (if (and (inexact? expression)
				(< (abs expression) *zero-threshold*))
			   (begin (set! redo #t) 0)
			   expression)
		       map
		       functions))
	    ((memq expression '(+ - / * D expt sqrt exp sin cos))
	     (continue expression map functions))
	    (else
	     (continue expression (list-adjoin expression map) functions))))
    (define (walk-list elist map functions continue)
      (if (pair? elist)
	  (walk-expression (car elist) map functions
	    (lambda (car-result car-map car-functions)
	      (walk-list (cdr elist) car-map car-functions 
			 (lambda (cdr-result cdr-map cdr-functions)
			   (continue (cons car-result cdr-result)
				     cdr-map
				     cdr-functions)))))
	  (continue elist map functions)))
    (let lp ((residual (solve-simplifier residual)))
      (walk-expression (if (quotient? residual)
			   (symb:dividend residual)
			   residual)
		       variables
		       functions
		       (lambda (expression map funs)
			 (if redo
			     (begin (set! redo #f)
				    (lp (solve-simplifier expression)))
			     (list expression map funs)))))))

#|
;;; Signs of life.  

(pp (solve-incremental
     (list (make-equation '(+ (* 3 x)     y  -7)  (list 'A))
	   (make-equation '(+ (* 3 x) (- y)  -5)  (list 'B)))
     '(x y)))
(() () (((= y 1) (B A)) ((= x 2) (B A))) ())


(pp (solve-incremental
     (list (make-equation '(+  x   y   z  1)  (list 'A))
	   (make-equation '(+  x   y      2)  (list 'B))
	   (make-equation '(+  x          1)  (list 'C)))
     '(x y z)))
(() () (((= z 1) (A B C)) ((= y -1) (B C)) ((= x -1) (C))) ())


(pp (solve-incremental
     (list (make-equation '(+  x          1)  (list 'C))
	   (make-equation '(+  x   y      2)  (list 'B))
	   (make-equation '(+  x   y   z  1)  (list 'A)))
     '(x y z)))
(() () (((= z 1) (A B C)) ((= y -1) (B C)) ((= x -1) (C))) ())

;;; The following signals a contradiction, as it should:

(pp (solve-incremental
     (list (make-equation '(+ (* 3 x)     y  -7)  (list 'A))
	   (make-equation '(+ (* 3 x)     y  -5)  (list 'B)))
     '(x y)))
(contradiction (B A))
(((2 (B A))) () (((= x (+ 7/3 (* -1/3 y))) (A))) (y))


;;; Some slightly nonlinear systems can be solved:

(pp (solve-incremental
     (list (make-equation '(-  3 (+ x y))  (list 'A))
	   (make-equation '(-  5 (- x y))  (list 'B))
	   (make-equation '(-  3 (+ (* (sqrt x) z) (square y)))  (list 'C)))
     '(x y z)))
(() () (((= z 1) (C B A)) ((= y -1) (B A)) ((= x 4) (B A))) ())

;;; Underdetermined systems can be reduced:

(pp (solve-incremental
     (list (make-equation '(+ (* (+ a b) (- a c)) c)  (list 'A))
	   (make-equation '(- 3 (+ a b))  (list 'B)))
     '(a b c)))
(() (c) (((= b (+ 3 (* -2/3 c))) (A B)) ((= a (* 2/3 c)) (A B))) ())


(pp (solve-incremental
     (list (make-equation '(+ (* (+ a b) (- a c)) c)  (list 'A))
	   (make-equation '(- 3 (- a c))  (list 'B)))
     '(a b c)))
(() (c) (((= b (+ -3 (* -4/3 c))) (A B)) ((= a (+ 3 c)) (B))) ())


;;; Even very hard ones are clarified.

(pp (solve-incremental
     (list (make-equation '(+ (* (+ a b) (- a c)) c)  (list 'A))
	   (make-equation '(- 3 (- a b))  (list 'B)))
     '(a b c)))
(()
 (b)
 (((= c (/ (+ 9 (* 2 (expt b 2)) (* 9 b)) (+ 2 (* 2 b)))) (A B))
  ((= a (+ 3 b)) (B)))
 ())

;;; The following are permutations of the solution sequence

(pp (solve-incremental
     (list (make-equation '(+ (* (- x (* 2 y)) (expt z 2)) (* 2 z) 1) (list 'C))
	   (make-equation '(+ (* 3 x)     y  -7)  (list 'A))
	   (make-equation '(+ (* 3 x) (- y)  -5)  (list 'B)))
     '(x y z)))
(() () (((= z -1/2) (C B A)) ((= y 1) (B A)) ((= x 2) (B A))) ())

(pp (solve-incremental
     (list (make-equation '(+ (* (- x (* 2 y)) (expt z 2)) (* 2 z) 1) (list 'C))
	   (make-equation '(+ (* 3 x)     y  -7)  (list 'A))
	   (make-equation '(+ (* 3 x) (- y)  -5)  (list 'B)))
     '(z x y)))
(() () (((= z -1/2) (C B A)) ((= y 1) (B A)) ((= x 2) (B A))) ())

(pp (solve-incremental
     (list (make-equation '(+ (* (- x (* 2 y)) (expt z 2)) (* 2 z) 1) (list 'C))
	   (make-equation '(+ (* 3 x)     y  -7)  (list 'A))
	   (make-equation '(+ (* 3 x) (- y)  -5)  (list 'B)))
     '(y z x)))
(() () (((= z -1/2) (C B A)) ((= x 2) (B A)) ((= y 1) (B A))) ())


(pp (solve-incremental
     (list (make-equation '(+ (* (- x (* 2 y)) (expt z 2)) (* 2 z) 1) (list 'C))
	   (make-equation '(+ (* 3 x)     y  -7)  (list 'A))
	   (make-equation '(+ (* 3 x) (- y)  -5)  (list 'B)))
     '(y x z)))
(() () (((= z -1/2) (C B A)) ((= x 2) (B A)) ((= y 1) (B A))) ())

(pp (solve-incremental
     (list (make-equation '(+ (* (- x (* 2 y)) (expt z 2)) (* 2 z) 1) (list 'C))
	   (make-equation '(+ (* 3 x)     y  -7)  (list 'A))
	   (make-equation '(+ (* 3 x) (- y)  -5)  (list 'B)))
     '(z y x)))
(() () (((= z -1/2) (C B A)) ((= x 2) (B A)) ((= y 1) (B A))) ())

(pp (solve-incremental
     (list (make-equation '(+ (* (- x (* 2 y)) (expt z 2)) (* 2 z) 1) (list 'C))
	   (make-equation '(+ (* 3 x)     y  -7)  (list 'A))
	   (make-equation '(+ (* 3 x) (- y)  -5)  (list 'B)))
     '(x z y)))
(() () (((= z -1/2) (C B A)) ((= y 1) (B A)) ((= x 2) (B A))) ())

;;; Loses Badly
(pp (solve-incremental
     (list (make-equation '(- 200/3 (/ 1 (+ (/ 1 R1) (/ 1 R2))))  (list 'A))
	   (make-equation '(-  1/3 (/ R2 (+ R1 R2)))  (list 'B)))
     '(R1 R2)))
((((+ (* -2/3 (expt R2 2)) (* 200/3 R2)) (B A) (R2)))
 ()
 (((= R1 (/ (* 200/3 R2) (+ -200/3 R2))) (A)))
 (R2))

;;; I still lose if I clear denominators and do it in the other order
(pp (solve-incremental
     (list (make-equation '(- (* 1/3 (+ R1 R2)) R2)  (list 'B))
	   (make-equation '(- (* 200/3 (+ R1 R2)) (* R1 R2))  (list 'A)))
     '(R1 R2)))
((((+ (* -2 (expt R2 2)) (* 200 R2)) (A B) (R2)))
 ()
 (((= R1 (* 2 R2)) (B)))
 (R2))

;;; Unfortunately, the extra root, R2=0 & R1=0 satisfies the given
;;; equations but not the original problem.
|#

#|
;;; A real problem, showing how dependencies keep track of contributions to solution
(define equations
  (list (make-equation
	 '(+ (* -1/6 eta sr0) (* 1/12 eta sr1) (* -1/2 nu siga0) (* -1/4 nu siga1)
	     (* -1/4 nu siga2) (* -1/2 nu siga3) (* -1 nu siga4) (* 1/2 sigd0)
	     (* 1/4 sigd1) (* 1/4 sigd2) (* 1/2 sigd3) sigd4)
	 (list 'A))
	(make-equation
	 '(+ (* -1/4 eta sd1) (* 1/8 eta sr1) (* -1/8 nu siga1) (* 1/8 nu siga2)
	     (* 1/8 sigd1) (* -1/8 sigd2))
	 (list 'B))
	(make-equation
	 '(+ (* -1/4 eta sd1) (* 1/8 eta sr1) (* -1/8 nu siga1) (* 1/8 nu siga2)
	     (* 1/8 sigd1) (* -1/8 sigd2))
	 (list 'C))
	(make-equation
	 '(+ (* -1/4 eta sr1) (* -1/4 nu siga1) (* -1/4 nu siga2) (* -1/2 nu siga3)
	     (* 1/4 sigd1) (* 1/4 sigd2) (* 1/2 sigd3))
	 (list 'D))
	(make-equation
	 '(+ (* -1 eta sd0) (* -1/2 eta sd1) (* -1/2 eta sr0) (* 1/4 eta sr1)
	     (* -1/2 nu siga0) (* -1/4 nu siga1) (* 1/4 nu siga2) (* 1/2 sigd0)
	     (* 1/4 sigd1) (* -1/4 sigd2))
	 (list 'E))
	(make-equation
	 '(+ (* -1/8 eta sa0) (* 1/16 eta sd1) (* -1/16 eta sr1) (* 1/16 nu sigd1)
	     (* -1/16 nu sigd2) (* -1/16 siga1) (* 1/16 siga2))
	 (list 'F))
	(make-equation
	 '(+ (* 1/8 eta sa0) (* -1/16 eta sd1) (* 1/16 eta sr1) (* -1/16 nu sigd1)
	     (* 1/16 nu sigd2) (* 1/16 siga1) (* -1/16 siga2))
	 (list 'G))
	(make-equation
	 '(+ (* -3/8 eta sa0) (* -1/2 eta sa1) (* -1/16 eta sd1) (* -3/16 eta sr1)
	     (* -1/16 nu sigd1) (* -3/16 nu sigd2) (* -1/4 nu sigd3) (* 1/16 siga1)
	     (* 3/16 siga2) (* 1/4 siga3))
	 (list 'H))
	(make-equation
	 '(+ (* 3/8 eta sa0) (* 1/2 eta sa1) (* 1/16 eta sd1) (* 3/16 eta sr1)
	     (* 1/16 nu sigd1) (* 3/16 nu sigd2) (* 1/4 nu sigd3) (* -1/16 siga1)
	     (* -3/16 siga2) (* -1/4 siga3))
	 (list 'I))
	(make-equation
	 '(+ (* -1/4 eta sd0) (* -1/8 eta sd1) (* -1/4 eta sr0) (* 1/8 eta sr1)
	     (* -1/4 nu sigd0) (* -1/8 nu sigd1) (* 1/8 nu sigd2) (* 1/4 siga0)
	     (* 1/8 siga1) (* -1/8 siga2))
	 (list 'J))
	(make-equation
	 '(+ (* -1/4 eta sd0) (* -1/8 eta sd1) (* 1/12 eta sr0) (* -1/24 eta sr1)
	     (* -1/4 nu sigd0) (* -1/8 nu sigd1) (* -3/8 nu sigd2) (* -1/2 nu sigd3)
	     (* -1 nu sigd4) (* 1/4 siga0) (* 1/8 siga1) (* 3/8 siga2) (* 1/2 siga3)
	     siga4)
	 (list 'K))
	))

(define unknowns
  '(siga0 siga1 siga2 siga3 siga4 sigd0 sigd1 sigd2 sigd3 sigd4 sa0 sa1 sd0 sd1))

(define solution (solve-incremental equations unknowns))

(pp solution)
(()					; no residuals left
 (sa0 sa1 sd0 sd1 siga3 sigd3)		; excess variables
 (((= sigd4				; substitutions
      (/ (+ (* 3 eta nu sa0)
	    (* 3 eta nu sa1)
	    (* eta nu sr0)
	    (* eta nu sr1)
	    (* 3 eta sd0)
	    (* eta sr0)
	    (* eta sr1))
	 (+ -3 (* 3 (expt nu 2)))))
   (K A J E H D F B))
  ((= sigd2
      (/ (+ (* -2 eta nu sa0)
	    (* -2 eta nu sa1)
	    (* -1 eta nu sr1)
	    (* -1 (expt nu 2) sigd3)
	    (* eta sd1)
	    (* -1 eta sr1)
	    sigd3)
	 (+ -1 (expt nu 2))))
   (H D F B))
  ((= sigd1
      (/ (+ (* -2 eta nu sa1)
	    (* -1 eta nu sd1)
	    (* -1 (expt nu 2) sigd3)
	    (* -1 eta sd1)
	    sigd3)
	 (+ -1 (expt nu 2))))
   (H D F B))
  ((= sigd0
      (/ (+ (* -1 eta nu sa0)
	    (* -1 eta nu sd0)
	    (* -1 eta nu sr0)
	    (* -2 eta sd0)
	    (* -1 eta sr0))
	 (+ -1 (expt nu 2))))
   (J E F B))
  ((= siga4
      (/ (+ (* 3 eta nu sd0)
	    (* eta nu sr0)
	    (* eta nu sr1)
	    (* 3 eta sa0)
	    (* 3 eta sa1)
	    (* eta sr0)
	    (* eta sr1))
	 (+ -3 (* 3 (expt nu 2)))))
   (K A J E H D F B))
  ((= siga2
      (/ (+ (* eta nu sd1)
	    (* -1 eta nu sr1)
	    (* -1 (expt nu 2) siga3)
	    (* -2 eta sa0)
	    (* -2 eta sa1)
	    (* -1 eta sr1)
	    siga3)
	 (+ -1 (expt nu 2))))
   (H D F B))
  ((= siga1
      (/ (+ (* -1 eta nu sd1)
	    (* -1 (expt nu 2) siga3)
	    (* -2 eta sa1)
	    (* -1 eta sd1)
	    siga3)
	 (+ -1 (expt nu 2))))
   (H D F B))
  ((= siga0
      (/ (+ (* -2 eta nu sd0)
	    (* -1 eta nu sr0)
	    (* -1 eta sa0)
	    (* -1 eta sd0)
	    (* -1 eta sr0))
	 (+ -1 (expt nu 2))))
   (J E F B)))
 ())		 ; no variables considered hopeless (no way to isolate)

;;; Check
(pp (map (lambda (equation)
	     (apply-substitutions-to-equation equation
					      (substitutions solution)))
	   equations))
((0 (K H D J E F B A) ())
 (0 (H D F B) ())
 (0 (H D F B C) ())
 (0 (H F B D) ())
 (0 (H D J F B E) ())
 (0 (H D B F) ())
 (0 (H D F B G) ())
 (0 (D F B H) ())
 (0 (H D F B I) ())
 (0 (H D E F B J) ())
 (0 (A H D J E F B K) ()))
|#

;;; To make this less painful, 
;;; given a structure of residuals that should be zero.


(define (simple-solve struct unknowns #!optional knowns show-eqns?)
  (define (make-equations resids)
    (map (lambda (e n)
	   (make-equation e (list (symbol 'eq: n))))
	 resids
	 (iota (length resids))))
  (define (make-substitutions news olds expression)
    (assert (= (length news) (length olds)))
    (let lp ((n news) (o olds) (expression expression))
      (cond ((null? n) expression)
	    ((equal? (car n) (car o))
	     (lp (cdr n) (cdr o) expression))
	    (else
	     (lp (cdr n)
		 (cdr o)
		 (substitute (car n) (car o) expression))))))
  (let* ((knowns (if (default-object? knowns) '() knowns))
	 (internal-unknowns
	  (map (lambda (unk)
		 (if (symbol? unk)
		     unk
		     (generate-uninterned-symbol 'x)))
	       unknowns))
	 (internal-knowns
	  (map (lambda (kn)
		 (if (symbol? kn)
		     kn
		     (generate-uninterned-symbol 'k)))
	       knowns))
	 (eqns
	  (flush-tautologies
	   (make-equations
	     (map (lambda (resid)
		    (make-substitutions internal-knowns knowns
		      (make-substitutions internal-unknowns unknowns
					  (simplify resid))))
		  (s:fringe struct)))))
	 (solns
	  (make-substitutions knowns internal-knowns
	    (make-substitutions unknowns internal-unknowns 
	      (solve-incremental eqns internal-unknowns)))))
    (if (not (or (default-object? show-eqns?)
		 (not show-eqns?)))
	(pp eqns))
    #| ;;; Check solver for wrong solutions.
    (pp (simplify
	 (make-substitutions (map substitution-expression (substitutions solns))
			     (map substitution-variable (substitutions solns))
			     (map simplify (s:fringe struct)))))
    |#
    (cons '*solution* solns)))

#|
(simple-solve
 (up '(+ (* 3 x)     y  -7)
     '(+ (* 3 x) (- y)  -5))
 '(x y))
#|
(*solution* ()
	    ()
	    (((= y 1) (eq:1 eq:0))
	     ((= x 2) (eq:1 eq:0)))
	    ()) 
|#

(simple-solve
 (up '(+ (* 3 (f x))     (g y)  -7)
     '(+ (* 3 (f x)) (- (g y))  -5))
 '((f x) (g y))
 '()
 #t)
(((+ -5 (* 3 G439) (* -1 G440)) (eq:0) (G440 G439))
 ((+ -7 (* 3 G439) G440) (eq:1) (G440 G439)))
#|
(*solution* ()
	    ()
	    (((= (g y) 1) (eq:1 eq:0))
	     ((= (f x) 2) (eq:1 eq:0)))
	    ()) 
|#

(simple-solve
 (up '(+ (* 3 (f x))     (g y)  (H q))
     '(+ (* 3 (f x)) (- (g y))  -5))
 '((f x) (g y))
 '((H q))
 #t)
(((+ -5 (* 3 x57) (* -1 x58)) (eq:0) (x58 x57))
 ((+ k59 (* 3 x57) x58) (eq:1) (x58 x57 k59)))
#|
(*solution* ()
	    ()
	    (((= (g y) (+ -5/2 (* -1/2 (H q)))) (eq:1 eq:0))
	     ((= (f x) (+ 5/6 (* -1/6 (H q)))) (eq:1 eq:0)))
	    ())
|#
|#
