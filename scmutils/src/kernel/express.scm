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

;;;;  Utilities for manipulating symbolic expressions

(declare (usual-integrations))

(define (operator exp) (car exp))
(define (operands exp) (cdr exp))

(define first-operand cadr)
(define second-operand caddr)
(define rest-operands cddr)

(define (substitute new old expression)
  (define (sloop exp)
    (cond ((equal? old exp) new)
	  ((pair? exp)
	   (cons (sloop (car exp))
		 (sloop (cdr exp))))
	  ((vector? exp)
	   ((vector-elementwise sloop) exp))
	  (else exp)))
  (if (equal? new old) expression (sloop expression)))


;;; Abstract quantities are represented with a type-tagged property list,
;;; implemented as an alist.

(define ((has-property? property-name) abstract-quantity)
  (cond ((pair? abstract-quantity)
	 (assq property-name (cdr abstract-quantity)))
	((symbol? abstract-quantity)
	 (if (eq? property-name 'expression)
	     (list 'expression abstract-quantity)
	     (error "Symbols have only EXPRESSION properties")))
	(else
	 (error "Bad abstract quantity"))))

(define (get-property abstract-quantity property-name #!optional default)
  (cond ((pair? abstract-quantity)
	 (let ((default (if (default-object? default) #f default))
	       (v (assq property-name (cdr abstract-quantity))))
	   (if v (cadr v) default)))
	((symbol? abstract-quantity)
	 (if (eq? property-name 'expression)
	     abstract-quantity
	     default))
	(else
	 (error "Bad abstract quantity"))))
	 

(define (add-property! abstract-quantity property-name property-value)
  (if (pair? abstract-quantity)
      (set-cdr! (last-pair abstract-quantity)
		(list (list property-name property-value)))
      (error "Bad abstract quantity -- ADD-PROPERTY!")))

;;; An abstract quantity may be have a type-tagged expression.

(define (make-numerical-literal expression)
  (make-literal '*number* expression))

(define (make-real-literal expression)
  (let ((e (make-numerical-literal expression)))
    (add-property! e 'real #t)
    e))

(define (make-literal type-tag expression)
  (list type-tag (list 'expression expression)))

(define (make-combination type-tag operator operands) 
  (make-literal type-tag (cons operator operands)))

(define (expression-of abstract-quantity)
  (cond ((pair? abstract-quantity)
	 (let ((v (assq 'expression (cdr abstract-quantity))))
	   (if v
	       (cadr v)
	       (error "No expression for abstract quantity"
		      abstract-quantity))))
	((symbol? abstract-quantity)
	 abstract-quantity)
	(else
	 (error "Bad abstract quantity"))))

;;; In this system, expressions never contain vectors or matrices,
;;; they only contain constructions for them.  Thus we need to be able
;;; to recognize the constructors:

(define (down-maker? expr)
  (and (pair? expr)
       (eq? (car expr) down-constructor-name)))

(define (up-maker? expr)
  (and (pair? expr)
       (eq? (car expr) up-constructor-name)))

(define (vector-maker? expr)
  (and (pair? expr)
       (eq? (car expr) 'vector)))

(define (quaternion-maker? expr)
  (and (pair? expr)
       (eq? (car expr) 'quaternion)))

(define (matrix-by-rows-maker? expr)
  (and (pair? expr)
       (eq? (car expr) 'matrix-by-rows)))

(define (matrix-by-columns-maker? expr)
  (and (pair? expr)
       (eq? (car expr) 'matrix-by-cols)))


(define (matrix-maker? expr)
  (and (pair? expr)
       (or (eq? (car expr) 'matrix-by-rows)
	   (eq? (car expr) 'matrix-by-cols))))

(define (compound-data-constructor? expr)
  (and (pair? expr)
       (memq (car expr)
	     '(list
	       vector
	       quaternion
	       down
	       up
	       matrix-by-rows
	       matrix-by-cols))))

(define (expression expr)
  (define (exprlp expr)
    (cond ((number? expr)
	   (if (and (inexact? expr) heuristic-number-canonicalizer)
	       (heuristic-number-canonicalizer expr)
	       expr))
	  ((symbol? expr) expr)	   
	  ((null? expr) expr)
	  ((differential? expr)
	   `(make-differential-quantity
	     (list ,@(map (lambda (term)
			    `(make-differential-term
			      ',(differential-tags term)
			      ,(exprlp (differential-coefficient term))))
			  (differential-term-list expr)))))
	  ((down? expr)
	   (cons down-constructor-name
		 (let lp ((i 0))
		   (if (fix:= i (s:length expr))
		       '()
		       (cons (exprlp (s:ref expr i))
			     (lp (fix:+ i 1)))))))
	  ((up? expr)		;subsumes vector? below.
	   (cons up-constructor-name
		 (let lp ((i 0))
		   (if (fix:= i (s:length expr))
		       '()
		       (cons (exprlp (s:ref expr i))
			     (lp (fix:+ i 1)))))))
#|	  
	  ((vector? expr)
	   (cons 'vector
		 (vector->list
		  ((vector-elementwise exprlp) expr))))
|#
	  ((quaternion? expr)
	   (cons 'quaternion
		 (vector->list
		  ((vector-elementwise exprlp) (cadr expr)))))
	  ((matrix? expr)
	   `(matrix-by-rows
	     ,@(map (lambda (r)
		      (cons 'list (vector->list r)))
		    (vector->list
		     (matrix->array ((m:elementwise exprlp) expr))))))
	  ((literal-number? expr)
	   (exprlp (expression-of expr)))
	  ((or (with-units? expr) (units? expr))
	   (exprlp (with-si-units->expression expr)))
	  ((pair? expr)
	   (cond ((eq? (car expr) '???) expr)
		 ((memq (car expr) abstract-type-tags)
		  (exprlp (expression-of expr)))
		 (else (safe-map exprlp expr))))
	  ((abstract-function? expr)
	   (exprlp (f:expression expr)))
	  ((operator? expr)
	   (exprlp (operator-name expr)))
	  ((procedure? expr)
	   (procedure-expression expr))
	  ((undefined-value? expr)
	   '*undefined-value*)
	  ((boolean? expr)
	   (if expr 'true 'false))
	  (else (error "Bad expression" expr))))
  (exprlp expr))

(define up-constructor-name 'up)
(define down-constructor-name 'down)

;;; Finds a name, if any, of the given object in the given
;;; environments.  If none, value is #f.

(define (object-name object #!rest environments)
  (let lp ((environments environments))
    (cond ((null? environments)	#f)
	  ((rlookup object (environment-bindings (car environments)))
	   => car)
	  (else (lp (cdr environments))))))

(define (procedure-name f)
  (let ((u2 (unsyntax (procedure-lambda f))))
    (and (pair? u2)
	 (cond ((eq? (car u2) 'named-lambda) (caadr u2))
	       ((eq? (car u2) 'lambda) `(??? ,@(cadr u2)))
	       (else
		(error "Unknown procedure type" f))))))

(define (procedure-expression f)
  (or (eq-get f 'function-name)
      (procedure-name f)
      (object-name f
		   user-generic-environment
		   generic-environment
		   rule-environment
		   numerical-environment
		   scmutils-base-environment)
      '???))


(define (generate-list-of-symbols base-symbol n)
  (generate-list n
    (lambda (i)
      (concatenate-names base-symbol
			 (string->symbol (number->string i))))))

#|
(define (variables-in expr)
  (cond ((pair? expr)
	 (reduce list-union
		 '()
		 (map variables-in expr)))
	((symbol? expr) (list expr))
	(else '())))
|#

(define (variables-in expr)
  (let lp ((expr expr)
	   (vars '())
	   (cont (lambda (vars) vars)))
    (cond ((pair? expr)
	   (lp (car expr)
	       vars
	       (lambda (vars)
		 (lp (cdr expr)
		     vars
		     cont))))
	  ((symbol? expr)
	   (if (memq expr vars)
	       (cont vars)
	       (cont (cons expr vars))))
	  (else (cont vars)))))


(define (pair-up vars vals table)
  (cond ((null? vars)
	 (cond ((null? vals) table)
	       (else
		(error "Too many vals -- PAIR-UP"
		       vars vals))))
	((null? vals)
	 (error "Too few vals -- PAIR-UP"
		vars vals))
	(else
	 (cons (list (car vars) (car vals))
	       (pair-up (cdr vars) (cdr vals)
			table)))))
		

;;; An evaluator for simple expressions

(define (expression-walker environment)
  (define (walk expr)
    (cond ((number? expr) expr)
	  ((symbol? expr)
	   (lookup expr environment))
	  ((pair? expr)
	   (apply (walk (car expr))
		  (map walk (cdr expr))))
	  (else
	   (error "Unknown expression type -- EXPRESSION-WALK"
		  expr))))
  walk)

(define (expr:< expr1 expr2)
  (cond ((null? expr1)
	 (if (null? expr2) #f #t))
	((null? expr2) #f)
	((real? expr1)
	 (if (real? expr2) (< expr1 expr2) #f))
	((real? expr2) #f)
	((symbol? expr1)
	 (if (symbol? expr2)
	     (variable<? expr1 expr2)
	     #f))
	((symbol? expr2) #f)
	((pair? expr1)
	 (cond ((pair? expr2)
		(cond ((fix:< (length expr1) (length expr2)) #t)
		      ((expr:= (car expr1) (car expr2))
		       (expr:< (cdr expr1) (cdr expr2)))
		      ((expr:< (car expr1) (car expr2)) #t)
		      (else #f)))
	       (else #f)))
	((pair? expr2) #f)
	((vector? expr1)
	 (cond ((vector? expr2)
		(cond ((fix:< (vector-length expr1)
			      (vector-length expr2))
		       #t)
		      ((fix:= (vector-length expr1)
			      (vector-length expr2))
		       (let ((n (vector-length expr1)))
			 (let lp ((i 0))
			   (cond ((fix:= i n) #f)
				 ((expr:< (vector-ref expr1 i)
					  (vector-ref expr2 i))
				  #t)
				 ((expr:= (vector-ref expr1 i)
					  (vector-ref expr2 i))
				  (lp (fix:+ i 1)))
				 (else #f)))))
		      (else #f)))
	       (else #f)))
	((vector? expr2) #f)
	((string? expr1)
	 (if (string expr2)
	     (string:<? expr1 expr2)
	     #f))
	((string? expr2) #f)
	(else
	 (< (hash expr1) (hash expr2)))))

(define expr:= equal?)
