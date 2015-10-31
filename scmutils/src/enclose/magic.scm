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

;;;; Magic interface to Scheme compiler, CPH & GJS

(declare (usual-integrations))

;;; This is the basic thing...

(define (compile-and-run sexp #!optional environment declarations keep?)
  (if (default-object? environment) (set! environment scmutils-base-environment))
  (if (default-object? declarations) (set! declarations '((usual-integrations))))
  (if (default-object? keep?) (set! keep? 'keep))
  (scode-eval (compile-expression sexp environment declarations keep?) environment))

(define (compile-and-run-numerical sexp #!optional environment declarations keep?)
  (if (default-object? environment) (set! environment scmutils-base-environment))
  (if (default-object? declarations) (set! declarations '((usual-integrations))))
  (if (default-object? keep?) (set! keep? 'keep))
  (scode-eval (compile-numerical sexp environment declarations keep?) environment))

(define (compile-numerical sexp #!optional environment declarations keep?)
  (if (default-object? environment) (set! environment scmutils-base-environment))
  (if (default-object? declarations) (set! declarations '((usual-integrations))))
  (if (default-object? keep?) (set! keep? 'keep))
  (compile-sexp ((compose generic->floating flonumize) sexp)
		environment
		declarations
		keep?))

(define (compile-and-run-sexp sexp #!optional environment declarations keep?)
  (if (default-object? environment) (set! environment scmutils-base-environment))
  (if (default-object? declarations) (set! declarations '((usual-integrations))))
  (if (default-object? keep?) (set! keep? 'keep))
  (scode-eval (compile-sexp sexp environment declarations keep?) environment))

(define (compile-sexp sexp #!optional environment declarations keep?)
  (if (default-object? environment) (set! environment scmutils-base-environment))
  (if (default-object? declarations) (set! declarations '((usual-integrations))))
  (if (default-object? keep?) (set! keep? 'keep))
  (compile-expression (text/cselim (gjs/cselim sexp))
		      environment
		      declarations keep?))



;;; This takes a closed procedure and makes a faster one

(define (compile-procedure procedure #!optional declarations keep-debugging-info?)
  (if (not (procedure? procedure))
      (error:wrong-type-argument procedure "procedure" 'compile-procedure))
  (if (default-object? declarations) (set! declarations '((usual-integrations))))
  (if (default-object? keep-debugging-info?) (set! keep-debugging-info? 'keep))
  (if (compound-procedure? procedure)
      (compiler-output->procedure
       (compile-procedure-text (procedure-lambda procedure)
			       declarations
			       keep-debugging-info?)			       
       (procedure-environment procedure))
      procedure))

;;; Imports from the Scheme compiler subsystem

(define integrate/sexp
  (access integrate/sexp (->environment '(scode-optimizer top-level))))

(define integrate/scode
  (access integrate/scode (->environment '(scode-optimizer top-level))))

(define compiler-output->procedure
  (access compiler-output->procedure (->environment '(compiler top-level))))

(define compile-scode
  (access compile-scode (->environment '(compiler top-level))))

(define scode-variable?
  (access variable? system-global-environment))

(define scode-variable-name
  (access variable-name system-global-environment))

(define make-scode-variable
  (access make-variable system-global-environment))

(define scode-combination-operator
  (access combination-operator system-global-environment))

(define scode-combination-operands
  (access combination-operands system-global-environment))

(define make-scode-combination
  (access make-combination system-global-environment))


;;; Interface procedures to the Scheme compiler

;;; This compiles an s-expression to something that can be evaluated with scode-eval

(define (compile-expression s-expression environment declarations keep-debugging-info?)
  (fluid-let ((sf:noisy? #f))
    (compile-scode
     (integrate/sexp s-expression environment declarations #f)
     (and keep-debugging-info? 'KEEP))))


;;; This compiles a procedure text

(define (compile-procedure-text procedure-text declarations keep-debugging-info?)
  (fluid-let ((sf:noisy? #f))
    (compile-scode
     (integrate/scode (make-declaration declarations procedure-text) #f)
     (and keep-debugging-info? 'KEEP))))


(define (named-combination-transformer do-leaf do-named-combination)
  (letrec
      ((do-expr
	(lambda (expr)
	  ((scode-walk scode-walker expr) expr)))
       (scode-walker
	(make-scode-walker
	 do-leaf
	 `((assignment
	    ,(lambda (expr)
	       (make-assignment (assignment-name expr)
				(do-expr (assignment-value expr)))))
	   (combination
	    ,(lambda (expr)
	       (if (scode-variable? (scode-combination-operator expr))
		   (do-named-combination expr)
		   (make-scode-combination (do-expr (scode-combination-operator expr))
					   (map do-expr
						(scode-combination-operands expr))))))
	   (comment
	    ,(lambda (expr)
	       (make-comment (comment-text expr)
			     (do-expr (comment-expression expr)))))
	   (conditional
	    ,(lambda (expr)
	       (make-conditional (do-expr (conditional-predicate expr))
				 (do-expr (conditional-consequent expr))
				 (do-expr (conditional-alternative expr)))))
	   (delay
	    ,(lambda (expr)
	       (make-delay (do-expr (delay-expression expr)))))
	   (disjunction
	    ,(lambda (expr)
	       (make-disjunction (do-expr (disjunction-predicate expr))
				 (do-expr (disjunction-alternative expr)))))
	   (definition
	    ,(lambda (expr)
	       (make-definition (definition-name expr)
				(do-expr (definition-value expr)))))
	   (lambda
	    ,(lambda (expr)
	       (lambda-components expr
		 (lambda (name required optional rest auxiliary decls body)
		   (make-lambda name required optional rest auxiliary decls
				(do-expr body))))))
	   (sequence
	    ,(lambda (expr)
	       (make-sequence (map do-expr (sequence-actions expr)))))))))
    do-expr))

(define flonumize
  (named-combination-transformer
   (lambda (expr)
     (if (and (number? expr)
	      (real? expr)
	      (exact? expr))
	 (exact->inexact expr)
	 expr))
   (lambda (expr)
     (let ((operator (scode-combination-operator expr))
	   (operands (scode-combination-operands expr)))
       (let ((operator-name (scode-variable-name operator)))
	 (case operator-name
	   ((make-vector make-initialized-vector v:generate vector:generate
	     make-list make-initialized-list
	     s:generate)
	    (make-scode-combination operator
				    (cons (car operands)
					  (map flonumize (cdr operands)))))
	   ((vector-ref vector-set! list-ref s:ref s:with-substituted-coord)
	    (make-scode-combination operator
				    (cons* (flonumize (car operands))
					   (cadr operands)
					   (map flonumize (cddr operands)))))
	   ((matrix-ref matrix-set!)
	    (make-scode-combination operator
				    (cons* (flonumize (car operands)) 
					   (cadr operands)
					   (caddr operands)
					   (map flonumize (cdddr operands)))))
	   ((m:minor m:submatrix ref)
	    (make-scode-combination operator
				    (cons* (flonumize (car operands))
					   (cdr operands))))
	   ((v:make-zero v:make-basis-unit
	     m:make-zero m:make-identity
	     exact->inexact)
	    expr)
	   ((expt)
	    (let ((base (flonumize (car expr)))
		  (e (cadr expr)))
	      (if (exact-integer? e)
		  (cond ((= e 0)
			 1.)
			((= e 1)
			 base)
			((<= 2 e 4)
			 (make-scode-combination (make-scode-variable '*)
						 (make-list base e)))
			(else
			 (make-scode-combination operator (list base e))))
		  (make-scode-combination operator (list base (flonumize e))))))
	   (else
	    (if (string-prefix? "fix:" (symbol->string operator-name))
		expr
		(make-scode-combination operator (map flonumize operands))))))))))

(define generic->floating
  (let ()
    (define (make-flo-bin op)
      (lambda (x y)
	(make-scode-combination (make-scode-variable op)
				(list x y))))
    (let ((flo:+:bin (make-flo-bin 'flo:+))
	  (flo:-:bin (make-flo-bin 'flo:-))
	  (flo:*:bin (make-flo-bin 'flo:*))
	  (flo:/:bin (make-flo-bin 'flo:/))
	  (flo:-:una
	   (lambda (x)
	     (make-scode-combination (make-scode-variable 'flo:-)
				     (list 0. y))))
	  (flo:/:una
	   (lambda (x)
	     (make-scode-combination (make-scode-variable 'flo:/)
				     (list 1. y)))))
      (named-combination-transformer
       (lambda (expr) expr)	     
       (lambda (expr)
	 (let ((operator (scode-combination-operator expr))
	       (operands (scode-combination-operands expr)))
	   (let ((operator-name (scode-variable-name operator)))
	     (case operator-name
	       ((+)
		(apply (accumulation flo:+:bin 0.)
		       (map generic->floating operands)))
	       ((*)
		(apply (accumulation flo:*:bin 1.)
		       (map generic->floating operands)))
	       ((-)
		(apply (inverse-accumulation flo:-:bin flo:+:bin flo:-:una 0.)
		       (map generic->floating operands)))
	       ((/)
		(apply (inverse-accumulation flo:/:bin flo:*:bin flo:/:una 1.)
		       (map generic->floating operands)))
	       ((sqrt exp abs cos sin tan)
		(make-scode-combination
		 (make-scode-variable
		  (string->symbol
		   (string-append "flo:"
				  (symbol->string operator-name))))
					(list (generic->floating (car operands)))))
	       (else
		(if (string-prefix? "fix:" (symbol->string operator-name))
		    expr
		    (make-scode-combination operator
					    (map generic->floating operands))))))))))))


#|
(define ((test-transformer trans) expr)
  (pp (trans (syntax expr scmutils-base-environment))))

((test-transformer (compose generic->floating flonumize))
 '(+ 1 (* 2 (tan 3) (sin a) (vector-ref b 5)) 6))
(flo:+ (flo:+ 1.
	      (flo:* (flo:* (flo:* 2. (flo:tan 3.))
			    (flo:sin a))
		     (vector-ref b 5)))
       6.)
|#
