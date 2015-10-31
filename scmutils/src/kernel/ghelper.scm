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

;;;;           Most General Generic-Operator Dispatch

(declare (usual-integrations))

;;; Generic-operator dispatch is implemented here by a discrimination
;;; list, where the arguments passed to the operator are examined by
;;; predicates that are supplied at the point of attachment of a
;;; handler (by ASSIGN-OPERATION).

;;; To be the correct branch all arguments must be accepted by
;;; the branch predicates, so this makes it necessary to
;;; backtrack to find another branch where the first argument
;;; is accepted if the second argument is rejected.  Here
;;; backtracking is implemented by OR.

(define (make-generic-operator arity #!optional name default-operation)
  (guarantee-procedure-arity arity 'make-generic-operator)
  (if (not (default-object? name))
      (guarantee-symbol name 'make-generic-operator))
  (if (not (default-object? default-operation))
      (guarantee-procedure-of-arity default-operation
				    arity
				    'make-generic-operator))
  (define (find-branch tree arg win)
    (let loop ((tree tree))
      (cond ((pair? tree)
	     (or (and ((caar tree) arg) (win (cdar tree)))
		 (loop (cdr tree))))
	    ((null? tree) #f)
	    (else tree))))
  (define (identity x) x)
  (let ((record (make-operator-record arity)))
    (define (general-find-handler arguments)
      (let loop ((tree (operator-record-tree record))
		 (args arguments))
	(find-branch tree (car args)
		     (if (pair? (cdr args))
			 (lambda (branch) (loop branch (cdr args)))
			 identity))))
    (define operator
      (case arity
	((1)
	 (lambda (arg)
	   ((find-branch (operator-record-tree record) arg identity)
	    arg)))
	((2)
	 (lambda (arg1 arg2)
	   ((find-branch (operator-record-tree record) arg1
			 (lambda (branch)
			   (find-branch branch arg2 identity)))
	    arg1 arg2)))
	(else
	 (lambda arguments
	   (if (not (acceptable-arglist? arguments arity))
	       (error:wrong-number-of-arguments operator arity arguments))
	   (apply (general-find-handler arguments)
		  arguments)))))

    (set! default-operation
      (if (default-object? default-operation)
	  (named-lambda (no-handler . arguments)
	    (no-way-known operator
			  (if (default-object? name) operator name)
			  arguments))
	  default-operation))
    (set-operator-record-finder! record general-find-handler)
    (set-operator-record! operator record)
    ;; For backwards compatibility with previous implementation:
    (if (not (default-object? name))
	(set-operator-record! name record))
    (assign-operation operator default-operation)
    operator))

(define *generic-operator-table*
  (make-eq-hash-table))

(define (get-operator-record operator)
  (hash-table/get *generic-operator-table* operator #f))

(define (set-operator-record! operator record)
  (hash-table/put! *generic-operator-table* operator record))

(define (make-operator-record arity) (list arity #f '()))
(define (operator-record-arity record) (car record))
(define (operator-record-finder record) (cadr record))
(define (set-operator-record-finder! record finder) (set-car! (cdr record) finder))
(define (operator-record-tree record) (caddr record))
(define (set-operator-record-tree! record tree) (set-car! (cddr record) tree))

(define (generic-operator-arity operator)
  (let ((record (get-operator-record operator)))
    (if record
        (operator-record-arity record)
        (error "Not an operator:" operator))))

(define (acceptable-arglist? lst arity)
  (let ((len (length lst)))
    (and (fix:<= (procedure-arity-min arity) len)
	 (or (not (procedure-arity-max arity))
	     (fix:>= (procedure-arity-max arity) len)))))

(define (assign-operation operator handler . argument-predicates)
  (let ((record (get-operator-record operator))
	(arity (length argument-predicates)))
    (if record
	(begin
	  (if (not (fix:<= arity (procedure-arity-min
				  (operator-record-arity record))))
	      (error "Incorrect operator arity:" operator))
	  (bind-in-tree
	   argument-predicates
	   handler
	   (operator-record-tree record)
	   (lambda (new)
	     (set-operator-record-tree! record new))))
	(error "Assigning a handler to an undefined generic operator"
	       operator)))
  operator)

(define defhandler assign-operation)

(define (bind-in-tree keys handler tree replace!)
  (let loop ((keys keys) (tree tree) (replace! replace!))
    (if (pair? keys)
	;; There are argument predicates left
	(let find-key ((tree* tree))
	  (if (pair? tree*)
	      (if (eq? (caar tree*) (car keys))
		  ;; There is already some discrimination list keyed
		  ;; by this predicate: adjust it according to the
		  ;; remaining keys
		  (loop (cdr keys)
			(cdar tree*)
			(lambda (new)
			  (set-cdr! (car tree*) new)))
		  (find-key (cdr tree*)))
	      (let ((better-tree
		     (cons (cons (car keys) '()) tree)))
		;; There was no entry for the key I was looking for.
		;; Create it at the head of the alist and try again.
		(replace! better-tree)
		(loop keys better-tree replace!))))
	;; Ran out of argument predicates
	(if (pair? tree)
	    ;; There is more discrimination list here, because my
	    ;; predicate list is a proper prefix of the predicate list
	    ;; of some previous assign-operation.  Insert the handler
	    ;; at the end, causing it to implicitly accept any
	    ;; arguments that fail all available tests.
	    (let ((p (last-pair tree)))
	      (if (not (null? (cdr p)))
		  (warn "Replacing a default handler:" (cdr p) handler))
	      (set-cdr! p handler))
	    (begin
	      ;; There is no discrimination list here, because my
	      ;; predicate list is not the proper prefix of that of
	      ;; any previous assign-operation.  This handler becomes
	      ;; the discrimination list, accepting further arguments
	      ;; if any.
	      (if (not (null? tree))
		  (warn "Replacing a handler:" tree handler))
	      (replace! handler))))))

;;; Failures make it to here.  Time to DWIM, with apologies to Warren
;;; Teitelman.  Can we look at some argument as a default numerical
;;; expression?  I want to get rid of this, since "when in doubt, dike 
;;; it out." -- Greenblatt, but metacirc/prop seems to need this.  
;;; It should be fixed.

(define (no-way-known operator name arguments)
  (let ((new-arguments (map dwim arguments)))
    (if (equal? arguments new-arguments)
	(error "Generic operator inapplicable:" operator name arguments))
    (apply operator new-arguments)))

(define (dwim argument)
  (if (pair? argument)
      (cond ((memq (car argument) type-tags)
	     argument)
	    ((memq (car argument) generic-numerical-operators)
	     (apply (eval (car argument) generic-environment)
		    (cdr argument)))
	    (else
	     argument))
      argument))

;;; A debugging aid

(define (get-handler operator . arguments)
  (let ((record (get-operator-record operator)))
    (if record
	(let ((handler-finder (operator-record-finder record)))
	  (handler-finder arguments))
	(error "Not a generic operator" operator))))

#|
(get-handler '+ (up 1 2) (up 3 4))
#| structure+structure |#
|#
