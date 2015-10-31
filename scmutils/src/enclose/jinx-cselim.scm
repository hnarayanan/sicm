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

;;; -*- Scheme -*-

(declare (usual-integrations))

;; $Header: expr.scm,v 1.13 90/09/22 16:37:09 GMT jinx Exp $

#|

*** To do: ***

- Cselim is done on expressions and an optional list of special
  operators with reordering properties be provided.  The notion
  of a variable being an operator is not inherent in the expression,
  but only part of constant-folding and cse-ing.

- Improve cselim to handle some (or all) of the cases that are
  specified there.

- Improve the reordering capabilities of cse so that if the operators
  are not associative, it won't do the wrong thing, but will still
  assume commutativity, for example.

- Implement constant folder.
  Takes a list of bindings of free variables and procedures to use.

- Use tables in bind to merge global variables, etc.
  Perhaps even collect the global environment in expressions rather
  than a list of global variables.  Or collect with names.
  We seem to be continually recomputing assq lists only to throw them
  out again at the end.

- Expression equality should treat operators differently, since abelian
  ones allow other orderings.  This is painful, but should be done.

- To fix in expression/bind:
  Binding a free variable to a lambda expression should beta-reduce
  any combinations where the free variable is the operator.

- To fix in expression/combine:
  If operator is lambda expression, it should beta-reduce.

|#

;;;; Common text utility.

(define (text/cselim text #!optional operators)
  ;; (expression->text (expression/cselim (text->expression text)))
  ;; Like ^ but avoids copying.
  (let ((lam (lam/make false)))
    (fluid-let ((*warn-body?* *allow-warnings?*)
		(*global-lam* lam)
		(*global-env* (table+/make))
		(*global-vars* '())
		(*constants* '()))
      (expression->graph text)
      (collect-input-set! (lam/node *global-lam*))
      (fluid-let ((*operators*
		   (process-operators
		    (if (default-object? operators)
			*standard-operators*
			operators))))
	(grow-subexpressions! (lam/node *global-lam*)))
      (if *recompute-input-sets?*
	  (collect-input-set! (lam/node *global-lam*)))
      (sort-subexpressions! (lam/node *global-lam*))
      (if *alpha-rename-lazily?*
	  (alpha-rename! (lam/node *global-lam*)))
      (graph->expression (lam/node *global-lam*)))))

;;;; Top level

;;; Coercions and other operations.

(define (text->expression text)
  (let ((lam (lam/make false)))
    (fluid-let ((*warn-body?* *allow-warnings?*)
		(*global-lam* lam)
		(*global-env* (table+/make))
		(*global-vars* '())
		(*constants* '()))
      (expression->graph text)
      (collect-input-set! (lam/node *global-lam*))
      (collect-expression))))

(define (expression->text expression)
  (with-copied-expression expression
    (lambda ()
      (if *recompute-input-sets?*
	  (collect-input-set! (lam/node *global-lam*)))
      (sort-subexpressions! (lam/node *global-lam*))
      (if *alpha-rename-lazily?*
	  (alpha-rename! (lam/node *global-lam*)))
      (graph->expression (lam/node *global-lam*)))))

(define (expression/pp expression)
  (pp (expression->text expression)
      (current-output-port)
      true))

(define (expression/copy expression)
  (fluid-let ((*associations* (table+/make)))
    (%expression/copy expression)))

(define (expression/equal? expression1 expression2)
  (fluid-let ((*associations* (table+/make))
	      (*reverse-associations* (table+/make)))
    (%expression/equal? expression1 expression2)))

;;; Higher level operations

(define (expression/cselim expression #!optional operators)
  (with-copied-expression expression
    (lambda ()
      (fluid-let ((*operators*
		   (process-operators
		    (if (default-object? operators)
			*standard-operators*
			operators))))
	(grow-subexpressions! (lam/node *global-lam*))
	(collect-expression)))))

;; These just need to check the global variables.

(define (expression/free? expression name)
  (there-exists? (expression/variables expression)
		 (lambda (node)
		   (eq? name (var-node/name node)))))

(define (expression/free-variables expression)
  (%map-1 var-node/name
	  (expression/variables expression)))

;;;; Top level operations (continued)

(define (lambda-expression->procedure expression #!optional environment)
  (eval (expression->text expression)
	(if (default-object? environment)
	    (nearest-repl/environment)
	    environment)))

(define (expression/rename expression renames)
  ;; Renames is a list of lists each containing an old name and a new name.
  ;; This is like expression/bind where the values are simple variable
  ;; nodes, but cheaper.
  (with-no-conflicting-variables
    expression
    (lambda ()
      (for-each
       (lambda (global-var)
	 (let ((place (assq (var-node/name node) renames)))
	   (if place
	       (set-var-node/name! global-var (cadr place)))))
       *global-vars*))))

(define (expression/bind expression bindings)
  #|					;
  ;; bindings is a list of lists each containing a name and a
  ;; value (another expression).  Essentially equivalent to

  (text->expression
   `(LET (,@(%map-1 (lambda (binding)
		      (list (car binding)
			    (expression->text (cadr binding))))
		    bindings))
      ,(expression->text expression)))
  |#
  #|					;
  ;; Eliminate useless bindings.

  (let ((bindings
	 (let ((free-vars (expression/free-variables expression)))
	   (list-transform-positive
	       bindings
	     (lambda (binding)
	       (memq (car binding) free-vars))))))

    (if (null? bindings)
	expression
	(fluid-let ((*associations* (table+/make)))
	  (let ((expr* (%expression/bind expression bindings)))
	    (collect-input-set! (expression/node expr*))
	    expr*))))

  |#

  ;; For now, until lambda expressions are processed correctly.
  ;; The second text->expression, etc. is to handle the case
  ;; where the LET binds lambdas.
  ;; *** This is a kludge! *** 

  (text->expression
   (expression->text
    (text->expression
     `(LET (,@(%map-1 (lambda (binding)
			(list (car binding)
			      (expression->text (cadr binding))))
		      bindings))
	,(expression->text expression))))))

;;;; Top level operations (continued)

(define (expression/capture expression parameter-names)
  #|
  ;; This binds parameter-names.  If any are free in expression,
  ;; they are removed from the free list.  Essentially equivalent to

  (text->expression
   `(LAMBDA ,parameter-names
      ,(expression->text expression)))
  |#
  (with-copied-expression
    expression
    (lambda ()
      (let* ((global-lam* (lam/make false))
	     (params
	      (let ((names&nodes
		     (%map-1 (lambda (node)
			       (set-var-node/lam! node global-lam*)
			       (cons (var-node/name node)
				     node))
			     *global-vars*)))
		(%map-1
		 (lambda (name)
		   (or (assq name names&nodes)
		       (cons name
			     (let ((node
				    (node/make 'INPUT 'VARIABLE
					       (var/make name global-lam*))))
			       (set-node/input-set! node (list node))
			       node))))
		 parameter-names))))
	(let ((lam *global-lam*)
	      (node (lam/node *global-lam*))
	      (params (%map-1 cdr params)))
	  (let ((node* (node/make 'REDUCIBLE 'LAMBDA global-lam*
				  (list node)))
		(global-vars*
		 (eq-set/difference *global-vars* params)))
	    (set-node/parents! node (list node*))
	    (set-node/input-set! node global-vars*)
	    (set-lam/params! lam params)
	    (set-lam/body! global-lam* node)
	    (set-lam/node! global-lam* node*)
	    (for-each (lambda (param)
			(set-var-node/lam! param lam))
		      params)
	    (expression/make true *constants*
			     global-vars* global-lam*)))))))

(define (expression/combine oprtrexp operandexps)
  ;; *** Handle combining a lambda expression! ***
  #|
  ;; Essentially

  (text->expression
   (cons (expression->text oprtrexp)
	 (%map-1 expression->text operandexps)))
  |#
  #|
  (fluid-let ((*associations* (table+/make)))
    (let ((expr* (%unify-1 (cons oprtrexp operandexps))))
      (%unify-2 expr*
		(node/make 'REDUCIBLE 'COMBINATION
			   false
			   (cons (expression/body oprtrexp)
				 (%map-1 expression/body operandexps))))
      expr*))
  |#

  ;; For now, until lambda expressions are handled

  (text->expression
   (cons (expression->text oprtrexp)
	 (%map-1 expression->text operandexps)))
  )

;;;; Top level operations (continued)

;; This assumes that no node is a child^n of itself.
;; letrec will make this fail!
;; *** If we write something like Y, will it fail? ***

(define (expression/walk expression recvr)
  (let ((context (context/make))
	(expr (expression/copy expression)))
    (if (not (expression/lazy-rename? expr))
	(alpha-rename! (expression/node expr)))
    (recvr
     context
     (expression/body expr)
     (lambda (node node-processor)
       (let walk-node ((node node))
	 (let* ((table (context/result-cache context))
		(result (table+/association table node)))
	   (cond ((not result)
		  (table+/associate! table node 'WALKING)
		  (let ((result* (node-processor node walk-node)))
		    (if (not (eq? (table+/association table node) 'WALKING))
			(error "expression/process: Multiple results" node)
			(begin
			  (table+/associate! table node (list result*))
			  result*))))
		 ((eq? result 'WALKING)
		  (error "expression/process: Circularity found" node))
		 (else
		  (car result)))))))))

;;;; Top level: utilities and state variables.

(define *allow-warnings?* true)

;; If the following two are set to true, the program will rename the
;; least number of variables to avoid conflicts.  Any other
;; combination will be faster, but will rename more variables.

(define *recompute-input-sets?* true)
(define *alpha-rename-lazily?* true)

(define *standard-operators*
  `((+ ABELIAN-GROUP 0 -)
    (* ABELIAN-GROUP 1 /)
    (- ABELIAN-GROUP-INVERSE 0 +)
    (/ ABELIAN-GROUP-INVERSE 1 *)))

(define *warn-body?*)
(define *operators*)

(define *global-lam*)
(define *global-env*)
(define *global-vars*)
(define *constants*)

(define *associations*)
(define *reverse-associations*)

(define-integrable (collect-expression)
  (expression/make *alpha-rename-lazily?*
		   *constants*
		   *global-vars*
		   *global-lam*))

(define (with-expression expression thunk)
  (fluid-let ((*alpha-rename-lazily?* (expression/lazy-rename? expression))
	      (*constants* (expression/constants expression))
	      (*global-vars* (expression/variables expression))
	      (*global-lam* (expression/graph expression)))
    (thunk)))

(define (with-copied-expression expression thunk)
  (with-expression (expression/copy expression)
    thunk))

;; This forces to lazy-rename? since otherwise
;; things would have to be renamed now.

(define (with-no-conflicting-variables expression recvr)
  (with-copied-expression
    (if (expression/lazy-rename? expression)
	expression
	(expression/make true
			 (expression/constants expression)
			 (expression/variables expression)
			 (expression/graph expression)))
    recvr))

;;;; Data structures

(define-structure (j-expression
		   (conc-name expression/)
		   (print-procedure
		    (lambda (state node)
		      (structure/unparse
		       state node
		       expression-unparse-fields
		       false)))
		   (constructor expression/make))
  (lazy-rename? false read-only true)
  (constants '() read-only true)
  (variables '() read-only true)
  (graph false read-only true))

(define expression-unparse-fields
  `("expression" ("lazy-rename?" ,expression/lazy-rename?)
		 ("graph" ,expression/graph)))

(define-integrable (expression/body expr)
  (lam/body (expression/graph expr)))

(define-integrable (expression/node expr)
  (lam/node (expression/graph expr)))

(define-structure (context
		   (conc-name context/)
		   (print-procedure
		    (lambda (state node)
		      (structure/unparse
		       state node
		       context-unparser-fields
		       false)))
		   (constructor context/make ()))
  (constants (table+/make) read-only true)
  (free-vars (table+/make) read-only true)
  (bound-vars (table+/make) read-only true)
  (result-cache (table+/make) read-only true))

(define context-unparser-fields
  `("context"
    #|
    ("constants" ,context/constants)
    ("free-vars" ,context/free-vars)
    ("bound-vars" ,context/bound-vars)
    ("result-cache" ,context/result-cache)
    |#
    ))

;;;; Data structures (continued)

(define-structure (node (conc-name node/)
			(print-procedure
			 (lambda (state node)
			   (structure/unparse
			    state node
			    node-unparse-fields
			    false)))
			(constructor
			 node/make
			 (type1 type2 extra #!optional children)))
  (type1 false read-only true)
  (type2 false read-only true)
  ;; This field is conceptually never changed, but copy-node needs it to
  ;; be mutable to avoid infinite recursion.
  (extra false read-only false)
  (children '() read-only false)
  (parents '() read-only false)
  (input-set '() read-only false)
  (generation 0 read-only false)
  (name false read-only false)
  ;; Used locally to avoid processing twice.
  (mark false read-only false))

(define node-unparse-fields
  `("node" ("type1" ,node/type1)
	   ("type2" ,node/type2)
	   ("extra" ,node/extra)
	   #|
	   ("children" ,node/children)
	   ("parents" ,node/parents)
	   ("input-set" ,node/input-set)
	   |#
	   ))

(define-integrable (node/type node)
  (node/type2 node))

(define-structure (var (conc-name var/)
		       (print-procedure
			(lambda (state var)
			  (structure/unparse
			   state var
			   var-unparse-fields
			   false)))
		       (constructor var/make (name lam)))
  (name false read-only false)
  (lam false read-only false))

(define var-unparse-fields
  `("var" ("name" ,var/name)
	  ("lam" ,var/lam)))

(define-integrable (var-node/name var)
  (var/name (node/extra var)))

(define-integrable (set-var-node/name! var name)
  (set-var/name! (node/extra var) name))

(define-integrable (var-node/lam var)
  (var/lam (node/extra var)))

(define-integrable (set-var-node/lam! var lam)
  (set-var/lam! (node/extra var) lam))

;;;; Data structures (continued)

(define-structure (lam (conc-name lam/)
		       (print-procedure
			(lambda (state lam)
			  (structure/unparse
			   state lam
			   lam-unparse-fields
			   false)))
		       (constructor lam/make (parent)))
  (parent false read-only false)
  (params '() read-only false)
  (body false read-only false)
  (node false read-only false)
  (aux false read-only false)
  (text false read-only false))

(define lam-unparse-fields
  `("lam" ("parent" ,lam/parent)
	  ("params" ,lam/params)
	  ("body" ,lam/body)
	  ("node" ,lam/node)
	  ("aux" ,lam/aux)))

(define-integrable (lam-node/params lam)
  (lam/params (node/extra lam)))

(define-integrable (lam-node/body lam)
  (lam/body (node/extra lam)))

(define-integrable (set-lam-node/body! lam body)
  (set-lam/body! (node/extra lam) body))

(define-integrable (lam-node/aux lam)
  (lam/aux (node/extra lam)))

(define-integrable (global-lam/make)
  (lam/make false))

(define-integrable (global-lam? lam)
  (false? (lam/parent lam)))

(define-integrable (constant-node/value lit)
  (node/extra lit))

;;;; Input processor

(define (expression->graph expression)
  (lambda-body->graph *global-lam* '() '() '() expression))

(define (->graph expression environment olam)
  (cond ((pair? expression)
	 (case (car expression)
	   ((LET*)
	    (->graph (canonicalize-let* expression)
		     environment
		     olam))
	   ((LET)
	    (let->graph expression environment olam))
	   ((LAMBDA)
	    (lambda->graph expression environment olam))
	   (else
	    (if (lambda-combination? expression)
		(->graph (canonicalize-lambda-combination expression)
			 environment
			 olam)
		(combination->graph expression environment olam)))))
	((variable? expression)
	 (var->graph expression environment olam))
	(else
	 (constant->graph expression environment olam))))

;;; Special handlers

(define (constant->graph expression environment olam)
  environment olam			; ignored
  (let ((place (assv expression *constants*)))
    (if place
	(cdr place)
	(let ((new (node/make 'INPUT 'CONSTANT expression)))
	  (set! *constants*
		(cons (cons expression new)
		      *constants*))
	  new))))

;;;; Input processor: special handlers (continued)

(define (var->graph name environment olam)
  olam					; ignored
  (cond ((j-lookup environment name))
	((table+/association *global-env* name))
	(else
	 (add-global-variable! name
			       (node/make 'INPUT 'VARIABLE
					  (var/make name *global-lam*))))))

(define (add-global-variable! name node)
  (table+/associate! *global-env* name node)
  (set! *global-vars* (cons node *global-vars*))
  node)

(define (let->graph expression environment olam)
  (destructure-let
   expression
   (lambda (names values body)
     (->graph body
	      (grow-environment
	       environment
	       names
	       (%map-2 (lambda (name value)
			 (let ((node (->graph value environment olam)))
			   ;; Note that this may be clobbering a previous name,
			   ;; since the node may come from another let, etc.
			   ;; The outermost let wins!
			   (if (not (eq? (node/type1 node) 'INPUT))
			       (set-node/name! node name))
			   node))
		       names values))
	      olam))))

(define (lambda-body->graph lam env names nodes body)
  (let* ((body (->graph body
			(grow-environment env names nodes)
			lam))
	 (node (node/make 'REDUCIBLE 'LAMBDA lam (list body))))
    (set-node/parents! body (cons node (node/parents node)))
    (set-lam/params! lam nodes)
    (set-lam/body! lam body)
    (set-lam/node! lam node)
    node))

(define (lambda->graph expression environment olam)
  (destructure-lambda
   expression
   (lambda (names body)
     (let* ((lam (lam/make olam))
	    (nodes
	     (%map-1
	      (lambda (name)
		;; If *alpha-rename-lazily?* is false, this
		;; alpha-renames eagerly.
		;; Important kludge: Free variables not yet seen can't
		;; present a conflict, thus it is fine to examine the
		;; current snapshot of the global environment.
		(let ((new-name
		       (if (and (not *alpha-rename-lazily?*)
				(or (j-lookup environment name)
				    (table+/association *global-env* name)))
			   (generate-new-name)
			   name)))
		  (node/make 'INPUT 'VARIABLE (var/make new-name lam))))
	      names)))
       (lambda-body->graph lam environment names nodes body)))))

;;;; Input processor: utilities (continued)

(define (combination->graph elements environment olam)
  (let ((operator (->graph (car elements) environment olam))
	(operands (%map-1 (lambda (child)
			    (->graph child environment olam))
			  (cdr elements))))
    ;; *** Handle this case specially ***
    ;; (eq? (node/type operator) 'LAMBDA)
    ;; For the time being it does nothing.
    (let* ((children (cons operator operands))
	   (new-node (node/make 'REDUCIBLE 'COMBINATION false children)))
      (for-each (lambda (child)
		  (set-node/parents!
		   child
		   (cons new-node (node/parents child))))
		children)
      (set-node/extra! new-node false)
      new-node)))

;;; Environment utilities

(define-integrable (j-lookup environment var)
  (let ((place (assq var environment)))
    (and place
	 (cdr place))))

(define-integrable (grow-environment environment names values)
  (map* environment
	(lambda (name value)
	  (cons name value))
	names
	values))

;;; operators

(define-integrable (j-operator? node)
  (assq node *operators*))

(define-integrable (can-reorder? node)
  (let ((place (assq node *operators*)))
    (and place
	 (eq? 'ABELIAN-GROUP (cadr (cdr place))))))

(define (process-operators operators)
  (let ((associations
	 (%map-1 (lambda (var)
		   (cons (var-node/name var)
			 var))
		 *global-vars*)))
    (delq false
	  (%map-1 (lambda (operator)
		    (let ((place (assq (car operator) associations)))
		      (if place
			  (cons (cdr place) operator)
			  false)))
		  operators))))

;;;; Input processor: syntactic utilities (continued)

(define-integrable variable? symbol?)

(define-integrable constant? number?)

(define-integrable (generate-new-name)
  (new-uninterned-symbol "V-"))

(define-integrable (generate-rename old)
  (new-uninterned-symbol
   (string-append (symbol->string old)
		  "-")))

(define (lambda-combination? expression)
  (and (pair? (car expression))
       (eq? (caar expression) 'LAMBDA)))

(define (canonicalize-lambda-combination expression)
  (destructure-lambda
   (car expression)
   (lambda (params body)
     (let ((arguments (cdr expression)))
       (if (not (= (length arguments) (length params)))
	   (error "canonicalize-lambda-combination: Wrong number of arguments"
		  expression)
	   `(LET (,@(%map-2 list params arguments))
	      ,body))))))

(define (canonicalize-let* expression)
  (let ((body (cddr expression))
	(bindings (cadr expression)))
    (define (process next rest)
      `(LET (,next)
	 ,@(if (null? rest)
	       body
	       `(,(process (car rest) (cdr rest))))))
    
    (if (not (null? bindings))
	(process (car bindings) (cdr bindings))
	(prepare-body body expression "canonicalize-let*"))))

(define (destructure-lambda expression recvr)
  (recvr (cadr expression)
	 (prepare-body (cddr expression) expression "destructure-lambda")))

(define (destructure-let expression recvr)
  (let ((bindings (cadr expression)))
    (recvr (%map-1 car bindings)
	   (%map-1 cadr bindings)
	   (prepare-body (cddr expression) expression "destructure-let"))))

(define (prepare-body body expression name)
  (if (and (not (null? body))
	   (null? (cdr body)))
      (car body)
      (begin
	(if *warn-body?*
	    (warn (string-append name ": Body is a sequence") expression))
	`(BEGIN ,@body))))

;;;; Output processor: pass one, insert LETs to bind subexpressions.

(define (sort-subexpressions! graph)
  (walk-graph!
   graph
   (lambda (node)
     (if (and (subexpression? node)
	      (not (eq? (node/type1 node) 'INPUT)))
	 (begin
	   ;; (bkpt "sort-subexpressions!")
	   (add-aux! node (find-target-lam node)))))))

;; A node is a subexpression if more than one node points at it.

(define-integrable (subexpression? node)
  (let ((parents (node/parents node)))
    (and (pair? parents)
	 (pair? (cdr parents)))))

;; This prevents migration past the binding point of variables, and
;; causes the proper nesting of lets in the output.

(define (find-target-lam node)
  (map&reduce var-node/lam
	      choose-descendant
	      *global-lam*
	      (node/input-set node)))

;; This does not remove the old node elements.
;; If we kept a "reference" count, we could do it,
;; but it does not hurt, since we are converting a copy
;; of the expression to an s-expression.
;; The only way it hurts is that we may get unnecessary
;; renamings since the conflict may have disappeared when
;; the subexpression was bound above the binding point
;; for the conflict.

(define (propagate-input! var)
  (let ((target (lam/node (var-node/lam var))))
    (define (propagate! node)
      (if (and (not (eq? node target))
	       (not (memq var (node/input-set node))))
	  (begin
	    (set-node/input-set!
	     node
	     (cons var (node/input-set node)))
	    (for-each propagate! (node/parents node)))))
    (for-each propagate! (node/parents var))))

;;;; Output processor: pass one, LET-bind one subexpression

(define (add-aux! node lam)
  (with-aux
    lam
    (lambda (lam aux)
      (let ((name (or (and *alpha-rename-lazily?*
			   (node/name node))
		      (generate-new-name))))
	(let ((var (node/make 'INPUT 'VARIABLE (var/make name aux)))
	      (combination (lam/body lam)))
	  (for-each
	   (lambda (parent)
	     (update-children!
	      parent
	      (subst var node (node/children parent))))
	   (node/parents node))
	  (set-lam/params! aux (cons var (lam/params aux)))
	  (set-node/children!
	   combination
	   (let ((old-children (node/children combination)))
	     (cons* (car old-children)
		    node
		    (cdr old-children))))
	  (set-node/parents! var (node/parents node))
	  (set-node/parents! node (list combination))
	  (set-node/input-set! var (list var))
	  (propagate-input! var))))))

;;; Main dispatch for graph->expression

(define (->expression node)
  (case (node/type node)
    ((VARIABLE)
     ;; This can't be shadowed because of the preemptive renaming done
     ;; in lambda->graph.
     ;; If lambda->graph renames too many things, the capture avoidance
     ;; can be done at the output by doing another pass before the final
     ;; translation.  This pass can detect conflicts and alpha rename.
     (var-node/name node))
    ((CONSTANT)
     (constant-node/value node))
    ((LAMBDA)
     `(lambda ,(%map-1 var-node/name (lam-node/params node))
	,(lambda-body->expression node)))
    ((COMBINATION)
     (%map-1 ->expression (node/children node)))
    (else
     (error "->expression: Unknown node type" node))))

;;;; Output processor: pass one (continued)

(define (with-aux lam recvr)
  (if (lam/aux lam)
      (recvr lam (lam/aux lam))
      (let ((old-node (lam/node lam))
	    (old-body (lam/body lam))
	    (new-lam (lam/make (lam/parent lam))))
	(let* ((new-body (node/make 'REDUCIBLE 'COMBINATION false
				    (list old-node)))
	       (new-node (node/make 'REDUCIBLE 'LAMBDA new-lam
				    (list new-body))))
	  (set-lam/aux! new-lam lam)
	  (set-lam/parent! lam new-lam)
	  (set-lam/params! new-lam (lam/params lam))
	  (set-lam/params! lam '())
	  (set-lam/body! new-lam new-body)
	  (set-lam/node! new-lam new-node)
	  (for-each
	   (lambda (parent)
	     (update-children!
	      parent
	      (subst new-node old-node (node/children parent))))
	   (node/parents old-node))
	  (set-node/parents! new-node (node/parents old-node))
	  (set-node/parents! old-node (list new-body))
	  (set-node/parents! new-body (list new-node))
	  (set-node/input-set! new-node (node/input-set old-node))
	  (set-node/input-set! old-node (node/input-set old-body))
	  (set-node/input-set! new-body (node/input-set old-body))
	  (let ((to-modify
		 ;; This cannot use global-lam? since we are
		 ;; editing the structure, and the parent
		 ;; is in fact changing.
		 (if (eq? lam *global-lam*)
		     (begin
		       (set! *global-lam* new-lam)
		       *global-vars*)
		     (lam/params new-lam))))
	    (for-each (lambda (var-node)
			(set-var-node/lam! var-node new-lam))
		      to-modify))
	  (if (and (lam/parent new-lam)
		   (eq? lam (lam/aux (lam/parent new-lam))))
	      (set-lam/aux! (lam/parent new-lam) new-lam))
	  (recvr new-lam lam)))))

;;;; Output processor: pass two, create the output

(define (graph->expression graph)
  (if (not (eq? (node/type graph) 'LAMBDA))
      (error "graph->expression: Not a lambda" graph))
  (lambda-body->expression graph))

(define (lambda-body->expression node)
  (define (collect node recvr)
    (let* ((lam (node/extra node))
	   (body (lam/body lam)))
      (if (not (lam/aux lam))
	  (recvr (->expression body)
		 '())
	  (collect
	   (lam/node (lam/aux lam))
	   (lambda (expr bindings)
	     (recvr expr
		    (cons
		     (%map-2 (lambda (node value)
			       (list (var-node/name node)
				     (->expression value)))
			     (lam/params (lam/aux lam))
			     (cdr (node/children body)))
		     bindings)))))))

  (collect node
	   (lambda (body bindings)
	     (define (finish bindings body)
	       (cond ((null? bindings)
		      body)
		     ((null? (cdr bindings))
		      `(LET ,bindings ,body))
		     (else
		      `(LET* ,bindings ,body))))

	     (define (collect-lets bindings collected)
	       (cond ((null? bindings)
		      (finish (reverse collected) body))
		     ((null? (cdar bindings))
		      (collect-lets (cdr bindings)
				    (cons (caar bindings)
					  collected)))
		     (else
		      (finish (reverse collected)
			      `(LET ,(car bindings)
				 ,(collect-lets (cdr bindings) '()))))))
	     (collect-lets bindings '()))))

;;;; Common utilities

;;; Free variable collection

(define (collect-input-set! graph)
  (walk-graph!
   graph
   (lambda (node)
     (set-node/input-set!
      node
      (case (node/type node)
	((VARIABLE)
	 (list node))
	((LAMBDA)
	 (let ((lam (node/extra node)))
	   (list-transform-negative
	       (node/input-set (lam/body lam))
	     (lambda (var-node)
	       (eq? (var-node/lam var-node) lam)))))
	(else
	  (map&reduce node/input-set
		      eq-set/union '()
		      (node/children node))))))))

(define (alpha-rename! graph)
  (walk-graph!
   graph
   (lambda (node)
     (if (eq? (node/type node) 'LAMBDA)
	 (let* ((lam (node/extra node))
		(free-vars (node/input-set node)))
	   (if (not (null? free-vars))
	       (for-each
		(lambda (param)
		  (let* ((var (node/extra param))
			 (name (var/name var)))
		    (if (there-exists? free-vars
				       (lambda (free-var)
					 (eq? name (var-node/name free-var))))
			(begin
			  ;; (bkpt "alpha-rename!")
			  (set-var/name! var (generate-rename name))))))
		(lam/params lam))))))))

;;;; Common subexpression manipulation

(define (grow-subexpressions! graph)
  (define (try-all subexps next-pass)
    (cond ((not (null? subexps))
	   (try-all (cdr subexps)
		    (possibly-grow-subexpression! (car subexps)
						  next-pass)))
	  ((not (null? next-pass))
	   (try-all next-pass '()))
	  (else
	   unspecific)))
  (try-all (graph-accumulate
	    graph
	    (lambda (node rest)
	      (if (subexpression? node)
		  (cons node rest)
		  rest))
	    '())
	   '()))

(define (possibly-grow-subexpression! node acc)
  (let ((all (node/parents node)))
    (if (null? all)
	acc
	(let loop ((next (car all))
		   (left (cdr all))
		   (acc acc))
	  (if (null? left)
	      acc
	      (loop (car left)
		    (cdr left)
		    (if (or (null? (node/children next)) ; deleted node
			    (operator&not-only-operand? node next))
			acc
			(try-pairwise next left acc))))))))

(define-integrable (operator&not-only-operand? node parent)
  (and (eq? node (combination-operator parent))
       #|
       ;; The code below is an open coding of this, for speed.
       (there-exists? (combination-operands parent)
		      (lambda (node*)
			(not (eq? node node*))))
       |#
       (let loop ((rands (combination-operands parent)))
	 (and (not (null? rands))
	      (or (not (eq? node (car rands)))
		  (loop (cdr rands)))))))

(define (same-operands? l1 l2)
  (if (null? l1)
      (null? l2)
      (and (not (null? l2))
	   (eq? (car l1) (car l2))
	   (same-operands? (cdr l1) (cdr l2)))))

;;;; Common subexpression detector

;;; *** Improvements: lambda expressions currently not considered. ***
;;; It can be done by using the isomorphism tester that appears elsewhere,
;;; triggered if the list of free variables (input set) is the same,
;;; and the number of parameters is the same.

(define (try-pairwise one others acc)
  (let loop ((others others)
	     (acc acc))
    (if (or (null? others) (null? (node/parents one)))
	acc
	(loop (cdr others)
	      (let ((result (try-pair one (car others))))
		(if result
		    (multi-set/union result acc)
		    acc))))))

(define (try-pair one two)
  ;; *** Is the control structure correct? ***
  ;; It seems that multiple nodes should be returned for the next pass!
  (if (eq? one two)
      ;; Repeated children
      (and (can-reorder? (combination-operator one))
	   (extract-repetitions! one))
      (and (not (null? (node/parents two)))
	   (eq? (node/type one) (node/type two))
	   (if (and (eq? 'COMBINATION (node/type one))
		    (eq? (combination-operator one)
			 (combination-operator two))
		    (can-reorder? (combination-operator one)))
	       (let ((set1 (combination-operands one))
		     (set2 (combination-operands two)))
		 (let ((common (multi-set/intersection set1 set2)))
		   (cond ((null? common)
			  (and (null? set1) (null? set2)
			       (replace! two one)))
			 ((null? (cdr common))
			  (and (null? (cdr set1)) (null? (cdr set2))
			       (replace! two one)))
			 (else
			  (let ((rem1 (multi-set/difference set1 common))
				(rem2 (multi-set/difference set2 common)))
			    (cond ((null? rem1)
				   (if (null? rem2)
				       (replace! two one)
				       (make-child! one two)))
				  ((null? rem2)
				   (make-child! two one))
				  (else
				   (let ((node
					  (node/make
					   'REDUCIBLE
					   'COMBINATION
					   false
					   (cons (combination-operator one)
						 common))))
				     (set-node/input-set!
				      node
				      (map&reduce node/input-set
						  eq-set/union
						  '()
						  common))
				     (make-child! node one)
				     (make-child! node two)))))))))
	       (and (same-operands? (node/children one)
				    (node/children two))
		    (or (not (eq? (node/type one) 'LAMBDA))
			(= (length (lam-node/params one))
			   (length (lam-node/params two))))
		    (replace! two one))))))

;;;; Common subexpression eliminator

(define (extract-repetitions! node)
  (let loop ((operands (reverse (combination-operands node)))
	     (reptd '())
	     (left '()))
    (cond ((null? operands)
	   (and reptd
		(not (null? (cdr reptd)))
		(let ((new-node (node/make 'REDUCIBLE
					   'COMBINATION
					   false
					   (cons (combination-operator node)
						 reptd))))
		  (for-each
		   (lambda (reptd)
		     (set-node/parents!
		      reptd
		      (cons new-node
			    (delq-once node
				       (delq-once node
						  (node/parents reptd))))))
		   reptd)
		  (set-node/parents! new-node (list node node))
		  (update-children!
		   node
		   `(,(combination-operator node)
		     ,new-node
		     ,new-node
		     ,@left))
		  reptd)))
	  ((memq (car operands) left)
	   (loop (cdr operands)
		 (cons (car operands) reptd)
		 (delq (car operands) left)))
	  (else
	   (loop (cdr operands)
		 reptd
		 (cons (car operands) left))))))

;;;; Common subexpression eliminator (continued)

(define (make-child! child parent)
  (for-each
   (lambda (child^2)
     (update-parents!
      child^2
      (cons child (delq-once parent (node/parents child^2)))))
   (node/children child))
  (update-children!
   parent
   (cons (combination-operator parent)
	 (cons child
	       (multi-set/difference (combination-operands parent)
				     (combination-operands child)))))
  (update-parents! child (cons parent (node/parents child)))
  (list child))

(define (replace! two one)
  ;; Replace two with one
  (for-each (lambda (parent)
	      (update-children!
	       parent
	       (subst one two (node/children parent))))
	    (node/parents two))
  (for-each (lambda (child)
	      (update-parents!
	       child
	       (subst one two (node/parents child))))
	    (node/children two))
  (update-parents! one
		   ;; This could be eq-set/union,
		   ;; but update-parents! takes care
		   ;; to check each parent only once.
		   ;; eq-set/union would still not
		   ;; guarantee uniqueness since a node
		   ;; can have a parent more than once.
		   (multi-set/union (node/parents two)
				    (node/parents one)))
  ;; This makes it no longer be a subexpression so that
  ;; the top-level subexpression loop will ignore it
  ;; if already cached.
  (set-node/parents! two '())
  (set-node/children! two '())
  (list one))

(define (update-parents! node possible-parents)
  (set-node/parents! node '())
  (for-each (lambda (parent)
	      (set-node/mark! parent false))
	    possible-parents)
  ;; This is done this way because there are multiple
  ;; links that need to be maintained.
  (let loop ((to-test possible-parents))
    (and (not (null? to-test))
	 (let ((this (car to-test)))
	   (if (node/mark this)
	       (loop (cdr to-test))
	       (begin
		 (set-node/mark! this true)
		 (for-each
		  (lambda (child)
		    (if (eq? child node)
			(set-node/parents!
			 node
			 (cons this
			       (node/parents node)))))
		  (node/children this))
		 (loop (cdr to-test))))))))

(define (update-children! parent new-children)
  (set-node/children! parent new-children)
  (if (eq? (node/type parent) 'LAMBDA)
      (begin
	(if (or (null? new-children)
		(not (null? (cdr new-children))))
	    (error "IMPLEMENTATION-ERROR: update-children!: Clobbering lambda"
		   parent new-children))
	(set-lam-node/body! parent (car new-children)))))

;;;; Structure copier

(define (%expression/copy expression)
  (let ((constants*
	 (%map-1 (lambda (constant)
		   (cons (car constant)
			 (copy-node (cdr constant))))
		 (expression/constants expression)))
	(vars* (%map-1 copy-node (expression/variables expression)))
	(global-lam* (copy-lam (expression/graph expression))))
    (for-each (lambda (var*)
		(set-var-node/lam! var* global-lam*))
	      vars*)
    (expression/make (expression/lazy-rename? expression)
		     constants* vars* global-lam*)))
  
(define-integrable (object-copier %copy copy-fields!)
  (lambda (object)
    (or (table+/association *associations* object)
	(let ((new-object (%copy object)))
	  (table+/associate! *associations* object new-object)
	  (copy-fields! new-object)
	  new-object))))

(define (%copy-lam-1 lam)
  (let ((lam* (lam/make (lam/parent lam))))
    (set-lam/params! lam* (lam/params lam))
    (set-lam/body! lam* (lam/body lam))
    (set-lam/node! lam* (lam/node lam))
    (set-lam/aux! lam* (lam/aux lam))
    lam*))

(define (%copy-lam-2 lam)
  (if (lam/parent lam)
      (set-lam/parent! lam (copy-lam (lam/parent lam))))
  (if (lam/aux lam)
      (set-lam/aux! lam (copy-lam (lam/aux lam))))
  (set-lam/body! lam (copy-node (lam/body lam)))
  (set-lam/node! lam (copy-node (lam/node lam)))
  (set-lam/params! lam
		   (%map-1 (lambda (node)
			     (let ((node* (copy-node node)))
			       (set-var-node/lam! node* lam) ; fix-var
			       node*))
			   (lam/params lam))))

(define copy-lam
  (object-copier %copy-lam-1 %copy-lam-2))

(define copy-var
  ;; The lam field is set by the binding lam, in the line marked fix-var above.
  (object-copier
   (lambda (var)
     (var/make (var/name var) false))
   (lambda (new-var)
     new-var)))

;;;; Structure copier (continued)

(define (%copy-node-1 node)
  (let ((new-node (node/make (node/type1 node)
			     (node/type2 node)
			     (node/extra node)
			     (node/children node))))
    ;; parents set below when relinked by container.
    (set-node/input-set! new-node (node/input-set node))
    (set-node/name! new-node (node/name node))
    new-node))

(define (%copy-node-2 node)
  (set-node/input-set! node (%map-1 copy-node (node/input-set node)))
  (set-node/children!
   node
   (%map-1 (lambda (node*)
	     (let ((node** (copy-node node*)))
	       (set-node/parents! node**
				  (cons node (node/parents node**)))
	       node**))
	   (node/children node)))
  (set-node/extra!
   node
   (let ((current (node/extra node)))
     (case (node/type node)
       ((VARIABLE)
	(copy-var current))
       ((LAMBDA)
	(copy-lam current))
       ((CONSTANT COMBINATION)
	current)
       (else
	(error "copy-node: Unknown type" node))))))

(define copy-node
  (object-copier %copy-node-1 %copy-node-2))

;;;; Binding

(define (%unify-1 expressions)
  (let ((global-lam* (lam/make false))
	(constants*
	 (initialize-constants! (%map-1 expression/constants expressions)))
	(vars*
	 (initialize-variables! (%map-1 expression/variables expressions))))
    (for-each (lambda (expr)
		(copy-unify! (expression/graph expr) global-lam*))
	      expressions)
    (expression/make true constants* vars* global-lam*)))

(define (%unify-2 expr* body)
  (let* ((global-lam* (expression/graph expr*))
	 (node* (node/make 'REDUCIBLE 'LAMBDA global-lam*)))
    (set-lam/node! global-lam* node*)
    (for-each (lambda (const)
		(%copy-node-2 (cdr const)))
	      (expression/constants expr*))
    (for-each (lambda (var)
		(%copy-node-2 var)
		(set-var-node/lam! var global-lam*))
	      (expression/variables expr*))
    (let ((body* (copy-node body)))
      (set-lam/body! global-lam* body*)
      (set-node/children! node* (list body*))
      (set-node/parents! body* (cons node* (node/parents body*))))))
  
;; *** Worry about binding operators ***
;; binding to lambdas should become subexpressions where procedure, etc.

(define (%expression/bind expression bindings)
  (split-list
   (expression/variables expression)
   (let ((to-be-bound (%map-1 car bindings)))
     (lambda (var)
       (memq (var-node/name var) to-be-bound)))
   (lambda (bound unaffected)
     (let* ((expr*
	     (%unify-1 (cons (expression/make true
					      (expression/constants expression)
					      unaffected
					      (expression/graph expression))
			     (%map-1 cadr bindings))))
	    (global-lam* (expression/graph expr*))
	    (bindings* (initialize-bindings! bindings global-lam*)))
       ;; Bind!
       (for-each
	(lambda (bound)
	  (let* ((name (var-node/name bound))
		 (node (cdr (assq name bindings*))))
	    (set-node/name! node name)
	    (copy-unify! bound node)))
	bound)
       (%unify-2 expr* (lam/body (expression/graph expression)))
       (for-each (lambda (binding)
		   (%copy-node-2 (cdr binding)))
		 bindings*)
       expr*))))

;;;; Binding (continued)

(define (initialize-constants! consts)
  (reduce copy-unify-associations!
	  (%map-1 initialize-association! (car consts))
	  (cdr consts)))

(define (initialize-variables! vars)
  (%map-1 cdr
	  (map&reduce (lambda (vars)
			(%map-1 (lambda (var)
				  (cons (var-node/name var)
					var))
				vars))
		      copy-unify-associations!
		      (%map-1 (lambda (var)
				(initialize-association!
				 (cons (var-node/name var)
				       var)))
			      (car vars))
		      (cdr vars))))

(define (initialize-bindings! bindings global-lam*)
  (%map-1 (lambda (binding)
	    (let ((lam (expression/graph (cadr binding))))
	      (copy-unify! lam global-lam*)
	      (initialize-association!
	       (cons (car binding)
		     (lam/body lam)))))
	  bindings))

(define (copy-unify! object object*)
  (let ((object** (table+/association *associations* object)))
    (cond ((not object**)
	   (table+/associate! *associations* object object*))
	  ((not (eq? object* object**))
	   (error "copy-unify!: Already copy-unified"
		  object object* object**)))))

(define (initialize-association! asspair)
  (cons (car asspair)
	(let ((node (cdr asspair)))
	  (or (table+/association *associations* node)
	      (let ((node* (%copy-node-1 node)))
		(table+/associate! *associations* node node*)
		node*)))))

(define (copy-unify-associations! asspairs asspairs*)
  (cond ((null? asspairs)
	 asspairs*)
	((assv (caar asspairs) asspairs*)
	 =>
	 (lambda (asspair*)
	   (copy-unify! (cdar asspairs) (cdr asspair*))
	   (copy-unify-associations! (cdr asspairs) asspairs*)))
	(else
	 (copy-unify-associations!
	  (cdr asspairs)
	  (cons (initialize-association! (car asspairs))
		asspairs*)))))

;;;; Isomorphism tester

;; This does not check the variables or constants lists.
;; If they are equal, all of them will be met eventually.
;; - Free variables should be OK as long as expression/bind fixes them.
;; - Constants should be OK as long as they are removed if unneeded
;; after constant folding.  This can be done by checking the parents
;; list.  Currently there is no constant folding.

(define (%expression/equal? expression1 expression2)
  (lam/equal? (expression/graph expression1)
	      (expression/graph expression2)))

(define-integrable (object-comparator %comparator)
  (lambda (obj1 obj2)
    (or (eq? obj1 obj2)			; when the objects share structure
	(let ((assoc1 (table+/association *associations* obj1)))
	  (if assoc1
	      (eq? assoc1 obj2)
	      (and (not (table+/association *reverse-associations* obj2))
		   (begin
		     (table+/associate! *associations* obj1 obj2)
		     (table+/associate! *reverse-associations* obj2 obj1)
		     (%comparator obj1 obj2))))))))

(define node/equal?
  (object-comparator
   (lambda (node1 node2)
     (and (eq? (node/type node1) (node/type node2))
	  (let ((extra1 (node/extra node1))
		(extra2 (node/extra node2)))
	    (case (node/type node1)
	      ((VARIABLE)
	       (var/equal? extra1 extra2))
	      ((CONSTANT)
	       (eqv? extra1 extra2))
	      ((LAMBDA)
	       (lam/equal? extra1 extra2))
	      ((COMBINATION)
	       true)
	      (else
	       (error "node/equal?: Unknown type" node1))))
	  #|
	  ;; These two are not needed since if the rest of the structure
	  ;; is the same, these should be the same as well, unless the
	  ;; program is broken, and that does not happen (-: :-).
	  ;; The parents are impossible to do, since there is no a-priori
	  ;; way to match them, and the ordering need not be the same.
	  (node-set/equal? (node/parents node1)
			   (node/parents node2))
	  (node-set/equal? (node/input-set node1)
			   (node/input-set node2))
	  |#
	  (node-list/equal? (node/children node1)
			    (node/children node2))))))

;;;; Isomorphism tester (continued)

(define (node-list/equal? list1 list2)
  (let loop ((list1 list1)
	     (list2 list2))
    (if (null? list1)
	(null? list2)
	(and (not (null? list2))
	     (node/equal? (car list1) (car list2))
	     (loop (cdr list1) (cdr list2))))))

(define lam/equal?
  (object-comparator
   (lambda (lam1 lam2)
     (and (lam?/equal? (lam/parent lam1) (lam/parent lam2))
	  (lam?/equal? (lam/aux lam1) (lam/aux lam2))
	  (node/equal? (lam/node lam1) (lam/node lam2))
	  (node-list/equal? (lam/params lam1) (lam/params lam2))
	  (node/equal? (lam/body lam1) (lam/body lam2))))))

(define (lam?/equal? lam1 lam2)
  (if (and lam1 lam2)
      (lam/equal? lam1 lam2)
      (and (not lam1) (not lam2))))

(define var/equal?
  (object-comparator
   (lambda (var1 var2)
     ;; The names of globals must match.  The names of others
     ;; do not.  The only thing that matters is position and nesting.
     ;; Since position and nesting is taken care of by lam/equal?,
     ;; we only need to compare the lams here.
     ;; It is sufficient to check that lam1 is a global-lam, since
     ;; lam/equal? guarantees that they occupy the same place in the
     ;; environment tree.
     (let ((lam1 (var/lam var1))
	   (lam2 (var/lam var2)))
       (and (lam/equal? lam1 lam2)
	    (or (not (global-lam? lam1))
		(eq? (var/name var1) (var/name var2))))))))

;;;; Utilities for expression/process

(define-integrable (constant-value node)
  (constant-node/value node))

(define-integrable (variable-name node)
  (var-node/name node))

(define-integrable (lambda-parameters node)
  (lam-node/params node))

(define-integrable (lambda-body node)
  (lam-node/body node))

(define-integrable (combination-operator node)
  (car (node/children node)))

(define-integrable (combination-operands node)
  (cdr (node/children node)))

;;;; Utilities for expression/process (continued)

(define (build-expression node)
  (let ((expr
	 (expression/copy
	  (expression/make true
			   '()
			   '()
			   (node/extra (build-lambda '() node))))))
    (relink-lambdas! expr)
    (collect-input-set! (expression/node expr))
    (let ((free-vars (node/input-set (expression/body expr)))
	  (lam (expression/graph expr)))
      (for-each (lambda (var)
		  (set-var-node/lam! var lam))
		free-vars)
      (expression/make true
		       (collect-constants expr)
		       free-vars
		       (expression/graph expr)))))

(define (find-variable context node)
  (if (global-lam? (var-node/lam node))
      (intern-free-variable context (var-node/name node))
      (or (table+/association (context/bound-vars context) node)
	  (error "find-variable: Not found!" node context))))

(define (build-lambda variables body)
  ;; The parent field for the lambda will be set when the
  ;; expression is relinked.  Similarly for the parent field
  ;; for body.
  (let* ((lam (lam/make false))
	 (node (node/make 'REDUCIBLE 'LAMBDA lam (list body))))
    (set-lam/params! lam variables)
    (set-lam/body! lam body)
    (set-lam/node! lam node)
    (for-each (lambda (var)
		(if (var-node/lam var)
		    (error "build-lambda: Rebinding variable" var)
		    (set-var-node/lam! var lam)))
	      variables)
    node))

(define (bind-variables context variables)
  (%map-1 (lambda (var)
	    (let ((table (context/bound-vars context)))
	      (if (table+/association table var)
		  (error "bind-variables: Already bound" var)
		  (let ((var* (build-variable (variable-name var))))
		    (table+/associate! table var var*)
		    var*))))
	  variables))

(define (build-combination operator operands)
  ;; Parent field for children will be set when the
  ;; expression is relinked.
  (node/make 'REDUCIBLE 'COMBINATION
	     false
	     (cons operator operands)))

(define-integrable (make-interner selector maker)
  (lambda (context value)
    (or (table+/association (selector context) value)
	(let ((node (maker value)))
	  (table+/associate! (selector context) value node)
	  node))))

;;;; Utilities for expression/process (continued)

(define intern-constant
  (make-interner context/constants
		 (lambda (constant)
		   (node/make 'INPUT 'CONSTANT constant))))

(define-integrable (build-variable name)
  (node/make 'INPUT 'VARIABLE (var/make name false)))

(define intern-free-variable
  (make-interner context/free-vars build-variable))

;; letrec will make this fail!

(define (relink-lambdas! expr)
  (define (walk-node node lam)
    (cond ((not (eq? (node/type node) 'LAMBDA))
	   (for-each (lambda (child) (walk-node child lam))
		     (node/children node)))
	  ((lam/parent (node/extra node))
	   =>
	   (lambda (current-parent)
	     (let ((lam* (node/extra node)))
	       (if (not (eq? current-parent lam))
		   (let ((real-parent
			  (find-common-ancestor lam current-parent)))
		     (set-lam/parent! lam* real-parent)
		     (walk-node (lam/body lam*) lam*))))))
	  (else
	   (let ((lam* (node/extra node)))
	     (set-lam/parent! lam* lam)
	     (walk-node (lam/body lam*) lam)))))

  (walk-node (expression/body expr)
	     (expression/graph expr)))

(define (find-common-ancestor lam1 lam2)
  (define (collect-chain lam)
    (cons lam
	  (let ((parent (lam/parent lam)))
	    (if parent
		(collect-chain parent)
		'()))))

  (let ((chain1 (reverse (collect-chain lam1)))
	(chain2 (reverse (collect-chain lam2))))
    (if (not (eq? (car chain1) (car chain2)))
	(error "find-common-ancestor: No common ancestor" lam1 lam2)
	(let loop ((answer (car chain1))
		   (chain1 (cdr chain1))
		   (chain2 (cdr chain2)))
	  (if (or (null? chain1)
		  (null? chain2)
		  (not (eq? (car chain1) (car chain2))))
	      answer
	      (loop (car chain1) (cdr chain1) (cdr chain2)))))))  

(define (collect-constants expr)
  (graph-accumulate
   (expression/node expr)
   (lambda (node rest)
     (if (eq? (node/type node) 'CONSTANT)
	 (cons (cons (constant-node/value node) node)
	       rest)
	 rest))
   '()))

;;;; Graph utilities

(define (walk-graph! graph procedure)
  (if (not (null? (node/parents graph)))
      (error "walk-graph!: Invoked on non-root" graph))
  (let ((new-generation (1+ (node/generation graph))))
    (define (walk-node node)
      (if (not (= (node/generation node) new-generation))
	  (begin
	    (set-node/generation! node new-generation)
	    (for-each walk-node (node/children node))
	    (procedure node))))
    (walk-node graph)))

(define (graph-accumulate graph accumulator null-value)
  (if (not (null? (node/parents graph)))
      (error "graph-accumulate: Invoked on non-root" graph))
  (let ((new-generation (1+ (node/generation graph))))
    (define (walk-node node result)
      (if (= (node/generation node) new-generation)
	  result
	  (begin
	    (set-node/generation! node new-generation)
	    (accumulator node
			 ;; This does not use reduce because the current
			 ;; version ignores the initial value when the
			 ;; list is not empty.
			 (map&reduce identity-procedure walk-node result
				     (node/children node))))))
    (walk-node graph null-value)))

(define (choose-descendant lam1 lam2)
  (define (try start ancestor)
    (let ((end *global-lam*))
      (let loop ((next start))
	(if (eq? next ancestor)
	    start
	    (and (not (eq? next end))
		 (loop (lam/parent next)))))))

  (or (try lam1 lam2)
      (try lam2 lam1)
      (error "IMPLEMENTATION-ERROR: choose-descendant: cousins"
	     lam1 lam2)))

'i-am-ready