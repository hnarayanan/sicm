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

;;;; Compiler interface to compile generic code for numerical execution.

(declare (usual-integrations))

(define (compile-procedure:numerical procedure #!optional
				     target-environment
				     declarations keep?)

  (if (default-object? target-environment)
      (set! target-environment numerical-environment))
  (if (default-object? declarations)
      (set! declarations '((usual-integrations))))
  (if (default-object? keep?)
      (set! keep? 'keep))

  (if (not (procedure? procedure))
      (error "Need procedure -- compile-procedure:numerical" procedure))
  (cond ((compound-procedure? procedure)
	 (let ((enclosing-environment (procedure-environment procedure))
	       (procedure-text (procedure-lambda procedure)))
	   (let ((compiled-text
		  (compile-procedure-text procedure-text declarations keep?)))
	     (let ((compiled-procedure
		    (compiler-output->procedure compiled-text
						target-environment)))
	       (let ((free-variables (analyze/expression procedure-text)))
		 (for-each (lambda (name)
			     (compile-named:numerical name
						      enclosing-environment
						      target-environment
						      declarations
						      keep?))
			   (filter symbol? free-variables)))
	       compiled-procedure))))
	((compiled-procedure? procedure)
	 procedure)
	((primitive-procedure? procedure)
	 procedure)
	(else
	 (error "Unknown procedure type -- compile-procedure:numerical"
		procedure))))

(define (compile-named:numerical name #!optional
				 source-environment target-environment
				 declarations keep?)

  (define (do-it source-value)
    (if (procedure? source-value)
	(compile-it source-value)
	(environment-define target-environment name source-value)))	

  (define (compile-it source-value)
    (if compiler-interface-wallp? (write-line `(compiling ,name)))
    (environment-define target-environment name 'pending-compilation)
    (let ((compiled-procedure
	   (compile-procedure:numerical source-value
					target-environment
					declarations keep?)))
      (remember-compiled! source-value compiled-procedure target-environment)
      (environment-define target-environment name compiled-procedure)))

  (if (default-object? source-environment)
      (set! source-environment generic-environment))
  (if (default-object? target-environment)
      (set! target-environment numerical-environment))
  (if (default-object? declarations)
      (set! declarations '((usual-integrations))))
  (if (default-object? keep?) (set! keep? 'keep))

  (if (memq name '(definite-integral))
      'ignore
      (if (environment-bound? target-environment name)
	  (if (environment-assigned? target-environment name)
	      (let ((target-value
		     (environment-lookup target-environment name)))
		(if (environment-bound? source-environment name)
		    (if (environment-assigned? source-environment name)
			(let ((source-value
			       (environment-lookup source-environment name)))
			  (cond ((procedure? source-value)
				 (if (or (eq? target-value 'pending-compilation)
					 (already-compiled? name
							    source-value
							    target-value
							    target-environment))
				     'done
				     (compile-it source-value)))
				((eq? source-value target-value)
				 'done)
				(else
				 (environment-define target-environment
						     name
						     source-value))))
			(write-line `(Warning:source-unassigned ,name)))
		    (write-line `(Warning:source-unbound ,name))))
	      (do-it (environment-lookup source-environment name)))
	  (do-it (environment-lookup source-environment name)))))

(define compiler-interface-wallp? #t)

(define (already-compiled? name source-value target-value target-environment)
  (if (and (environment-bound? system-global-environment name)
	   (environment-assigned? system-global-environment name)
	   (eq? (environment-lookup system-global-environment name)
		target-value))
      #t
      (begin
	(if (not (environment-bound? target-environment '*compiled-table*))
	    (environment-define target-environment '*compiled-table*
				(make-eq-hash-table)))
	(equal? source-value
		(hash-table/get (environment-lookup target-environment
						    '*compiled-table*)
				target-value
				#f)))))

(define (remember-compiled! source-value target-value target-environment)
  (if (not (environment-bound? target-environment '*compiled-table*))
      (environment-define target-environment '*compiled-table*
			  (make-eq-hash-table)))
  (hash-table/put! (environment-lookup target-environment '*compiled-table*)
		   target-value
		   source-value))

(define (get-environment-chain environment)
  (if (environment-has-parent? environment)
      (cons environment
	    (get-environment-chain
	     (environment-parent environment)))
      (list environment)))