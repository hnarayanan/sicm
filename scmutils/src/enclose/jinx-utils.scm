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

;;; $Header: utils.scm,v 1.13 90/08/23 02:07:44 GMT jinx Exp $

;;;; Control utilities

;;; Allowing callees to default correctly.

(define make-default-object
  ;; Cacheing kludge
  (let ((result '()))
    (named-lambda (make-default-object)
      (if (null? result)
	  (set! result
		(cons
		 (unmap-reference-trap (make-unassigned-reference-trap))
		 '())))
      (car result))))

;;; Keyword parameter passing.

;; This should include an option that disables "Unknown option" errors
;; and passes left over parameters along.

(define (get-options all-options option-names recvr)
  (define (pass paired-options)
    (let loop ((names (reverse option-names))
	       (parameters '()))
      (cond ((null? names)
	     (apply recvr parameters))
	    ((assq (car names) paired-options)
	     =>
	     (lambda (pair)
	       (loop (cdr names)
		     (cons (cdr pair) parameters))))
	    (else
	     (loop (cdr names)
		   (cons (make-default-object) parameters))))))	  

  (let loop ((options all-options)
	     (acc '()))
    (cond ((null? options)
	   (pass acc))
	  ((not (memq (car options) option-names))
	   (error "get-options: Unknown option" (car options)))
	  ((null? (cdr options))
	   (error "get-options: No value" (car options)))
	  (else
	   (loop (cddr options)
		 (cons (cons (car options)
			     (cadr options))
		       acc))))))

;;;; Higher order utilities

(define make-reducer
  (let ()
    (define (construct-reducer binop direction min-args max-args null-value)
      (let ((reducer-maker
	     (if (eq? direction 'RIGHT)
		 (named-lambda (make-right-reducer handle-last)
		   (define (reducer next rest)
		     (if (null? rest)
			 (handle-last next)
			 (binop next
				(reducer (car rest) (cdr rest)))))
		   reducer)
		 (named-lambda (make-left-reducer handle-last)
		   (lambda (next rest)
		     (let loop ((accum (handle-last next))
				(rest rest))
		       (if (null? rest)
			   accum
			   (loop (binop accum (car rest))
				 (cdr rest))))))))
	    (handle1
	     (cond ((or (zero? max-args)
			(< max-args min-args))
		    (lambda (x) x))
		   ((eq? direction 'RIGHT)
		    (lambda (x)
		      (binop x null-value)))
		   (else
		    (lambda (x)
		      (binop null-value x))))))
	(case min-args
	  ((2)
	   (let ((process (reducer-maker handle1)))
	     (lambda (all)
	       (if (or (null? all) (null? (cdr all)))
		   (error "reducer: Too few arguments" all)
		   (process (car all) (cdr all))))))
	  ((1)
	   (if (= max-args 2)
	       (let ((process (reducer-maker handle1)))
		 (lambda (all)
		   (if (null? all)
		       (error "reducer: Too few arguments" all)
		       (process (car all) (cdr all)))))
	       (let ((process (reducer-maker (lambda (x) x))))
		 (lambda (all)
		   (cond ((null? all)
			  (error "reducer: Too few arguments" all))
			 ((null? (cdr all))
			  (handle1 (car all)))
			 (else
			  (process (car all) (cdr all))))))))
	  ((0)
	   (if (= max-args 2)
	       (let ((process (reducer-maker handle1)))
		 (lambda (all)
		   (if (null? all)
		       null-value
		       (process (car all) (cdr all)))))
	       (let ((process (reducer-maker (lambda (x) x))))
		 (lambda (all)
		   (cond ((null? all)
			  null-value)
			 ((null? (cdr all))
			  (handle1 (car all)))
			 (else
			  (process (car all) (cdr all))))))))
	  (else
	   (error "make-tail-collector: Inconsistency" min-args)))))

    (named-lambda (make-reducer binop . options)
      (get-options
       options
       '(DIRECTION MIN-ARGS MAX-ARGS-USING-NULL-VALUE NULL-VALUE)
       (lambda (direction min-args max-args null-value)
	 (let* ((min-args
		 (if (default-object? min-args)
		     2
		     min-args))
		(max-args
		 (cond ((not (default-object? max-args))
			max-args)
		       ((default-object? null-value)
			0)
		       (else
			2))))
	   ;; Paranoia check.
	   (cond ((or (not (integer? min-args))
		      (not (<= 0 min-args 2)))
		  (error "make-reducer: Bad min-args" min-args))
		 ((or (not (integer? max-args))
		      (not (<= 0 max-args 2)))
		  (error "make-reducer: Bad max-args-using-null-value"
			 max-args))
		 ((default-object? null-value)
		  (if (or (= min-args 0)
			  (>= max-args min-args))
		      (error "make-reducer: required null-value not supplied"
			     `((MIN-ARGS ,min-args)
			       (MAX-ARGS-USING-NULL-VALUE ,max-args)))))
		 ((< max-args min-args)
		  (error "make-reducer: null-value meaningless"
			 `((NULL-VALUE ,null-value)
			   (MIN-ARGS ,min-args)
			   (MAX-ARGS-USING-NULL-VALUE ,max-args)))))

	   (construct-reducer binop
			      (if (default-object? direction)
				  'LEFT
				  direction)
			      min-args
			      max-args
			      (if (default-object? null-value)
				  (make-default-object)
				  null-value))))))))

;;;; Random utilities: association tables (eq? based)
#|
;;; 6 May 2003 No longer works, but no longer needed -- GJS
(let-syntax ((primitive
	      (lambda (name)
		`',(make-primitive-procedure name))))
  (define-integrable string-hash (primitive string-hash))
  (define-integrable symbol-print-name (primitive system-pair-car)))
|#

(define (find-next-prime number)
  (let loop ((primes 
	      '(1009 2003 4001 8009 16001 32003 64007
		     128021 256019 512009 1024021)))
    (cond ((null? primes)
	   (error "find-next-prime: number too large" number))
	  ((< number (car primes))
	   (car primes))
	  (else (loop (cdr primes))))))

(define-integrable (size->table-size size)
 (find-next-prime (quotient (* 5 size) 4)))

(define-integrable (%table/make size)
  (make-vector size '()))

(define (table/make size)
  (%table/make (size->table-size size)))

(define (table/index table object)
  (modulo
   (cond ((exact-integer? object)
	  object)
	 ((symbol? object)
	  #| 6 May 2003 No longer works -- GJS
	  (string-hash (symbol-print-name object))|#
	  (symbol-hash object))
	 ((string? object)
	  (string-hash object))
	 (else
	  (object-hash object)))
   (vector-length table)))

(define (table/association table key)
  (let ((bucket (vector-ref table (table/index table key))))
    (and (not (null? bucket))
	 (let ((place (assq key bucket)))
	   (and (pair? place)
		(cdr place))))))

(define (table/associate! table key value)
  (let* ((index (table/index table key))
	 (bucket (vector-ref table index))
	 (place (assq key bucket)))
    (if (not place)
	(begin
	  (vector-set! table index (cons (cons key value) bucket))
	  true)
	(begin
	  (set-cdr! place value)
	  false))))

;;;; Growing association tables

(define-structure (table+ (conc-name table+/)
			  (constructor %table+/make))
  (size false read-only false)
  (limit false read-only false)
  (entries false read-only false)
  (table false read-only false))

(define-integrable (table+-size->limit size)
  (quotient (* 4 size) 5))

(define (table+/make #!optional size)
  (let* ((size
	  ;; Bug in sf 4.8
	  (let ((input-size
		 (if (default-object? size)
		     100
		     size)))
	    (size->table-size input-size)))
	 (table (%table/make size)))
    (%table+/make size
		  (table+-size->limit size)
		  0
		  table)))

(define-integrable (table+/association table+ key)
  (table/association (table+/table table+) key))

(define (table+/associate! table+ key value)
  (if (table/associate! (table+/table table+) key value)
      (let ((entries (1+ (table+/entries table+))))
	(set-table+/entries! table+ entries)
	(if (> entries (table+/limit table+))
	    (table+/grow! table+)))))

(define (table+/grow! table+)
  (let* ((next-size (find-next-prime (table+/size table+)))
	 (new-table (%table/make next-size)))
    (for-each-vector-element
     (table+/table table+)
     (lambda (bucket)
       (for-each (lambda (pair)
		   (table/associate! new-table (car pair) (cdr pair)))
		 bucket)))
    (set-table+/limit! table+ (table+-size->limit next-size))
    (set-table+/table! table+ new-table)
    (set-table+/size! table+ next-size)))

;;;; I/O utilities

(define (warning . arguments)
  (apply warn arguments)
  (warning/stop-hook))

(define (warning/stop-hook/bkpt)
  (bkpt "Breakpoint at warning"))

(define (warning/stop-hook/default)
  unspecific)

(define warning/stop-hook
  warning/stop-hook/default)

(define message-tag
  (list '*MESSAGE-NOISE*))

(define (message/noise noise)
  (cons message-tag noise))

(define (message/noise? noise)
  (and (pair? noise)
       (eq? (car noise) message-tag)))

(define (message/pluralize string number #!optional suffix)
  (let ((result
	 (if (= number 1)
	     string
	     (string-append string "s"))))
    (message/noise
     (if (default-object? suffix)
	 result
	 (string-append result suffix)))))

(define (message string . values)
  (newline)
  (write-string string)
  (for-each (lambda (value)
	      (write-char #\Space)
	      (if (message/noise? value)
		  (write-string (cdr value))
		  (write value)))
	    values))

;;;; PP does a horrible job on lists.

(define (write-list all-l #!optional tab-position tagged?)
  (let ((port (current-output-port)))

    (let ((write-list/write-string
	   (output-port/operation/write-string port))
	  (write-list/write-char
	   (output-port/operation/write-char port)))

      (let ((port port)
	    (initial-tab-position (if (default-object? tab-position)
				      0
				      tab-position))
	    (size (output-port/x-size port)))
	(let* ((tab-position
		(if (default-object? tagged?)
		    initial-tab-position
		    (+ (1+ initial-tab-position)
		       (string-length (symbol->string (car all-l))))))
	       (prefix (make-string (1+ tab-position) #\Space)))

	  (define-integrable (write-string string)
	    (write-list/write-string port string))

	  (define-integrable (write-char char)
	    (write-list/write-char port char))

	  (define-integrable (newline)
	    (write-list/write-char port #\newline))

	  (newline)
	  (write-string (make-string initial-tab-position #\Space))
	  (write-char #\()
	  (let loop ((l all-l)
		     (start? true)
		     (left (- size (1+ initial-tab-position))))
	    (cond ((not (null? l))
		   (let* ((next (write-to-string (car l)))
			  (new-size
			   (if (not (null? (cdr l)))
			       (1+ (string-length next))
			       (+ (string-length next) 2))))
		     (cond ((<= new-size left)
			    (if start?
				(begin
				  (write-string next)
				  (loop (cdr l)
					false
					(- left (-1+ new-size))))
				(begin
				  (write-char #\Space)
				  (write-string next)
				  (loop (cdr l)
					false
					(- left new-size)))))
			   (start?
			    (write-string next)
			    (newline)
			    (write-string prefix)
			    (loop (cdr l)
				  true
				  left))
			   (else
			    (newline)
			    (write-string prefix)
			    (write-string next)
			    (loop (cdr l)
				  false
				  (- size (+ tab-position new-size)))))))
		  ((not (positive? left))
		   (newline)
		   (write-string prefix))))
	  (write-char #\))
	  (output-port/flush-output port)
	  unspecific)))))

;;;; Structure utilities

(define *structure-unparse-level* 1)

(define (structure/unparse state node description
			   #!optional show-false?)
  (let ((show-false?
	 (if (default-object? show-false?)
	     true
	     show-false?)))
    (unparse-string state "#[")
    (unparse-string state (car description))
    (unparse-char state #\Space)
    (unparse-object state (object-hash node))
    (if (not (zero? *structure-unparse-level*))
	(fluid-let ((*structure-unparse-level*
		     (-1+ *structure-unparse-level*)))
	  (for-each
	   (lambda (field)
	     (let ((value ((cadr field) node)))
	       (and (or show-false? value)
		    (begin
		      (unparse-char state #\Space)
		      (unparse-string state (car field))
		      (unparse-string state ": ")
		      (unparse-object state value)))))
	   (cdr description))))
    (unparse-string state "]")))

(define (@ object . selectors)
  (let loop ((object object)
	     (selectors selectors))
    (if (null? selectors)
	object
	(loop ((car selectors) object)
	      (cdr selectors)))))

(define (write-structures . structures)
  (for-each (lambda (obj)
	      (write-line
	       (cond ((not (number? obj))
		      obj)
		     ((valid-hash-number? obj)
		      (object-unhash obj))
		     (else
		      `(invalid-hash-number ,obj)))))
	    structures))

;;;; Random Scheme utilities

(define (errset thunk success failure)
  ((call-with-current-continuation
    (lambda (abort)
      (let ((result
	     (bind-condition-handler condition-type:error
		 (lambda (condition)
		   condition		; ignored
		   (abort failure))
	       thunk)))
	;; This could invoke abort,
	;; but there is no reason.
	(lambda ()
	  (success result)))))))
#|
;; Adapted from generate-uninterned-symbol in the runtime system
;; to allow string prefixes.

(define new-uninterned-symbol
  (let ((name-counter 0)
	(name-prefix "G"))
    (lambda (#!optional argument)
      (if (not (default-object? argument))
	  (cond ((symbol? argument)
		 (set! name-prefix (symbol->string argument)))
		((string? argument)
		 (set! name-prefix argument))
		((exact-nonnegative-integer? argument)
		 (set! name-counter argument))
		(else
		 (error "new-uninterned-symbol: Bad argument" argument))))
      (string->uninterned-symbol
       (string-append name-prefix
		      (number->string
		       (let ((result name-counter))
			 (set! name-counter (1+ name-counter))
			 result)))))))
|#

;; The version in the runtime system now allows strings as well.

(define-integrable new-uninterned-symbol
  generate-uninterned-symbol)