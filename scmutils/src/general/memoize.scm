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

;;;; Memoizers

(declare (usual-integrations))

;;; A weak alist of memoized functions.

(define *memoizers* '())

(define (memoizer-gc-daemon)
  (set! *memoizers*
	(clean-weak-alist *memoizers*))
  'done)

(add-gc-daemon! memoizer-gc-daemon)

;;;(define *auditing-memoizers* #f)
(define *auditing-memoizers* #t)

(define (show-memoizer-statistics)
  (for-each (lambda (m)
	      (let ((f (weak-car m)) (s ((cadr (weak-cdr m)))))
		(if f
		    (pp `(,(car s)
			  ,(cadr s)
			  ,(function-expression (cadddr (weak-cdr m))
						f))))))
	    *memoizers*)
  'done)

(define (function-expression f memo-f)
  (or (object-name memo-f
		   generic-environment
		   rule-environment
		   numerical-environment
		   scmutils-base-environment)
      (procedure-name f)))

  
(define (clear-memoizer-tables)
  (for-each (lambda (m)
	      (let ((f (weak-car m)))
		(if f ((caddr (weak-cdr m))))))
	    *memoizers*)
  'done)

;;; Single-argument linear memoizer.  Can use weak alists for
;;; single-argument keys.

(define (linear-memoize-1arg f #!optional max-table-size finder)
  (let ((max-table-size			;set to 0 for no limit
	 (if (default-object? max-table-size)
	     12
	     max-table-size))
	(finder
	 (if (default-object? finder) weak-find-equal? finder))
	(table '())
	(memo-hits 0)
	(memo-misses 0))
    (let ((info
	   (lambda ()
	     (list memo-hits memo-misses table)))
	  (reset
	   (lambda ()
	     (set! memo-hits 0)
	     (set! memo-misses 0)
	     (set! table '())))
	  (memo-f
	   (lambda (x)
	     (let ((seen (finder x table)))
	       (if seen
		   (begin (if *auditing-memoizers*
			      (set! memo-hits (int:+ memo-hits 1)))
			  seen)
		   (let ((ans (f x)))
		     (if *auditing-memoizers*
			 (set! memo-misses (int:+ memo-misses 1)))
		     (set! table
			   (purge-list (cons (weak-cons x ans) table)
				       max-table-size))
		     ans))))))
      (set! *memoizers*
	    (cons (weak-cons memo-f
			     (list max-table-size info reset f))
		  *memoizers*))
      memo-f)))

;;; A general linear-time memoizer for functions.  In this case the
;;; arg lists are ALWAYS unprotected, so we cannot use weak pairs in
;;; the alist structure here.  However, we can use weak lists as
;;; arglists.

(define (linear-memoize f #!optional max-table-size finder)
  (let ((max-table-size			;set to 0 for no limit
	 (if (default-object? max-table-size)
	     12
	     max-table-size))
	(finder
	 (if (default-object? finder) weak-find-equal-args? finder))
	(table '())
	(memo-hits 0)
	(memo-misses 0))
    (let ((info
	   (lambda ()
	     (list memo-hits memo-misses table)))
	  (reset
	   (lambda ()
	     (set! memo-hits 0)
	     (set! memo-misses 0)
	     (set! table '())))
	  (memo-f
	   (lambda x
	     (let ((seen (finder x table)))
	       (if seen
		   (begin (if *auditing-memoizers*
			      (set! memo-hits (int:+ memo-hits 1)))
			  seen)
		   (let ((ans (apply f x)))
		     (if *auditing-memoizers*
			 (set! memo-misses (int:+ memo-misses 1)))
		     (set! table
			   (purge-list (cons (cons (list->weak-list x)
						   ans)
					     table)
				       max-table-size))
		     ans))))))
      (set! *memoizers*
	    (cons (weak-cons memo-f
			     (list max-table-size info reset f))
		  *memoizers*))
      memo-f)))

;;; Equality of arguments in argument lists or weak argument lists.

(define (same-args? same?)
  (define (safe-car x)
    (if (weak-pair? x) (weak-car x) (car x)))
  (define (safe-cdr x)
    (if (weak-pair? x) (weak-cdr x) (cdr x)))
  (define (the-test args1 args2)
    (cond ((null? args1)
	   (cond ((null? args2) #t)
		 (else #f)))
	  ((null? args2) #f)
	  ((same? (safe-car args1) (safe-car args2))
	   (the-test (safe-cdr args1) (safe-cdr args2)))
	  (else #f)))
  the-test)


(define equal-args? (same-args? equal?))

(define eqv-args? (same-args? eqv?))

(define eq-args? (same-args? eq?))


(define weak-find-equal-args? (weak-finder equal-args?))

(define weak-find-eqv-args? (weak-finder eqv-args?))

(define weak-find-eq-args? (weak-finder eq-args?))

;;; The following memoizers use hash tables

(define *not-seen* (list '*not-seen*))


;;; Single argument hash memoizer.  Can use weak table here.

(define (hash-memoize-1arg f)
  (let ((table) (memo-hits) (memo-misses))
    (let ((info
	   (lambda ()
	     (list memo-hits memo-misses table)))
	  (reset
	   (lambda ()
	     (set! memo-hits 0)
	     (set! memo-misses 0)
	     (set! table
		   ((weak-hash-table/constructor equal-hash-mod
						 equal?
						 #t)))))
	  (memo-f
	   (lambda (x)
	     (let ((seen (hash-table/get table x *not-seen*)))
	       (if (not (eq? seen *not-seen*))
		   (begin (if *auditing-memoizers*
			      (set! memo-hits (int:+ memo-hits 1)))
			  seen)
		   (let ((ans (f x)))
		     (if *auditing-memoizers*
			 (set! memo-misses (int:+ memo-misses 1)))
		     (hash-table/put! table x ans)
		     ans))))))
      (reset)
      (set! *memoizers*
	    (cons (weak-cons memo-f (list -1 info reset f))
		  *memoizers*))
      memo-f)))

;;; A general hash memoizer for functions.  In this case the arg lists
;;; are ALWAYS unprotected, so we cannot use a weak table here.

(define (hash-memoize f)
  (let ((table) (memo-hits) (memo-misses))
    (let ((info
	   (lambda ()
	     (list memo-hits memo-misses table)))
	  (reset
	   (lambda ()
	     (set! memo-hits 0)
	     (set! memo-misses 0)
	     (set! table
		   ((strong-hash-table/constructor equal-hash-mod
						   equal?
						   #t)))))
	  (memo-f
	   (lambda x
	     (let ((seen (hash-table/get table x *not-seen*)))
	       (if (not (eq? seen *not-seen*))
		   (begin (if *auditing-memoizers*
			      (set! memo-hits (int:+ memo-hits 1)))
			  seen)
		   (let ((ans (apply f x)))
		     (if *auditing-memoizers*
			 (set! memo-misses (int:+ memo-misses 1)))
		     (hash-table/put! table x ans)
		     ans))))))
      (reset)
      (set! *memoizers*
	    (cons (weak-cons memo-f (list -1 info reset f))
		  *memoizers*))
      memo-f)))


;;; To install and remove memoizers on named procedures

(define (memoize-procedure! name #!optional memo-type environment)
  (assert (symbol? name))
  (if (default-object? environment)
      (set! environment (nearest-repl/environment))
      (assert (environment? environment)))
  (assert (environment-bound? environment name))
  (if (default-object? memo-type)
      (set! memo-type 'linear)
      (assert (memq memo-type '(linear hash))))
  (if (environment-bound? environment '*memoized-procedures*)
      (if (assq name (eval '*memoized-procedures* environment))
	  (begin (warn name "rememoizing!")
		 (unmemoize-procedure! name environment))))
  (let ((proc (eval name environment)))
    (assert (procedure? proc))
    (let ((arity (procedure-arity proc)))
      (let ((memoized-procedure
	     (cond ((equal? arity '(0 . 0))
		    (let ((ran? #f) (value))
		      (lambda ()
			(if ran?
			    value
			    (begin
			      (set! value (proc))
			      (set! ran? #t)
			      value)))))
		   ((equal? arity '(1 . 1))
		    (case memo-type
		      ((linear) (linear-memoize-1arg proc))
		      ((hash) (hash-memoize-1arg proc))))
		   (else
		    (case memo-type
		      ((linear) (linear-memoize proc))
		      ((hash) (hash-memoize proc)))))))	
	(if (not (environment-bound? environment
				     '*memoized-procedures*))
	    (environment-define environment
				'*memoized-procedures*
				(list (cons name proc)))
	    (environment-assign! environment
				 '*memoized-procedures*
				 (cons (cons name proc)
				       (eval '*memoized-procedures*
					     environment))))
	(environment-assign! environment
			     name
			     memoized-procedure)
	'done))))

(define (unmemoize-procedure! name #!optional environment)
  (assert (symbol? name))
  (if (default-object? environment)
      (set! environment (nearest-repl/environment))
      (assert (environment? environment)))
  (assert (environment-bound? environment name))
  (assert (environment-bound? environment '*memoized-procedures*))
  (let ((vcell (assq name (eval '*memoized-procedures* environment))))
    (assert vcell)
    (environment-assign! environment (car vcell) (cdr vcell))
    (environment-assign! environment
			 '*memoized-procedures*
			 (delete vcell
				 (eval '*memoized-procedures* environment)))
    'done!))
