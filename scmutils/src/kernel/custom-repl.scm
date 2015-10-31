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

(declare (usual-integrations))

#|
;;; I don't trust this!

(define saved-repl-eval
  (access default/repl-eval
	  (->environment '(runtime rep))))

(define (scmutils/repl-eval s-expression environment repl)
  ;; Reset the differential tag count each time we start a new eval.
  ;; See kernel/deriv.scm.
  (set! (access differential-tag-count scmutils-base-environment) 0)
  (saved-repl-eval s-expression environment repl))

(set! hook/repl-eval scmutils/repl-eval)
|#

(define saved-repl-eval
  (access default/repl-eval
	  (->environment '(runtime rep))))

(define (scmutils/repl-eval s-expression environment repl)
  ((access clear-notes! scmutils-base-environment))
  (saved-repl-eval s-expression environment repl))

(set! hook/repl-eval scmutils/repl-eval)


(define saved-repl-write
  (access default/repl-write
	  (->environment '(runtime rep))))

(define (scmutils/repl-write object s-expression environment repl)
  (let* ((port (cmdl/port repl))
	 (edwin? (edwin-port? port)))
    (define (maybe-message val)
      (if edwin? (edwin/transcript-write val #f)))
    (define (simplifiable object)
      (prepare-for-printing object simplify)
      (let ((val (*last-expression-printed*)))
	(if ((disjunction symbol? number?) val)
	    (print-unsimplifiable val)
	    (begin (display "#|\n" port)
		   (pp val port)
		   (display "|#\n" port)
		   (maybe-message val)))))
    (define (print-unsimplifiable object)
      (display "#| " port)
      (write object port environment)
      (display " |#\n" port)
      (maybe-message object))
    (cond ((unsimplifiable? object)
	   (if (undefined-value? object)
	       (begin (newline port)
                      (display ";No return value." port)
		      (maybe-message object))
	       (print-unsimplifiable object)))
	  ((or (symbol? object)
	       (list? object)
	       (vector? object)
	       (procedure? object))
	   (simplifiable object))
	  ((record? object)
	   (simplifiable
	    `(*record*
	      ,(record-type-name (record-type-descriptor object))
	      ,@(record-description object))))
	  (else (print-unsimplifiable object)))))

(define (start-scmutils-print)
  (set! hook/repl-write scmutils/repl-write))

(define (stop-scmutils-print)
  (set! hook/repl-write saved-repl-write))


(define edwin/write-result
  (access operation/write-result (->environment '(edwin inferior-repl))))

(define edwin/transcript-write
  (access transcript-write (->environment '(edwin inferior-repl))))

(define (edwin-port? port)
  (eq? (port/operation port 'write-result)
       edwin/write-result))


(define (display-expression)
  (if (or (undefined-value? (*last-expression-printed*))
	  (and (procedure? (*last-expression-printed*))
	       (not (operator? (*last-expression-printed*)))))
      (*last-expression-printed*)
      (internal-show-expression (*last-expression-printed*))))

(define de display-expression)
