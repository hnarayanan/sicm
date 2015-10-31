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

;;;;         Match and Substitution Language Interpreter

(declare (usual-integrations))

;;;   This is a descendent of the infamous 6.001 rule interpreter,
;;; originally written by GJS for a lecture in the faculty course held
;;; at MIT in the summer of 1983, and subsequently used and tweaked
;;; from time to time.  This subsystem has been a serious pain in the
;;; ass, because of its expressive limitations, but I have not had the
;;; guts to seriously improve it since its first appearance. -- GJS

;;; January 2006.  I have the guts now! The new matcher is based on
;;; combinators and is in matcher.scm.  -- GJS

;;; "Call-by-value", as per Alan Bundy.

(define (rule-simplifier the-rules)
  (define (simplify-expression expression)
    (if (pair? expression)
	(let ((ssubs (map simplify-expression expression)))
	  (let ((result (try-rules ssubs the-rules)))
	    (if result
		(simplify-expression result)
		ssubs)))
	expression))
  (rule-memoize simplify-expression))


(define (try-rules expression the-rules)
  (define (scan rules)
    (if (null? rules)
	#f
	(or ((car rules) expression)
	    (scan (cdr rules)))))
  (scan the-rules))


;;;;  Rule applicator, using combinator-based matcher.

(define (rule:make pattern-expression consequent)
  (let ((matcher (match:->combinators pattern-expression)))
    (define (the-rule expression)
      (matcher (list expression)
	       '()
	       (lambda (dictionary unmatched-tail)
		 (and (null? unmatched-tail)
		      (apply consequent
			     (map (lambda (binding)
				    (let ((v (match:value binding)))
				      (if (vector? v)
					  (match:extract-segment v)
					  v)))
				  dictionary))))))
    the-rule))

(define (match:extract-segment v)
  (let ((beg (match:segment-beginning v))
	(end (match:segment-end v)))
    (if (null? end) beg
	(let lp ((p beg))
	  (if (eq? p end) '()
	      (cons (car p) (lp (cdr p))))))))

#|
;;; "Call-by-name", as per Alan Bundy.
;;; Crudely written.  Does not produce same answers...sigh.

(define (rule-simplifier the-rules)
  (define (simplify-exprs exprs resimp?)
    (let ((result
	   (let lp ((exprs exprs))
	     (cond ((null? exprs) '())
		   ((try-rules (car exprs) the-rules)
		    => (lambda (result)
			 (set! resimp? #t)
			 (cons (if (pair? result)
				   (lp result)
				   result)
			       (cdr exprs))))
		   (else
		    (cons (car exprs)
			  (lp (cdr exprs))))))))
      (if resimp?
	  (simplify-expression result)
	  result)))
  (define (simplify-expression expression)
    (if (pair? expression)
	(let ((result (try-rules expression the-rules)))
	  (if result
	      (if (pair? result)
		  (simplify-exprs result #t)
		  result)
	      (simplify-exprs expression #f)))
	expression))
  (rule-memoize simplify-expression))
|#
