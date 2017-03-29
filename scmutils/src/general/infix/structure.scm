#| -*-Scheme-*-

Copyright (C) 1986, 1987, 1988, 1989, 1990, 1991, 1992, 1993, 1994,
    1995, 1996, 1997, 1998, 1999, 2000, 2001, 2002, 2003, 2004, 2005,
    2006, 2007, 2008, 2009, 2010, 2011, 2012, 2013, 2014, 2015, 2016
    Massachusetts Institute of Technology

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

;;; takes a list of tokens and breaks it up, packaging the algebraic
;;; expressions.

(define (parse-structure tokens)
  (let lp ((in (reverse tokens)) (out '()))
    (if (null? in)
	out
	(cond
	 ((and (token-lparen? (car in))
	       (pair? out)
	       (token-var? (car out)))
	  (lp (cdr in)
	      (cons* lparen-token apply-token out)))
	 ((and (token-lparen? (car in))
	       (pair? out)
	       (token-rparen? (car out)))
	  (lp (cdr in)
	      (cons* lparen-token apply-token out)))
       	 ((token-lambda? (car in))
	  (let* ((body-result (match-lambda (cdr in)))
		 (lambda-body (car body-result))
		 (remaining-in (cdr body-result)))
		(lp remaining-in
		    (cons* (make-expr-token 
			    (lp lambda-body '()))
			   (car in) 
		    out))))
	 ((token-if? (car in))
	  (let* ((cond-result (match-if (cdr in)))
		 (if-cond 
		  (make-expr-token (lp (car cond-result) '())))
		 (true-result (match-then (cddr cond-result)))
		 (if-true 
		  (make-expr-token (lp (car true-result) '())))
		 (false-result (match-else (cddr true-result)))
		 (if-false 
		  (make-expr-token (lp (car false-result) '())))
		 (rest (cdr false-result)))
	    (lp rest
		(cons* if-false else-token if-true then-token if-cond
		       if-token out))))
	 (else 
	  (lp (cdr in) (cons (car in) out)))))))

(define (make-matcher-breaks adds? minuses?)
  (lambda (input)
     (let lp ((input input) (n 1) (matched '()))
    (cond ((and (not (null? matched))
		(<= n 0))
	   (cons* (reverse (cdr matched)) (car matched) input))
	  ((null? input)
	   (cons (reverse matched) input))
	  ((adds? (car input)) 
	   (lp (cdr input) 
	       (+ n 1)
	       (cons (car input) matched)))
	  ((minuses? (car input))
	   (lp (cdr input)
	       (- n 1)
	       (cons (car input) matched)))
	  ((and (or (token-end? (car input))
		    (token-comma? (car input)))
		(<= n 1))
	   (cons (reverse matched) input))
	  (else 
	   (lp (cdr input)
	       n
	       (cons (car input) matched)))))))

(define match-lambda
  (make-matcher-breaks token-lparen? token-rparen?))
	
(define match-then 
  (make-matcher-breaks token-then? token-else?))

(define match-else 
  (make-matcher-breaks (lambda (t) (or (token-else? t)
				       (token-lparen? t))) 
		       token-rparen?))
(define match-if 
  (make-matcher-breaks token-if? token-then?))

(define match-paren  (make-matcher 0 token-lparen?
				   token-rparen? #f))

