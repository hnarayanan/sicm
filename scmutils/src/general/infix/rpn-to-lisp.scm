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

(define (analyze-tokens tokens)
  (car (analyze-dispatch tokens)))

(define (analyze-dispatch tokens)
  (if (null? tokens)
      (cons #f tokens)
      (let ((token (car tokens)))
	(cond
	 ;; Self evaluating
	 ((token-number? token)
	  (analyze-number-token tokens))
	 ((token-var? token)
	  (analyze-var-token tokens))
	 ;; Special non-bin-op Forms
	 ((and (token-lambda? token) 
	       (pair? (cdr tokens))
	       (token-expr? (cadr tokens)))
	  (analyze-lambda-token tokens))
	 ;; Unary operator or function
	 ((token-unop? token)
	  (analyze-unop-token tokens))
	 ((token-quotation? token)
	  (analyze-quotation-token tokens))
	 ((token-expr? token)
	  (cons (analyze-dispatch (cdr token))
		(cdr tokens)))
	 ;; Binary operator
	 ((token-binop? token)
	  (analyze-binop-token tokens))
	 ;; Catch
	 ((or (token-lparen? token) (token-rparen? token))
	  (error "Mismatched parentheses in infix string"))
	 (else (error "Bad token in infix string" token))))))

(define (analyze-number-token tokens)
  (cons (token-number-value (car tokens)) (cdr tokens)))

(define (analyze-var-token tokens)
  (cons (token-var-name (car tokens)) (cdr tokens)))

(define (analyze-lambda-token tokens)
  (let* ((body-rest (analyze-dispatch (cdr tokens)))
	 (body (car body-rest))
	 (rest (cdr body-rest)))
    (cons  (append (get-lambda-args (car tokens)) 
		   body)
	   rest)))

(define (analyze-unop-token tokens)
  (let* ((token (car tokens))
	 (result-rest (analyze-dispatch (cdr tokens)))
	 (result (car result-rest))
	 (rest (cdr result-rest)))
    (cond ((eq? (token-op-name token) 'if)
	   (cons (list (token-op-name token)
		       (car result))
		 rest))
	  ((number? result)
	   (if (token-unary-minus? token)
	       (cons (- result) rest)
	       (cons (+ result) rest)))
	  (else
	   (cons (list (token-op-name token) result)
		 rest)))))

(define (analyze-quotation-token tokens)
  (let* ((result-rest (analyze-dispatch (cdr tokens)))
	 (result (car result-rest))
	 (rest (cdr result-rest)))
    (cons (list 'quote result) rest)))

(define (analyze-binop-token tokens)
  (let* ((token (car tokens))
	 (result-rest-left (analyze-dispatch (cdr tokens)))
	 (result-left (car result-rest-left))
	 (rest-left (cdr result-rest-left))
	 (result-rest-right (analyze-dispatch rest-left))
	 (result-right (car result-rest-right))
	 (rest (cdr result-rest-right)))
    (case (token-op-name token)
      ((apply)
       (cons (cons 
	      result-left
	      (let flatten ((r result-right))
		(if (and (pair? r) (eq? (car r)
					comma-tag))
		    (append (flatten (cadr r)) (cddr r))
		    (list r))))
	     rest))
      ((else then)
       (cons (append result-left result-right) rest)) 
      (else
       (cons (list (token-op-name token) result-left
		   result-right)
	     rest)))))
