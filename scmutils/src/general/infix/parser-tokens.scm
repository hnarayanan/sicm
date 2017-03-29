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

;;; Token support

(define lparen-tag 'lparen)

(define lparen-token (list lparen-tag))

(define (token-lparen? token)
  (and (pair? token) (eq? (car token) lparen-tag)))


(define rparen-tag 'rparen)

(define rparen-token (list rparen-tag))

(define (token-rparen? token)
  (and (pair? token) (eq? (car token) rparen-tag)))


(define number-tag 'number)

(define (token-number? token)
  (and (pair? token) (eq? (car token) number-tag)))

(define (token-number-value token) (cadr token))
(define (make-number-value chars)
  (string->number (list->string chars)))

(define (make-numeric-constant-token chars)
  (list number-tag (make-number-value chars)))


(define variable-tag 'var)

(define (token-var? token)
  (and (pair? token) (eq? (car token) variable-tag)))

(define (token-var-name token) (cadr token))

(define (make-var-token chars)
  (list variable-tag (chars->symbol chars)))

(define expr-tag 'expr)

(define (token-expr? token)
  (and (pair? token) (eq? (car token) expr-tag)))

(define (make-expr-token expr)
  (cons expr-tag expr))

(define unop-tag 'unop)

(define (token-unop? token)
  (and (pair? token) (eq? (car token) unop-tag)))

(define (make-unop-token op-name op-precedence)
  (list unop-tag op-precedence op-name))

(define unary-minus-token (make-unop-token '- 3))
(define unary-plus-token (make-unop-token '+ 3))

(define (token-unary-minus? token)
  (and (token-unop? token)
       (eq? (token-op-name token) '-)))

(define binop-tag 'binop)

(define (token-binop? token)
  (and (pair? token) (eq? (car token) binop-tag)))

;;; For token-function, token-unary-minus, token-unary-plus
(define (token-op-precedence token) (cadr token))
(define (token-op-name token) (caddr token))
(define (make-binop-token op-name op-precedence)
  (list binop-tag op-precedence op-name))

(define expt-token (make-binop-token 'expt 5))
(define mul-token (make-binop-token '* 3))
(define div-token (make-binop-token '/ 3))
(define add-token (make-binop-token '+ 1))
(define sub-token (make-binop-token '- 1))
(define >-token (make-binop-token '> 0))
(define <-token (make-binop-token '< 0))
(define >=-token (make-binop-token '>= 0))
(define <=-token (make-binop-token '<= 0))
(define ==-token (make-binop-token '= 0))

(define comma-tag 'comma)

(define (token-comma? token)
  (and (token-binop? token)
       (eq? (token-op-name token) comma-tag)))

(define comma-token
  (make-binop-token comma-tag -1))

(define lambda-tag 'lambda)

(define (token-lambda? token)
  (and (token-unop? token)
       (pair? (token-op-name token))
       (eq? (car (token-op-name token)) lambda-tag)))

(define (token-lambda-args token) (cddr token))

(define (get-lambda-args token)
  (caddr token))

(define (make-lambda-args args)
  (define (finish-arg a)
    (chars->symbol (reverse a)))
  (let iter ((todo args) (current '()) (done '()))
    (cond ((null? todo) 
	   (reverse (cons (finish-arg current) done)))
	  ((memq (car todo) whitespace)
	   (iter (cdr todo) current done))
	  ((eq? (car todo) #\,)
	   (iter (cdr todo) 
		 '() 
		 (cons (finish-arg current) done)))
	  (else
	   (iter (cdr todo) 
		 (cons (car todo) current)
		 done)))))

(define (make-lambda-token args)
  (make-unop-token (list lambda-tag (make-lambda-args args)) 1000))
 
(define (make-sequence-token body)
  (list sequence-tag body))

(define apply-tag 'apply)

(define apply-token (make-binop-token apply-tag 700))

(define (token-apply? token)
  (and (token-binop? token)
       (eq? (token-op-name token) apply-tag)))

(define end-tag 'begin)

(define end-token (make-binop-token end-tag -1000))

(define (token-end? token)
  (and (token-binop? token)
       (eq? (token-op-name token) end-tag)))

(define define-tag 'define)

(define define-token (make-binop-token define-tag -500))

(define (token-define? token)
  (and (token-binop? token)
       (eq? (token-op-name token) define-tag)))

(define if-tag 'if)

(define (token-if? token)
  (and (token-unop? token)
       (eq? (token-op-name token) if-tag)))

(define if-token (make-unop-token if-tag 1000))

(define else-tag 'else)

(define (token-else? token)
  (and (token-binop? token)
       (eq? (token-op-name token) else-tag)))

(define else-token (make-binop-token else-tag 1000))

(define then-tag 'then)

(define (token-then? token)
  (and (token-binop? token)
       (eq? (token-op-name token) then-tag)))

(define then-token (make-binop-token then-tag 1000))

(define quotation-tag 'quote)

(define (token-quotation? token)
  (and (token-binop? token)
       (eq? (token-op-name token) quotation-tag)))

(define quotation-token (make-binop-token quotation-tag 0))
