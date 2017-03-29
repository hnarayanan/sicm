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

;;; Parse tokens of algebraic expressions.
;;;   Lambda and if-then-else and separators are 
;;;   treated like arithmetic operations.

(define (shunting-yard tokens)
  (define (token-case-1? op)
    (or (token-binop? op)
	(token-unop? op)))
	;(token-lambda? op)))
  (let shunt ((tokens tokens) (out-stack '()) (op-stack '()))
    (if (not (null? tokens))
        (let ((token (car tokens)))
          (cond ((or (token-number? token)
                     (token-var? token))		     
                 (shunt (cdr tokens)
                        (cons token out-stack)
                        op-stack))
		((token-expr? token)
		 (let ((new-body (shunting-yard (cdr token))))
		   (shunt (cdr tokens)
			(cons (make-expr-token new-body) out-stack)
			op-stack)))
                ((token-rparen? token)
                 (shunt (cdr tokens) out-stack (cons token op-stack)))
                ((token-case-1? token)
                 (do ((out-stack out-stack
                                 (cons (car op-stack) out-stack))
                      (op-stack op-stack (cdr op-stack)))
                     ((or (null? op-stack)
                          (not (token-case-1? (car op-stack)))
                          (>= (token-op-precedence token)
                              (token-op-precedence (car op-stack))))
                      (shunt (cdr tokens)
                             out-stack
                             (cons token op-stack)))))
                ;; Match parentheses
                ((token-lparen? token)
                 (do ((out-stack out-stack
                                 (cons (car op-stack) out-stack))
                      (op-stack op-stack (cdr op-stack)))
                     ((or (null? op-stack)
                          (token-rparen? (car op-stack)))
                      (shunt (cdr tokens)
                             out-stack
			     (cdr op-stack)))))
                (else (error "Unknown token type" token))))
        ;; Read all tokens, so empty the op-stack and interpret
        (append (reverse op-stack) out-stack))))
