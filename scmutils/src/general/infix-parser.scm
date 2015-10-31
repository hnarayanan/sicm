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

;;;;               Simple Infix parser 
;;; Original version from Aaron Graham-Horowitz, Spring 2014
;;; Cleaned up and Extended by GJS, Spring 2014
;;;  GJS added
;;;      Multi-argument functions, args separated by commas.
;;;      Whitespace is now ignored, except as stops.
;;;      Quotation of symbols and expressions.  (But not lists!)

;;; Tokenizes a string of infix expressions and then parses tokens
;;; using Shunting-yard algorithm.  Can evaluate arbitrary functions
;;; of several arguments using parentheses for application and commas
;;; for separating arguments.

;;; A macro is provided at the end to allow one to enter infix
;;; algebraic expressions in a Scheme program.  (For those who care!)

;;; Token support

(define lparen-tag (list 'lparen))

(define lparen-token (list lparen-tag))

(define (token-lparen? token)
  (and (pair? token) (eq? (car token) lparen-tag)))


(define rparen-tag (list 'rparen))

(define rparen-token (list rparen-tag))

(define (token-rparen? token)
  (and (pair? token) (eq? (car token) rparen-tag)))


(define number-tag (list 'number))

(define (token-number? token)
  (and (pair? token) (eq? (car token) number-tag)))

(define (token-number-value token) (cadr token))
(define (make-number-value chars)
  (string->number (list->string chars)))

(define (make-numeric-constant-token chars)
  (list number-tag (make-number-value chars))) 


(define variable-tag (list 'var))

(define (token-var? token)
  (and (pair? token) (eq? (car token) variable-tag)))

(define (token-var-name token) (cadr token))
(define (make-var-name chars)
  (string->symbol (list->string chars)))

(define (make-var-token chars)
  (list variable-tag (make-var-name chars)))

(define function-tag (list 'function))

(define (token-function? token)
  (and (pair? token) (eq? (car token) function-tag)))

(define function-precedence 666)

(define (make-function-token function-name)
  (list function-tag function-precedence function-name))


(define unary-minus-tag (list '-))

(define (token-unary-minus? token)
  (and (pair? token) (eq? (car token) unary-minus-tag)))

(define unary-minus-precedence 3)

(define unary-minus-token
  (list unary-minus-tag unary-minus-precedence '-))


(define op-tag (list 'op))

(define (token-op? token)
  (and (pair? token) (eq? (car token) op-tag)))

;;; These are for token-function and token-unary-minus too

(define (token-op-precedence token) (cadr token))
(define (token-op-name token) (caddr token))

(define (make-op-token op-name op-precedence)
  (list op-tag op-precedence op-name))

(define expt-token (make-op-token 'expt 5))
(define mul-token (make-op-token '* 3))
(define div-token (make-op-token '/ 3))
(define add-token (make-op-token '+ 1))
(define sub-token (make-op-token '- 1))

(define comma-tag (list 'comma))

(define (token-comma? token)
  (and (token-op? token)
       (eq? (token-op-name token) comma-tag)))

(define comma-token
  (make-op-token comma-tag -1))


(define quotation-tag (list 'quote))
  
(define (token-quotation? token)
  (and (token-op? token)
       (eq? (token-op-name token) quotation-tag)))

(define quotation-token (make-op-token quotation-tag 0))

;;; Tokenizer produces tokens in reverse order for prefix notation.
;;;  This part knows about how characters map to tokens.

(define (tokenize infix-string)
  (define whitespace (list #\space #\tab #\newline #\return #\page))
  (define stops (append (string->list "()^*/+-,") whitespace))
  (define numerals (string->list ".0123456789"))
  (let lp ((chars (string->list infix-string)) (tokens '()))
    (if (null? chars)
        tokens
        (let ((last-token (if (null? tokens) '() (car tokens))))
          (case (car chars)
	    ((#\space #\tab #\newline #\return #\page)
	     (lp (cdr chars) tokens))
            (( #\( )
             (if (token-var? last-token) ;function
                 (let* ((read-result (match-paren chars))
                        (arg-tokens (lp (car read-result) '()))
                        (remaining-chars (cdr read-result)))
                   (lp remaining-chars
                       (append arg-tokens
                               (cons (make-function-token
				      (token-var-name last-token))
                                     (cdr tokens)))))
                 (lp (cdr chars) (cons lparen-token tokens))))
            (( #\) ) (lp (cdr chars) (cons rparen-token tokens)))
            (( #\^ ) (lp (cdr chars) (cons expt-token tokens)))
            (( #\* ) (lp (cdr chars) (cons mul-token tokens)))
            (( #\/ ) (lp (cdr chars) (cons div-token tokens)))
            (( #\+ ) (lp (cdr chars) (cons add-token tokens)))
            (( #\- )
             (if (or (null? last-token)
                     (token-lparen? last-token)
                     (token-op? last-token))
                 (lp (cdr chars) (cons unary-minus-token tokens))
                 (lp (cdr chars) (cons sub-token tokens))))
	    (( #\' ) (lp (cdr chars) (cons quotation-token tokens)))
	    (( #\, ) (lp (cdr chars) (cons comma-token tokens)))
            (else
             (let* ((read-result (read-until chars stops))
                    (read-chars (car read-result))
                    (remaining-chars (cdr read-result)))
	       (cond ((memq (car chars) numerals)
		      (lp remaining-chars
			  (cons (make-numeric-constant-token read-chars)
				tokens)))
		     (else
		      (lp remaining-chars
			  (cons (make-var-token read-chars)
				tokens)))))))))))

(define (match-paren chars)
  (let lp ((chars chars) (n 0) (matched '()))
    (if (or (null? chars)
            (and (not (null? matched))
                 (<= n 0)))
        (cons (reverse matched) chars)
        (let ((n (case (car chars)
                   (( #\( ) (+ n 1))
                   (( #\) ) (- n 1))
                   (else n))))
          (lp (cdr chars)
              n
              (cons (car chars) matched))))))

(define (read-until chars stops)
  (let lp ((chars chars) (matched '()))
    (if (or (null? chars) (memq (car chars) stops))
        (cons (reverse matched) chars)
        (lp (cdr chars) (cons (car chars) matched)))))

;;; Parse tokens

(define (shunting-yard tokens)
  (let shunt ((tokens tokens) (out-stack '()) (op-stack '()))
    (if (not (null? tokens))
        (let ((token (car tokens)))
          (cond ((or (token-number? token)
                     (token-var? token)
                     (token-function? token))
                 (shunt (cdr tokens)
                        (cons token out-stack)
                        op-stack))
                ((or (token-rparen? token) (token-function? token))
                 (shunt (cdr tokens) out-stack (cons token op-stack)))
                ((or (token-op? token)
		     (token-unary-minus? token)
		     (token-quotation? token))
                 (do ((out-stack out-stack
                                 (cons (car op-stack) out-stack))
                      (op-stack op-stack (cdr op-stack)))
                     ((or (null? op-stack)
                          (not (or (token-op? (car op-stack))
                                   (token-unary-minus? (car op-stack))
				   (token-quotation? (car op-stack))))
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

(define (analyze-tokens tokens)
  (define (analyze-rec tokens)
    (if (null? tokens)
        (cons #f tokens)
        (let ((token (car tokens)))
          (cond
           ;; Self evaluating
           ((token-number? token)
            (cons (token-number-value token) (cdr tokens)))
           ((token-var? token)
            (cons (token-var-name token) (cdr tokens)))
           ;; Unary operator or function
           ((token-unary-minus? token)
            (let* ((result-rest (analyze-rec (cdr tokens)))
                   (result (car result-rest))
                   (rest (cdr result-rest)))
	      (if (number? result)
		  (cons (- result) rest)
		  (cons (list (token-op-name token) result) rest))))
	   ((token-quotation? token)
            (let* ((result-rest (analyze-rec (cdr tokens)))
                   (result (car result-rest))
                   (rest (cdr result-rest)))
	      (cons (list 'quote result) rest)))
	   ((token-function? token)
	    (let* ((result-rest (analyze-rec (cdr tokens)))
		   (result (car result-rest))
		   (rest (cdr result-rest)))
	      (cons (cons (token-op-name token)
			  (let flatten ((r result))
			    (if (and (pair? r) (eq? (car r) comma-tag))
				(append (flatten (cadr r)) (cddr r))
				(list r))))
		    rest)))
           ;; Binary operator
	   ((token-op? token)
            (let* ((result-rest-left (analyze-rec (cdr tokens)))
                   (result-left (car result-rest-left))
                   (rest-left (cdr result-rest-left))
                   (result-rest-right (analyze-rec rest-left))
                   (result-right (car result-rest-right))
                   (rest (cdr result-rest-right)))
              (cons (list (token-op-name token) result-left
                          result-right)
                    rest)))
           ;; Catch
           ((or (token-lparen? token) (token-rparen? token))
            (error "Mismatched parentheses in infix string"))
           (else (error "Bad token in infix string" token))))))
  (car (analyze-rec tokens)))

(define (infix-string->combination infix-string)
  (analyze-tokens (shunting-yard (tokenize infix-string))))

(define-syntax infix
  (er-macro-transformer
   (lambda (exp rename compare)
     (infix-string->combination (cadr exp)))))

#|
;;; Tests

(pp (tokenize "3*foo(bar)^2"))
#|
(((number) 2)
 ((op) 5 expt)
 ((rparen))
 ((var) bar)
 ((lparen))
 ((function) 666 foo)
 ((op) 3 *)
 ((number) 3))
|#

(infix-string->combination "3*foo(bar)^2")
;Value: (* 3 (expt (foo bar) 2))


(pp (tokenize "3*foo(bar^2)"))
#|
(((rparen))
 ((number) 2)
 ((op) 5 expt)
 ((var) bar)
 ((lparen))
 ((function) 666 foo)
 ((op) 3 *)
 ((number) 3))
|#

(infix-string->combination "3*foo(bar^2)")
;Value: (* 3 (foo (expt bar 2)))

(infix-string->combination "-3*foo(bar^2)")
;Value: (* -3 (foo (expt bar 2)))

(infix-string->combination "3*foo(bar^(-2))")
;Value: (* 3 (foo (expt bar -2)))

;;; Ugh...
(infix-string->combination "3*foo(bar^-2)")
;Value: (* 3 (foo (- (expt bar 2))))

;;; Bletch! Unary + is bad...
(infix-string->combination "+3*foo(bar^2)")
;Value: (+ (* 3 (foo (expt bar 2))) #f)
|#

#|
(infix-string->combination "a*x^2+b*x+c")
;Value: (+ (+ (* a (expt x 2)) (* b x)) c)

(infix-string->combination "x^2+b/a*x+c/a")
;Value: (+ (+ (expt x 2) (* (/ b a) x)) (/ c a))

(infix-string->combination "x^2+b/(a*x)+c/a")
;Value: (+ (+ (expt x 2) (/ b (* a x))) (/ c a))

(infix-string->combination "-x^2")
;Value (- (expt x 2))

(infix-string->combination "(-x)^2")
;Value: (expt (- x) 2)

(infix-string->combination "-2.4e20 * foo(bar^2)")
;Value: (* -2.4e20 (foo (expt bar 2)))

(infix-string->combination "-2*foo(bar^2, 'bletch, mum)")
;Value: (* -2 (foo (expt bar 2) (quote bletch) mum))

;;; Leaving out a comma is disasterous
(infix-string->combination "-2*foo(bar^2, 'bletch mum)")
;Value: (* -2 (foo (expt bar 2) (quote bletch)))

;;; Indeed, those commas are very essential!
(infix-string->combination "-2*foo(bar^2 'bletch, mum)")
;Value: (* -2 (foo (quote (expt bar 2)) bletch))
|#

