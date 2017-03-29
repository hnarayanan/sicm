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

;;; Tokenizer produces tokens in reverse order for prefix notation.
;;;  This part knows about how characters map to tokens.

(define (tokenize infix-string)
  (let lp ((chars (string->list infix-string)) (tokens '()))
    (if (null? chars)
        tokens
        (let ((last-token (if (null? tokens) '() (car tokens))))
          (define (unary-op-check unary-token binary-token)
            (if (or (null? last-token)
                    (token-lparen? last-token)
                    (token-binop? last-token))
                (lp (cdr chars) (cons unary-token tokens))
                (lp (cdr chars) (cons binary-token tokens))))
          (case (car chars)
            ((#\space #\tab #\newline #\return #\page)
             (lp (cdr chars) tokens))
            (( #\( ) (lp (cdr chars) (cons lparen-token tokens)))                
	    (( #\; ) (lp (cdr chars) (cons end-token tokens)))
            (( #\) ) (lp (cdr chars) (cons rparen-token tokens)))
            (( #\^ ) (lp (cdr chars) (cons expt-token tokens)))
            (( #\* ) (lp (cdr chars) (cons mul-token tokens)))
            (( #\/ ) (lp (cdr chars) (cons div-token tokens)))
            (( #\+ ) (unary-op-check unary-plus-token add-token))
            (( #\- ) (unary-op-check unary-minus-token sub-token))
            (( #\' ) (lp (cdr chars) (cons quotation-token tokens)))
            (( #\, ) (lp (cdr chars) (cons comma-token tokens)))
	    (( #\< ) (if (and (pair? (cdr chars))
			      (eq? (cadr chars) #\=))
			 (lp (cddr chars) (cons <=-token tokens))
			 (lp (cdr chars) (cons <-token tokens))))
	    (( #\> ) (if (and (pair? (cdr chars))
			      (eq? (cadr chars) #\=))
			 (lp (cddr chars) (cons >=-token tokens))
			 (lp (cdr chars) (cons >-token tokens))))
	    ;; Continued on next page

            (else
             (let* ((read-result (read-until chars stops))
                    (read-chars (car read-result))
		    (read-symbol (chars->symbol read-chars))
                    (remaining-chars (cdr read-result)))
               (cond
		((and (pair? (cdr chars))
		      (eq? (car chars) #\=)
		      (eq? (cadr chars) #\=))
		 (lp (cddr chars) (cons ==-token tokens)))
		((and (pair? (cdr chars))
		      (eq? (car chars) #\:)
		      (eq? (cadr chars) #\=))
		 (lp (cddr chars) (cons define-token tokens)))
		((eq? read-symbol 'if)
		 (lp remaining-chars (cons if-token tokens)))
		((eq? read-symbol 'then)
		 (lp remaining-chars (cons then-token tokens)))
		((eq? read-symbol 'else)
		 (lp remaining-chars (cons else-token tokens)))
		((eq? read-symbol 'lambda)
		 (let* ((args-result (read-until remaining-chars '(#\:)))
			(lambda-args (car args-result))
			(remaining-chars (cdr args-result)))
		   (lp (cdr remaining-chars)
		       (cons (make-lambda-token lambda-args) tokens))))
		((memq (car chars) numerals)
		 (tokenize-number read-chars remaining-chars tokens lp))
		(else
		 (lp remaining-chars
		     (cons (make-var-token read-chars)
			   tokens)))))))))))

(define (tokenize-number read-chars remaining-chars tokens lp)
  (if (and (memq (last read-chars) '(#\e #\E))
	   (memq (car remaining-chars) '(#\+
					 #\-)))
      (let ((exponent-split
	     (read-until (cdr remaining-chars)
			 stops)))
	(let ((n (make-number-value
		  (append read-chars
			  (cons (car
				 remaining-chars)
				(car
				 exponent-split))))))
	  (if n
	      (lp (cdr exponent-split)
		  (cons (list number-tag n)
			tokens))
	      (lp remaining-chars
		  (cons (make-var-token
			 read-chars)
			tokens)))))
      (let ((n (make-number-value read-chars)))
	(if n
	    (lp remaining-chars
		(cons (list number-tag n)
		      tokens))))))