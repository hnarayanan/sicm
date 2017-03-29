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

;;; Utility

(define whitespace
  (list #\space #\tab #\newline #\return #\page))

(define (chars->symbol chars)
  (string->symbol (list->string chars)))

(define stops
  (append (string->list ";()^*/+-,<>=:")
	  whitespace))

(define numerals (string->list ".0123456789"))


(define (read-until chars stops)
  (let lp ((chars chars) (matched '()))
    (if (or (null? chars) (memq (car chars) stops))
        (cons (reverse matched) chars)
        (lp (cdr chars) (cons (car chars) matched)))))


(define (make-matcher n add? minus? include-end?)
  (lambda (input)
    (let lp ((input input) (n n) (matched '()))
      (cond ((and (not (null? matched))
		  (<= n 0))
	     (if include-end?
		 (cons (reverse matched) input)
		 (cons* (reverse (cdr matched))
			(car matched)
			input)))
	    ((null? input)
	     (cons (reverse matched) input))
	    (else
	     (let ((n
		    (cond ((add? (car input)) (+ n 1))
			  ((minus? (car input)) (- n 1))
			  (else n))))
	       (lp (cdr input)
		   n
		   (cons (car input) matched))))))))

(define match-paren-chars
  (make-matcher 0
		(lambda (c) (eq? #\( c))
		(lambda (c) (eq? #\) c))
		#t))

