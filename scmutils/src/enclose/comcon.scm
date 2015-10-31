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

;;;;                    COMCON.SCM

(declare (usual-integrations))

;;; Useful utilities for programs that construct SCHEME programs.
;;; Needs UTILS.SCM

(define (lambdafy n body-generator)
  (cond ((exact-integer? n)
	 (let ((bvl (make-bound-variables n)))
	   `(lambda ,bvl ,(expression (g:apply body-generator bvl)))))

	((list? n)
	 (let llp ((n n) (body-generator body-generator))
	   (if (null? (cdr n))
	       (let ((bvl (make-bound-variables (car n))))
		 `(lambda ,bvl ,(expression (g:apply body-generator bvl))))
	       (let ((bvl (make-bound-variables (car n))))
		 `(lambda ,bvl
		    ,(llp (cdr n) (g:apply body-generator bvl)))))))
	((and (pair? n)
	      (exact-integer? (car n))
	      (exact-integer? (cdr n))
	      (fix:= (car n) (cdr n)))
	 (lambdafy (car n) body-generator))
	((pair? n)
	 ;; In Scheme 7.5 #f=() so (3) and (3 . #f) are not distinguished.
	 (error "General arity is unimplemented -- LAMBDAFY"
		n))
	(else
	 (error "Bad variable specification -- LAMBDAFY"
		n))))

(define (make-bound-variables n)
  ;;n is a general arity
  (let do-loop ((i 0) (names '()))
    (if (fix:= i n)
        names
        (do-loop (fix:1+ i)
                 (cons (generate-uninterned-symbol 'x) names)))))





(define (letify vals body-generator)
  (if (null? vals)
      (body-generator '())
      (let ((names (map (lambda (x) (generate-uninterned-symbol 'y)) vals)))
        `(let ,(map list names vals) ,(body-generator names)))))

(define (definify name definition-expression)
  (if (pair? definition-expression)
      (if (eq? (car definition-expression) 'lambda)
	  `(define (,name . ,(cadr definition-expression))
	     . ,(cddr definition-expression))
	  `(define ,name ,definition-expression))
      `(define ,name ,definition-expression)))
	     


