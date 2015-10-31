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

;;;; Compiler for rules.

(declare (usual-integrations))

;;; Rule syntax.
;;; Rule does not apply if consequent expression returns #f.

(define (rule:compile rule)
  (cond ((= (length rule) 3)
         (let* ((pattern-expression
                 (pattern:compile (rule:pattern rule)))
                (predicate-expression
                 (predicate:compile (rule:predicate rule)))
                (skeleton-expression
                 (skel:compile (rule:skeleton rule))))
           (let ((vars (pattern:vars pattern-expression)))
             (let ((consequent-expression
                    (if (eq? predicate-expression 'none)
                        `(lambda ,vars ,skeleton-expression)
                        `(lambda ,vars
                           (let ((predicate-value ,predicate-expression))
                             (and predicate-value
                                  ,skeleton-expression))))))
               `(rule:make ,pattern-expression
                           ,consequent-expression)))))
        ((= (length rule) 2)
         (let* ((pattern-expression
                 (pattern:compile (rule:pattern rule))))
           (let ((vars (pattern:vars pattern-expression)))
             (let ((consequent-expression
                    `(lambda ,vars
                       ,(rule:consequent rule))))
               `(rule:make ,pattern-expression
                           ,consequent-expression)))))
        (else
         (error "Badly-formed rule" rule))))

(define (rule:pattern rule)
  (car rule))

(define (rule:predicate rule)
  (cadr rule))

(define (rule:skeleton rule)
  (caddr rule))

(define (rule:consequent rule)
  (cadr rule))

(define (pattern:compile pattern) 
  (define (compile pattern)
    (cond ((match:element? pattern)
	   (if (match:restricted? pattern)
	       (list '?
		     (match:variable-name pattern)
		     (list 'unquote (match:restriction pattern)))
	       pattern))
	  ((match:segment? pattern) pattern)
	  ((match:reverse-segment? pattern) pattern)
	  ((list? pattern) (map compile pattern))
	  ((pair? pattern)
	   (cons (compile (car pattern))
		 (compile (cdr pattern))))
	  (else pattern)))
  (list 'quasiquote (compile pattern)))


(define (pattern:vars pattern)
  (let ((vars '()))
    (define (add-var! v)
      (or (memq v vars)
	  (set! vars (cons v vars))))
    (define (compile pattern)
      (cond ((match:element? pattern)
	     (add-var! (match:variable-name pattern)))
	    ((match:segment? pattern)
	     (add-var! (match:variable-name pattern)))
	    ((match:reverse-segment? pattern)
	     (add-var! (match:variable-name pattern)))
	    ((list? pattern)
	     (for-each compile pattern))
	    ((pair? pattern)
	     (compile (car pattern))
	     (compile (cdr pattern)))
	    (else 'nothing)))
    (compile pattern)
    vars))

(define (match:element? pattern)
  (and (pair? pattern)
       (eq? (car pattern) '?)))

(define (match:segment? pattern)
  (and (pair? pattern)
       (eq? (car pattern) '??)))

(define (match:variable-name pattern)
  (cadr pattern))


(define (match:restricted? pattern)
  (not (null? (cddr pattern))))

(define (match:restriction pattern)
  (caddr pattern))

(define (match:reverse-segment? pattern)
  (and (pair? pattern)
       (eq? (car pattern) '$$)))

(define (predicate:compile predicate)
  predicate)

(define none #t)





(define (skel:compile skeleton)
  (define (compile skeleton)
    (cond ((skel:constant? skeleton) skeleton)
	  ((skel:element? skeleton)
	   (list 'unquote (skel:element-expression skeleton)))
	  ((skel:segment? skeleton)
	   (list 'unquote-splicing (skel:segment-expression skeleton)))
	  ((list? skeleton)
	   (map compile skeleton))
	  ((pair? skeleton)
	   (cons (compile (car skeleton))
		 (compile (cdr skeleton))))
	  (else
	   (error "Unknown skeleton entry -- skel:compile"
		  skeleton))))
   (list 'quasiquote (compile skeleton)))
		       
(define (skel:constant? skeleton)
  (not (pair? skeleton)))


(define (skel:element? skeleton)
  (and (pair? skeleton)
       (eq? (car skeleton) ':)))

(define (skel:element-expression skeleton)
  (cadr skeleton))


(define (skel:segment? skeleton)
  (and (pair? skeleton)
       (eq? (car skeleton) '::)))

(define (skel:segment-expression skeleton)
  (cadr skeleton))
