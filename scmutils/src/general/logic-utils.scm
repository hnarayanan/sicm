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

(declare (usual-integrations))

#|
;;; In Scheme system: runtime/boole.scm
(define (false? x) (eq? x #f))
|#

(define (true? x) (eq? x #t))


;;; Debugging aids

(define (assert p #!optional error-comment irritant)
  (if (not p)
      (begin
	(if (not (default-object? irritant))
	    (pp irritant))
	(error (if (default-object? error-comment)
		   "Failed assertion"
		   error-comment)))))

(define (with-protection protected-value-thunk untrusted-thunk
			 #!optional violation-thunk)
  (let ((saved-value (protected-value-thunk))
	(return-value (untrusted-thunk)))
    (if (not (equal? saved-value (protected-value-thunk)))
	(if (default-object? untrusted-thunk)
	    (error "Protection violation")
	    (violation-thunk))
	return-value)))

;;;; Assumptions made in processing are noted.  See kernel/utils.scm .

(define *assumption-tolerance-multiplier* 100)

(define (assume! predicate-expression responsible-party #!optional if-false)
  (define (do-false)
    (if (default-object? if-false)
        (add-assumption! `(false! ,predicate-expression) responsible-party)
        (if-false)))
  (define (default) (add-assumption! predicate-expression responsible-party))
  (define (default-numeric rator rands)
    (cond ((environment-bound? scmutils-base-environment rator)
           (let ((predicate
                  (environment-lookup scmutils-base-environment rator)))
             (if (procedure? predicate)
                 (let ((val (apply predicate rands)))
                   (cond ((not val) (do-false))
                         ((true? val) 'OK)
                         (else (default))))
                 (error "Bad assumption"
                        predicate-expression responsible-party))))
          (else (default))))
  (define (simple-numeric rator rands)
    (case rator
      ((=) (if (or (inexact? (car rands)) (inexact? (cadr rands)))
               (if (close-enuf? (car rands) (cadr rands)
                                *assumption-tolerance*)
                   'OK
                   (do-false))
               (if (= (car rands) (cadr rands)) 'OK (do-false))))
      (else (default-numeric rator rands))))
  (define *assumption-tolerance*
    (* *assumption-tolerance-multiplier* *machine-epsilon*))
  (cond ((pair? predicate-expression)
         (let ((rator (operator predicate-expression))
               (rands (operands predicate-expression)) )
           (if (every number? rands)
               (simple-numeric rator rands)
               (default))))
        ((not predicate-expression) (do-false)) ;(eq? predicate-expression #f)
        ((true? predicate-expression) 'OK)
        (else (default))))                

(define (add-assumption! assumption responsible-party)
  (let ((a `(assuming ,assumption)))
    (eq-adjoin! a 'rules responsible-party)
    (note-that! a)))

#|
;;; Replaced by for-all?, there-exists? in boole, with args reversed.

(define (forall l p?)
  (let loop ((l l))
    (cond ((null? l) true)
	  ((p? (car l)) (loop (cdr l)))
	  (else false))))


(define (exists p? l)
  (let loop ((l l))
    (cond ((null? l) false)
	  ((p? (car l)) true)
	  (else (loop (cdr l))))))
|#

(define (&or disjuncts)
  (cond ((null? disjuncts) false)
	((car disjuncts) true)
	(else (&or (cdr disjuncts)))))

(define (*or . disjuncts) (&or disjuncts))


(define (&and conjuncts)
  (cond ((null? conjuncts) true)
	((car conjuncts) (&and (cdr conjuncts)))
	(else false)))

(define (*and . conjuncts) (&and conjuncts))


(define (conjunction predicate1 predicate2)
  (lambda (x)
    (and (predicate1 x) (predicate2 x))))

(define (disjunction predicate1 predicate2)
  (lambda (x)
    (or (predicate1 x) (predicate2 x))))

(define (negation predicate)
  (lambda (x) (not (predicate x))))

(define (implication antecedent consequent)
  (lambda (x) (or (not (antecedent x)) (consequent x))))
