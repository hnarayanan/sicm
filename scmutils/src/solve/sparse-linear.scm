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

;;;; Sparse linear equation solver
;;;   Written by GJS with help from Radhika Nagpal, 25 November 1998.

(declare (usual-integrations + - * / zero?))


;;; Equations are represented as variable terms and a constant.  Each
;;; variable term has a variable identifier and a coefficient.

(define (seq:<identifier? id1 id2)
  (if (and (integer? id1) (integer? id2))
      (fix:< id1 id2)
      (variable<? id1 id2)))

(define-integrable (seq:make-term identifier coefficient)
  (list identifier coefficient))

(define-integrable (seq:term->identifier term)
  (car term))

(define-integrable (seq:term->coefficient term)
  (cadr term))

(define-integrable (seq:occurs? identifier termlist)
  (assoc identifier termlist))


(define-integrable (seq:make-equation terms constant)
  (list terms constant))

(define-integrable (seq:equation->terms equation)
  (car equation))

(define-integrable (seq:equation->constant equation)
  (cadr equation))

(define-integrable (seq:no-terms? terms)
  (null? terms))


(define-integrable (seq:make-substitution isolated-var-id value)
  (list isolated-var-id value))

(define-integrable (seq:substitution->identifier substitution)
  (car substitution))

(define-integrable (seq:substitution->value substitution)
  (cadr substitution))

(define (seq:solve equations)
  (let eqlp ((unprocessed equations)
	     (substitutions '()))
    (if (null? unprocessed)
	(sort substitutions
	      (lambda (s1 s2)
		(seq:<identifier? (seq:substitution->identifier s1)
				  (seq:substitution->identifier s2))))
	(let ((subst (seq:isolate-one-var (car unprocessed))))
	  (cond ((eq? subst 'tautology)
		 (eqlp (cdr unprocessed) substitutions))
		((eq? subst 'contradiction)
		 (error "Contradiction discovered -- SEQ:SOLVE")
		 equations)
		(else
		 (eqlp (map (lambda (equation)
			      (seq:backsubstitute subst equation))
			    (cdr unprocessed))
		       (cons subst
			     (map (lambda (substitution)
				    (seq:make-substitution
				     (seq:substitution->identifier
				      substitution)
				     (seq:backsubstitute
				      subst
				      (seq:substitution->value
				       substitution))))
				  substitutions)))))))))


(define (seq:isolate-one-var equation)
  (if (seq:no-terms? (seq:equation->terms equation))
      (if (zero? (seq:equation->constant equation))
	  'tautology
	  'contradiction)
      (let ((pivot-term (seq:choose-pivot (seq:equation->terms equation))))
	(let ((pivot-coefficient (- (seq:term->coefficient pivot-term))))
	  (seq:make-substitution
	   (seq:term->identifier pivot-term)
	   (seq:make-equation
	    (map (lambda (term)
		   (seq:make-term (seq:term->identifier term)
				  (/ (seq:term->coefficient term)
				     pivot-coefficient)))
		 (delq pivot-term (seq:equation->terms equation)))
	    (/ (seq:equation->constant equation)
	       pivot-coefficient)))))))


;;; Hal's strategy

(define (seq:choose-pivot terms)
  (if (for-all? (map seq:term->coefficient terms) number?)
      (let lp ((terms (cdr terms)) (best (car terms)))
	(cond ((null? terms) best)
	      ((> (seq:term->coefficient (car terms))
		  (seq:term->coefficient best))
	       (lp (cdr terms) (car terms)))
	      (else (lp (cdr terms) best))))
      (car terms)))

(define (seq:backsubstitute substitution equation)
  (let ((identifier (seq:substitution->identifier substitution))
	(value (seq:substitution->value substitution))
	(equation-terms (seq:equation->terms equation))
	(equation-constant (seq:equation->constant equation)))
    (let ((value-terms (seq:equation->terms value))
	  (value-constant (seq:equation->constant value)))
      (let ((occurrence (seq:occurs? identifier equation-terms)))
	(if occurrence
	    (seq:make-equation
	     (seq:add-terms (seq:scale-terms (seq:term->coefficient occurrence)
					     value-terms)
			    (delq occurrence equation-terms))
	     (+ (* (seq:term->coefficient occurrence) value-constant)
		equation-constant))
	    equation)))))

(define (seq:scale-terms constant terms)
  (map (lambda (term)
	 (seq:make-term (seq:term->identifier term)
			(* constant (seq:term->coefficient term))))
       terms))

(define (seq:add-terms terms1 terms2)
  (cond ((seq:no-terms? terms1) terms2)
	((seq:no-terms? terms2) terms1)
	((seq:<identifier? (seq:term->identifier (car terms1))
			   (seq:term->identifier (car terms2)))
	 (cons (car terms1)
	       (seq:add-terms (cdr terms1) terms2)))
	((seq:<identifier? (seq:term->identifier (car terms2))
			   (seq:term->identifier (car terms1)))
	 (cons (car terms2)
	       (seq:add-terms terms1 (cdr terms2))))
	(else				;Same identifier
	 (let ((new-coefficient (+ (seq:term->coefficient (car terms1))
				   (seq:term->coefficient (car terms2)))))
	   (if (zero? new-coefficient)
	       (seq:add-terms (cdr terms1) (cdr terms2))
	       (cons (seq:make-term (seq:term->identifier (car terms1))
				    new-coefficient)
		     (seq:add-terms (cdr terms1) (cdr terms2))))))))

#|
(define eqs1
  (list (seq:make-equation
	 (list (seq:make-term 'x 3) (seq:make-term 'y 4))
	 -7)
	(seq:make-equation
	 (list (seq:make-term 'x 2) (seq:make-term 'y -1))
	 -1)))

(seq:solve eqs1)
;Value: ((y (() 1)) (x (() 1)))


(define eqs2
  (list (seq:make-equation
	 (list (seq:make-term 'x -3) (seq:make-term 'y 4))
	 -7)
	(seq:make-equation
	 (list (seq:make-term 'x 2) (seq:make-term 'y -1))
	 -1)))
(seq:solve eqs2)
;Value: ((y (() 17/5)) (x (() 11/5)))
|#

(define (seq:matrix->sparse matrix vect)
  (let ((n (m:num-rows matrix)) (m (m:num-cols matrix)))
    (let rowlp ((i 0))
      (if (fix:= i n)
	  '()
	  (cons
	   (seq:make-equation
	    (let collp ((j 0))
	      (cond ((fix:= j m) '())
		    ((zero? (m:ref matrix i j))
		     (collp (fix:+ j 1)))
		    (else
		     (cons (seq:make-term j (m:ref matrix i j))
			   (collp (fix:+ j 1))))))
	    (- (ref vect i)))
	   (rowlp (fix:+ i 1)))))))

#|
(define testm
  (matrix-by-rows (list 1 1 0 0)
		  (list 0 1 0 0)
		  (list 0 0 1 1)
		  (list 0 0 0 1)))

(define testv
  (vector 1 2 3 4))

(seq:solve (seq:matrix->sparse testm testv))
;Value: ((3 (() 4)) (2 (() -1)) (1 (() 2)) (0 (() -1)))

(/ testv testm)
;Value: #(-1 2 -1 4)		  
|#
