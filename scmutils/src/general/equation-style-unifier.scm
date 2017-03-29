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

;;;; Unifier with segment variables -- GJS -- 1 May 2016

(declare (usual-integrations))

(define (unify t1 t2)
  (unify:internal t1 t2 '() (lambda (dict) dict)))

(define (unify:internal t1 t2 dictionary succeed)
  (let lp ((xs (list t1)) (ys (list t2))
	   (dict dictionary) (succeed succeed))
    (define (next xs ys dict succeed)
      (lp (unify:value xs dict) (unify:value ys dict) dict succeed))
    (define (element var target)
      (and (not (unify:segment? target))
	   (or (not (unify:restricted? var))
	       ((unify:restriction var) target))
	   (not (unify:occurs-in? var target))
	   (next (cdr xs) (cdr ys)
		 (unify:bind var target dict) succeed)))
    (define (segment var sources targets)
      (and (or (not (unify:restricted? var))
	       ((unify:restriction var) targets))
	   (let slp ((n (length targets)))
	     (and (>= n 0)
		  (or (let ((seg (list-head targets n)))
			(and (not (unify:occurs-in? var seg))
			     (next (append seg (cdr sources))
				   targets
				   (unify:bind var seg dict)
				   succeed)))
		      (slp (- n 1)))))))
    (cond ((and (null? xs) (null? ys)) (succeed dict))
          ((or (null? xs) (null? ys)) #f)
	  ((and (pair? xs) (pair? ys))
	   (let ((x (car xs)) (y (car ys)))
	     (cond ((equal? x y)
		    (lp (cdr xs) (cdr ys) dict succeed))
		   ((unify:element? x) (element x y))
		   ((unify:element? y) (element y x))
                   ((unify:segment? x) (segment x xs ys))
		   ((unify:segment? y) (segment y ys xs))
		   ((and (list? x) (list? y))
		    (lp x y dict
			(lambda (ndict)
			  (and ndict
			       (next (cdr xs) (cdr ys) ndict
				     succeed)))))
		   (else #f))))
	  (else #f))))

(define (unify:value expr dict)
  (let elp ((expr expr))
    (cond ((unify:element? expr)
	   (let ((vcell (unify:lookup expr dict)))
	     (if vcell
		 (unify:content vcell)
		 expr)))
	  ((unify:segment? expr)
	   (let ((vcell (unify:lookup expr dict)))
	     (if vcell
		 (error "should not get here")
		 expr)))
	  ((list? expr)
	   (let xlp ((xs expr))
	     (if (null? xs)
		 '()
		 (let ((x (car xs)) (rxs (cdr xs)))
		   (if (unify:segment? x)
		       (let ((vcell (unify:lookup x dict)))
			 (if vcell
			     (append (unify:content vcell)
				     (xlp rxs))
			     (cons x (xlp rxs))))
		       (cons (elp x) (xlp rxs)))))))
	  (else expr))))

(define (unify:occurs-in? var expr)
  (let ((name (unify:name var)))
    (let lp ((x expr))
      (cond ((unify:variable? x)
	     (eqv? (unify:name x) name))
	    ((list? x) (any lp x))
	    (else #f)))))

;;; Dictionary

(define (unify:bind var val dict)
  (let* ((new-entry (list (unify:name var) val (unify:type var)))
	 (ndict (list new-entry)))
    (cons new-entry
	  (map (lambda (entry)		;Backsubstitute
		 (list (car entry)
		       (unify:value (cadr entry) ndict)))
	       dict))))

(define (unify:lookup var dict)
  (assq (unify:name var) dict))

(define (unify:content vcell)
  (cadr vcell))


;;; Syntax

(define (unify:element? x)
  (and (pair? x) (eq? (car x) '?)))

(define (unify:segment? x)
  (and (pair? x) (eq? (car x) '??)))

(define (unify:variable? x)
  (and (pair? x) (memq (car x) '(? ??))))

(define (unify:name var)
  (cadr var))


(define (unify:restricted? pattern)
  (not (null? (cddr pattern))))

(define (unify:restriction pattern)
  (caddr pattern))

(define (unify:type pattern)
  (car pattern))
