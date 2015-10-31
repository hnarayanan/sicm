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

;;;;   Nice Subsets of the Real Line
;;; Each interval is a lower bound and an upper bound
;;; Each bound may be closed or opened.

(define (make-interval lower-bound-closed?
		       lower-bound
		       upper-bound
		       upper-bound-closed?)
  (cond ((extended<= lower-bound upper-bound)
	 (if (and (number? lower-bound)
		  (number? upper-bound)
		  (= lower-bound upper-bound))
	     (if (or lower-bound-closed?
		     upper-bound-closed?)
		 lower-bound
		 the-empty-interval)
	     (list interval-type-tag
		   (list lower-bound-closed?
			 lower-bound)
		   (list upper-bound
			 upper-bound-closed?))))
	(else the-empty-interval)))
	 

(define interval-type-tag '*interval*)

(define (interval? int)
  (or (eq? int the-empty-interval)
      (and (pair? int)
	   (eq? (car int) interval-type-tag))))

(define (interval-empty? int)
  (eq? int the-empty-interval))

(define the-empty-interval 'the-empty-interval)

(define (interval-lower-bound-closed? int)
  (or (number? int) (car (cadr int))))
(define (interval-lower-bound int)
  (if (number? int) int (cadr (cadr int))))
(define (interval-upper-bound int)
  (if (number? int) int (car (caddr int))))
(define (interval-upper-bound-closed? int)
  (or (number? int) (cadr (caddr int))))

;;; Arithmetic on extended reals

#| ;;; In src/numerics/quadrature/quadrature.scm
(define :+infinity ':+infinity)
(define :-infinity ':-infinity)
|#

(define (+infinity? x)
  (eq? x :+infinity))

(define (-infinity? x)
  (eq? x :-infinity))

(define (extended-negative? x)
  (or (and (number? x) (negative? x))
      (-infinity? x)))

(define (extended-positive? x)
  (or (and (number? x) (positive? x))
      (+infinity? x)))

(define (extended< x y)
  (or (and (number? x) (number? y) (< x y))
      (and (-infinity? x) (or (number? y) (+infinity? y)))
      (and (+infinity? y) (or (number? x) (-infinity? x)))))

(define (extended<= x y)
  (or (and (number? x) (number? y) (<= x y))
      (-infinity? x)
      (+infinity? y)))

(define (extended= x y)
  (or (and (number? x) (number? y) (= x y))
      (and (-infinity? x) (-infinity? y))
      (and (+infinity? x) (+infinity? y))))

(define (extended-negate x)
  (cond ((number? x) (- x))
	((+infinity? x) :-infinity)
	((-infinity? x) :+infinity)))

(define (extended-invert x)
  (cond ((number? x) (/ x))
	((+infinity? x) :-infinity)
	((-infinity? x) :+infinity)))

(define (extended+ x y)
  (cond ((and (number? x) (number? y)) (+ x y))
	((or (and (-infinity? x) (number? y))
	     (and (-infinity? y) (number? x))
	     (and (-infinity? x) (-infinity? y)))
	 :-infinity)
	((or (and (+infinity? x) (number? y))
	     (and (+infinity? y) (number? x))
	     (and (+infinity? x) (+infinity? y)))
	 :+infinity)
	(else
	 (error "How to add " x y))))

(define (extended* x y)
  (cond ((and (number? x) (number? y)) (* x y))
	((or (and (-infinity? x) (number? y))
	     (and (-infinity? y) (number? x))
	     (and (-infinity? x) (-infinity? y)))
	 :-infinity)
	((or (and (+infinity? x) (number? y))
	     (and (+infinity? y) (number? x))
	     (and (+infinity? x) (+infinity? y)))
	 :+infinity)
	(else
	 (error "How to multiply " x y))))

(define (interval-disjoint? i1 i2)
  (let ((i1l (interval-lower-bound i1))
	(i1u (interval-upper-bound i1))
	(i2l (interval-lower-bound i2))
	(i2u (interval-upper-bound i2)))
    (and (extended<= i1l i2l)
	 (or (extended< i1u i2l)
	     (and (extended= i1u i2l)
		  (not (interval-upper-bound-closed? i1)
		       (interval-lower-bound-closed? i2)))))))

;;; This is a pain in the arse!

(define (interval-intersect i1 i2)
  (let ((i1l (interval-lower-bound i1))
	(i1u (interval-upper-bound i1))
	(i2l (interval-lower-bound i2))
	(i2u (interval-upper-bound i2)))
    (cond ((extended< i1l i2l)
	   (cond ((extended< i1u i2u)
		  (make-interval (interval-lower-bound-closed? i2)
				 i2l i1u
				 (interval-upper-bound-closed? i1)))
		 ((extended< i2u i1u) i2)
		 (else
		  (make-interval (interval-lower-bound-closed? i2)
				 i2l
				 i2u
				 (and (interval-upper-bound-closed? i1)
				      (interval-upper-bound-closed? i2))))))
	  ((extended< i2l i1l)
	   (cond ((extended< i2u i1u)
		  (make-interval (interval-lower-bound-closed? i1)
				 i1l i2u
				 (interval-upper-bound-closed? i2)))
		 ((extended< i1u i2u) i1)
		 (else
		  (make-interval (interval-lower-bound-closed? i1)
				 i1l
				 i1u
				 (and (interval-upper-bound-closed? i1)
				      (interval-upper-bound-closed? i2))))))
	  (else
	   (let ((lc? (and (interval-lower-bound-closed? i1)
			   (interval-lower-bound-closed? i2))))
	     (cond ((extended< i1u i2u)
		    (make-interval lc? i1l i1u (interval-upper-bound-closed? i1)))
		   ((extended< i2u i1u)
		    (make-interval lc? i1l i2u (interval-upper-bound-closed? i2)))
		   (else
		    (make-interval lc? i1l i1u
				   (and (interval-upper-bound-closed? i1)
					(interval-upper-bound-closed? i2))))))))))

(define (interval+ i1 i2)
  (make-interval
   (and (interval-lower-bound-closed? i1)
	(interval-lower-bound-closed? i2))
   (extended+ (interval-lower-bound i1)
	      (interval-lower-bound i2))
   (extended+ (interval-upper-bound i1)
	      (interval-upper-bound i2))
   (and (interval-upper-bound-closed? 11)
	(interval-upper-bound-closed? i2))))

(define (interval-negate int)
  (make-interval (interval-upper-bound-closed? int)
		 (extended-negate (interval-upper-bound int))
		 (extended-negate (interval-lower-bound int))
		 (interval-lower-bound-closed? int)))

(define (interval- i1 i2)
  (interval+ i1 (interval-negate i2)))

(define (interval* i1 i2)
  (let ((first
	 (cons (extended* (interval-lower-bound i1)
			  (interval-lower-bound i2))
	       (and (interval-lower-bound-closed? i1)
		    (interval-lower-bound-closed? i2)))))
    (let lp ((all
	      (list
	       (cons (extended* (interval-lower-bound i1)
				(interval-upper-bound i2))
		     (and (interval-lower-bound-closed? i1)
			  (interval-upper-bound-closed? i2)))

	       (cons (extended* (interval-upper-bound i1)
				(interval-lower-bound i2))
		     (and (interval-upper-bound-closed? i1)
			  (interval-lower-bound-closed? i2)))
	       (cons (extended* (interval-upper-bound i1)
				(interval-upper-bound i2))
		     (and (interval-upper-bound-closed? i1)
			  (interval-upper-bound-closed? i2)))))
	     (max first)
	     (min first))
      (cond ((null? all)
	     (make-interval (cdr min) (car min) (car max) (cdr max)))
	    ((extended< (caar all) (car min))
	     (lp (cdr all) max (car all)))
	    ((extended< (car max) (caar all))
	     (lp (cdr all) (car all) min))
	    (else
	     (lp (cdr all) max min))))))

(define (interval-invert int)
  (if (interval-spans-zero? int)
      (interval-invert-zero-span int)
      (make-interval (interval-upper-bound-closed? int)
		     (extended-invert (interval-upper-bound int))
		     (extended-invert (interval-lower-bound int))
		     (interval-lower-bound-closed? int))))

(define (interval/ i1 i2)
  (interval* i1 (interval-invert i2)))

(define (interval-spans-zero? int)
  (or (and (extended-negative? (interval-lower-bound int))
	   (extended-positive? (interval-upper-bound int)))
      (and (number? (interval-lower-bound int))
	   (zero? (interval-lower-bound int)))
      (and (number? (interval-upper-bound int))
	   (zero? (interval-upper-bound int)))))


(define (interval-invert-zero-span int)
  (error "I don't know how to invert this" int))

;;; A number is a closed interval with itself as both bounds

(define (number->interval x)
  (make-interval #t x x #t))

(define ((n1->interval proc) x y)
  (proc (number->interval x) y))

(define ((n2->interval proc) x y)
  (proc x (number->interval y)))

(defhandler '+ interval+ interval? interval?)
(defhandler '+ (n1->interval interval+) number? interval?)
(defhandler '+ (n2->interval interval+) interval? number?)

(defhandler '- interval- interval? interval?)
(defhandler '- (n1->interval interval-) number? interval?)
(defhandler '- (n2->interval interval-) interval? number?)

(defhandler '* interval* interval? interval?)
(defhandler '* (n1->interval interval*) number? interval?)
(defhandler '* (n2->interval interval*) interval? number?)

(defhandler '/ interval/ interval? interval?)
(defhandler '/ (n1->interval interval/) number? interval?)
(defhandler '/ (n2->interval interval/) interval? number?)

;;;; To be improved...
;;; Each subset is the finite union of intervals
;;; We represent a subset as a list of intervals.  
;;; In canonical form, there are no overlaps. 
;;; The intervals are ordered so the lowest bound is first.

(define (canonicalize-intervals list-of-intervals)
  (let lp ((sl
	    (sort list-of-intervals
		  (lambda (i1 i2)
		    (extended< (interval-lower-bound i1)
			       (interval-lower-bound i2))))))
    (cond ((null? sl) sl)		;no intervals
	  ((null? (cdr sl)) sl)		;one interval
	  ((or (extended< (interval-lower-bound i2) ; merge intervals?
			 (interval-upper-bound i1))
	       (and (extended= (interval-lower-bound i2)
			       (interval-upper-bound i1))
		    (or (interval-lower-bound-closed? i2)
			(interval-upper-bound-closed? i1))))
	   (lp (cons (make-interval
		      (interval-lower-bound-closed? i1)
		      (interval-lower-bound i1)
		      (interval-upper-bound i2)
		      (interval-upper-bound-closed? i2))
		     (cddr sl))))
	  (else (lp (cdr sl))))))
