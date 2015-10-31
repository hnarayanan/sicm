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

;;;; Sets -- Implementation as ordered lists of symbols

(declare (usual-integrations))

;;; The arguments determine the type of the elements.

(define (make-sets-package set-equal-elements? set-less-elements?)

  (define the-empty-set '())
  (define set-empty? null?)

  (define set-first car)
  (define set-rest cdr)

  (define (set-singleton? s)
    (if (null? s) false (null? (cdr s))))

  (define (set-singleton x)
    (list x))

  (define (set-adjoin x set)
    (cond ((null? set) (list x))
	  ((set-equal-elements? x (car set)) set)
	  ((set-less-elements? x (car set)) (cons x set))
	  (else (cons (car set) (set-adjoin x (cdr set))))))

  (define (set-remove x set)
    (cond ((null? set) '())
	  ((set-equal-elements? x (car set)) (cdr set))
	  ((set-less-elements? x (car set)) set)
	  (else (cons (car set) (set-remove x (cdr set))))))

  (define (set-element? x set)
    (cond ((null? set) false)
	  ((set-equal-elements? x (car set)) true)
	  ((set-less-elements? x (car set)) false)
	  (else (set-element? x (cdr set)))))

  (define (set-intersection set1 set2)
    (cond ((null? set1) '())
	  ((null? set2) '())
	  ((set-equal-elements? (car set1) (car set2))
	   (cons (car set1) (set-intersection (cdr set1) (cdr set2))))
	  ((set-less-elements? (car set1) (car set2))
	   (set-intersection (cdr set1) set2))
	  (else (set-intersection set1 (cdr set2)))))

  (define (set-union set1 set2)
    (cond ((null? set1) set2)
	  ((null? set2) set1)
	  ((set-equal-elements? (car set1) (car set2))
	   (cons (car set1) (set-union (cdr set1) (cdr set2))))
	  ((set-less-elements? (car set1) (car set2))
	   (cons (car set1) (set-union (cdr set1) set2)))
	  (else (cons (car set2) (set-union set1 (cdr set2))))))

  (define (set-difference set1 set2)
    (cond ((null? set2) set1)
	  ((null? set1) '())
	  ((set-equal-elements? (car set1) (car set2))
	   (set-difference (cdr set1) (cdr set2)))
	  ((set-less-elements? (car set2) (car set1))
	   (set-difference set1 (cdr set2)))
	  (else (cons (car set1) (set-difference (cdr set1) set2)))))

  (define (set-subset? s1 s2)
    (cond ((null? s1) true)
	  ((null? s2) false)
	  ((set-equal-elements? (car s1) (car s2))
	   (set-subset? (cdr s1) (cdr s2)))
	  ((set-less-elements? (car s1) (car s2)) false)
	  (else (set-subset? s1 (cdr s2)))))

  (define (list->set lst)
    (define (remove-duplicates lst)
      (cond ((null? lst) lst)
	    ((null? (cdr lst)) lst)
	    ((set-equal-elements? (car lst) (cadr lst))
	     (remove-duplicates (cdr lst)))
	    (else
	     (cons (car lst)
		   (remove-duplicates (cdr lst))))))
    (remove-duplicates (sort lst set-less-elements?)))

  (define (set->list set) set)

  (vector the-empty-set
	  set-empty?
	  set-singleton
	  set-singleton?
	  set-adjoin
	  set-remove
	  set-element?
	  set-intersection
	  set-union
	  set-difference
	  set-subset?
	  list->set
	  set->list))

(define (empty-set set-type) (vector-ref set-type 0))
(define (empty-set? set-type) (vector-ref set-type 1))
(define (singleton-set set-type) (vector-ref set-type 2))
(define (singleton-set? set-type) (vector-ref set-type 3))
(define (adjoin-set set-type) (vector-ref set-type 4))
(define (remove-set set-type) (vector-ref set-type 5))
(define (element-set? set-type) (vector-ref set-type 6))
(define (intersect-sets set-type) (vector-ref set-type 7))
(define (union-sets set-type) (vector-ref set-type 8))
(define (difference-sets set-type) (vector-ref set-type 9))
(define (subset-sets? set-type) (vector-ref set-type 10))
(define (list->set set-type) (vector-ref set-type 11))
(define (set->list set-type) (vector-ref set-type 12))

(define symbols (make-sets-package eq? variable<?))
(define real-numbers (make-sets-package = <))

;;; There is no nice way to compare complex numbers, 
;;; but a kludge is necessary to impose order for 
;;; sets of them.

(define (<numbers z1 z2)
  (if (real? z1)
      (if (real? z2)
	  (< z1 z2)
	  #t)
      (if (real? z2)
	  #f
	  (cond ((< (real-part z1) (real-part z2))
		 #t)
		((= (real-part z1) (real-part z2))
		 (< (imag-part z1) (imag-part z2)))
		(else #f)))))

(define numbers (make-sets-package = <numbers))

#|
;;; For example

((set->list symbols)
 ((union-sets symbols)
  ((list->set symbols) '(a c e))
  ((list->set symbols) '(d e f))))
;Value: (a c d e f)
|#

;;;; Sets represented as unsorted lists of elements

;;; elements are tested with equal?

(define (list-adjoin item list)
  (if (member item list)
      list
      (cons item list)))

(define (list-union l1 l2)
  (cond ((null? l1) l2)
	((member (car l1) l2)
	 (list-union (cdr l1) l2))
	(else (cons (car l1)
		    (list-union (cdr l1) l2)))))

(define (list-intersection l1 l2)
  (cond ((null? l1) '())
	((member (car l1) l2)
	 (cons (car l1)
	       (list-intersection (cdr l1) l2)))
	(else (list-intersection (cdr l1) l2))))

(define (list-difference l1 l2)
  (cond ((null? l1) '())
	((member (car l1) l2)
	 (list-difference (cdr l1) l2))
	(else
	 (cons (car l1)
	       (list-difference (cdr l1) l2)))))

(define (duplications? lst)
  (cond ((null? lst) false)
	((member (car lst) (cdr lst)) true)
	(else (duplications? (cdr lst)))))

(define (remove-duplicates list)
  (if (null? list)
      '()
      (let ((rest (remove-duplicates (cdr list))))
        (if (member (car list) rest)
            rest
            (cons (car list) rest)))))

(define (subset? s1 s2)
  (if (null? s1)
      true
      (and (member (car s1) s2)
	   (subset? (cdr s1) s2))))

(define (same-set? s1 s2)
  (and (subset? s1 s2)
       (subset? s2 s1)))

;;;; eq-set utilities from Jinx

(define-integrable (eq-set/make-empty)
  '())

(define-integrable (eq-set/empty? set)
  (null? set))

(define-integrable (eq-set/member? element set)
  (memq element set))

(define-integrable (eq-set/adjoin element set)
  (if (eq-set/member? element set)
      set
      (cons element set)))

(define (eq-set/remove element set)
  (if (not (eq-set/member? element set))
      set
      (delq element set)))

;; Important: This will return set2 itself (rather than a copy) if the
;; union is set2.  Thus eq? can be used on the return value to
;; determine whether the set has grown.

(define (eq-set/union set1 set2)
  (define (loop set new-elements)
    (if (null? new-elements)
	set
	(loop (eq-set/adjoin (car new-elements) set)
	      (cdr new-elements))))

  ;; If set2 is smaller than set1, the union is guaranteed not to be set2.
  (if (< (length set2) (length set1))
      (loop set1 set2)
      (loop set2 set1)))

(define (eq-set/intersection set1 set2)
  (define (examine set1 set2)
    (let process ((set #| (reverse set1) |# set1)
		  (result (eq-set/make-empty)))
      (if (null? set)
	  result
	  (process (cdr set)
		   (if (eq-set/member? (car set) set2)
		       (cons (car set) result)
		       result)))))

  (if (< (length set2) (length set1))
      (examine set2 set1)
      (examine set1 set2)))

(define (eq-set/difference set1 set2)
  (if (null? set2)
      set1
      (let process ((set set1) (result (eq-set/make-empty)))
	(cond ((null? set)
	       result)
	      ((eq-set/member? (car set) set2)
	       (process (cdr set) result))
	      (else
	       (process (cdr set)
			(cons (car set) result)))))))

(define (eq-set/subset? set1 set2)
  (or (eq-set/empty? set1)
      (and (eq-set/member? (car set1) set2)
	   (eq-set/subset? (cdr set1) set2))))

(define (eq-set/equal? set1 set2)
  (or (eq? set1 set2)
      (and (eq-set/subset? set1 set2)
	   (eq-set/subset? set2 set1))))

;;;; multi-set utilities from Jinx

(define-integrable (multi-set/empty)
  '())

(define-integrable (multi-set/adjoin element set)
  (cons element set))

(define-integrable (multi-set/empty? set)
  (null? set))

(define-integrable (multi-set/first set)
  (car set))

(define-integrable (multi-set/rest set)
  (cdr set))

(define-integrable (multi-set/remove element set)
  (delq-once element set))

(define-integrable (multi-set/element? element set)
  (memq element set))

(define-integrable (multi-set/union set1 set2)
  (%reverse set1 set2))

(define (multi-set/intersection set1 set2)
  (define (process set1 set2 result)
    (cond ((multi-set/empty? set1)
	   result)
	  ((not (multi-set/element? (multi-set/first set1) set2))
	   (process (multi-set/rest set1) set2 result))
	  (else
	   (process (multi-set/rest set1)
		    (multi-set/remove (multi-set/first set1)
				      set2)
		    (multi-set/adjoin (multi-set/first set1)
				      result)))))

  (if (< (length set2) (length set1))
      (process set2 set1 (multi-set/empty))
      (process set1 set2 (multi-set/empty))))

(define (multi-set/difference set1 set2)
  (define (process set1 set2 result)
    (cond ((multi-set/empty? set1)
	   result)
	  ((multi-set/element? (multi-set/first set1) set2)
	   (process (multi-set/rest set1)
		    (multi-set/remove (multi-set/first set1) set2)
		    result))
	  (else
	   (process (multi-set/rest set1)
		    set2
		    (multi-set/adjoin (multi-set/first set1)
				      result)))))
  (process set1 set2 (multi-set/empty)))
