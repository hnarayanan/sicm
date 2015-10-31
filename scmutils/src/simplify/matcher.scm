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

;;;; Matcher based on match combinators, CPH/GJS style.
;;;     Idea is in Hewitt's PhD thesis (1969).

(declare (usual-integrations))

;;; There are match procedures that can be applied to data items.  A
;;; match procedure either accepts or rejects the data it is applied
;;; to.  Match procedures can be combined to apply to compound data
;;; items.

;;; A match procedure takes a list containing a data item, a
;;; dictionary, and a success continuation.  The dictionary
;;; accumulates the assignments of match variables to values found in
;;; the data.  The success continuation takes two arguments: the new
;;; dictionary, and the tail of the list resulting from matching its
;;; initial segment.  If a match procedure fails it returns #f.

;;; Primitive match procedures:

(define (match:predicate p?)
  (define (predicate-match data dictionary succeed)
    (and (pair? data)
	 (p? (car data))
	 (succeed dictionary (cdr data))))
  predicate-match)

(define (match:equal pattern-object #!optional equality?)
  (if (default-object? equality?) (set! equality? equal?))
  (match:predicate
   (lambda (data-object)
     (equality? pattern-object data-object))))


;;; A useful special case

(define (match:eqv pattern-object)
  (define (eqv-match data dictionary succeed)
    (and (pair? data)
	 (eqv? (car data) pattern-object)
	 (succeed dictionary (cdr data))))
  eqv-match)

(define (match:element variable #!optional restriction?)
  (if (default-object? restriction?) (set! restriction? (lambda (x) #t)))
  (define (element-match data dictionary succeed)
    (and (pair? data)
	 (restriction? (car data))
	 (let ((vcell (match:lookup variable dictionary)))
	   (if vcell
	       (and (datum=? (match:value vcell) (car data))
		    (succeed dictionary (cdr data)))
	       (succeed (match:bind variable (car data) dictionary)
			(cdr data))))))
  element-match)

(define (match:segment variable)
  (define (segment-match data dictionary succeed)
    (and (or (pair? data) (null? data))
	 (let ((vcell (match:lookup variable dictionary)))
	   (if vcell
	       (let ((v (match:value vcell)))
		 (let ((end (match:segment-end v)))
		   (let scan ((vptr (match:segment-beginning v))
			      (dptr data))
		     (cond ((eq? vptr end)
			    (succeed dictionary dptr))
			   ((not (pair? dptr)) #f)
			   ((datum=? (car vptr) (car dptr))
			    (scan (cdr vptr) (cdr dptr)))
			   (else #f)))))
	       (let try-seg ((end data))
		 (or (succeed (match:bind variable
					  (match:make-segment data end)
					  dictionary)
			      end)
		     (and (pair? end)
			  (try-seg (cdr end)))))))))
  segment-match)

(define (match:make-segment begin end)
  (vector begin end))

(define (match:segment-beginning value)
  (vector-ref value 0))

(define (match:segment-end value)
  (vector-ref value 1))


(define (match:list . match-combinators)
  (define (list-match data dictionary succeed)
    (and (pair? data)
	 (let lp ((items (car data))
		  (matchers match-combinators)
		  (dictionary dictionary))
	   (cond ((pair? matchers)
		  ((car matchers) items dictionary
		      (lambda (new-dictionary rest)
			(lp rest
			    (cdr matchers)
			    new-dictionary))))
		 ((pair? items) #f)
		 ((null? items)
		  (succeed dictionary (cdr data)))
		 (else #f)))))
  list-match)

(define (match:reverse-segment variable #!optional submatch)
  (if (default-object? submatch) (set! submatch match:equal))
  (define (reverse-segment-match data dictionary succeed)
    (if (list? data)
	(let ((vcell (match:lookup variable dictionary)))
	  (if vcell
	      (let ((v (match:value vcell)))
		 (let ((beg (match:segment-beginning v))
		       (end (match:segment-end v)))
		   (let ((revseg
			  (let revlp ((p beg) (rev '()))
			    (cond ((eq? p end) rev)
				  ((pair? p)
				   (revlp (cdr p) (cons (car p) rev)))
				  (else (error "Bad segment--reverse"))))))
		     (let scan ((vptr revseg) (dptr data))
		       (cond ((null? vptr)
			      (succeed dictionary
				       (list-tail data (length revseg))))
			     ((not (pair? dptr)) #f)
			     ((datum=? (car vptr) (car dptr))
			      (scan (cdr vptr) (cdr dptr)))
			     (else #f))))))
	      #f))
	#f))
  reverse-segment-match)

(define (datum=? datum1 datum2)
  (if (pair? datum1)
      (and (pair? datum2)
	   (datum=? (car datum1) (car datum2))
	   (datum=? (cdr datum1) (cdr datum2)))
      (eqv? datum1 datum2)))

;;; Support for the dictionary.

(define (match:bind variable data-object dictionary)
  (cons (cons variable data-object) dictionary))

(define (match:lookup variable dictionary)
  (assq variable dictionary))

(define (match:value vcell)
  (cdr vcell))

;;; Syntax of matching is determined here.

(define (match:->combinators pattern)
  (define (compile pattern)
    (cond ((match:element? pattern)
	   (if (match:restricted? pattern)
	       (match:element (match:variable-name pattern)
			      (match:restriction pattern))
	       (match:element (match:variable-name pattern))))
	  ((match:segment? pattern)
	   (match:segment (match:variable-name pattern)))
	  ((match:reverse-segment? pattern)
	   (match:reverse-segment (match:variable-name pattern)))
	  ((null? pattern) (match:eqv '()))
	  ((list? pattern)
	   (apply match:list (map compile pattern)))
	  (else (match:eqv pattern))))
  (compile pattern))

#|
;;; In rule-syntax.scm

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
|#

#|
((match:->combinators '(a ((? b) 2 3) 1 c))
 '((a (1 2 3) 1 c))
 '()
  (lambda (x y) `(succeed ,x ,y)))
;Value: (succeed ((b . 1)) ())

((match:->combinators `(a ((? b ,number?) 2 3) 1 c))
 '((a (1 2 3) 1 c))
 '()
  (lambda (x y) `(succeed ,x ,y)))
;Value: (succeed ((b . 1)) ())

((match:->combinators `(a ((? b ,symbol?) 2 3) 1 c))
 '((a (1 2 3) 1 c))
 '()
  (lambda (x y) `(succeed ,x ,y)))
;Value: #f

((match:->combinators '(a ((? b) 2 3) (? b) c))
 '((a (1 2 3) 2 c))
 '()
  (lambda (x y) `(succeed ,x ,y)))
;Value: #f

((match:->combinators '(a ((? b) 2 3) (? b) c))
 '((a (1 2 3) 1 c))
 '()
  (lambda (x y) `(succeed ,x ,y)))
;Value: (succeed ((b . 1)) ())

((match:->combinators '(a (?? x) (?? y) (?? x) c))
 '((a b b b b b b c))
 '()
 (lambda (x y)
   (pp `(succeed ,x ,y))
   #f))
(succeed ((y . #((b b b b b b c) (c))) (x . #((b b b b b b c) (b b b b b b c)))) ())
(succeed ((y . #((b b b b b c) (b c))) (x . #((b b b b b b c) (b b b b b c)))) ())
(succeed ((y . #((b b b b c) (b b c))) (x . #((b b b b b b c) (b b b b c)))) ())
(succeed ((y . #((b b b c) (b b b c))) (x . #((b b b b b b c) (b b b c)))) ())
;Value: #f

(define (palindrome? x)
  ((match:->combinators '((?? x) ($$ x)))
   (list x) '() (lambda (x y) (null? y))))
;Value: palindrome?

(palindrome? '(a b c c b a))
;Value: #t

(palindrome? '(a b c c a b))
;Value: #f
|#
