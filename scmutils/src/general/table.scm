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

;;;; Property Tables

(declare (usual-integrations))

;;; Properties are n-dimensional sparse tables implemented as 
;;; nests of association lists.

;;; For any given sequence of keys, there can be both a value
;;; and a subtable.  A table is a list of a value and some entries.
;;; An entry is a pair, whose CAR is a key and whose CDR is a
;;; the subtable for that key.

(define (make-table table-name assoc)
  (let ((local-table (list *no-value*)))

    (define (lookup keys)
      (define (loop keys table)
	(if (null? keys) (car table)
	    (let ((entry (assoc (car keys) (cdr table))))
	      (if entry
		  (loop (cdr keys) (cdr entry))
		  *no-value*))))
      (loop keys local-table))

    (define (smash! keys value)
      (define (loop keys table)
	(if (null? keys) (set-car! table value)
	    (let ((entry (assoc (car keys) (cdr table))))
	      (if entry
		  (loop (cdr keys) (cdr entry))
		  (set-cdr! table
			    (cons (cons (car keys)
					(make-subtable (cdr keys) value))
				  (cdr table)))))))
      (loop keys local-table)
      local-table)

    (define (make-subtable keys value)
      (if (null? keys) (list value)
	  (list *no-value*
		(cons (car keys)
		      (make-subtable (cdr keys) value)))))

    (define (accumulator! increment-procedure initial-value keys value)
      (define (loop keys table)
	(if (null? keys)
	    (if (eq? (car table) *no-value*)
		(set-car! table (increment-procedure value initial-value))
		(set-car! table (increment-procedure value (car table))))
	    (let ((entry (assoc (car keys) (cdr table))))
	      (if entry
		  (loop (cdr keys) (cdr entry))
		  (set-cdr! table
			    (cons (cons (car keys)
					(make-subtable (cdr keys)
						       (increment-procedure value
									    initial-value)))
				  (cdr table)))))))
      (loop keys local-table)
      local-table)

    (define (remove! keys) (smash! keys *no-value*))

    (vector table-name lookup smash! accumulator! remove!)))


(define *no-value* (list '*no-value*))

(define (no-value? value)
  (eq? value *no-value*))


(define (get table . keys)
  ((vector-ref table 1) keys))

(define ((getter table) . keys)
  ((vector-ref table 1) keys))


(define (put! table value . keys)
  ((vector-ref table 2) keys value)
  'done)

(define ((putter! table) value . keys)
  ((vector-ref table 2) keys value)
  'done)


(define (get-with-default table default . keys)
  (let ((v ((vector-ref table 1) keys)))
    (if (eq? v *no-value*)
	default
	v)))

(define ((getter-with-default table default) . keys)
  (let ((v ((vector-ref table 1) keys)))
    (if (eq? v *no-value*)
	default
	v)))


(define (get-with-check table . keys)
  (let ((v ((vector-ref table 1) keys)))
    (if (eq? v *no-value*)
	(error "can't find value in table"
	       (list table keys))
	v)))

(define ((getter-with-check table) . keys)
  (let ((v ((vector-ref table 1) keys)))
    (if (eq? v *no-value*)
	(error "can't find value in table"
	       (list table keys))
	v)))


(define (add-to-list! object table . keys)
  ((vector-ref table 3) cons '() keys object)
  'done)

(define (adjoin-to-list! object table . keys)
  ((vector-ref table 3) list-adjoin '() keys object)
  'done)

(define (store! object table . keys)
  ((vector-ref table 2) keys object)
  'done)

;;; Elementary table utilities implemented in ALISTs

(define (lookup key table)
  (let ((val (assq key table)))
    (if val
	(cadr val)
	(error "key not in table -- LOOKUP" key))))

(define (rlookup key table)
  (cond ((null? table) false)
	((null? (cdar table)) (rlookup key (cdr table)))
	((eq? key (cadar table)) (car table))
	(else (rlookup key (cdr table)))))

(define (rassq key table)
  (cond ((null? table) false)
	((eq? key (cdar table)) (car table))
	(else (rassq key (cdr table)))))

(define (rassoc key table)
  (cond ((null? table) false)
	((equal? key (cdar table)) (car table))
	(else (rassoc key (cdr table)))))

(define (disassoc key alist)
  (cond ((null? alist) '())
	((equal? key (caar alist))
	 (cdr alist))
	(else
	 (cons (car alist)
	       (disassoc key (cdr alist))))))


;;; Elementary table utility implemented as PLISTs

(define (default-lookup name default list)
  (let ((L (memq name list)))
    (if L (cadr L) default)))

(define (table-of is? keys values)
  (define (lookup key)
    (let next ((ks keys) (vs values))
      (cond ((null? ks)
             (error "Key not in table" key))
            ((is? key (car ks)) (car vs))
            (else (next (cdr ks) (cdr vs))))))
  lookup)
