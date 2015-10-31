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

;;;; cons-unique (aka hashcons)
;;;  Apparently invented by Ershov (CACM 1, 8, August 1958, pp. 3--6)
;;;  Re-introduced by E.Goto in 1974.
;;;  Current version by GJS and Taylor Campbell 2010, improved by 
;;;  Taylor Campbell in 2011.

(declare (usual-integrations))

;;; Given two arguments cons-unique returns a pair.  If exactly the
;;; same two arguments were previously combined with cons-unique it
;;; returns the same pair it returned the first time.

#|
;;; Test for correctness.  This should never return.  
(let lp ((t (list 'a (cons 'b 3) 'c)))
  (let ((c1 (canonical-copy t)) (c2 (canonical-copy t)))
    (if (not (and (equal? t c1) (equal? t c2) (eq? c1 c2)))
	`(lose (,(hash c1) ,c1) (,(hash c2) ,c2))
	(lp (list 'a (cons 'b 3) 'c)))))
;Value: (lose (21 (a (b . 3) c)) (20 (a (b . 3) c)))
;;; This is what a loss looks like.
|#

(define cons-unique
  ;; I don't want to cons if unnecessary.
  (let ((the-test-pair (cons #f #f)))  
    (define (generator) (weak-cons the-test-pair #f))
    (define (hashcons x y)
      (set-car! the-test-pair x)
      (set-cdr! the-test-pair y)
      (let ((weak-pair
	     (hash-table/intern! the-cons-table
				 the-test-pair
				 generator)))
	(let ((the-canonical-pair (weak-car weak-pair)))
	  (cond ((eq? the-canonical-pair the-test-pair)
		 ;; test pair used, make a new one
		 (set! the-test-pair (cons #f #f)))
		(else
		 ;; clear test pair for next try.
		 (set-car! the-test-pair #f)
		 (set-cdr! the-test-pair #f)))
	  the-canonical-pair)))
    hashcons))

(define hash-cons cons-unique)

;;; Support for the hashcons system.

(define (pair-eqv? u v)
  (and (eqv? (car u) (car v))
       (eqv? (cdr u) (cdr v))))

(define (pair-eqv-hash-mod key modulus)
  (fix:remainder
   (fix:xor (eqv-hash-mod (car key) modulus)
	    (eqv-hash-mod (cdr key) modulus))
   modulus))

(define the-cons-table
  ((hash-table/constructor pair-eqv-hash-mod
			   pair-eqv?
			   #t
			   hash-table-entry-type:key-weak)))

;;; Given a list structure, to get a canonical copy equal to the given
;;; list structure.  Must canonicalize and share all substructure.
	    
(define (canonical-copy x)
  (define (recurse)
    (cons-unique (canonical-copy (car x))
		 (canonical-copy (cdr x))))
  (if (pair? x)
      (let ((seen		; already canonical?
	     (hash-table/get the-cons-table x #f)))
	(if seen
	    (or (weak-car seen)
		(and (not (weak-pair/car? seen))
		     (recurse)))
	    (recurse)))
      x))


(define (map-unique p lst)
  (if (pair? lst)
      (cons-unique (p (car lst))
		   (map-unique p (cdr lst)))
      lst))

#|
;;; For example...

(define foo
  '(define (canonical-copy x)
     (if (pair? x)
	 (let ((canonical-pair
		(hash-table/get the-cons-table x #f)))
	   (or canonical-pair
	       (let ((new
		      (cons (canonical-copy (car x))
			    (canonical-copy (cdr x)))))
		 (hash-table/put! the-cons-table new new)
		 new)))
	 x)))

(define bar
  '(define cons-unique
     ;; I don't want to cons if unnecessary.
     (let ((the-pair (cons #f #f)))  
       (define (hashcons x y)
	 (set-car! the-pair x)
	 (set-cdr! the-pair y)
	 (let ((canonical-pair
		(hash-table/get the-cons-table the-pair #f)))
	   (or canonical-pair
	       (let ((new the-pair))
		 (hash-table/put! the-cons-table new new)
		 (set! the-pair (cons #f #f))
		 new))))
       hashcons)))

(define cfoo
  (canonical-copy foo))
;Value: cfoo

(eq? cfoo (canonical-copy foo))
;Value: #t

(define cbar (canonical-copy bar))
;Value: cbar

(define baz (caddr (caddr (caddr (caddr (caddr cfoo))))))
;Value: baz

baz
;Value: (hash-table/put! the-cons-table new new)


(define mum (caddr (caddr (caddr (car (cddddr (caddr (caddr cbar))))))))
;Value: mum

mum
;Value: (hash-table/put! the-cons-table new new)

(eq? baz mum)
;Value: #t
|#
