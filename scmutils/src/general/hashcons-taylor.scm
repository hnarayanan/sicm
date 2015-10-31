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

;;;;         hash-cons
;;;  Apparently invented by Ershov 
;;;    (see CACM 1, 8, August 1958, pp. 3--6)
;;;  Re-introduced by E.Goto in 1974.
;;;  Current version by Taylor Campbell 2010.

(declare (usual-integrations))

;;; Given two arguments hash-cons
;;; returns a pair.  If exactly the same
;;; two arguments were previously
;;; combined with hash-cons it returns
;;; the same pair it returned the first
;;; time.

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

(define the-hashed-conses
  ((weak-hash-table/constructor
    (lambda (pair modulus)
      (fix:remainder
       (fix:xor (eqv-hash-mod (car pair)
			      modulus)
                (eqv-hash-mod (cdr pair)
			      modulus))
       modulus))
    (lambda (p1 p2)
      (and (eqv? (car p1) (car p2))
           (eqv? (cdr p1) (cdr p2))))
    #t)))

(define (hash-cons a d)
  (let* ((pair (cons a d))
         (weak-pair
          (hash-table/intern!
	   the-hashed-conses pair
            (lambda () (weak-cons pair #f))))
         (pair* (weak-car weak-pair)))
    ;; Prevent the GC from garbage-collecting
    ;; PAIR before we compute PAIR*.
    (identity-procedure pair)
    pair*))

(define cons-unique hash-cons)

;;; Given a list structure, to get a
;;; canonical copy equal to the given
;;; list structure.  Must canonicalize
;;; and share all substructure.
	    
(define (canonical-copy x)
  (define (recurse)
    (cons-unique (canonical-copy (car x))
		 (canonical-copy (cdr x))))
  (if (pair? x)
      (let ((seen		; already canonical?
	     (hash-table/get the-hashed-conses x #f)))
	(if seen
	    (or (weak-car seen)
		(and (not (weak-pair/car? seen))
		     (recurse)))
	    (recurse)))
      x))

#|
(eq? '(a (b . 3) c) '(a (b . 3) c))
;Value: #f

(eq? (canonical-copy '(a (b . 3) c))
     (canonical-copy '(a (b . 3) c)))
;Value: #t
|#

;;; Useful stuff

(define (list-unique . lst)
  (canonical-copy lst))

(define (append-unique l1 l2)
  (if (null? l1)
      l2
      (hash-cons (car l1)
		 (append-unique (cdr l1)
				l2))))

(define (map-unique p lst)
  (if (pair? lst)
      (hash-cons (p (car lst))
		 (map-unique p (cdr lst)))
      lst))

