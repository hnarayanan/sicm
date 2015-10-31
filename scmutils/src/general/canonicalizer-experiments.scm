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


#|
;;; Implementation in terms of strong lists

(define (make-new-expression-table)
  (vector '() '() '()))

(define (table-enders table) (ref table 0))

(define (add-table-ender! table obj)
  (vector-set! table 0 (cons obj (ref table 0))))

(define (object-in-table-enders object table)
  (let ((a (member object (table-enders table))))
    (if a (car a) a)))


(define (table-extenders table) (ref table 1))

(define (add-table-extender! table obj rest)
  (vector-set! table 1 (cons (cons obj rest) (ref table 1))))

(define (clear-table-extenders! table)
  (vector-set! table 1 '()))

(define (object-in-table-extenders object table)
  (let ((a (assoc object (table-extenders table))))
    (if a (car a) #f)))

(define extender-object car)

(define extender-stream cdr)


(define (table-subindex table) (ref table 2))

(define (add-table-subindex! table key value)
  (vector-set! table 2 (cons (cons key value) (ref table 2))))

(define (terminal-table? table)
  (null? (table-subindex table)))

(define (find-next-subtable! feature table)
  (let ((subindex (table-subindex table)))
    (let ((v (assv feature subindex)))
      (if v
	  (cdr v)
	  (let ((nt (make-new-expression-table)))
	    (add-table-subindex! table feature nt)
	    nt)))))
|#

#|
;;; Implementation in terms of weak lists and vectors

(define (make-new-expression-table)
  (vector '() '() '()))

(define (table-enders table) (ref table 0))

(define (add-table-ender! table obj)
  (vector-set! table 0 (weak-cons obj (ref table 0))))

(define (object-in-table-enders object table)
  (get-weak-member object (table-enders table)))


(define (table-extenders table) (ref table 1))

(define (add-table-extender! table obj rest)
  (vector-set! table 1 (cons (weak-cons obj rest) (ref table 1))))

(define (clear-table-extenders! table)
  (vector-set! table 1 '()))

(define (object-in-table-extenders object table)
  (weak-find object (table-extenders table)))

(define extender-object weak-car)

(define extender-stream weak-cdr)


(define (table-subindex table) (ref table 2))

(define (add-table-subindex! table key value)
  (vector-set! table 2 (cons (cons key value) (ref table 2))))

(define (terminal-table? table)
  (null? (table-subindex table)))

(define (find-next-subtable! feature table)
  (let ((subindex (table-subindex table)))
    (let ((v (assv feature subindex)))
      (if v
	  (cdr v)
	  (let ((nt (make-new-expression-table)))
	    (add-table-subindex! table feature nt)
	    nt)))))

(define (expression-canonicalizer-gc-daemon)
  (if *expression-tables*
      (begin
	(let lp ((expression-tables *expression-tables*))
	  (if (weak-pair? expression-tables)
	      (begin
		(let ((table (weak-car expression-tables)))
		  (if table (clean-expression-table table)))
		(lp (weak-cdr expression-tables)))))
	(set! *expression-tables*
	      (clean-weak-list *expression-tables*))))
  'done)

(define (clean-expression-table table)
  (vector-set! table 0 (clean-weak-list (ref table 0)))
  (vector-set! table 1 (clean-weak-alist (ref table 1)))
  (vector-set! table 2 (clean-subtable-alist (ref table 2)))
  (or (not (null? (ref table 0)))
      (not (null? (ref table 1)))
      (not (null? (ref table 2)))))

(define (clean-weak-list weak-list)
  (if (weak-pair? weak-list)
      (if (weak-pair/car? weak-list)
	  (weak-cons (weak-car weak-list)
		     (clean-weak-list (weak-cdr weak-list)))
	  (clean-weak-list (weak-cdr weak-list)))
      weak-list))

(define (clean-weak-alist weak-alist)
  (if (pair? weak-alist)
      (cond ((not (car weak-alist))
	     (clean-weak-alist (cdr weak-alist)))
	    ((weak-pair/car? (car weak-alist))
	     (cons (car weak-alist)
		   (clean-weak-alist (cdr weak-alist))))
	    (else (clean-weak-alist (cdr weak-alist))))
      weak-alist))

(define (clean-subtable-alist alist)
  (if (pair? alist)
      (if (clean-expression-table (cdar alist))
	  (cons (car alist)
		(clean-subtable-alist (cdr alist)))
	  (clean-subtable-alist (cdr alist)))
      alist))
|#
