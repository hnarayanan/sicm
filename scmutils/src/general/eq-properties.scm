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

;;;; Traditional LISP property lists
;;; extended to work on any kind of eq? data structure.

(declare (usual-integrations))

;;; Property lists are a way of creating data that looks like a record
;;; structure without commiting to the fields that will be used until
;;; run time.  The use of such flexible structures is frowned upon by
;;; most computer scientists, because it is hard to statically
;;; determine the bounds of the behavior of a program written using
;;; this stuff.  But it makes it easy to write programs that confuse
;;; such computer scientists.  I personally find it difficult to write
;;; without such crutches.  -- GJS


(define eq-properties (make-weak-eq-hash-table))

(define (eq-put! node property value)
  (let ((plist (hash-table/get eq-properties node '())))
    (let ((vcell (assq property plist)))
      (if vcell
	  (set-cdr! vcell value)
	  (hash-table/put! eq-properties node
			   (cons (cons property value)
				 plist)))))
  node)

(define (eq-get node property)
  (let ((plist (hash-table/get eq-properties node '())))
    (let ((vcell (assq property plist)))
      (if vcell
	  (cdr vcell)
	  #f))))

(define (eq-rem! node . properties)
  (for-each
   (lambda (property)
     (let ((plist
	    (hash-table/get eq-properties node '())))
       (let ((vcell (assq property plist)))
	 (if vcell
	     (hash-table/put! eq-properties node
			      (delq! vcell plist))))))
   properties)
  node)

(define (eq-adjoin! node property new)
  (eq-put! node property
	   (lset-adjoin eq?
			(or (eq-get node property) '())
			new))
  node)

(define (eq-plist node)
  (let ((plist (hash-table/get eq-properties node #f)))
    (if plist (cons node plist) #f)))

(define (eq-clone! source target)
  (hash-table/put! eq-properties target
    (hash-table/get eq-properties source '()))
  target)

(define (eq-label! node . plist)
  (let loop ((plist plist))
    (cond ((null? plist) node)
	  ((null? (cdr plist)) (error "Malformed plist"))
	  (else
	   (eq-put! node (car plist) (cadr plist))
	   (loop (cddr plist))))))

;;; Path names are built with properties.

(define (eq-path path)
  (define (lp node)
    (if node
	(if (pair? path)
	    (eq-get ((eq-path (cdr path)) node)
		    (car path))
	    node)
	#f))
  lp)
