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

;;;; Weak list utilities

(declare (usual-integrations))

(define (get-weak-member obj weak-list)
  (if (null? weak-list)
      #f
      (let ((a (weak-car weak-list)))
	(if (equal? obj a)
	    a
	    (get-weak-member obj (weak-cdr weak-list))))))

(define (weak-find obj weak-alist)
  (if (null? weak-alist)
      #f
      (let ((pair (car weak-alist)))
	(if pair
	    (let ((a (weak-car pair)))
	      (if a
		  (if (equal? obj a)
		      a
		      (weak-find obj (cdr weak-alist)))
		  (begin (set-car! weak-alist #f)
			 #f)))
	    (weak-find obj (cdr weak-alist))))))

(define (weak-length weak-list)
  (if (weak-pair? weak-list)
      (fix:+ (weak-length (weak-cdr weak-list)) 1)
      0))

;;; Weak-alist searches.  These scan a weak alist for an object,
;;; returning the associated value if found.  They also clean up the
;;; alist by clobbering out value cells that have lost their key.
;;; These also work for strong alists, but strong alists are not
;;; modified.

(define (weak-finder same?)
  (define (the-finder obj weak-alist)
    (if (null? weak-alist)
	#f
	(let ((pair (car weak-alist)))
	  (cond ((weak-pair? pair)
		 (let ((a (weak-car pair)))
		   (if a		; assumes no key is #f
		       (if (same? obj a)
			   (weak-cdr pair)
			   (the-finder obj (cdr weak-alist)))
		       (begin (set-car! weak-alist #f)
			      #f))))
		((pair? pair)
		 (let ((a (car pair)))
		   (if (same? obj a)
		       (cdr pair)
		       (the-finder obj (cdr weak-alist)))))
		(else
		 (the-finder obj (cdr weak-alist)))))))
  the-finder)


(define weak-find-equal? (weak-finder equal?))


(define weak-find-eqv? (weak-finder eqv?))


(define weak-find-eq? (weak-finder eq?))


;;; The following clips out dead linkages that have been clobbered by
;;; a weak finder (above).  It also limits the size of the alist to
;;; the maximum size specified, by chopping off the tail.  max-size
;;; must be a positive integer larger than 1.

(define (purge-list list max-size)
  (let ((ans (delq! #f list)))
    (let loop ((ans ans) (i 1))
      (if (pair? ans)
	  (if (fix:= i max-size)
	      (set-cdr! ans '())
	      (loop (cdr ans) (fix:+ i 1)))))
    ans))

;;; Weak list cleanups

(define (clean-weak-list weak-list)
  (let clean-head ((this weak-list))
    (if (weak-pair? this)
	(let ((next (weak-cdr this)))
	  (if (weak-pair/car? this)
	      (begin
		(let clean-tail ((this next) (prev this))
		  (if (weak-pair? this)
		      (let ((next (weak-cdr this)))
			(if (weak-pair/car? this)
			    (clean-tail next this)
			    (begin
			      (weak-set-cdr! prev next)
			      (clean-tail next prev))))))
		this)
	      (clean-head next)))
	this)))

(define (clean-weak-alist weak-alist)
  (clean-alist weak-alist
	       (lambda (p)
		 (if (not (weak-pair? p))
		     (error:bad-range-argument weak-alist #f))
		 (weak-pair/car? p))))

(define (clean-subtable-alist alist)
  (clean-alist alist
	       (lambda (p)
		 (if (not (pair? p))
		     (error:bad-range-argument alist #f))
		 (clean-expression-table (cdr p)))))

(define (clean-alist alist clean-association)
  (let clean-head ((this alist))
    (if (pair? this)
	(let ((next (cdr this)))
	  (if (clean-association (car this))
	      (begin
		(let clean-tail ((this next) (prev this))
		  (if (pair? this)
		      (let ((next (cdr this)))
			(if (clean-association (car this))
			    (clean-tail next this)
			    (begin
			      (set-cdr! prev next)
			      (clean-tail next prev))))))
		this)
	      (clean-head next)))
	this)))
