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

;;; Performance enhancement (memoizing derivative)
;;; Helps, at 2x cost, but must be improved before on in distribution.

;;; Table utilities

(define (make-equal-table)
  ((weak-hash-table/constructor equal-hash-mod equal? #t)))

(define (equal-table/get table key default)
  (hash-table/get table key default))

(define (equal-table/put! table key value)
  (hash-table/put! table key value))

#|
(define *max-table-size* 3)

(define (make-equal-table)
  (list 0))

(define (equal-table/get table key default)
  (let ((vcell (assoc key (cdr table))))
    (if vcell (cdr vcell) default)))

(define (equal-table/put! table key value)
  (set-cdr! table
	    (cons (cons key value)
		  (cdr table)))
  (if (fix:= (car table) *max-table-size*)
      (except-last-pair! table)
      (set-car! table
		(fix:+ (car table) 1))))
|#

(define *memoizing-derivative* #f)

(define simple-derivative-internal
  (let ((*not-seen* (list '*not-seen*))
	(ftable (make-eq-hash-table)))
    (define (the-derivative f x)
      (if *memoizing-derivative*
	  (let ((f (1arg-real-function-canonicalizer f)))
	    (define (compute-derivative)
	      (let ((dx (make-differential-tag)))
		(set! dmiss (+ dmiss 1))
		(extract-dx-part dx (f (make-x+dx x dx)))))
	    (let ((dtable (hash-table/get ftable f *not-seen*)))
	      (set! dcount (+ dcount 1))
	      (if (eq? dtable *not-seen*)
		  (let ((dtable (make-equal-table))
			(df/dx (compute-derivative)))
		    (hash-table/put! ftable f dtable)
		    (equal-table/put! dtable x df/dx)
		    df/dx)
		  (let ((df/dx (equal-table/get dtable x *not-seen*)))
		    (if (eq? df/dx *not-seen*)
			(let ((df/dx (compute-derivative)))
			  (equal-table/put! dtable x df/dx)
			  df/dx)
			(begin
			  (set! dhit (+ dhit 1))
			  df/dx))))))
	  (let ((dx (make-differential-tag)))
	    (extract-dx-part dx (f (make-x+dx x dx))))))
    the-derivative))

(define dhit 0)
(define dmiss 0)
(define dcount 0)

(define (reset-derivative-statistics)
  (set! dhit 0)
  (set! dmiss 0)
  (set! dcount 0))

;;; A function canonicalizer

(define 1arg-real-function-canonicalizer
  (let ((*not-seen* (list '*not-seen*))
	(stable (make-eq-hash-table))
	(ftable (make-equal-table)))
    (let ((my-x (generate-uninterned-symbol 'x)))
      (define (function-canonicalizer f)
	(let ((seen (hash-table/get stable f *not-seen*)))
	  (if (eq? seen *not-seen*)
	      (let ((val (f my-x)))
		(set! fmiss (+ fmiss 1))
		(let ((cf (equal-table/get ftable val *not-seen*)))
		  (if (eq? cf *not-seen*)
		      (begin
			(equal-table/put! ftable val f)
			(hash-table/put! stable f (cons f val))
			f)
		      (begin
			(set! falthit (+ falthit 1))
			(hash-table/put! stable f (cons cf val))
			cf))))
	      (let ((cf (car seen)) (vf (cdr seen)))
		(set! fhithit (+ fhithit 1))
		cf))))
      function-canonicalizer)))

(define fmiss 0)
(define falthit 0)
(define fhithit 0)

(define (reset-function-statistics)
  (set! fmiss 0)
  (set! falthit 0)
  (set! fhithit 0))

(define (reset-derivative-memo-statistics)
  (reset-function-statistics)
  (reset-derivative-statistics))

(define (show-derivative-memo-statistics)
  (pp `(((fmiss ,fmiss) (falthit ,falthit) (fhithit ,fhithit))
	((dmiss ,dmiss) (dhit ,dhit) (dcount ,dcount)))))
