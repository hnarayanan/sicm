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

;;;;      Moments of a Distribution

;;;   Probably written by Matthew Halfant.
;;;   Modified by GJS on 17 December 2012.

(declare (usual-integrations))

;;; First some specific moments of a vector

(define (v:mean v)
  (let ((n (vector-length v)))
    (assert (not (zero? n))
	    "Need some data -- v:mean")
    (let lp ((i 0) (sum 0))
      (if (fix:= i n)
	  (/ sum n)
	  (lp (fix:+ i 1)
	      (+ sum (vector-ref v i)))))))

;;; Watch out! The following program incorporates
;;;  numerical-analysis magic.  The value of SUM
;;;  may appear to be zero, but see equation 1.7 
;;;  in Chan, Golub, and LeVeque; TR#222, Yale 
;;;  University CS Dept. 1983.

;;; The default here is "Population variance", but if 
;;; SAMPLE? is not false we compute "Sample variance".
;;; My HP calculator computes the latter--GJS. 

(define (v:variance-helper v sample?)
  (define (square x) (* x x))
  (let ((n (vector-length v))
	(mean (v:mean v)))
    (assert (not (and (one? n) sample?))
	    "Need more data -- v:sample-variance")
    (let lp ((i 0) (sumsq 0) (sum 0))
      (if (fix:= i n)
	  (/ (- sumsq (/ (square sum) n))
	     (if (not sample?)
		 n
		 (fix:- n 1)))
	  (let ((y (- (vector-ref v i) mean)))
	    (lp (fix:+ i 1)
		(+ sumsq (square y))
		(+ sum y)))))))
    
(define (v:variance v)
  (v:variance-helper v #f))
    
(define (v:sample-variance v)
  (v:variance-helper v #t))

(define (v:standard-deviation v)
  (sqrt (v:variance-helper v #f)))

(define (v:sample-standard-deviation v)
  (sqrt (v:variance-helper v #t)))

(define (v:average-deviation v)
  (let ((n (vector-length v))
	(mean (v:mean v)))
    (let lp ((i 0) (sum 0))
      (if (fix:= i n)
	  (/ sum n)
	  (let ((y (abs (- (vector-ref v i) mean))))
	    (lp (fix:+ i 1)
		(+ sum y)))))))

;;; We can calculate them all at once

(define (v:moments-helper v sample? cont)
  ;; cont =  (lambda (mean var std skew kurt adev) ...)
  (define (square x) (* x x))
  (let ((n (vector-length v))
	(mean (v:mean v)))
    (assert (not (and (one? n) sample?))
	    "Need more data -- sample-variance")
    (let lp ((i 0) (sum 0) (sumsq 0) (sumcb 0) (sumqu 0) (asum 0))
      (if (fix:= i n)
	  (let* ((var (/ (- sumsq (/ (square sum) n))
			 (if (not sample?)
			     n
			     (fix:- n 1))))
		 (std (sqrt var))
		 (skew (/ sumcb (* n var std)))
		 (kurt (- (/ sumqu (* n (square var))) 3.0))
		 (adev (/ asum n)))
	    (cont mean var std skew kurt adev))
	  (let* ((y (- (vector-ref v i) mean))
		 (yy (* y y))
		 (yyy (* y yy))
		 (yyyy (* y yyy)))
	    (lp (fix:+ i 1)
		(+ sum y)
		(+ sumsq yy)
		(+ sumcb yyy)
		(+ sumqu yyyy)
		(+ asum (abs y))))))))

;;; For lists

(define (mean l)
  (v:moments-helper (list->vector l)
		    #f
		    (lambda (mean var std skew kurt adev)
		      mean)))

(define (variance l)
  (v:moments-helper (list->vector l)
		    #f
		    (lambda (mean var std skew kurt adev)
		      var)))

(define (standard-deviation l)
  (v:moments-helper (list->vector l)
		    #f
		    (lambda (mean var std skew kurt adev)
		      std)))

(define (sample-variance l)
  (v:moments-helper (list->vector l)
		    #t
		    (lambda (mean var std skew kurt adev)
		      var)))

(define (sample-standard-deviation l)
  (v:moments-helper (list->vector l)
		    #t
		    (lambda (mean var std skew kurt adev)
		      std)))

(define (skewness l)
  (v:moments-helper (list->vector l)
		    #f
		    (lambda (mean var std skew kurt adev)
		      skew)))

(define (kurtosis l)
  (v:moments-helper (list->vector l)
		    #f
		    (lambda (mean var std skew kurt adev)
		      kurt)))

(define (average-deviation l)
  (v:moments-helper (list->vector l)
		    #f
		    (lambda (mean var std skew kurt adev)
		      adev)))

;;; Streams of data have running moments

(define (running-mean decay stream)
  (let loop ((sum (head stream))
	     (count 1)
	     (stream (tail stream)))
    (cons-stream (/ sum count)
		 (loop (+ (head stream) (* decay sum))
		       (+ 1 (* decay count))
		       (tail stream)))))

