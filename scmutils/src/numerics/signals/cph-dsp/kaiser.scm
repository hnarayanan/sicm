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

;;; -*-Scheme-*-
;;;
;;; $Id: kaiser.scm,v 1.1 1994/07/20 19:42:33 cph Exp $
;;;
;;; Copyright (c) 1993 Massachusetts Institute of Technology
;;;
;;; This material was developed by the Scheme project at the
;;; Massachusetts Institute of Technology, Department of Electrical
;;; Engineering and Computer Science.  Permission to copy this
;;; software, to redistribute it, and to use it for any purpose is
;;; granted, subject to the following restrictions and understandings.
;;;
;;; 1. Any copy made of this software must include this copyright
;;; notice in full.
;;;
;;; 2. Users of this software agree to make their best efforts (a) to
;;; return to the MIT Scheme project any improvements or extensions
;;; that they make, so that these may be included in future releases;
;;; and (b) to inform MIT of noteworthy uses of this software.
;;;
;;; 3. All materials developed as a consequence of the use of this
;;; software shall duly acknowledge such use, in accordance with the
;;; usual standards of acknowledging credit in academic research.
;;;
;;; 4. MIT has made no warrantee or representation that the operation
;;; of this software will be error-free, and MIT is under no
;;; obligation to provide any services, by way of maintenance, update,
;;; or otherwise.
;;;
;;; 5. In conjunction with products arising from the use of this
;;; material, there shall be no use of the name of the Massachusetts
;;; Institute of Technology nor of any adaptation thereof in any
;;; advertising, promotional, or sales literature without prior
;;; written consent from MIT in each case.

;;;; Kaiser-Window FIR Filter Design Program

(declare (usual-integrations))

(define (kaiser-lowpass-filter omega-p omega-t delta)
  (make-kaiser-filter
   omega-t
   delta
   (ideal-lowpass-filter-function (+ omega-p (/ omega-t 2)))))

(define (ideal-lowpass-filter-function omega-c)
  (lambda (alpha)
    (lambda (n)
      (let ((n-alpha (- n alpha)))
	(if (= 0 n-alpha)
	    (/ omega-c pi)
	    (/ (sin (* omega-c n-alpha)) (* pi n-alpha)))))))

(define (kaiser-highpass-filter omega-p omega-t delta)
  (make-kaiser-filter
   omega-t
   delta
   (ideal-highpass-filter-function (- omega-p (/ omega-t 2)))))

(define (ideal-highpass-filter-function omega-c)
  (lambda (alpha)
    (lambda (n)
      (let ((n-alpha (- n alpha)))
	(if (= 0 n-alpha)
	    (- 1 (/ omega-c pi))
	    (let ((pna (* pi n-alpha)))
	      (/ (- (sin pna) (sin (* omega-c n-alpha)))
		 pna)))))))

(define (kaiser-bandpass-filter omega-c bw-p omega-t delta)
  (make-kaiser-filter omega-t
		      delta
		      (let ((bw-p/2 (/ bw-p 2)))
			(ideal-bandpass-filter-function (- omega-c bw-p/2)
							(+ omega-c bw-p/2)))))

(define (ideal-bandpass-filter-function omega-a omega-b)
  (lambda (alpha)
    (lambda (n)
      (let ((n-alpha (- n alpha)))
	(if (= 0 n-alpha)
	    (/ (- omega-b omega-a) pi)
	    (/ (- (sin (* omega-b n-alpha))
		  (sin (* omega-a n-alpha)))
	       (* pi n-alpha)))))))

(define (make-kaiser-filter omega-t delta filter)
  (let ((a (kaiser-a delta)))
    (let ((m (kaiser-m a omega-t)))
      (flo:make-initialized-vector (+ m 1)
	(let ((alpha (/ m 2)))
	  (let ((window (kaiser-window-function (kaiser-beta-from-a a) alpha))
		(filter (filter alpha)))
	    (lambda (n)
	      (* (window n) (filter n)))))))))

(define (make-kaiser-window beta m)
  (flo:make-initialized-vector (+ m 1) (kaiser-window-function beta (/ m 2))))

(define (normalize-kaiser-window window)
  (let ((n (flo:vector-length window)))
    (flo:make-initialized-vector n
      (let ((scale
	     (do ((i 0 (fix:+ i 1))
		  (sum 0. (flo:+ sum (flo:vector-ref window i))))
		 ((fix:= i n) sum))))
	(lambda (i)
	  (flo:/ (flo:vector-ref window i) scale))))))

(define (kaiser-a delta)
  (- (20log10 delta)))

(define (kaiser-beta-from-a a)
  (cond ((< a 21)
	 0)
	((< a 51)
	 (let ((a-21 (- a 21)))
	   (+ (* .5842 (expt a-21 .4))
	      (* .07886 a-21))))
	(else
	 (* .1102 (- a 8.7)))))

(define (kaiser-beta-from-asl asl)
  (cond ((< asl 13.26)
	 0)
	((< asl 60)
	 (let ((delta (- asl 13.26)))
	   (+ (* .76609 (expt delta .4))
	      (* .09834 delta))))
	(else
	 (* .12438 (- asl 6.3)))))

(define (kaiser-m a omega-t)
  (round->exact (/ (- a 8) (* 2.285 omega-t))))

(define (kaiser-l asl delta-ml)
  (round->exact (+ (/ (* (* 24 pi) (+ asl 12)) (* 155 delta-ml)) 1)))

(define (kaiser-window-function beta alpha)
  (let ((i0-beta (kaiser-i0 beta)))
    (lambda (n)
      (/ (kaiser-i0 (* beta
		       (sqrt (- 1.
				(let ((x (/ (- n alpha) alpha)))
				  (* x x))))))
	 i0-beta))))

(define (kaiser-i0 x)
  (let loop ((d 0) (ds 1) (s 1))
    (let* ((d (+ d 2))
	   (ds (* ds (/ (* x x) (* d d))))
	   (s (+ s ds)))
      (if (< ds (* s 2e-9))
	  s
	  (loop d ds s)))))