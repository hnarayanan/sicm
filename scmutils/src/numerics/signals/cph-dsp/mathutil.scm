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
;;; $Id: mathutil.scm,v 1.2 1998/06/09 05:18:02 cph Exp $
;;;
;;; Copyright (c) 1993-98 Massachusetts Institute of Technology
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

;;;; Math Utilities for DSP

(declare (usual-integrations))

(define 20log10
  (let ((scale (/ 20 (log 10))))
    (lambda (x)
      (* scale (log x)))))

(define 10log10
  (let ((scale (/ 10 (log 10))))
    (lambda (x)
      (* scale (log x)))))

(define log10
  (let ((scale (/ 1 (log 10))))
    (lambda (x)
      (* scale (log x)))))

(define (passband-ripple->delta ripple)
  (- 1. (expt 10. (/ (- ripple) 40.))))

(define (passband-delta->ripple delta)
  (* -2. (20log10 (- 1. delta))))

(define (stopband-attenuation->delta attenuation)
  (expt 10. (/ (- attenuation) 20.)))

(define (stopband-delta->attenuation delta)
  (- (20log10 delta)))

(define (hz->radians f sampling-rate)
  (/ (* 2pi f) sampling-rate))

(define pi
  (* 4 (atan 1 1)))

(define 2pi
  (* 2 pi))

(define (square x)
  (* x x))

(define (vector-elementwise-product v1 v2)
  (if (not (vector? v1))
      (error:wrong-type-argument v1 "vector" 'VECTOR-ELEMENTWISE-PRODUCT))
  (if (not (vector? v2))
      (error:wrong-type-argument v2 "vector" 'VECTOR-ELEMENTWISE-PRODUCT))
  (let ((n1 (vector-length v1))
	(n2 (vector-length v1)))
    (let ((n (if (fix:< n1 n2) n1 n2)))
      (let ((result (make-vector n)))
	(do ((i 0 (fix:+ i 1)))
	    ((fix:= i n))
	  (vector-set! result i (* (vector-ref v1 i) (vector-ref v2 i))))
	result))))

(define (flo:vector-elementwise-product v1 v2)
  (if (not (flo:flonum? v1))
      (error:wrong-type-argument v1 "flonum vector"
				 'FLO:VECTOR-ELEMENTWISE-PRODUCT))
  (if (not (flo:flonum? v2))
      (error:wrong-type-argument v2 "flonum vector"
				 'FLO:VECTOR-ELEMENTWISE-PRODUCT))
  (let ((n1 (flo:vector-length v1))
	(n2 (flo:vector-length v1)))
    (let ((n (if (fix:< n1 n2) n1 n2)))
      (let ((result (flo:vector-cons n)))
	(do ((i 0 (fix:+ i 1)))
	    ((fix:= i n))
	  (flo:vector-set! result
			   i
			   (flo:* (flo:vector-ref v1 i)
				  (flo:vector-ref v2 i))))
	result))))

(define (flo:vector-elementwise-sum v1 v2)
  (if (not (flo:flonum? v1))
      (error:wrong-type-argument v1 "flonum vector"
				 'FLO:VECTOR-ELEMENTWISE-SUM))
  (if (not (flo:flonum? v2))
      (error:wrong-type-argument v2 "flonum vector"
				 'FLO:VECTOR-ELEMENTWISE-SUM))
  (let ((n1 (flo:vector-length v1))
	(n2 (flo:vector-length v1)))
    (let ((n (if (fix:< n1 n2) n1 n2)))
      (let ((result (flo:vector-cons n)))
	(do ((i 0 (fix:+ i 1)))
	    ((fix:= i n))
	  (flo:vector-set! result
			   i
			   (flo:+ (flo:vector-ref v1 i)
				  (flo:vector-ref v2 i))))
	result))))

(define (flo:vector-elementwise-difference v1 v2)
  (if (not (flo:flonum? v1))
      (error:wrong-type-argument v1 "flonum vector"
				 'FLO:VECTOR-ELEMENTWISE-DIFFERENCE))
  (if (not (flo:flonum? v2))
      (error:wrong-type-argument v2 "flonum vector"
				 'FLO:VECTOR-ELEMENTWISE-DIFFERENCE))
  (let ((n1 (flo:vector-length v1))
	(n2 (flo:vector-length v1)))
    (let ((n (if (fix:< n1 n2) n1 n2)))
      (let ((result (flo:vector-cons n)))
	(do ((i 0 (fix:+ i 1)))
	    ((fix:= i n))
	  (flo:vector-set! result
			   i
			   (flo:- (flo:vector-ref v1 i)
				  (flo:vector-ref v2 i))))
	result))))

(define (flo:oversample samples n)
  (let ((l (flo:vector-length samples)))
    (let ((v (flo:make-vector (fix:* l n))))
      (let loop ((t 0) (t* 0))
	(if (not (fix:= t l))
	    (let ((x (flo:vector-ref samples t))
		  (next-t* (fix:+ t* n)))
	      (do ((t* t* (fix:+ t* 1)))
		  ((fix:= t* next-t*))
		(flo:vector-set! v t* x))
	      (loop (fix:+ t 1) next-t*))))
      v)))

(define (flo:decimate samples n)
  (let ((l (flo:vector-length samples))
	(n/2 (fix:quotient n 2)))
    (let ((l* (fix:quotient l n)))
      (let ((v (flo:make-vector l*)))
	(do ((t n/2 (fix:+ t n))
	     (t* 0 (fix:+ t* 1)))
	    ((fix:= t* l*))
	  (flo:vector-set! v t* (flo:vector-ref samples t)))
	v))))