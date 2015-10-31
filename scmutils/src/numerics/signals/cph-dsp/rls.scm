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
;;; $Id: rls.scm,v 1.1 1994/07/20 19:42:41 cph Exp $
;;;
;;; Copyright (c) 1993-94 Massachusetts Institute of Technology
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

;;;; Recursive Least Squares Adaptive Filter

(declare (usual-integrations))

(define (rls-adaptive M delta decay)
  ;; H is the coefficient vector of the filter.
  ;; P is the inverse of the input autocorrelation matrix.
  ;; Only the lower half of P is stored, since it is symmetric.
  (let ((N (fix:* M (fix:- M 1))))
    (let ((h (flo:make-vector M 0.))
	  (P (flo:make-vector N 0.))
	  (Pu (flo:vector-cons M))
	  (k (flo:vector-cons M))
	  (decay (exact->inexact decay))
	  (ri (vector-cons M))
	  (temp (flo:vector-cons 1)))

      (define-syntax do-iter
	(lambda (var start end . body)
	  `(DO ((,var ,start (FIX:+ ,var 1)))
	       ((FIX:= ,var ,end))
	     ,@body)))

      (define-integrable (vref v i)
	(flo:vector-ref v i))

      (define-integrable (vset! v i x)
	(flo:vector-set! v i x))

      (define-integrable (v+* v i x y)
	(vset! v i
	       (flo:+ (vref v i)
		      (flo:* x y))))

      (define-integrable (v-* v i x y)
	(vset! v i
	       (flo:- (vref v i)
		      (flo:* x y))))

      (define-integrable (Pref r c)
	(vref P (fix:+ (vector-ref ri r) c)))

      (define-integrable (Pset! r c x)
	(vset! P (fix:+ (vector-ref ri r) c) x))

      (define-integrable (P+* r c x y)
	(v+* P (fix:+ (vector-ref ri r) c) x y))

      (define-integrable (tref)
	(vref temp 0))

      (define-integrable (tset! x)
	(vset! temp 0 x))

      (define-integrable (t+* x y)
	(v+* temp 0 x y))

      (define-integrable (t-* x y)
	(v-* temp 0 x y))

      (do ((i 0 (fix:+ i 1))
	   (j 0 (fix:+ j (fix:+ i 1))))
	  ((fix:= i M))
	(vector-set! ri i j))

      (tset! (/ 1. delta))
      (do-iter i 0 M (Pset! i i (tref)))

      (lambda (u un d dn e en y yn)

	(define-integrable (uref i)
	  (vref u (fix:- un i)))

	;; Apply exponential decay factor to P:
	(do-iter i 0 N (vset! P i (flo:/ (vref P i) decay)))

	;; Compute Pu:
	(tset! (flo:* (uref 0) (Pref 0 0)))
	(do-iter col 1 M (t+* (uref col) (Pref col 0)))
	(vset! Pu 0 (tref))
	(do-iter row 1 M
	  (tset! (flo:* (uref 0) (Pref row 0)))
	  (do-iter col 1 row (t+* (uref col) (Pref row col)))
	  (do-iter col row M (t+* (uref col) (Pref col row)))
	  (vset! Pu row (tref)))

	;; Compute k:
	(tset! 1.)
	(do-iter i 0 M (t+* (uref i) (vref Pu i)))
	(do-iter i 0 M (vset! k i (flo:/ (vref Pu i) (tref))))

	;; Update P:
	(do-iter i 0 M
	  (let ((k_i (vref k i))
		(i+1 (fix:+ i 1)))
	    (do-iter j 0 i+1 (P+* i j k_i (vref Pu j)))))

	;; Update h:
	(tset! (vref d dn))
	(do-iter i 0 M (t-* (uref i) (vref h i)))
	(do-iter i 0 M (v+* h i (vref k i) (tref)))

	;; Compute outputs:
	(tset! (flo:* (uref 0) (vref h 0)))
	(do-iter i 1 M (t+* (uref i) (vref h i)))
	(if y (vset! y yn (tref)))
	(if e (vset! e en (flo:- (vref d dn) (tref))))))))