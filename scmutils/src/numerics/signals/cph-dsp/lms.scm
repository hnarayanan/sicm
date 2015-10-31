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
;;; $Id: lms.scm,v 1.1 1994/07/20 19:42:35 cph Exp $
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

;;;; Least-Mean-Square Adaptive Filter

(declare (usual-integrations))

(define (lms-adaptive m mu)
  (let ((h (flo:make-vector m 0.))
	(temp (flo:vector-cons 1)))
    (lambda (u un d dn e en y yn)
      (flo:vector-set! temp 0
		       (flo:* (flo:vector-ref u un)
			      (flo:vector-ref h 0)))
      (do ((i 1 (fix:+ i 1)))
	  ((fix:= i m))
	(flo:vector-set! temp 0
			 (flo:+ (flo:vector-ref temp 0)
				(flo:* (flo:vector-ref u (fix:- un i))
				       (flo:vector-ref h i)))))
      (if y (flo:vector-set! y yn (flo:vector-ref temp 0)))
      (flo:vector-set! temp 0
		       (flo:- (flo:vector-ref d dn)
			      (flo:vector-ref temp 0)))
      (if e (flo:vector-set! e en (flo:vector-ref temp 0)))
      (flo:vector-set! temp 0 (flo:* mu (flo:vector-ref temp 0)))
      (do ((i 0 (fix:+ i 1)))
	  ((fix:= i m))
	(flo:vector-set! h i
			 (flo:+ (flo:vector-ref h i)
				(flo:* (flo:vector-ref u (fix:- un i))
				       (flo:vector-ref temp 0)))))
      h)))

(define (lms-ale m mu delta)
  (let ((filter (lms-adaptive m mu)))
    (lambda (input-buffer input-start input-end output-buffer output-start)
      (let loop
	  ((u (fix:- input-start delta))
	   (d input-start)
	   (y output-start)
	   (h #f))
	(if (fix:= d input-end)
	    h
	    (loop (fix:+ u 1)
		  (fix:+ d 1)
		  (fix:+ y 1)
		  (filter input-buffer u
			  input-buffer d
			  #f #f
			  output-buffer y)))))))