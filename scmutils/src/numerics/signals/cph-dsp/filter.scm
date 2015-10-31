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
;;; $Id: filter.scm,v 1.2 1998/06/09 03:46:40 cph Exp $
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

;;;; FIR Filter Constructors

(declare (usual-integrations))

(define (flo:apply-filter-hn input-data hn)
  (flo:apply-filter input-data
		    (direct-form hn)
		    (fix:- (flo:vector-length hn) 1)))

(define (flo:apply-filter input-data filter overlap)
  (let ((length (flo:vector-length input-data)))
    (let ((buffer (flo:vector-cons (fix:+ length overlap)))
	  (result (flo:vector-cons length)))
      (flo:subvector-fill! buffer 0 overlap 0.)
      (do ((i 0 (fix:+ i 1)))
	  ((fix:= i length))
	(flo:vector-set! buffer (fix:+ overlap i)
			 (flo:vector-ref input-data i)))
      (do ((i 0 (fix:+ i 1)))
	  ((fix:= i length))
	(filter buffer (fix:+ overlap i) result i))
      result)))

(define (direct-form hn)
  (let ((m (- (flo:vector-length hn) 1)))
    (if (even? m)
	(direct-form-even hn m)
	(direct-form-odd hn m))))

(define (direct-form-even hn m)
  (let ((m/2 (quotient m 2)))
    (lambda (input input-index output output-index)
      (let ((k (fix:- input-index m)))
	(flo:vector-set! output
			 output-index
			 (flo:* (flo:vector-ref hn 0)
				(flo:+ (flo:vector-ref input input-index)
				       (flo:vector-ref input k))))
	(do ((i 1 (fix:+ i 1)))
	    ((fix:= i m/2)
	     (flo:vector-set!
	      output
	      output-index
	      (flo:+ (flo:vector-ref output output-index)
		     (flo:* (flo:vector-ref hn i)
			    (flo:vector-ref input (fix:- input-index i))))))
	  (flo:vector-set!
	   output
	   output-index
	   (flo:+ (flo:vector-ref output output-index)
		  (flo:* (flo:vector-ref hn i)
			 (flo:+ (flo:vector-ref input (fix:- input-index i))
				(flo:vector-ref input (fix:+ k i)))))))))))

(define (direct-form-odd hn m)
  (let ((m+1/2 (quotient (+ m 1) 2)))
    (lambda (input input-index output output-index)
      (let ((k (fix:- input-index m)))
	(flo:vector-set! output
			 output-index
			 (flo:* (flo:vector-ref hn 0)
				(flo:+ (flo:vector-ref input input-index)
				       (flo:vector-ref input k))))
	(do ((i 1 (fix:+ i 1)))
	    ((fix:= i m+1/2))
	  (flo:vector-set!
	   output
	   output-index
	   (flo:+ (flo:vector-ref output output-index)
		  (flo:* (flo:vector-ref hn i)
			 (flo:+ (flo:vector-ref input (fix:- input-index i))
				(flo:vector-ref input (fix:+ k i)))))))
	unspecific))))

(define (direct-form-file hn filename)
  (call-with-output-file (pathname-default-type filename "scm")
    (lambda (port)
      (write '(DECLARE (USUAL-INTEGRATIONS)) port)
      (newline port)
      (write (direct-form-expression hn) port))))

(define (direct-form-procedure hn)
  (scode-eval (compile-expression (direct-form-expression hn)
				  '((USUAL-INTEGRATIONS))
				  system-global-syntax-table)
	      system-global-environment))

(define (compile-expression s-expression declarations syntax-table)
  (compile-scode (syntax&integrate s-expression declarations syntax-table)))

(define (direct-form-expression hn)
  (let ((m (- (flo:vector-length hn) 1)))
    (if (even? m)
	(direct-form-even-expression hn m)
	(direct-form-odd-expression hn m))))

(define (direct-form-even-expression hn m)
  (let ((apply-hn
	 (lambda (j x)
	   (let ((h (flo:vector-ref hn j)))
	     (if (= h 0)
		 0.
		 `(FLO:* ,h ,x)))))
	(accumulate
	 (lambda (term accum)
	   (if (eqv? term 0.)
	       accum
	       `(FLO:+ ,term ,accum))))
	(input-ref
	 (lambda (offset)
	   `(FLO:VECTOR-REF INPUT
			    ,(if (= offset 0)
				 'INPUT-INDEX
				 `(FIX:- INPUT-INDEX ,offset))))))
    (let ((m/2 (quotient m 2)))
      `(LAMBDA (INPUT INPUT-INDEX OUTPUT OUTPUT-INDEX)
	 (FLO:VECTOR-SET!
	  OUTPUT
	  OUTPUT-INDEX
	  ,(let loop
	       ((j 1)
		(accum (apply-hn 0 `(FLO:+ ,(input-ref 0) ,(input-ref m)))))
	     (if (fix:= j m/2)
		 (accumulate (apply-hn j (input-ref j)) accum)
		 (loop (fix:+ j 1)
		       (accumulate
			(apply-hn j
				  `(FLO:+ ,(input-ref j) ,(input-ref (- m j))))
			accum)))))))))

(define (direct-form-odd-expression hn m)
  (let ((apply-hn
	 (lambda (j x)
	   (let ((h (flo:vector-ref hn j)))
	     (if (= h 0)
		 0.
		 `(FLO:* ,h ,x)))))
	(accumulate
	 (lambda (term accum)
	   (if (eqv? term 0.)
	       accum
	       `(FLO:+ ,term ,accum))))
	(input-ref
	 (lambda (offset)
	   `(FLO:VECTOR-REF INPUT
			    ,(if (= offset 0)
				 'INPUT-INDEX
				 `(FIX:- INPUT-INDEX ,offset))))))
    (let ((m+1/2 (quotient (+ m 1) 2)))
      `(LAMBDA (INPUT INPUT-INDEX OUTPUT OUTPUT-INDEX)
	 (FLO:VECTOR-SET!
	  OUTPUT
	  OUTPUT-INDEX
	  ,(let loop
	       ((j 1)
		(accum (apply-hn 0 `(FLO:+ ,(input-ref 0) ,(input-ref m)))))
	     (if (fix:= j m+1/2)
		 accum
		 (loop (fix:+ j 1)
		       (accumulate
			(apply-hn j
				  `(FLO:+ ,(input-ref j) ,(input-ref (- m j))))
			accum)))))))))