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
;;; $Id: flovec.scm,v 1.1 1994/07/20 19:42:32 cph Exp $
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

;;;; Floating-Point Vector Utilities

(declare (usual-integrations))

(define (flonum-vector->vector vector)
  (guarantee-flonum-vector vector 'FLONUM-VECTOR->VECTOR)
  (let ((length (flo:vector-length vector)))
    (let ((result (make-vector length)))
      (do ((i 0 (fix:+ i 1)))
	  ((fix:= i length))
	(vector-set! result i (flo:vector-ref vector i)))
      result)))

(define (flonum-vector->list vector)
  (guarantee-flonum-vector vector 'FLONUM-VECTOR->LIST)
  (let ((length (flo:vector-length vector)))
    (let ((result (make-list length)))
      (do ((i 0 (fix:+ i 1))
	   (l result (cdr l)))
	  ((fix:= i length))
	(set-car! l (flo:vector-ref vector i)))
      result)))

(define (vector->flonum-vector vector)
  (if (not (vector? vector))
      (error:wrong-type-argument vector "vector" 'VECTOR->FLONUM-VECTOR))
  (let ((length (vector-length vector)))
    (let ((result (flo:vector-cons length)))
      (do ((i 0 (fix:+ i 1)))
	  ((fix:= i length))
	(flo:vector-set! result i (->flonum (vector-ref vector i))))
      result)))

(define (list->flonum-vector list)
  (let ((length (length list)))
    (let ((result (flo:vector-cons length)))
      (do ((i 0 (fix:+ i 1))
	   (l list (cdr l)))
	  ((fix:= i length))
	(flo:vector-set! result i (->flonum (car l))))
      result)))

(define (flo:make-vector n #!optional value)
  (guarantee-nonnegative-fixnum n 'FLO:MAKE-VECTOR)
  (let ((result (flo:vector-cons n)))
    (if (not (or (default-object? value) (not value)))
	(let ((value (->flonum value)))
	  (do ((i 0 (fix:+ i 1)))
	      ((fix:= i n))
	    (flo:vector-set! result i value))))
    result))

(define (flo:make-initialized-vector length initialization)
  (guarantee-nonnegative-fixnum length 'FLO:MAKE-INITIALIZED-VECTOR)
  (let ((vector (flo:vector-cons length)))
    (do ((i 0 (fix:+ i 1)))
	((fix:= i length))
      (flo:vector-set! vector i (->flonum (initialization i))))
    vector))

(define (flo:subvector vector start end)
  (guarantee-flonum-subvector vector start end 'FLO:SUBVECTOR)
  (let ((result (flo:vector-cons (fix:- end start))))
    (do ((i 0 (fix:+ i 1))
	 (j start (fix:+ j 1)))
	((fix:= j end))
      (flo:vector-set! result i (flo:vector-ref vector j)))
    result))

(define (flo:vector-grow vector length #!optional value)
  (guarantee-flonum-vector vector 'FLO:VECTOR-GROW)
  (guarantee-nonnegative-fixnum length 'FLO:VECTOR-GROW)
  (let ((length* (flo:vector-length vector)))
    (if (not (fix:>= length length*))
	(error:bad-range-argument length 'FLO:VECTOR-GROW))
    (let ((result (flo:vector-cons length)))
      (do ((i 0 (fix:+ i 1)))
	  ((fix:= i length*))
	(flo:vector-set! result i (flo:vector-ref vector i)))
      (if (not (or (default-object? value) (not value)))
	  (let ((value (->flonum value)))
	    (do ((i length* (fix:+ i 1)))
		((fix:= i length))
	      (flo:vector-set! result i value))))
      result)))

(define (flo:subvector-move! source start-source end-source
			     target start-target)
  (guarantee-flonum-subvector source start-source end-source
			      'FLO:SUBVECTOR-MOVE!)
  (guarantee-flonum-vector target 'FLO:SUBVECTOR-MOVE!)
  (guarantee-nonnegative-fixnum start-target 'FLO:SUBVECTOR-MOVE!)
  (let ((end-target (fix:+ start-target (fix:- end-source start-source))))
    (guarantee-flonum-subvector-range target start-target end-target
				      'FLO:SUBVECTOR-MOVE!)
    (if (and (eq? source target) (fix:< start-source start-target))
	(let ((limit (fix:- start-source 1)))
	  (do ((scan-source (fix:- end-source 1) (fix:- scan-source 1))
	       (scan-target (fix:- end-target 1) (fix:- scan-target 1)))
	      ((fix:= scan-source limit) unspecific)
	    (flo:vector-set! target
			     scan-target
			     (flo:vector-ref source scan-source))))
	(do ((scan-source start-source (fix:+ scan-source 1))
	     (scan-target start-target (fix:+ scan-target 1)))
	    ((fix:= scan-source end-source) unspecific)
	  (flo:vector-set! target
			   scan-target
			   (flo:vector-ref source scan-source))))))

(define (flo:vector-map vector procedure)
  (guarantee-flonum-vector vector 'FLO:VECTOR-MAP)
  (let ((length (flo:vector-length vector)))
    (let ((result (flo:vector-cons length)))
      (do ((i 0 (fix:+ i 1)))
	  ((fix:= i length))
	(flo:vector-set! result i
			 (->flonum (procedure (flo:vector-ref vector i)))))
      result)))

(define (flo:vector-map! vector procedure)
  (guarantee-flonum-vector vector 'FLO:VECTOR-MAP!)
  (let ((length (flo:vector-length vector)))
    (do ((i 0 (fix:+ i 1)))
	((fix:= i length))
      (flo:vector-set! vector i
		       (->flonum (procedure (flo:vector-ref vector i))))))
  vector)

(define (flo:vector-for-each vector procedure)
  (guarantee-flonum-vector vector 'FLO:VECTOR-FOR-EACH)
  (let ((length (flo:vector-length vector)))
    (do ((i 0 (fix:+ i 1)))
	((fix:= i length) unspecific)
      (procedure (flo:vector-ref vector i)))))

(define (flo:subvector-for-each vector start end procedure)
  (guarantee-flonum-subvector vector start end 'FLO:SUBVECTOR-FOR-EACH)
  (do ((i start (fix:+ i 1)))
      ((fix:= i end) unspecific)
    (procedure (flo:vector-ref vector i))))

(define (flo:vector-fill! vector value)
  (guarantee-flonum-vector vector 'FLO:VECTOR-FILL!)
  (let ((length (flo:vector-length vector))
	(value (->flonum value)))
    (do ((i 0 (fix:+ i 1)))
	((fix:= i length) unspecific)
      (flo:vector-set! vector i value))))

(define (flo:subvector-fill! vector start end value)
  (guarantee-flonum-subvector vector start end 'FLO:SUBVECTOR-FILL!)
  (let ((value (->flonum value)))
    (do ((i start (fix:+ i 1)))
	((fix:= i end) unspecific)
      (flo:vector-set! vector i value))))

#|
(define-syntax ucode-primitive
  (lambda (name arity)
    (make-primitive-procedure name arity)))

(define-syntax ucode-type
  (lambda (name)
    (microcode-type name)))
|#

(define (flo:set-vector-length! vector n)
  (guarantee-flonum-vector vector 'FLO:SET-VECTOR-LENGTH!)
  (guarantee-nonnegative-fixnum n 'FLO:SET-VECTOR-LENGTH!)
  (if (not (fix:<= n (flo:vector-length vector)))
      (error:bad-range-argument n 'FLO:SET-VECTOR-LENGTH!))
  (let ((mask (set-interrupt-enables! interrupt-mask/none)))
    ((make-primitive-procedure 'primitive-object-set! 3)
     ;;(ucode-primitive primitive-object-set! 3)
     vector
     0
     ((make-primitive-procedure 'primitive-object-set-type 2)
      ;;(ucode-primitive primitive-object-set-type 2)
      (microcode-type 'manifest-nm-vector)
      ;;(ucode-type manifest-nm-vector)
      (fix:+ n n)))
    (set-interrupt-enables! mask)
    unspecific))

(define (->flonum x)
  (if (flo:flonum? x)
      x
      (let ((x (exact->inexact x)))
	(if (not (flo:flonum? x))
	    (error:datum-out-of-range x))
	x)))

(define-integrable (guarantee-flonum-vector object procedure)
  (if (not (flo:flonum? object))
      (error:wrong-type-argument object "flonum vector" procedure)))

(define-integrable (guarantee-nonnegative-fixnum object procedure)
  (if (not (and (fix:fixnum? object) (fix:>= object 0)))
      (error:wrong-type-argument object "non-negative fixnum" procedure)))

(define-integrable (guarantee-flonum-subvector v s e procedure)
  (guarantee-flonum-vector v procedure)
  (guarantee-nonnegative-fixnum s procedure)
  (guarantee-nonnegative-fixnum e procedure)
  (guarantee-flonum-subvector-range v s e procedure))

(define-integrable (guarantee-flonum-subvector-range v s e procedure)
  (if (not (fix:<= s e))
      (error:bad-range-argument s procedure))
  (if (not (fix:<= e (flo:vector-length v)))
      (error:bad-range-argument e procedure)))