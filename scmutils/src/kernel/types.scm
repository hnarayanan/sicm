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

;;;; This is needed to load particular types

(declare (usual-integrations))

(define (make-type type-tag abstract-type-tag
		   quantity-predicate concrete-predicate abstract-predicate)
  (list type-tag abstract-type-tag
	quantity-predicate concrete-predicate abstract-predicate))

(define (type-tag type)
  (car type))

(define (abstract-type-tag type)
  (cadr type))

(define (quantity-predicate type)
  (caddr type))

(define (concrete-predicate type)
  (cadddr type))

(define (abstract-predicate type)
  (car (cddddr type)))


(define-integrable number-type-tag '*number*)

(define-integrable with-units-type-tag '*with-units*)

(define-integrable unit-type-tag '*unit*)

(define-integrable vector-type-tag '*vector*)

(define-integrable abstract-vector-type-tag '*vector*)

(define quaternion-type-tag '*quaternion*)


;;; Up vectors are implemented as scheme vectors

(define-integrable up-type-tag '*vector*)

(define-integrable abstract-up-type-tag '*vector*)

(define-integrable down-type-tag '*down*)

(define-integrable abstract-down-type-tag '*abstract-down*)


(define-integrable matrix-type-tag '*matrix*)

(define-integrable abstract-matrix-type-tag '*abstract-matrix*)


(define-integrable function-type-tag '*function*)

(define-integrable abstract-function-type-tag '*function*)

(define-integrable differential-type-tag '*diff*)

(define operator-type-tag '*operator*)

(define-integrable series-type-tag '*series*)

(define type-tags
  (list number-type-tag
	unit-type-tag
	with-units-type-tag
	vector-type-tag
	quaternion-type-tag
	;;abstract-vector-type-tag
	;;up-type-tag
	;;abstract-up-type-tag
	down-type-tag
	abstract-down-type-tag
	matrix-type-tag
	abstract-matrix-type-tag
	function-type-tag
	differential-type-tag
	operator-type-tag
	series-type-tag))

(define compound-type-tags
  (list vector-type-tag
	;;up-type-tag
	quaternion-type-tag
	down-type-tag
	matrix-type-tag
	series-type-tag
	abstract-matrix-type-tag))

(define abstract-type-tags
  (list number-type-tag
	vector-type-tag
	;;abstract-vector-type-tag
	;;abstract-up-type-tag
	abstract-down-type-tag
	abstract-matrix-type-tag))

(define (abstract-quantity? x)
  (memq (g:type x) abstract-type-tags))


;;; NUMBER? is defined by Scheme system

(define (abstract-number? x)
  (or (literal-number? x)
      (symbol? x)))

(define (literal-number? x)
  (and (pair? x)
       (eq? (car x) number-type-tag)))

(define (literal-real? x)
  (and (literal-number? x)
       ((has-property? 'real) x)))

(define (numerical-quantity? x)
  (or (number? x)
      (abstract-number? x)
      (and (differential? x)
	   (numerical-quantity? (differential-of x)))
      (and (with-units? x)
	   (numerical-quantity? (u:value x)))))

(define (with-units? x)
  (and (pair? x)
       (eq? (car x) with-units-type-tag)))

(define (units? x)
  (or (eq? x '&unitless)
      (and (pair? x)
	   (eq? (car x) unit-type-tag))))


(define *number*
  (make-type '*number* '*number* numerical-quantity? number? abstract-number?))


(define (compound-type-tag? x)
  ;; Will need to add tensors, etc.
  (memq x compound-type-tags))

(define (not-compound? x)
  (not (or (vector? x)
	   (and (pair? x)
		(compound-type-tag? (car x))))))

(define (scalar? x)
  (not (or (vector? x)
	   (and (pair? x)
		(compound-type-tag? (car x)))
	   (function? x))))

;;; Scheme vectors are used to represent concrete vectors.
;;; VECTOR? is defined by Scheme system

(define (abstract-vector? x)
  (and (pair? x)
       (eq? (car x) vector-type-tag)))

(define (vector-quantity? v)
  (or (vector? v)
      (abstract-vector? v)
      (and (differential? v)
	   (vector-quantity? (differential-of v)))))


(define *vector*
  (make-type vector-type-tag
	     abstract-vector-type-tag
	     vector-quantity? vector? abstract-vector?))


(define (quaternion? v)
  (and (pair? v)
       (eq? (car v) quaternion-type-tag)))

(define (quaternion-quantity? v)
  (quaternion? v))


(define (up? x)
  ;;(and (pair? x) (eq? (car x) up-type-tag))
  (vector? x))

(define (abstract-up? x)
  (and (pair? x) (eq? (car x) abstract-up-type-tag)))

(define (up-quantity? v)
  (or (up? v)
      (abstract-up? v)
      (and (differential? v)
	   (up-quantity? (differential-of v)))))


(define *up*
  (make-type up-type-tag
	     abstract-up-type-tag
	     vector-quantity? up? abstract-up?))


(define (down? x)
  (and (pair? x)
       (eq? (car x) down-type-tag)))

(define (abstract-down? x)
  (and (pair? x) (eq? (car x) abstract-down-type-tag)))

(define (down-quantity? v)
  (or (down? v)
      (abstract-down? v)
      (and (differential? v)
	   (down-quantity? (differential-of v)))))


(define *down*
  (make-type
   down-type-tag
   abstract-down-type-tag
   down-quantity? down? abstract-down?))

(define (structure? x)
  (or (up? x) (down? x)))


(define (abstract-structure? x)
  (or (abstract-up? x) (abstract-down? x)))

(define (matrix? m)		
  (and (pair? m)
       (eq? (car m) matrix-type-tag)))

(define (matrix-quantity? m)
  (or (matrix? m)
      (abstract-matrix? m)
      (and (differential? m)
	   (matrix-quantity? (differential-of m)))))

(define (abstract-matrix? m)
  (and (pair? m)
       (eq? (car m) abstract-matrix-type-tag)))

(define *matrix*
  (make-type matrix-type-tag
	     abstract-matrix-type-tag
	     matrix-quantity? matrix? abstract-matrix?))

(define (square-matrix? matrix)
  (and (matrix? matrix)
       (fix:= (caadr matrix) (cdadr matrix))))

(define (square-abstract-matrix? matrix)
  (and (pair? matrix)
       (eq? (car matrix) abstract-matrix-type-tag)
       ((has-property? 'square) matrix)))


(define (operator? x)
  (and (apply-hook? x)
       (eq? (car (apply-hook-extra x))
	    operator-type-tag)))

(define (not-operator? x)
  (not (operator? x)))

(define (function-quantity? f)
  (procedure? f))			;apply hooks are procedures.

(define (function? f)
  (and (procedure? f)
       (not (operator? f))))

(define (cofunction? f)			;may be combined with a function
  (not (operator? f)))

(define (abstract-function? f)
  (and (typed-or-abstract-function? f)
       (f:expression f)))

(define (typed-function? f)
  (and (typed-or-abstract-function? f)
       (not (f:expression f))))

(define (typed-or-abstract-function? f)
  (and (apply-hook? f)
       (eq? (car (apply-hook-extra f))
	    function-type-tag)))

(define *function*
  (make-type function-type-tag
	     abstract-function-type-tag
	     function-quantity? function? abstract-function?))


(define (differential? obj)
  (and (pair? obj)
       (eq? (car obj) differential-type-tag)))

(define (not-differential? obj)
  (not (differential? obj)))


(define (series? s)
  (and (pair? s)
       (eq? (car s) series-type-tag)))

(define (not-series? s)
  (not (series? s)))


(define (not-differential-or-compound? x)
  (not (or (vector? x)
	   (and (pair? x)
		(or (compound-type-tag? (car x))
		    (eq? (car x) differential-type-tag))))))

(define (not-d-c-u? x)
  (not (or (eq? x '&unitless)
	   (vector? x)
	   (and (pair? x)
		(or (compound-type-tag? (car x))
		    (eq? (car x) differential-type-tag)
		    (eq? (car x) with-units-type-tag)
		    (eq? (car x) unit-type-tag))))))

