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

;;;; Simplifier loader

;;; Canonical simplifiers are based on Rational Canonical Form,
;;;  which is, in turn, based on Polynomial Canonical Form.

;;;The following two files must be loaded in the given order.
(load "pcf"      scmutils-base-environment)
(load "rcf"      scmutils-base-environment)


;;; We need flattened polynomials to support rule-based simplifiers.

(load "fpf" scmutils-base-environment)


;;; Canonical simplifiers are glued together with SIMPLIFY.

(load "simplify" scmutils-base-environment)

(load "split-poly" scmutils-base-environment)

;;; Rule-based simplifiers

(load "symbenv" scmutils-base-environment)

(define rule-environment symbolic-environment)

(define (rule-memoize f) f)
;;; (define (rule-memoize f) (linear-memoize-1arg f))
;;; (define (rule-memoize f) (linear-memoize f))
;;; (define (rule-memoize f) (hash-memoize f))
;;; (define (rule-memoize f) (hash-memoize-1arg f))

(load "syntax" scmutils-base-environment)
(load "rule-syntax" scmutils-base-environment)
(load "matcher" scmutils-base-environment)
(load "rule-simplifier" scmutils-base-environment)
(load "rules" rule-environment)

(for-each (lambda (name)
	    #|
	    ;; This code exports by copying the binding:
	    (local-assignment scmutils-base-environment
			      name
			      (environment-lookup rule-environment
						  name))
	    |#
	    ;; This code shares the binding:
	    (environment-link-name scmutils-base-environment
				   rule-environment
				   name))
	  '(;; Useful simplifiers
	    ->poisson-form
	    new-simplify
	    easy-simplify
	    full-simplify

            logexp
            simsqrt
            sqrt-expand
            sqrt-contract
            specfun->logexp
            logexp->specfun
            log-expand
            log-contract
            exp-expand
            exp-contract
            canonicalize-partials

            trig->sincos
            sincos->trig
            triginv
            special-trig
            angular-parity
            expand-multiangle
            trig-sum-to-product
            trig-product-to-sum
            contract-expt-trig
            half-angle
            sin^2->cos^2
            cos^2->sin^2
            sincos-flush-ones
            flush-obvious-ones
            sincos-random
            sincos->exp1
            sincos->exp2
            exp->sincos
	    trigexpand
	    trigcontract

            complex-rules
            
            divide-numbers-through

	    ;; Boolean simplifier controls.
	    log-exp-simplify
	    sqrt-expt-simplify
	    sqrt-factor-simplify
            aggressive-atan-simplify
	    inverse-simplify
            sin-cos-simplify
            half-angle-simplify
	    ignore-zero-simplify
	    commute-partials-simplify
	    divide-numbers-through-simplify
	    trig-product-to-sum-simplify

	    log-exp-simplify?
	    sqrt-expt-simplify?
	    sqrt-factor-simplify?
            aggressive-atan-simplify?
	    inverse-simplify?
            sin-cos-simplify?
            half-angle-simplify?
	    commute-partials?
	    divide-numbers-through-simplify?
	    trig-product-to-sum-simplify?

	    ))
#;
(define (default-simplify exp)
  (new-simplify (expression exp)))

(load "default" scmutils-base-environment)

(load "sparse-load" scmutils-base-environment)
