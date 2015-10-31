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

(define (generic-environment-maker)
  (let ((e (extend-top-level-environment scmutils-base-environment)))
    (let ((d (lambda (name value)
	       (environment-define e name value))))
	(d '*environment* 'generic-environment)

	;; Unary operators from generic.scm

	(d 'type g:type)
	(d 'type-predicate g:type-predicate)
	(d 'arity g:arity)

	(d 'inexact? g:inexact?)

	(d 'zero-like g:zero-like)
	(d 'one-like g:one-like)
	(d 'identity-like g:identity-like)
	
	(d 'zero? g:zero?)
	(d 'one? g:one?)
	(d 'identity? g:identity?)

	(d 'negate g:negate)
	(d 'invert g:invert)

	(d 'square g:square)
	(d 'cube   g:cube)

	(d 'sqrt g:sqrt)

	(d 'exp g:exp)
	(d 'log g:log)

	(d 'exp2  g:exp2)
	(d 'exp10 g:exp10)
	(d 'log2  g:log2)
	(d 'log10 g:log10)

	(d 'sin g:sin)
	(d 'cos g:cos)
	(d 'tan g:tan)
	(d 'cot g:cot)
	(d 'sec g:sec)
	(d 'csc g:csc)

	(d 'asin g:asin)
	(d 'acos g:acos)

	(d 'sinh g:sinh)
	(d 'cosh g:cosh)
	(d 'tanh g:tanh)
	(d 'sech g:sech)
	(d 'csch g:csch)

	(d 'asinh g:asinh)
	(d 'acosh g:acosh)
	(d 'atanh g:atanh)

	(d 'abs g:abs)

	(d 'determinant g:determinant)
	(d 'trace g:trace)
        (d 'transpose g:transpose)
	(d 'dimension g:dimension)

        (d 'solve-linear g:solve-linear)

	(d 'derivative g:derivative)

	;; Binary (and nary) operators from generic.scm

	(d '= g:=)
	(d '< g:<)
	(d '<= g:<=)
	(d '> g:>)
	(d '>= g:>=)

	(d '+ g:+)
	(d '- g:-)
	(d '* g:*)
	(d '/ g:/)

	(d 'dot-product g:dot-product)
	(d 'cross-product g:cross-product)

	(d 'outer-product g:outer-product)

	(d 'expt g:expt)
	(d 'gcd g:gcd)


        ;; Complex operators from generic.scm

	(d 'make-rectangular g:make-rectangular)
	(d 'make-polar g:make-polar)

	(d 'real-part g:real-part)
	(d 'imag-part g:imag-part)
	(d 'magnitude g:magnitude)
	(d 'angle g:angle)

	(d 'conjugate g:conjugate)


	;; Wierd operators from generic.scm

	(d 'atan g:atan)

	(d 'partial-derivative g:partial-derivative)
	(d 'partial g:partial)

	(d 'apply g:apply)


	;; Compound operators from mathutil.scm

	(d 'arg-scale g:arg-scale)
	(d 'arg-shift g:arg-shift)

	(d 'sigma g:sigma)

        (d 'ref   g:ref)
	(d 'size  g:size)

	(d 'compose g:compose)
	)
    e))

(define generic-environment
  (generic-environment-maker))

(define generic-numerical-operators
  '(	
	zero-like
	one-like
	identity-like

	negate
	invert

	square
	cube

	sqrt

	exp
	log

	exp2
	exp10
	log2
	log10

	sin
	cos
	tan
	sec
	csc

	asin
	acos

	sinh
	cosh
	tanh
	sech
	csch

	abs

	+
	-
	*
	/

	expt
	gcd

	make-rectangular
	make-polar

	real-part
	imag-part
	magnitude
	angle

	conjugate

	atan))

#|
(let ((numerical-environment
       (extend-top-level-environment generic-environment)))
  (environment-define scmutils-base-environment
		      'numerical-environment
		      numerical-environment)
  (environment-define numerical-environment
		      '*environment*
		      'numerical-environment))
|#


(let ((numerical-environment
       (extend-top-level-environment scmutils-base-environment)))
  (environment-define scmutils-base-environment
		      'numerical-environment
		      numerical-environment)
  (environment-define numerical-environment
		      '*environment*
		      'numerical-environment))
