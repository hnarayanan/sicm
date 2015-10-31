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

;;;; Symbolic environment for simplification

(declare (usual-integrations))

(define (symbolic-operator operator-symbol)
  (let ((v (hash-table/get symbolic-operator-table operator-symbol #f)))
    (if v
	v
	(error "Undefined symbolic operator" operator-symbol))))

(define (symbolic-environment-maker)
  (let ((e (extend-top-level-environment scmutils-base-environment)))
    (let ((d
	   (lambda (name value)
	     (environment-define e name value))))

      (d '*environment* 'symbolic-environment)

      ;; Unary operators from generic.scm
      #|
      (d 'type (symbolic-operator 'type))
      (d 'type-predicate (symbolic-operator 'type-predicate))
      (d 'arity (symbolic-operator 'arity))

      (d 'inexact? (symbolic-operator 'inexact?))

      (d 'zero-like (symbolic-operator 'zero-like))
      (d 'one-like (symbolic-operator 'one-like))
      (d 'identity-like (symbolic-operator 'identity-like))
      |#
      (d 'zero? (symbolic-operator 'zero?))
      (d 'one? (symbolic-operator 'one?))
      ;;	(d 'identity? (symbolic-operator 'identity?))

      (d 'negate (symbolic-operator 'negate))
      (d 'invert (symbolic-operator 'invert))

      (d 'square (symbolic-operator 'square))
      (d 'cube   (symbolic-operator 'cube))

      (d 'sqrt (symbolic-operator 'sqrt))

      (d 'exp (symbolic-operator 'exp))
      (d 'log (symbolic-operator 'log))
      #|
      (d 'exp2  (symbolic-operator 'exp2))
      (d 'exp10 (symbolic-operator 'exp10))
      (d 'log2  (symbolic-operator 'log2))
      (d 'log10 (symbolic-operator 'log10))
      |#
      (d 'sin (symbolic-operator 'sin))
      (d 'cos (symbolic-operator 'cos))
      (d 'tan (symbolic-operator 'tan))
      (d 'sec (symbolic-operator 'sec))
      (d 'csc (symbolic-operator 'csc))

      (d 'asin (symbolic-operator 'asin))
      (d 'acos (symbolic-operator 'acos))

      (d 'sinh (symbolic-operator 'sinh))
      (d 'cosh (symbolic-operator 'cosh))
      #|
      (d 'tanh (symbolic-operator 'tanh))
      (d 'sech (symbolic-operator 'sech))
      (d 'csch (symbolic-operator 'csch))
      |#
      (d 'abs (symbolic-operator 'abs))

      ;; (d 'derivative (symbolic-operator 'derivative))
      
      ;; Binary (and nary) operators from generic.scm

      (d 'expt (symbolic-operator 'expt))
      ;; (d 'gcd (symbolic-operator 'gcd))


      ;; Complex operators from generic.scm

      (d 'make-rectangular (symbolic-operator 'make-rectangular))
      (d 'make-polar (symbolic-operator 'make-polar))

      (d 'real-part (symbolic-operator 'real-part))
      (d 'imag-part (symbolic-operator 'imag-part))
      (d 'magnitude (symbolic-operator 'magnitude))
      (d 'angle (symbolic-operator 'angle))

      (d 'conjugate (symbolic-operator 'conjugate))

      (d 'atan (symbolic-operator 'atan))


      (d '= (symbolic-operator '=))

      (d '+ (symbolic-operator '+))

      (d '* (symbolic-operator '*))

      (d '- (symbolic-operator '-))

      (d '/ (symbolic-operator '/))

      )
    e))

(define symbolic-environment (symbolic-environment-maker))
