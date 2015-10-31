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

;;; Edited by GJS 10Jan09

;;; Hairy polynomial rootfinder.  Not so good, but I tried (GJS)
(load "polyroot" scmutils-base-environment)

;;; The Polynomial Interpolator: generic code.
;;; export = (Lagrange-interpolation-function ys xs)
(with-case-preserved
    (lambda ()
      (load "interp-generic" generic-environment)))

;;; A compiled Polynomial Interpolator: fast numerical code.
;;; export = (lagrange-interpolation-function ys xs)
(load "interp" scmutils-base-environment)


;;;     Halfant stuff
;;; Elegant constructor of custom Lagrange interpolator procedures
(load "lagrange" generic-environment)

;;;     Actual polynomial codes (requires pcf)
;;; Shifts and scales polynomial domains, 
;;; makes interpolation polynomials, estimates errors.
(load "polyinterp" scmutils-base-environment)

;;; Legendre polynomials
(load "legendre" scmutils-base-environment)

;;; Hermite interpolators (splines)
(load "hermite" scmutils-base-environment)

;;; Chebyshev expansions, economization
(load "nchebpoly" scmutils-base-environment)

;;; Piecewise polynomial approximations; good function memoizers
(load "ppa" scmutils-base-environment)
