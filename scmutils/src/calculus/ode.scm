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

#|
;;; Let (sigma t) be the state of a system at time t.  Let the
;;; (first-order) system of differential equations governing the
;;; evolution of this state be:

;;;  ((D sigma) t) = (R (sigma t))  
;;;     or  (D sigma) = (compose R sigma)

;;; i.e. R is a system derivative.

;;; Let F be any function of state, then a differential equation for
;;; the evolution of F, as it is dragged along the integral curve
;;; sigma is:

;;; (D (compose F sigma)) = (* (compose (D F) sigma) (D sigma))
;;;                       = (compose (* (D F) R) sigma)

;;; Let's call this operation Lie-D (the Lie derivative for
;;; coordinates).  We define
|#

(define (Lie-D R)
  (define (the-LD F) (* (D F) R))
  (make-operator the-LD `(Lie-D ,R)))

    