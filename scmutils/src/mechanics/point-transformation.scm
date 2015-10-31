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

;;; Makes a canonical point transformation from a 
;;;  time-invariant coordinate transformation T(q)

(define ((F->CH F) H-state)
  (up (time H-state)
      (F H-state)
      (solve-linear-right (momentum H-state)
                          (((partial 1) F) H-state))))

(define F->CT F->CH)

(define ((F->K F) H-state)
  (- (* (solve-linear-right (momentum H-state)
                            (((partial 1) F) H-state))
        (((partial 0) F) H-state))))

#|
;;; For display in book -- assumes flat coordinates

(define ((F->CH F) H-state)
  (->H-state (time H-state)
             (F H-state)
             (* (momentum H-state)
                (invert (((partial 1) F) H-state)))))

(define ((F->K F) H-state)
  (- (* (* (momentum H-state)
	   (invert (((partial 1) F) H-state)))
	(((partial 0) F) H-state))))

|#
#|

(define ((H-central m V) state)
  (let ((x (coordinate state))
	(p (momentum state)))
    (+ (/ (square p) (* 2 m))
       (V (sqrt (square x))))))

(show-expression
 ((compose (H-central 'm (literal-function 'V))
           (F->CT p->r))
  (->H-state 't
             (coordinate-tuple 'r 'phi)
             (momentum-tuple 'p_r 'p_phi))))
(+ (V r)
   (/ (* 1/2 (expt p_r 2)) m)
   (/ (* 1/2 (expt p_phi 2)) (* m (expt r 2))))

|#

