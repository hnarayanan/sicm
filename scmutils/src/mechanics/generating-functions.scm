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
;;; This file is just a comment about generating functions.

;;; identity
(define (F2-identity t q p-prime)
  (+ (* (ref q 0) (ref p-prime 0))
     (* (ref q 1) (ref p-prime 1))))

;;; flip q & p
(define (F1-flip t q q-prime)
  (+ (* (ref q 0) (ref q-prime 0))
     (* (ref q 1) (ref q-prime 1))))

(define ((F2->C-inv F2) H-state)
  (let ((t (time H-state))
	(q (coordinate H-state))
	(p (momentum H-state)))
    (let ((p-func
	   (lambda (p-prime)
	     (((partial 1) F2) t q p-prime))))
      (let ((p-prime ((linear-inverse p-func) p)))
	(let ((q-prime (((partial 2) F2) t q p-prime)))
	  (->H-state t q-prime p-prime))))))
    
;;; x = f(x') is linear
(define ((linear-inverse f) p)
  (let ((b (f (zero-like p)))
	(a ((D f) (zero-like p))))
    (* (- p b)
       (s:inverse (compatible-shape p) a p))))

(define a-state
  (->H-state 't 
	     (coordinate-tuple 'x 'y)
	     (momentum-tuple 'p_x 'p_y)))

(print-expression ((F2->C-inv F2-identity) a-state))
(up t (up x y) (down p_x p_y))

(define b-state
  (->H-state
   't
   (coordinate-tuple
    (coordinate-tuple 'x_1 'y_1)
    (coordinate-tuple 'x_2 'y_2))
   (momentum-tuple
    (momentum-tuple 'p_x_1 'p_y_1)
    (momentum-tuple 'p_x_2 'p_y_2))))

(pe ((F2->C-inv F2-identity) b-state))
(up t
    (up (up x_1 y_1) (up x_2 y_2))
    (down (down p_x_1 p_y_1) (down p_x_2 p_y_2)))
 
(print-expression
 ((time-independent-canonical? (F2->C-inv F2-identity))
  a-state))
(up 0 (up 0 0) (down 0 0))

(pe (((partial 1) F2-identity) 
     't 
     (coordinate b-state)
     (typical-object (momentum b-state))))
(down (down x14885 x14886) (down x14887 x14888))

|#
