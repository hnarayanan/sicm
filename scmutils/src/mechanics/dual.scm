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

;;;; This is obsoelete.

(define ((dual-canonical? C) s)
  (let ((DC* (flip-indices ((D C) s))))
    ((- J*-func
	(compose (Phi DC*) 
		 J*-func
		 (Phi* DC*)))
     (typical-object s))))

#|
(print-expression
 ((dual-canonical? (F->CT p->r))
  (->H-state 't
	     (coordinate-tuple 'r 'phi)
	     (momentum-tuple 'p_r 'p_phi))))
(down 0 (down 0 0) (up 0 0))

;;; but not all transforms are

(define (a-non-canonical-transform Istate)
  (let ((t (time Istate))
        (theta (coordinate Istate))
	(p (momentum Istate)))
    (let ((x (* p (sin theta)))
	  (p_x (* p (cos theta))))
      (->H-state t x p_x))))

(print-expression
 ((dual-canonical? a-non-canonical-transform)
  (->H-state 't 'theta 'p)))
(down 0 (+ (* -1 p x11190) x11190) (+ (* p x11189) (* -1 x11189)))


(print-expression
 ((dual-canonical? (polar-canonical 'alpha))
  (->H-state 't 'a 'I)))
(down 0 0 0)
|#

#|
(define (Cmix H-state)
  (let ((t (time H-state))
	(q (coordinate H-state))
	(p (momentum H-state)))
    (->H-state t
	       (coordinate-tuple (ref q 0) (- (ref p 1)))
	       (momentum-tuple   (ref p 0) (ref q 1)))))

(define a-state (->H-state 't 
			   (coordinate-tuple 'x 'y)
			   (momentum-tuple 'p_x 'p_y)))
(print-expression
 ((dual-canonical? Cmix)
  a-state))
(down 0 (down 0 0) (up 0 0))

(define (Cmix2 H-state)
  (let ((t (time H-state))
	(q (coordinate H-state))
	(p (momentum H-state)))
    (->H-state t
	       (flip-outer-index p)
	       (- (flip-outer-index q)))))

(print-expression
 ((dual-canonical? Cmix2)
  a-state))
(down 0 (down 0 0) (up 0 0))
|#

#|
(define ((C m0 m1) state)
  (let ((x (coordinate state))
	(p (momentum state)))
    (let ((x0 (ref x 0))
	  (x1 (ref x 1))
	  (p0 (ref p 0))
	  (p1 (ref p 1)))
      (->H-state 
       (time state)
       (coordinate-tuple (/ (+ (* m0 x0) (* m1 x1)) (+ m0 m1))
			 (- x1 x0))
       (momentum-tuple (+ p0 p1)
		       (/ (- (* m0 p1) (* m1 p0))
			  (+ m0 m1)))))))

(define b-state
  (->H-state
   't
   (coordinate-tuple
    (coordinate-tuple 'x_1 'y_1)
    (coordinate-tuple 'x_2 'y_2))
   (momentum-tuple
    (momentum-tuple 'p_x_1 'p_y_1)
    (momentum-tuple 'p_x_2 'p_y_2))))

(print-expression
 ((dual-canonical? (C 'm1 'm2)) b-state))
(down 0 (down (down 0 0) (down 0 0)) (up (up 0 0) (up 0 0)))

|#
