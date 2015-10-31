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

;;;; Special Relativity -- Boosts

(define (make-4tuple ct space)
   (up ct (ref space 0) (ref space 1) (ref space 2)))

(define (4tuple->ct v)
   (ref v 0))

(define (4tuple->space v)
   (up (ref v 1) (ref v 2) (ref v 3)))


(define (proper-time-interval 4tuple)
   (sqrt (- (square (4tuple->ct 4tuple))
	   (square (4tuple->space 4tuple)))))

(define (proper-space-interval 4tuple)
   (sqrt (- (square (4tuple->space 4tuple))
	   (square (4tuple->ct 4tuple)))))


(define ((general-boost beta) xi-p)
  (let ((gamma (expt (- 1 (square beta)) -1/2)))
    (let ((factor (/ (- gamma 1) (square beta))))
      (let ((xi-p-time (4tuple->ct xi-p))
            (xi-p-space (4tuple->space xi-p)))
        (let ((beta-dot-xi-p (dot-product beta xi-p-space)))
          (make-4tuple
           (* gamma (+ xi-p-time beta-dot-xi-p))
           (+ (* gamma beta xi-p-time)
              xi-p-space
              (* factor beta beta-dot-xi-p))))))))

#|
(- (proper-space-interval
     ((general-boost (up 'vx 'vy 'vz))
      (make-4tuple 'ct (up 'x 'y 'z))))
   (proper-space-interval
     (make-4tuple 'ct (up 'x 'y 'z))))
#| 0 |#
|#

;;; It is inconvenient that the general boost as just defined does not
;;; work if $\bfbeta$ is zero.  An alternate way to specify a boost is
;;; through the magnitude of $v/c$ and a direction:

;;; this one works for zero v/c ...
;;; direction is a unit 3-vector, v/c is the speed, a number.

(define ((general-boost2 direction v/c) 4tuple-prime)
   (let ((delta-ct-prime (4tuple->ct 4tuple-prime))
	(delta-x-prime (4tuple->space 4tuple-prime)))
     (let ((betasq (square v/c)))
       (let ((bx (dot-product direction delta-x-prime))
	    (gamma (/ 1 (sqrt (- 1 betasq)))))
	(let ((alpha (- gamma 1)))
	  (let ((delta-ct
		 (* gamma (+ delta-ct-prime (* bx v/c))))
		(delta-x
		 (+ (* gamma v/c direction delta-ct-prime)
		    delta-x-prime
		    (* alpha direction bx))))
	    (make-4tuple delta-ct delta-x)))))))
#|
(let ((beta (up (/ 'v^x :c) (/ 'v^y :c) (/ 'v^z :c))))
   (- ((general-boost2 (up 1 0 0) 0) (up 'u0 'u1 'u2 'u3))
      (up 'u0 'u1 'u2 'u3)))
#|(up 0 0 0 0) |#
|#


;;;----------------------------------------------------------------
;;; extended rotations

;;; Boosts are linear functions of incremental vectors.
;;; To be parallel we take rotations to functions as well 
;;; rather than as multipliers.

(define ((extended-rotation R) xi-p)
  (make-4tuple
   (4tuple->ct xi-p)
   (R (4tuple->space xi-p))))


#|
;;; Check of the relation between boosts and rotations.

(let ((beta (up 'bx 'by 'bz))
      (xi (make-4tuple 'ct (up 'x 'y 'z)))
      (R (compose
          (rotate-x 'theta)
          (rotate-y 'phi)
          (rotate-z 'psi)))
      (R-inverse (compose
                  (rotate-z (- 'psi))
                  (rotate-y (- 'phi))
                  (rotate-x (- 'theta)))))
  (- ((general-boost beta) xi)
     ((compose (extended-rotation R-inverse)
               (general-boost (R beta))
               (extended-rotation R))
        xi)))
#|
(up 0 0 0 0)
|#
|#

