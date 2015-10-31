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

;;; The Lie transform is just the time-advance operator using the Lie
;;;  derivative (see Hamiltonian.scm).

(define (Lie-transform H delta-t)
  (make-operator 
    (exp (* delta-t (Lie-derivative H)))
    `(Lie-transform ,H ,delta-t)))
  

;;; The generalization of Lie-transform to include time dependence.

(define (flow-transform H delta-t)
  (make-operator 
   (exp (* delta-t (flow-derivative H)))
   `(flow-transform ,H ,delta-t)))

#|
;;; The general solution for a trajectory is:
;;;
;;;  q(t,q0,p0) = A(q0,p0) cos (sqrt(k/m)*t + phi(q0,p0))
;;;
;;;  where A(q0,p0) = sqrt(2/k)*sqrt(p0^2/(2*m) + (k/2)*q0^2)
;;;                 = sqrt((2/k)*E0)
;;;
;;;  and   phi(q0,p0) = - atan((1/sqrt(k*m))*(p0/q0))
;;;
;;; Thus, with initial conditions q0, p0
;;;   we should get q(t) = q0*cos(sqrt(k/m)*t)+p0*sin(sqrt(k/m)*t)
;;;
;;; We can expand this as a Lie series:

(define ((H-harmonic m k) state)
  (let ((q (coordinate state))
	(p (momentum state)))
    (+ (/ (square p) (* 2 m))
       (* 1/2 k (square q)))))

;;; This works, but it takes forever! -- hung in deriv, not in simplify!

(series:for-each print-expression
 (((Lie-transform (H-harmonic 'm 'k) 'dt)
   state->q)
  (->H-state 0 'x_0 'p_0))
 6)
x_0
(/ (* dt p_0) m)
(/ (* -1/2 (expt dt 2) k x_0) m)
(/ (* -1/6 (expt dt 3) k p_0) (expt m 2))
(/ (* 1/24 (expt dt 4) (expt k 2) x_0) (expt m 2))
(/ (* 1/120 (expt dt 5) (expt k 2) p_0) (expt m 3))
;Value: ...

(series:for-each print-expression
 (((Lie-transform (H-harmonic 'm 'k) 'dt)
   momentum)
  (->H-state 0 'x_0 'p_0))
 6)
p_0
(* -1 dt k x_0)
(/ (* -1/2 (expt dt 2) k p_0) m)
(/ (* 1/6 (expt dt 3) (expt k 2) x_0) m)
(/ (* 1/24 (expt dt 4) (expt k 2) p_0) (expt m 2))
(/ (* -1/120 (expt dt 5) (expt k 3) x_0) (expt m 2))
;Value: ...

(series:for-each print-expression
 (((Lie-transform (H-harmonic 'm 'k) 'dt)
   (H-harmonic 'm 'k))
  (->H-state 0 'x_0 'p_0))
 6)
(/ (+ (* 1/2 k m (expt x_0 2)) (* 1/2 (expt p_0 2))) m)
0
0
0
0
0
;Value: ...
|#

#|
(define ((H-central-polar m V) state)
  (let ((q (coordinate state))
        (p (momentum state)))
    (let ((r ((component 0) q))
          (phi ((component 1) q))
          (pr ((component 0) p))
          (pphi ((component 1) p)))
      (+ (/ (+ (square pr)
	       (square (/ pphi r)))
	    (* 2 m))
         (V r)))))

(series:for-each print-expression
 (((Lie-transform
    (H-central-polar 'm (literal-function 'U))
    'dt)
   state->q)
  (->H-state 0
	      (coordinate-tuple 'r_0 'phi_0)
	      (momentum-tuple 'p_r_0 'p_phi_0)))
 4)
(up r_0 phi_0)
(up (/ (* dt p_r_0) m) (/ (* dt p_phi_0) (* m (expt r_0 2))))
(up
 (+ (/ (* -1/2 (expt dt 2) ((D U) r_0)) m)
    (/ (* 1/2 (expt dt 2) (expt p_phi_0 2)) (* (expt m 2) (expt r_0 3))))
 (/ (* -1 (expt dt 2) p_phi_0 p_r_0) (* (expt m 2) (expt r_0 3))))
(up
 (+
  (/ (* -1/6 (expt dt 3) p_r_0 (((expt D 2) U) r_0)) (expt m 2))
  (/ (* -1/2 (expt dt 3) (expt p_phi_0 2) p_r_0) (* (expt m 3) (expt r_0 4))))
 (+ (/ (* 1/3 (expt dt 3) p_phi_0 ((D U) r_0)) (* (expt m 2) (expt r_0 3)))
    (/ (* -1/3 (expt dt 3) (expt p_phi_0 3)) (* (expt m 3) (expt r_0 6)))
    (/ (* (expt dt 3) p_phi_0 (expt p_r_0 2)) (* (expt m 3) (expt r_0 4)))))
;Value: ...
|#

#|
(define ((L-central-polar m V) local)
  (let ((q (coordinate local))
        (qdot (velocity local)))
    (let ((r (ref q 0))
          (phi (ref q 1))
          (rdot (ref qdot 0))
          (phidot (ref qdot 1)))
      (- (* 1/2 m
           (+ (square rdot)
              (square (* r phidot))) )
         (V r)))))


;;; I left this one that uses the Lagrangian because it appears to be 
;;; used for timings
(show-time
 (lambda ()
   (series:print
    (((Lie-transform
       (Lagrangian->Hamiltonian
	(L-central-polar 'm (lambda (r) (- (/ 'GM r)))))
       'dt)
      state->q)
     (->H-state 0
		 (coordinate-tuple 'r_0 'phi_0)
		 (momentum-tuple 'p_r_0 'p_phi_0)))
    4)))
#|
;;; 13 March 2012: I changed the system so that the original
;;; normalization is available, without causing the original gcd bug.
;;; This is done by adding an additional stage of simplification.
;;; This new stage is enabled by "(divide-numbers-through-simplify
;;; true/false)" The control is in simplify/rules.scm.  The default is
;;; now true, yielding the old representation.
(up r_0 phi_0)
(up (/ (* dt p_r_0) m) (/ (* dt p_phi_0) (* m (expt r_0 2))))
(up
 (+ (/ (* -1/2 GM (expt dt 2)) (* m (expt r_0 2)))
    (/ (* 1/2 (* (expt dt 2) (expt p_phi_0 2))) (* (expt m 2) (expt r_0 3))))
 (/ (* -1 (expt dt 2) p_r_0 p_phi_0) (* (expt m 2) (expt r_0 3))))
(up
 (+ (/ (* 1/3 (* GM (expt dt 3) p_r_0)) (* (expt m 2) (expt r_0 3)))
    (/ (* -1/2 (expt dt 3) p_r_0 (expt p_phi_0 2)) (* (expt m 3) (expt r_0 4))))
 (+ (/ (* (expt dt 3) p_phi_0 (expt p_r_0 2)) (* (expt m 3) (expt r_0 4)))
    (/ (* 1/3 (* GM (expt dt 3) p_phi_0)) (* (expt m 2) (expt r_0 5)))
    (/ (* -1/3 (expt dt 3) (expt p_phi_0 3)) (* (expt m 3) (expt r_0 6)))))
;process time: 1570 (1570 RUN + 0 GC); real time: 1573#| ... |#


;;; 30 Jan 2011: I changed the normalization of rational functions to
;;; favor integer coefficients.  This was to eliminate a bug in the
;;; construction of polynomial gcds.
;;; This is the new result.  It is algebraically equivalent to the old
;;; result.
(up r_0 phi_0)
(up (/ (* dt p_r_0) m) (/ (* dt p_phi_0) (* m (expt r_0 2))))
(up
 (+ (/ (* -1 GM (expt dt 2)) (* 2 m (expt r_0 2)))
    (/ (* (expt dt 2) (expt p_phi_0 2)) (* 2 (expt m 2) (expt r_0 3))))
 (/ (* -1 (expt dt 2) p_r_0 p_phi_0) (* (expt m 2) (expt r_0 3))))
(up
 (+ (/ (* GM (expt dt 3) p_r_0) (* 3 (expt m 2) (expt r_0 3)))
    (/ (* -1 (expt dt 3) (expt p_phi_0 2) p_r_0) (* 2 (expt m 3) (expt r_0 4))))
 (+ (/ (* (expt dt 3) (expt p_r_0 2) p_phi_0) (* (expt m 3) (expt r_0 4)))
    (/ (* GM (expt dt 3) p_phi_0) (* 3 (expt m 2) (expt r_0 5)))
    (/ (* -1 (expt dt 3) (expt p_phi_0 3)) (* 3 (expt m 3) (expt r_0 6)))))
;;; Binah 30 Jan 2011
;process time: 1600 (1600 RUN + 0 GC); real time: 1607#| ... |#
|#
#|
(up r_0 phi_0)
(up (/ (* dt p_r_0) m) (/ (* dt p_phi_0) (* m (expt r_0 2))))
(up
 (+ (/ (* -1/2 GM (expt dt 2)) (* m (expt r_0 2)))
    (/ (* 1/2 (expt dt 2) (expt p_phi_0 2)) (* (expt m 2) (expt r_0 3))))
 (/ (* -1 (expt dt 2) p_phi_0 p_r_0) (* (expt m 2) (expt r_0 3))))
(up
 (+
  (/ (* 1/3 GM (expt dt 3) p_r_0) (* (expt m 2) (expt r_0 3)))
  (/ (* -1/2 (expt dt 3) (expt p_phi_0 2) p_r_0) (* (expt m 3) (expt r_0 4))))
 (+ (/ (* (expt dt 3) p_phi_0 (expt p_r_0 2)) (* (expt m 3) (expt r_0 4)))
    (/ (* 1/3 GM (expt dt 3) p_phi_0) (* (expt m 2) (expt r_0 5)))
    (/ (* -1/3 (expt dt 3) (expt p_phi_0 3)) (* (expt m 3) (expt r_0 6)))))
|#
;;; Binah: 9 December 2009
;;;  With simple-derivative-internal memoized
;;;   process time: 2830 (2830 RUN + 0 GC); real time: 2846
;;;  Without memoization
;;;   process time: 1360 (1360 RUN + 0 GC); real time: 1377
;;;  But memoization makes some stuff feasible (see calculus/tensor.scm).
;;;
;;; Earlier
;;; MAHARAL 
;;;         process time: 3940 (3710 RUN + 230 GC); real time: 3956
;;; HOD     
;;;         process time: 14590 (13610 RUN + 980 GC); real time: 14588
;;; PLANET003 600MHz PIII
;;;         process time: 19610 (17560 RUN + 2050 GC); real time: 19610
;;; HEIFETZ xeon 400MHz 512K
;;;         process time: 27380 (24250 RUN + 3130 GC); real time: 27385
;;; GEVURAH 300 MHz
;;;         process time: 36070 (33800 RUN + 2270 GC); real time: 36072
;;; MAHARAL 
;;;         process time: 56390 (50970 RUN + 5420 GC); real time: 56386
;;; ACTION1 200MHz Pentium Pro
;;;         process time: 55260 (49570 RUN + 5690 GC); real time: 55257
;;; PPA     200MHz Pentium Pro
;;;         process time: 58840 (56500 RUN + 2340 GC); real time: 59165
;;; ZOHAR   33MHz 486
;;;         process time: 463610 (443630 RUN + 19980 GC); real time: 485593
|#
