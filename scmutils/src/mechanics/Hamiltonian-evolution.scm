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
(define ((T-pend m l g ys) local)
  (let ((t (time local))
        (theta (coordinate local))
        (thetadot (velocity local)))
    (let ((ysdot (D ys)))
      (* 1/2 m
         (+ (square (* l thetadot))
            (square (ysdot t))
            (* 2 (ysdot t) l (sin theta) thetadot))))))

(define ((V-pend m l g ys) local)
  (let ((t (time local))
        (theta (coordinate local)))
    (* m g (- (ys t) (* l (cos theta))))))

(define L-pend (- T-pend V-pend))

(define ((periodic-drive amplitude frequency phase) t)
  (* amplitude (cos (+ (* frequency t) phase))))

(define (L-periodically-driven-pendulum m l g a omega)
  (let ((ys (periodic-drive a omega 0)))
    (L-pend m l g ys)))

(show-expression
 ((Lagrangian->Hamiltonian
    (L-periodically-driven-pendulum 'm 'l 'g 'a 'omega))
  (->H-state 't 'theta 'p_theta)))
(+
 (* -1/2
    (expt a 2)
    m
    (expt omega 2)
    (expt (cos theta) 2)
    (expt (sin (* omega t)) 2))
 (* a g m (cos (* omega t)))
 (/ (* a omega p_theta (sin theta) (sin (* omega t))) l)
 (* -1 g l m (cos theta))
 (/ (* 1/2 (expt p_theta 2)) (* (expt l 2) m)))

(define (H-pend-sysder m l g a omega)
  (Hamiltonian->state-derivative
    (Lagrangian->Hamiltonian
      (L-periodically-driven-pendulum m l g a omega))))

;;; for driven-pendulum-phase-space

(define window (frame -pi pi -10.0 10.0))

(start-gnuplot "pendulum-2x")

(define ((monitor-p-theta win) state)
  (let ((q ((principal-value pi) (coordinate state)))
	(p (momentum state)))
    (plot-point win q p)))
  
(let ((m 1.)		                ;m=1kg
      (l 1.)				;l=1m
      (g 9.8)				;g=9.8m/s$^2$
      (A 0.1)				;A=1/10 m
      (omega (* 2 (sqrt 9.8))))
  ((evolve H-pend-sysder m l g A omega)
   (up 0.0				;t$_0$=0
       1.0				;theta$_0$=1 radian
       0.0)				;thetadot$_0$=0 radians/s
   (monitor-p-theta window)
   0.01					;step between plotted points
   100.0				;final time
   1.0e-12))

(stop-gnuplot)

(graphics-clear window)

;;; for driven-pend-nuniq1 and 2
(let ((m 1.)		                ;m=1kg
      (l 1.)				;l=1m
      (g 9.8)				;g=9.8m/s$^2$
      (A 0.1)				;A=1/10 m
      (omega (* 2 (sqrt 9.8))))
  ((evolve H-pend-sysder m l g A omega)
   (up 0.0				;t$_0$=0
       1.0				;theta$_0$=1 radian
       0.0)				;thetadot$_0$=0 radians/s
   (monitor-p-theta window)
   0.01					;step between plotted points
   10.0					;final time
   1.0e-12))

(define ((monitor-pprime-theta mlA omega win) state)
  (let ((t (time state))
	(q ((principal-value pi) (coordinate state)))
	(p (momentum state)))
    (plot-point 
     win
     q 
     (+ p (* mlA omega (sin q) (sin (* omega t)))))))

(let ((m 1.)		                ;m=1kg
      (l 1.)				;l=1m
      (g 9.8)				;g=9.8m/s$^2$
      (A 0.1)				;A=1/10 m
      (omega (* 2 (sqrt 9.8))))
  ((evolve H-pend-sysder m l g A omega)
   (up 0.0				;t$_0$=0
       1.0				;theta$_0$=1 radian
       0.0)				;thetadot$_0$=0 radians/s
   (monitor-pprime-theta (* m l A) omega window)
   0.01					;step between plotted points
   10.0					;final time
   1.0e-12))

(graphics-close window)
|#
