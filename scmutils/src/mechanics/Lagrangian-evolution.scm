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
;;; Driven pendulum example

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

(show-expression
 ((L-pend 'm 'l 'g (literal-function 'y_s))
  (->local 't 'theta 'thetadot)))
(+ (* 1/2 (expt l 2) m (expt thetadot 2))
   (* l m thetadot ((D y_s) t) (sin theta))
   (* g l m (cos theta))
   (* -1 g m (y_s t))
   (* 1/2 m (expt ((D y_s) t) 2)))

(show-expression
 (((Lagrange-equations
    (L-pend 'm 'l 'g (literal-function 'y_s)))
   (literal-function 'theta))
  't))
(+ (* g l m (sin (theta t)))
   (* (expt l 2) m (((expt D 2) theta) t))
   (* l m (((expt D 2) y_s) t) (sin (theta t))))
|#

#|
;;; Going back to the driven pendulum...

(define ((periodic-drive amplitude frequency phase) t)
  (* amplitude (cos (+ (* frequency t) phase))))

(define (L-periodically-driven-pendulum m l g a omega)
  (let ((ys (periodic-drive a omega 0)))
    (L-pend m l g ys)))

(show-expression
 (((Lagrange-equations
    (L-periodically-driven-pendulum 'm 'l 'g 'a 'omega))       
   (literal-function 'theta))
  't))
(+ (* -1 a l m (expt omega 2) (sin (theta t)) (cos (* omega t)))
   (* g l m (sin (theta t)))
   (* (expt l 2) m (((expt D 2) theta) t)))

(show-expression
 ((Lagrange-explicit (L-periodically-driven-pendulum 'm 'l 'g 'a 'omega))
  (->local 't 'theta 'thetadot)))
(+ (/ (* a (expt omega 2) (sin theta) (cos (* omega t))) l)
   (/ (* -1 g (sin theta)) l))

(show-expression
 ((Lagrangian->state-derivative
   (L-periodically-driven-pendulum 'm 'l 'g 'a 'omega))
  (up 't 'theta 'thetadot)))
(up
 1
 thetadot
 (+ (/ (* a (expt omega 2) (cos (* omega t)) (sin theta)) l)
    (/ (* -1 g (sin theta)) l)))

(define (pend-sysder m l g a omega)
  (Lagrangian->state-derivative
    (L-periodically-driven-pendulum m l g a omega)))
|#

#|
;;; Using these we can do graphing of trajectories:

(define plot-win (frame 0. 100. -pi pi))

(define ((monitor-theta win) state)
  (let ((theta ((principal-value pi) (coordinate state))))
    (plot-point win (time state) theta)))
  
((evolve pend-sysder 
         1.0                    ;m=1kg
         1.0                    ;l=1m
         9.8                    ;g=9.8m/s$^2$
         0.1                    ;a=1/10 m
         (* 2.0 (sqrt 9.8)) )
 (up 0.0             ;t$_0$=0
     1.22            ;theta$_0$=1 radian
     1e-10)          ;thetadot$_0$=0 radians/s
 (monitor-theta plot-win)
 0.01                     ;step between plotted points
 100.0                    ;final time
 1.0e-13)

;;;(set! *ode-integration-method* 'bulirsch-stoer)
;;;(set! *ode-integration-method* 'qcrk4)

((evolve pend-sysder 
         1.0                    ;m=1kg
         1.0                    ;l=1m
         9.8                    ;g=9.8m/s$^2$
         0.1                    ;a=1/10 m
         (* 2.0 (sqrt 9.8)) )
 (up 0.0             ;t$_0$=0
     1.22            ;theta$_0$=1 radian
     0.0)          ;thetadot$_0$=0 radians/s
 (monitor-theta plot-win)
 0.01                     ;step between plotted points
 100.0                    ;final time
 1.0e-13)

(show-time 
 (lambda ()
   ((evolve pend-sysder 
         1.0                    ;m=1kg
         1.0                    ;l=1m
         9.8                    ;g=9.8m/s$^2$
         0.1                    ;a=1/10 m
         (* 2.0 (sqrt 9.8)) )
 (up 0.0             ;t$_0$=0
     1.22            ;theta$_0$=1 radian
     0.0)          ;thetadot$_0$=0 radians/s
 (monitor-theta plot-win)
 0.01                     ;step between plotted points
 100.0                    ;final time
 1.0e-13)))

;;; qcrk4 -- process time: 47220 (41550 RUN + 5670 GC); real time: 47208
;;; bs    -- process time: 6440 (5680 RUN + 760 GC); real time: 6435

(graphics-clear plot-win)
(graphics-close plot-win)

;;; Picture stored in mechanics/driven-pend-theta.xwd
|#
