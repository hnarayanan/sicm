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

;;; (declare (usual-integrations))

;;; periodically driven pendulum

#|

ml^2 D^2 theta = - ml (g + D^2 ys(t)) sin theta
               = - ml (g - Aw^2 (cos wt)) sin theta
|#

(define ((Ldp ml^2 mlg mlAw^2 omega) state)
  (let ((t (state->t state))
	(theta (state->q state))
	(thetadot (state->qdot state)))
    (+ (* 1/2 ml^2 (square thetadot))
       (* (cos theta) (- mlg (* mlAw^2 (cos (* omega t))))))))

#|
(define (Hdp ml^2 mlg mlAw^2 omega)
  (Lagrangian->Hamiltonian (Ldp  ml^2 mlg mlAw^2 omega)))

|#

(define ((Hdp ml^2 mlg mlAw^2 omega) state)
  (let ((t (state->t state))
	(theta (state->q state))
	(ptheta (state->p state)))
    (- (* 1/2 (/ (square ptheta) ml^2))
       (* (cos theta) (- mlg (* mlAw^2 (cos (* omega t))))))))

(define (dp-sysder ml^2 mlg mlAw^2 omega)
  (Hamiltonian->state-derivative (Hdp ml^2 mlg mlAw^2 omega)))

(define (dp-map ml^2 mlg mlAw^2 omega)
  (let ((n 5) (pv (principal-value pi)))
    (let ((dt (/ 2pi (* n omega))))
      (lambda (theta ptheta cont fail)
	(let loop ((state (->H-state 0. theta ptheta)) (n n))
	  (if (fix:= n 0)
	      (let ((theta (pv (state->q state)))
		    (ptheta (state->p state)))
		(cont theta ptheta))
	      (loop ((state-advancer 
		      dp-sysder
		      ml^2 mlg mlAw^2 omega)
		     state 
		     dt
		     1.e-13) 
		    (fix:- n 1))))))))
#|
(define (animate-dp win A omega dt)
  (let ((g 9.8) (l 1) (m 1))
    (let ((ml^2 (* m l l))
	  (mlg (* m l g))
	  (mlAw^2 (* m l A omega omega)))
      (lambda (state0)
	(let loop ((state state0))
	  (let ((t (state->t state))
		(theta (state->q state)))
	    (let ((ys (* A (cos (* omega t)))))
	      (let ((x (* l (sin theta)))
		    (y (- ys (* l (cos theta)))))
		(graphics-clear win)
		(plot-line win 0. ys x y)
		(graphics-flush win)
		(loop ((state-advancer 
			dp-sysder
			ml^2 mlg mlAw^2 omega) 
		      state 
		      dt
		      1.e-13))))))))))

(define (animate-dp2 win A omega dt)
  (let ((g 0.0) (l 1) (m 1))
    (let ((ml^2 (* m l l))
	  (mlg (* m l g))
	  (mlAw^2 (* m l A omega omega)))
      (lambda (state0)
	(let loop ((state state0))
	  (let ((t (state->t state))
		(theta (state->q state)))
	    (let ((ys (* A (cos (* omega t)))))
	      (let ((x (* l (sin theta)))
		    (y (- ys (* l (cos theta)))))
		(graphics-clear win)
		(plot-line win 0. ys x y)
		(graphics-flush win)
		(loop ((state-advancer
			dp-sysder-compiled
			ml^2 mlg mlAw^2 omega)
				    state 
				    dt
				    1.e-13))))))))))

#|
(define win (frame -2. 2. -2. 2.))
(graphics-disable-buffering win)
((animate-dp win .1 (* 4.2 (sqrt 9.8)) .001)
 (->state 0. 2. 0.))
|#
|#

#|
(define win (frame -pi pi -10 10))
(graphics-clear win)
(graphics-close win)

(define (do-it win A )
  (graphics-clear win)
  (let ((g 9.8) (l 1) (m 1))
    (let ((omega (* 4.2 (sqrt (/ g l)))))
      (let ((ml^2 (* m l l))
	    (mlg (* m l g))
	    (mlAw^2 (* m l A omega omega)))
	(explore-map win 
		     (dp-map ml^2 mlg mlAw^2 omega)
		     100)))))

(define win (frame -pi pi -10 10))
(do-it win .1)
(define win (frame -pi pi -20 20))
(do-it win .001)
(graphics-close win)

(define win (frame -1.e-4 1.e-4 13.884 13.886))
(do-it win .001)

ic's for dpend-ic c program in demos for make blow up of small chaotic zone
0.001
13.148079707698765
-.0001 .0001 13.8995 13.9000
0.0 13.899787234042554118 1000000
|#

(define (dp-aa-map ml^2 mlg mlAw^2 omega)
  (let ((n 5) (pv (principal-value pi)))
    (let ((dt (/ 2pi (* n omega))))
      (lambda (phi J cont fail)
	(let ((state ((pendulum-global-aa-state-to-state ml^2 mlg)
		      (->H-state 0. phi J))))
	  (let loop ((state state) (n n))
	    (if (fix:= n 0)
		(let ((theta (pv (state->q state)))
		      (ptheta (state->p state)))
		  (let ((state ((pendulum-state-to-global-aa-state ml^2 mlg)
				(->H-state 0. theta ptheta))))
		    (cont (pv (state->q state)) (state->p state))))
		(loop (ode-advancer (dp-sysder-compiled ml^2 mlg mlAw^2 omega) 
				    state 
				    dt
				    1.e-13) 
		      (fix:- n 1)))))))))

#|

(define win2 (frame -pi pi 0. 8.))
(define (do-it win A )
  (graphics-clear win)
  (let ((g 9.8) (l 1) (m 1))
    (let ((omega (* 4.2 (sqrt (/ g l)))))
      (let ((ml^2 (* m l l))
	    (mlg (* m l g))
	    (mlAw^2 (* m l A omega omega)))
	(explore-map win2
		     (dp-aa-map ml^2 mlg mlAw^2 omega)
		     100)))))
(do-it win2 .001)
(graphics-clear win2)
(graphics-close win2)
|#


