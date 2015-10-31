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

;;; THE PENDULUM

(declare (usual-integrations))

;;; definition H = p^2/(2alpha) - beta cos(theta)
;;; ASSUME alpha > 0 and beta > 0
;;; alpha = ml^2  beta = mgl for ordinary pendulum

(define ((Hpendulum alpha beta) state)
  (let ((theta (state->q state))
	(ptheta (state->p state)))
    (- (/ (square ptheta) (* 2 alpha))
       (* beta (cos theta)))))

(define (pendulum-sysder alpha beta)
  (Hamiltonian->state-derivative (Hpendulum alpha beta)))

(define pendulum-Hamiltonian Hpendulum)

;;;----------------------------------------------------------------
;;; oscillating case

(define (pendulum-oscillating-frequency alpha beta E)
  (let ((k (sqrt (/ (+ E beta) (* 2. beta))))
	(omega0 (sqrt (abs (/ beta alpha)))))
    (/ (* pi omega0) (* 2. (first-elliptic-integral k)))))

(define (pendulum-oscillating-angle alpha beta E)
  (let ((k (sqrt (/ (+ E beta) (* 2. beta))))
	(omega-0 (sqrt (/ beta alpha))))
    (lambda (time)
      (Jacobi-elliptic-functions 
       (* omega-0 time) 
       k
       (lambda (sn cn dn)
	 (* 2. (asin (* k sn))))))))

(define (pendulum-oscillating-angular-momentum alpha beta E)
  (let ((k (sqrt (/ (+ E beta) (* 2. beta))))
	(omega-0 (sqrt (/ beta alpha))))
    (lambda (time)
      (Jacobi-elliptic-functions 
       (* omega-0 time) 
       k
       (lambda (sn cn dn)
	 (* 2. alpha omega-0 k cn))))))

#|

freq = (/ (* pi omega0) (* 2 (first-elliptic-integral k)))
period = 4 K / omega0

omega0 period = 4 K the period of sn

|#

(define (pendulum-oscillating-action alpha beta E)
  (let ((k^2 (/ (+ beta E) (* 2. beta))))
    (if (= k^2 1.)
	(* (/ 8. pi) (sqrt (* beta alpha)))
	(elliptic-integrals 
	 (sqrt k^2)
	 (lambda (Kk Ek)
	   (* (/ 8. pi) 
	      (sqrt (* beta alpha))
	      (- Ek (* (- 1. k^2) Kk))))))))

(define ((pendulum-oscillating-action-to-E alpha beta) action)
  (let ((f (/ action (* (/ 8. pi) (sqrt (* beta alpha))))))
    (let ((k (pendulum-inverse-f f)))
      (* beta (- (* 2. (square k)) 1.)))))

;;; action angle -pi to pi
(define (pendulum-oscillating-phase alpha beta)
  (let ((omega-0 (sqrt (/ beta alpha))))
    (lambda (state)
      (let ((theta (state->q state))
	    (ptheta (state->p state)))
	(let ((E ((Hpendulum alpha beta) state)))
	  (if (> E (- beta))
	      (let ((k (sqrt (/ (+ E beta) (* 2. beta))))
		    (period (/ 2pi (pendulum-frequency alpha beta E))))
		(let* ((sin-phi (/ (sin (/ theta 2.)) k))
		       (dt0 (/ (elliptic-integral-F (asin sin-phi) k) omega-0)))
		  (let ((dt (if (< ptheta 0) (- (/ period 2.) dt0) dt0)))
		    ((principal-value pi) (* 2pi (/ dt period))))))
	      (error "at the fixed point the phase is undefined")))))))

;;; time from theta=0 to state
(define ((pendulum-oscillating-dt alpha beta) state)
  (let ((E ((Hpendulum alpha beta) state))
	(phase ((pendulum-oscillating-phase alpha beta) state)))
    (let ((period (/ 2pi (pendulum-frequency alpha beta E))))
      (* phase (/ period 2pi)))))

(define ((pendulum-oscillating-aa-state-to-state alpha beta) aa-state)
  (let ((angle (state->q aa-state))
	(action (state->p aa-state)))
    (let* ((E ((pendulum-oscillating-action-to-E alpha beta) action))
	   (period (/ 2pi (pendulum-frequency alpha beta E))))
      (let ((dt (* (/ period 2pi) angle)))
	(->H-state (state->t aa-state)
		   ((pendulum-oscillating-angle alpha beta E) dt)
		   ((pendulum-oscillating-angular-momentum alpha beta E) dt))))))
	      
#|

(define ((pendulum-oscillating-action-to-E alpha beta) action)
  (bisect (lambda (E) (- (pendulum-oscillating-action alpha beta E) action))
	  (- beta)
	  beta
	  (* *action-to-E-bugger-factor* *machine-epsilon*)))

(define *action-to-E-bugger-factor* 1000.)

|#

(define ((pendulum-oscillating-state-to-aa-state alpha beta) state)
  (let ((E ((Hpendulum alpha beta) state)))
    (let ((action ((pendulum-oscillating-action alpha beta E) state))
	  (angle ((pendulum-oscillating-phase alpha beta) state)))
      (->H-state (state->t state) angle action))))


;;;----------------------------------------------------------------
;;; ciculating case

(define (pendulum-circulating-frequency alpha beta E)
  (let ((k (sqrt (/ (* 2. beta) (+ E beta))))
	    (omegaR (sqrt (abs (/ (+ E beta) (* 2. alpha))))))
	(/ (* pi omegaR) (first-elliptic-integral k))))

(define (pendulum-circulating-angle alpha beta E)
  (let ((k (sqrt (/ (* 2. beta) (+ E beta))))
	(omega-R (sqrt (abs (/ (+ E beta) (* 2. alpha)))))
	(period (/ 2pi (pendulum-frequency alpha beta E))))
    (lambda (time)
      (Jacobi-elliptic-functions 
       (* omega-R ((principal-range period) time))
       k
       (lambda (sn cn dn)
	 (* 2. (asin sn)))))))

(define (pendulum-circulating-angular-momentum alpha beta E)
  (let ((k (sqrt (/ (* 2. beta) (+ E beta))))
	(omega-R (sqrt (abs (/ (+ E beta) (* 2. alpha)))))
	(period (/ 2pi (pendulum-frequency alpha beta E))))
    (lambda (time)
      (Jacobi-elliptic-functions 
       (* omega-R ((principal-range period) time))
       k
       (lambda (sn cn dn)
	 (* 2. alpha omega-R dn))))))

#|
;;; Defined in kernel/numeric.scm

(define ((principal-range period) time)
  (let ((t (- time (* period (floor (/ time period))))))
    (if (< t (/ period 2.))
	t
	(- t period))))
|#

#|

omega =	(/ (* pi omegaR) (first-elliptic-integral k)))))
period = 2pi / omega = 2 K / omegaR
so period*omegaR = 2 K but the period of sn is 4 K
so if period*omegaR is in the range 2K to 4K the
program would not work without the principal-range call

|#

(define (pendulum-circulating-action alpha beta E)
  (let* ((k (sqrt (/ (* 2. beta) (+ beta E))))
	 (Ek (second-elliptic-integral k)))
    (* (/ 4. pi) 
       (sqrt (* beta alpha))
       (/ Ek k))))

(define ((pendulum-circulating-action-to-E alpha beta) action)
  (let ((g (/ action (* (/ 4. pi) (sqrt (* beta alpha))))))
    (let ((k (pendulum-inverse-g g)))
      (let ((k^2 (square k)))
	(/ (* beta (- 2. k^2)) k^2)))))  


(define ((pendulum-circulating-phase alpha beta) state)
  (let ((theta (state->q state))
	(ptheta (state->p state)))
    (let ((E ((Hpendulum alpha beta) state)))
      (let ((k (sqrt (/ (* 2. beta) (+ E beta))))
	    (omega-R (sqrt (abs (/ (+ E beta) (* 2. alpha)))))
	    (period (/ 2pi (pendulum-frequency alpha beta E))))
	(let ((dt (/ (elliptic-integral-F 
		      (/ ((principal-value pi) theta) 2.) k)
		     omega-R)))
	  ((principal-value pi) (* 2pi (/ dt period))))))))
	  
;;; time from theta=0 to state
(define ((pendulum-circulating-dt alpha beta) state)
  (let ((E ((Hpendulum alpha beta) state))
	(phase ((pendulum-circulating-phase alpha beta) state)))
    (let ((period (/ 2pi (pendulum-frequency alpha beta E))))
      (* phase (/ period 2pi)))))

(define ((pendulum-circulating-aa-state-to-state alpha beta) aa-state)
  (let ((angle (state->q aa-state))
	(action (state->p aa-state)))
    (let* ((E ((pendulum-circulating-action-to-E alpha beta) action))
	   (period (/ 2pi (pendulum-frequency alpha beta E))))
      (let ((dt (* (/ period 2pi) angle)))
	(->H-state (state->t aa-state)
		   ((pendulum-circulating-angle alpha beta E) dt)
		   ((pendulum-circulating-angular-momentum alpha beta E) dt))))))


#|
(define ((pendulum-circulating-action-to-E alpha beta) action)
  (bisect (lambda (E) (- (pendulum-circulating-action alpha beta E) action))
	  beta
	  (+ (/ (square action) (* 2. alpha)) beta)
	  (* *action-to-E-bugger-factor* *machine-epsilon*)))
|#

(define ((pendulum-circulating-state-to-aa-state alpha beta) state)
  (let ((E ((Hpendulum alpha beta) state)))
    (let ((action ((pendulum-circulating-action alpha beta E) state))
	  (angle ((pendulum-circulating-phase alpha beta) state)))
      (->H-state (state->t state) angle action))))

(define (pendulum-f k)
  (if (= k 1.)
      1.
      (elliptic-integrals 
       k 
       (lambda (Kk Ek)
	 (- Ek (* (- 1. (square k)) Kk))))))

(define (pendulum-g k)
  (/ (second-elliptic-integral k) k))

(define (pendulum-inverse-f fk) 
  (let ((sfk (sqrt fk)))
    (bisect (lambda (k) 
	      (- (sqrt (pendulum-f k)) sfk))
	    0. 1. 1.e-10)))

(define (pendulum-inverse-g gk)
  (let ((inv-gk (/ 1. gk)))
    (bisect (lambda (k) 
	      (if (= k 0.)
		  (- inv-gk)
		  (- (/ 1. (pendulum-g k)) inv-gk)))
	    0.0 1. 1.e-10)))


;;;----------------------------------------------------------------
;;; separatrix case

(define ((pendulum-separatrix-angle alpha beta) time)
  (let ((omega-0 (sqrt (abs (/ beta alpha)))))
    (* 2. (gudermannian (* omega-0 time)))))

(define ((pendulum-separatrix-angular-momentum alpha beta) time)
  (let ((theta ((pendulum-separatrix-angle alpha beta) time)))
    (sqrt (* 2. alpha beta (+ 1. (cos theta))))))

(define (gudermannian x)
  (- (* 2. (atan (exp x))) pi/2))

(define (inverse-gudermannian x)
  (log (tan (+ (/ x 2.) pi/4))))


;;; area of "eye"
(define (pendulum-separatrix-action alpha beta)
  (* (/ 8. pi) (sqrt (* alpha beta))))

;;;----------------------------------------------------------------
;;; pendulum state advancer

(define (((pendulum-advance alpha beta) state) time)
  (let ((E ((Hpendulum alpha beta) state)))
    (if (< E beta)
	(let ((dt ((pendulum-oscillating-dt alpha beta) state)))
	  (let ((t (+ dt (- time (state->t state)))))
	    (->H-state time
		       ((pendulum-oscillating-angle alpha beta E) t)
		       ((pendulum-oscillating-angular-momentum alpha beta E) t))))

	(if (> (state->p state) 0)
	    (let ((dt ((pendulum-circulating-dt alpha beta) state)))
	      (let ((t (+ dt (- time (state->t state)))))
		(->H-state time
			   ((pendulum-circulating-angle alpha beta E) t)
			   ((pendulum-circulating-angular-momentum alpha beta E) t))))
	    (let ((dt ((pendulum-circulating-dt alpha beta) 
		       (->H-state (- (state->t state))
				  (- (state->q state))
				  (- (state->p state))))))
	      (let ((t (+ dt (- time (state->t state)))))
		(->H-state 
		 time
		 (- ((pendulum-circulating-angle alpha beta E) t))
		 (- ((pendulum-circulating-angular-momentum alpha beta E) t)
		    ))))))))
	

(define (((pendulum-integration alpha beta eps) state) time)
  (let ((state2
	 ((state-advancer pendulum-sysder alpha beta)
	  state (- time (state->t state)) eps)))
    (->H-state (state->t state2)
	     ((principal-value pi) (state->q state2))
	     (state->p state2))))
  


;;;----------------------------------------------------------------
;;; series solutions

#|
(define ((pendulum-oscillating-solution-series alpha beta E omega eps) time)
  (let ((k (sqrt (/ (+ E beta) (* 2. beta))))
	(omega-0 (sqrt (/ beta alpha))))
    (let ((Kp (first-elliptic-integral (sqrt (- 1. (square k))))))
      (define (term n)
	(let ((omega-n (* omega (- (* 2. n) 1.))))
	  (/ (sin (* omega-n time))
	     (* omega-n (cosh (/ (* omega-n Kp) omega-0))))))
      (* 4. omega (sum-series term eps)))))

(define ((pendulum-circulating-solution-series alpha beta E omega eps) time)
  (let ((k (sqrt (/ (* 2. beta) (+ E beta))))
	(omega-R (sqrt (abs (/ (+ E beta) (* 2. alpha))))))
    (let ((Kp (first-elliptic-integral (sqrt (- 1. (square k))))))
      (define ((term time) n)
	(let ((omega-n (* omega n)))
	  (/ (sin (* omega-n time))
	     (* omega-n (cosh (/ (* omega-n Kp) omega-R))))))
      (+ (* omega time)
	 (* 2. omega (sum-series (term time) eps))))))

;;; don't use this without thinking...
(define (sum-series term eps)
  (let loop ((n 1) (sum 0.) (lastf 1.))
    (let ((f (term n)))
      (if (and (< (abs f) eps) (< (abs lastf) eps))
	  sum
	  (loop (fix:+ n 1) (+ sum f) f)))))
;;; purpose of checking last two is 
;;; to prevent some premature terminations
;;; because a term is "accidently" zero

|#


#|

(define (((pendulum-solution-series alpha beta) state) time)
  (let ((E ((Hpendulum alpha beta) state)))
    (let ((omega (pendulum-frequency alpha beta E))
	  (beta (abs beta)))    
      (if (< E beta)
	  (let ((k (sqrt (/ (+ E beta) (* 2 beta))))
		(omega-0 (sqrt (abs (/ beta alpha)))))
	    (let ((Kp (first-elliptic-integral (sqrt (- 1 (square k))))))
	      (define (term n)
		(let ((omega-n (* omega (- (* 2 n) 1))))
		  (/ (sin (* omega-n time))
		     (* omega-n (cosh (/ (* omega-n Kp) omega-0))))))
	      (* 4 omega (series:generate (lambda (i) (term (+ i 1)))))))
	  (let ((k (sqrt (/ (* 2 beta) (+ E beta))))
		(omega-R (sqrt (abs (/ (+ E beta) (* 2 alpha))))))
	    (let ((Kp (first-elliptic-integral (sqrt (- 1 (square k))))))
	      (define ((term time) n)
		(let ((omega-n (* omega n)))
		  (/ (sin (* omega-n time))
		     (* omega-n (cosh (/ (* omega-n Kp) omega-R))))))
	      (+ (* omega time)
		 (* 2 omega (series:generate (lambda (i) ((term time) (+ i 1))))))))))))

(series:print
 (((pendulum-solution-series 1. 9.8)
   (->H-state 0. 0. 4.9006733894348145)) 't)
 10)
(* 1.8349993630564594 (sin (* 2.5043962735932013 t)))
(* .03821300344597103 (sin (* 7.513188820779604 t)))
(* .00135312864251141 (sin (* 12.521981367966006 t)))
(* 5.702944261999213e-5 (sin (* 17.53077391515241 t)))
(* 2.617233223741749e-6 (sin (* 22.53956646233881 t)))
(* 1.2635138738869227e-7 (sin (* 27.548359009525214 t)))
(* 6.308369363000512e-9 (sin (* 32.55715155671162 t)))
(* 3.225945107424557e-10 (sin (* 37.56594410389802 t)))
(* 1.679527336266625e-11 (sin (* 42.57473665108442 t)))
(* 8.866866369088442e-13 (sin (* 47.583529198270824 t)))
;Value: ...

|#

    
  
#|
;;; Check that the canonical transformation is area-preserving.

(define (der-qq f state)
  ((richardson-derivative 
    (lambda (q)
      (state->q 
       (f (->H-state (state->t state) q (state->p state)))))
    1.e-8
    .01)
   (state->q state)))

(define (der-qp f state)
  ((richardson-derivative 
    (lambda (p)
      (state->q 
       (f (->H-state (state->t state) (state->q state) p))))
    1.e-8
    .01)
   (state->p state)))

(define (der-pq f state)
  ((richardson-derivative 
    (lambda (q)
      (state->p 
       (f (->H-state (state->t state) q (state->p state)))))
    1.e-8
    .01)
   (state->q state)))

(define (der-pp f state)
  ((richardson-derivative 
    (lambda (p)
      (state->p 
       (f (->H-state (state->t state) (state->q state) p))))
    1.e-8
    .01)
   (state->p state)))

;;;----------------------------------------------------------------

(let ((f (pendulum-circulating-aa-state-to-state 2.0 9.8))
      (g (pendulum-circulating-state-to-aa-state 2.0 9.8))) 
  (let* ((state (->H-state 1. 1. 15.))
	 (aa-state (g state)))
    (- (* (der-qq f aa-state) (der-pp f aa-state))
       (* (der-pq f aa-state) (der-qp f aa-state)))))
;Value: 1.0000000000003484

(let ((f (pendulum-circulating-aa-state-to-state 2.0 9.8))
      (g (pendulum-circulating-state-to-aa-state 2.0 9.8))) 
  (let* ((state (->H-state 1. 1. 15.))
	 (aa-state (g state)))
    (- (* (der-qq g state) (der-pp g state))
       (* (der-pq g state) (der-qp g state)))))
;Value: .9999999999986688

(let ((f (pendulum-oscillating-aa-state-to-state 2.0 9.8))
      (g (pendulum-oscillating-state-to-aa-state 2.0 9.8))) 
  (let* ((state (->H-state 1. 1. 1.))
	 (aa-state (g state)))
    (- (* (der-qq g state) (der-pp g state))
       (* (der-pq g state) (der-qp g state)))))
;Value: 1.000000000000521

(let ((f (pendulum-oscillating-aa-state-to-state 2.0 9.8))
      (g (pendulum-oscillating-state-to-aa-state 2.0 9.8))) 
  (let* ((state (->H-state 1. 1. 1.))
	 (aa-state (g state)))
    (- (* (der-qq f aa-state) (der-pp f aa-state))
       (* (der-pq f aa-state) (der-qp f aa-state)))))
;Value: 1.000000000000406

|#

;;;----------------------------------------------------------------

(define (pendulum-frequency alpha beta E)
  (cond ((< E beta) (pendulum-oscillating-frequency alpha beta E))
	((> E beta) (pendulum-circulating-frequency alpha beta E))
	(else
	 0.)))

;;;----------------------------------------------------------------
;;; global action angle coordinates for pendulum

#|

Oscillation region:
-pi < phi < pi
0 <  I < Isep 

Upper circulation region:
-pi < phi < pi  ->  -pi/2 < phi' < pi/2   phi' = phi/2
Isep < 2I                  Isep < I'      I' = 2I

Lower circulation region:
...

|#

(define ((pendulum-state-to-global-aa-state alpha beta) state)
  (let ((E ((Hpendulum alpha beta) state)))
    (cond ((< E beta)
	   ((pendulum-oscillating-state-to-aa-state alpha beta) state))
	  ((and (> E beta) (> (state->p state) 0.))
	   (let ((aa-state 
		  ((pendulum-circulating-state-to-aa-state alpha beta)
		   state)))
	     (->H-state (state->t state)
			(* 0.5 (state->q aa-state))
			(* 2.0 (state->p aa-state)))))
	  ((and (> E beta) (< (state->p state) 0.))
	   (let ((aa-state 
		  ((pendulum-circulating-state-to-aa-state alpha beta)
		   state)))
	     (->H-state (state->t state)
			((principal-value pi)
			 (- pi (* 0.5 (state->q aa-state))))
			(* 2.0 (state->p aa-state)))))
	  ((= E beta)
	   'go-figure))))

(define (pendulum-global-aa-state-to-state alpha beta)
  (let ((separatrix-action (pendulum-separatrix-action alpha beta)))
    (lambda (aa-state)
      (let ((angle (state->q aa-state))
	    (action (state->p aa-state)))
	(cond ((< action separatrix-action)
	       ((pendulum-oscillating-aa-state-to-state alpha beta) aa-state))
	      ((> action separatrix-action)
	       (if (and (< angle pi/2) (>= angle -pi/2))
		   ((pendulum-circulating-aa-state-to-state alpha beta)
		    (->H-state (state->t aa-state)
			       (* 2. (state->q aa-state))
			       (* 0.5 (state->p aa-state))))
		   (let ((state
			  ((pendulum-circulating-aa-state-to-state alpha beta)
			   (->H-state (state->t aa-state)
				      (* 2. (state->q aa-state))
				      (* 0.5 (state->p aa-state))))))
		     (->H-state (state->t state)
				(- (state->q state))
				(- (state->p state))))))
	      ((= action separatrix-action)
	       'oh-well))))))


