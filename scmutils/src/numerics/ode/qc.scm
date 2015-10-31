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

;;;; Quality-controlled adaptive integrators.

(declare (usual-integrations))

;;; QUALITY-CONTROL upgrades a one-step method into an adaptive
;;; quality-control method.  Given the one-step method and its "order"
;;; as required by Press et.al. (Numerical Recipes, section 15.2),
;;; QUALITY-CONTROL returns the STEPPER-MAKER, a procedure of two
;;; arguments: the system-derivative and an accuracy specification.

;;; STEPPER-MAKER, when called, returns a quality-controlled stepper
;;; that adjusts its step-size to meet the accuracy spec.  The
;;; quality-controlled stepper, when given a state and a suggested
;;; increment of the independent variable, calls its continuation with
;;; the new state, the actual step taken, and a suggested next step
;;; size.

;;; The method supplied to QUALITY-CONTROL takes as arguments the
;;; system-derivative, the error tolerance, and possibly other
;;; parameters.  Next is a (curried) initial state.  It returns a
;;; stepper procedure that takes a proposed step-size and two
;;; continuations, one for success and one for failure.  The stepper
;;; calls its success continuation with the new state if it succeeds
;;; in making the step.  It calls the failure continuation if it
;;; cannot, say because of an attempt to invert a singular matrix, or
;;; some similar disaster.

(define (quality-control method order)
  (let ((2^order (expt 2.0 order))
	(error-scale-down (/ -1.0 (+ (exact->inexact order) 1.0)))
	(error-scale-up (/ -1.0 (exact->inexact order))))
    (let ((halfweight (v:scale (/ 2^order (- 2^order 1.0))))
	  (fullweight (v:scale (/ 1.0 (- 2^order 1.0))))
	  (h-adjust-down
	    (lambda (h err)
	      (* qc-damping h (expt (max err qc-zero-protect) error-scale-down))))
	  (h-adjust-up
	    (lambda (h err)
	      (* qc-damping h (expt (max err qc-zero-protect) error-scale-up)))))
      (define (qc-stepper-maker der tolerance . others)
	(let ((error-measure (parse-error-measure tolerance))
	      (mder (apply method der tolerance others)))
	  (define (qc-stepper state h-init continue)
	    (let ((stepper (mder state)))
	      (if qc-wallp? (write-line `(qc state: ,state)))
	      (let loop ((h h-init))
		(if qc-wallp? (write-line `(qc h: ,h)))
		(let ((h/2 (* 0.5 h)))
		  (stepper		;first halfstep
		   h/2
		   (lambda (halfstep nh) ;first halfstep succeeded
		     ((mder halfstep)	;second halfstep
		      h/2
		      (lambda (2halfsteps n2h) ;second halfstep succeeded
			(stepper
			 h
			 (lambda (fullstep nf) ;fullstep succeeded
			   (let ((err (error-measure 2halfsteps fullstep)))
			     (if qc-wallp?
				 (write-line `(qc fullstep err: ,err ,nh ,n2h ,nf)))
			     (if (> err *qc-trigger-point*) 
				 (loop (h-adjust-up h err))
				 (continue (vector-vector (halfweight 2halfsteps)
							  (fullweight fullstep))
					   h
					   (h-adjust-down h err)))))
			 (lambda ()	;fullstep failed
			   (if qc-wallp? (write-line `(qc: fullstep failed)))
			   (loop (* *qc-fullstep-reduction-factor* h)))))
		      (lambda ()	;second halfstep failed
			(if qc-wallp? (write-line `(qc: second halfstep failed)))
			(loop (* *qc-2halfsteps-reduction-factor* h)))))
		   (lambda ()		;first halfstep failed
		     (if qc-wallp? (write-line `(qc: first halfstep failed)))
		     (loop (* *qc-halfstep-reduction-factor* h))))))))
	  qc-stepper))
      qc-stepper-maker)))

(define *qc-trigger-point* 1.5)	; to make dead zone around 1.0 -- GJS
(define *qc-halfstep-reduction-factor* 0.3) ;must be less than .5
(define *qc-2halfsteps-reduction-factor* 0.5)
(define *qc-fullstep-reduction-factor* 0.5)

(define qc-wallp? false)
(define qc-damping .9)
(define qc-zero-protect 1.0e-20)

;;; A "simple" explicit method, based on fourth-order Runge-Kutta:

#| For example:

((advance-generator
  ((quality-control rk4 4)		;integration method
   (lambda (v) v)			;x' = x
   .0001))				;error tolerated
 #(1.0)					;initial state (at t = t0)
 1.0					;proceed to t = t0 + 1
 1.0					;first step no larger than .1
 0.5					;no step larger than .5
 (lambda (ns dt h cont)
   (pp ns)
   (cont))
 (lambda (ns dt sdt)
   ;; assert ns = #(2.718...)
   ;; assert dt = 1.000...+-
   (list ns dt sdt)))
#(1.)
#(1.648716933638961)
#(2.588254411801423)
;Value: (#(2.7182707063734948) 1. .4041654277154577)
|#

(define (rk4 der tolerance)		;ignores tolerance
  (define 2* (v:scale 2.0))
  (define 1/2* (v:scale 0.5))
  (define 1/6* (v:scale (/ 1.0 6.0)))  
  (lambda (state)
    (let ((dydx (der state)))
      (define (rkstep h succeed fail)
	(let* ((h* (v:scale h))
	       (k0 (h* dydx))
	       (k1 (h* (der (vector+vector state (1/2* k0)))))
	       (k2 (h* (der (vector+vector state (1/2* k1)))))
	       (k3 (h* (der (vector+vector state k2)))))
	  (succeed
	   (vector+vector state
			  (1/6* (vector+vector (vector+vector k0 (2* k1))
					       (vector+vector (2* k2) k3))))
	   1)))
      rkstep)))


(add-integrator! 'rkqc
 (lambda (derivative lte-tolerance start-state
		     step-required h-suggested max-h continue done)
   ((advance-generator ((quality-control rk4 4) derivative lte-tolerance))
    start-state
    step-required
    h-suggested
    max-h
    continue
    done))
 '(derivative lte-tolerance start-state step-required h-suggested max-h continue done))

;;; The following are predictor-corrector methods.

(define pc-wallp? false)

;;; A trapezoid method: xn+1 is found by corrector iteration.

#| For example:

((advance-generator
  ((quality-control c-trapezoid 2)	;integration method
   (lambda (v) v)			;x' = x
   0.0001				;qc error tolerated
   1.0e-5))				;corrector convergence
 #(1.0)					;initial state (at t = t0)
 1.0					;proceed to t = t0 + 1
 0.1					;first step no larger than .1
 0.5					;no step larger than .5
 (lambda (ns dt h cont)
   (pp ns)
   (cont))
 (lambda (ns dt sdt)
   ;; assert ns = #(2.718...)
   ;; assert dt = 1.000...+-
   (list ns dt sdt)))
#(1.)
#(1.1051688554687495)
#(1.2583037182508854)
#(1.4307307288261986)
#(1.6229900169138303)
#(1.8372249433735863)
#(2.0758338727890635)
#(2.3414853586609374)
#(2.637148056178347)
;Value: (#(2.7182832352360498) 1. .11894979864256087)
|#

(define (c-trapezoid f qc-tolerance #!optional convergence-tolerance)
  (let ((error-measure
	 (parse-error-measure
	  (if (default-object? convergence-tolerance)
	      qc-tolerance
	      convergence-tolerance))))
    (lambda (xn)
      (let ((d (f xn)))
	(define (trapstep dt succeed fail)
	  (let* ((dt/2 (* dt 0.5))
		 (predicted (vector+vector xn (scalar*vector dt d)))
		 (corrected
		  (vector+vector xn
				 (scalar*vector dt/2
						(vector+vector (f predicted) d)))))
	    (let lp ((predicted predicted) (corrected corrected) (count 1))
	      (let ((verr (error-measure predicted corrected)))
		(if (< verr 2.0)
		    (succeed corrected count)
		    (let* ((ncorr
			    (vector+vector xn
					   (scalar*vector dt/2
							  (vector+vector (f corrected)
									 d))))
			   (nverr (error-measure ncorr corrected)))
		      (if (< nverr verr)
			  (lp corrected ncorr (fix:+ count 1))
			  (begin (if pc-wallp? (write-line `(pc failed: ,nverr ,verr)))
				 (fail)))))))))
	trapstep))))

(add-integrator! 'ctqc
 (lambda (derivative lte-tolerance convergence-tolerance start-state
		     step-required h-suggested max-h continue done)
   ((advance-generator
     ((quality-control c-trapezoid 2) derivative lte-tolerance convergence-tolerance))
    start-state
    step-required
    h-suggested
    max-h
    continue
    done))
 '(derivative lte-tolerance convergence-tolerance start-state
	      step-required h-suggested max-h continue done))

;;; A trapezoid method:  xn+1 is found by Newton iteration, rather
;;;   than corrector iteration as in c-trapezoid, above.
;;;   Be careful, the predictor here is not good enough for stiff systems.
;;;   Note, here f yields both the derivative and the Jacobian.

#|  For example:

((advance-generator
  ((quality-control n-trapezoid 2)	;integration method
   (lambda (v cont)			;x' = x
     (cont v (array->matrix #(#(1.0)))))
   0.0001				;qc-error tolerated
   1					;state dimension
   1.0e-5))				;corrector convergence
 #(1.0)					;initial state (at t = t0)
 1.0					;proceed to t = t0 + 1
 0.1					;first step no larger than .1
 0.5					;no step larger than .5
 (lambda (ns dt h cont)
   (pp ns)
   (cont))
 (lambda (ns dt sdt)
   ;; assert ns = #(2.718...)
   ;; assert dt = 1.000...+-
   (list ns dt sdt)))
#(1.)
#(1.1051708824988178)
#(1.259109256732086)
#(1.4307187104000767)
#(1.6219876372976312)
#(1.8350462370303233)
#(2.0722642596439016)
#(2.3362773683519777)
#(2.630016590666874)
;Value: (#(2.718279922395027) 1. .11684285320335219)
|#

(define (n-trapezoid f-df qc-tolerance dimension #!optional convergence-tolerance)
  (let* ((convergence-tolerance
	  (if (default-object? convergence-tolerance)
	      qc-tolerance
	      convergence-tolerance))
	 (error-measure (parse-error-measure convergence-tolerance))
	 (clip-vector (vector-clipper dimension))
	 (pad-vector (vector-padder dimension))
	 (I (m:make-identity (J-dimension dimension))))
    (lambda (xn)
      (f-df xn
	    (lambda (fn dfn)
	      (define (trapstep h succeed fail)
		(let ((h/2 (* h 0.5))
		      (predicted (vector+vector xn (scalar*vector h fn))))
		  (define (corrector xn+1)
		    (f-df xn+1
			  (lambda (fn+1 dfn+1)
			    (let ((A (matrix-matrix (scalar*matrix h/2 dfn+1) I))
				  (b (vector-vector xn+1
				       (vector+vector xn
					 (scalar*vector h/2
							(vector+vector fn fn+1))))))
			      (gauss-jordan-invert-and-solve
			       A (clip-vector b)
			       (lambda (dx . ignore)
				 (vector+vector (pad-vector dx) xn+1))
			       (lambda ()
				 (if pc-wallp? (pp `(gj failed: ,A ,b)))
				 (fail)))))))
		  (let lp ((predicted predicted)
			   (corrected (corrector predicted))
			   (count 1))
		    (let ((verr (error-measure predicted corrected)))
		      (if (< verr 2.0)
			  (succeed corrected count)
			  (let* ((ncorr (corrector corrected))
				 (nverr (error-measure ncorr corrected)))
			    (if (< nverr verr)
				(lp corrected ncorr (fix:+ count 1))
				(begin (if pc-wallp?
					   (write-line `(gj nr failed: ,nverr ,verr)))
				       (fail)))))))))
	      trapstep)))))

(add-integrator!
 'ntqc
 (lambda (derivative-and-jacobian
	  dimension
	  lte-tolerance
	  convergence-tolerance
	  start-state
	  step-required
	  h-suggested
	  max-h
	  continue
	  done)
   ((advance-generator
     ((quality-control n-trapezoid 2)
      derivative-and-jacobian lte-tolerance dimension convergence-tolerance))
    start-state
    step-required
    h-suggested
    max-h
    continue
    done))
 '(derivative-and-jacobian
   dimension
   lte-tolerance
   convergence-tolerance
   start-state
   step-required
   h-suggested
   max-h
   continue
   done))
