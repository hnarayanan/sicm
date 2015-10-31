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

;;;; Gear integrators for stiff differential equations

;;; Assumption: all states have their time as the first component.
#| For example:

((gear-advance-generator
  (lambda (x cont)
    (cont (vector 1.0 (vector-ref x 1))
	  (array->matrix
	   #(#(0.0 0.0)			;jacobian-dimension
	     #(0.0 1.0)))))
  2					;simple dimension
  .000001)				;lte
 #(0.0 1.0)				;initial conditions
 1.0					;target advance
 0.1					;initial step
 0.5					;max step
 (lambda (ns dt h cont)
   (pp ns)				;print each new state
   (cont))
 (lambda (ns dt sdt)
   ;; assert ns = #(1.000... 2.718...)
   ;; assert dt = 1.000...
   (list ns dt sdt)))

((gear-advance-generator
  (lambda (x cont)
    (cont (vector 1.0 (vector-ref x 1))
	  (array->matrix
	   #(#(1.0)))))			;a 1X1 Jacobian
  '(2 1 2)				;clip out time
  .000001)				;lte
 #(0.0 1.0)
 1.0
 0.1
 0.5
 (lambda (ns dt h cont)
   (pp ns)
   (cont))
 (lambda (ns dt sdt)
   ;; assert ns = #(1.000... 2.718...)
   ;; assert dt = 1.000...
   (list ns dt sdt)))



((gear-advance-generator
  (lambda (v cont)
    (let ((x (vector-ref v 1)) (y (vector-ref v 2)))
      (cont (vector 1.0 (- y) x)
	    (array->matrix
	     #(#(  0.0    -1.0 )
	       #(  1.0     0.0 ) )))))
  '(3 1 3)
  1e-6)
 #( 0.0 1.0 0.0 )
 2pi
 .1
 0.5
 (lambda (ns dt h cont)
   (pp ns)
   (cont))
 list)


(define (circle state alpha predicted ctolerance succeed fail)
  (let ((t (vector-ref state 0))
	(b1 (vector-ref state 1))
	(b2 (vector-ref state 2))
	(gamma (/ 1 (+ 1 (* alpha alpha)))))
    (let ((beta (- (+ b2 (* alpha b1)))))
      (let ((x1 (* beta gamma))
	    (x2 (+ b1 (* alpha beta gamma))))
	(succeed (vector (/ (- 1 t) alpha)
			 x1
			 x2
			 (+ (square x1) (square x2)))
		 1)))))

((gear-advance-generator
  circle
  '(4 0 3)				;do not ignore time
  .000001				;lte
  .0000001				;convergence
  true)					;implicit
 #(0.0 1.0 0.0 1.0)			;initial conditions
 2pi					;target advance
 0.1					;initial step
 0.5					;max step
 (lambda (ns dt h cont)
   (pp ns)				;print each new state
   (cont))
 (lambda (ns dt sdt)
   ;; assert ns = #(1.000... 2.718...)
   ;; assert dt = 1.000...
   (list ns dt sdt)))

|#

(add-integrator!
 'gear
 (lambda (derivative-and-jacobian
	  dimension
	  lte-tolerance
	  convergence-tolerance
	  spice-mode?
	  start-state
	  step-required
	  h-suggested
	  max-h
	  continue
	  done)
   ((gear-advance-generator
     derivative-and-jacobian dimension lte-tolerance
     convergence-tolerance false spice-mode?)
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
   spice-mode?
   start-state
   step-required
   h-suggested
   max-h
   continue
   done))

(add-integrator!
 'implicit-gear
 (lambda (generalized-corrector
	  dimension
	  lte-tolerance
	  convergence-tolerance
	  spice-mode?
	  start-state
	  step-required
	  h-suggested
	  max-h
	  continue
	  done)
   ((gear-advance-generator
     generalized-corrector dimension lte-tolerance
     convergence-tolerance true spice-mode?)
    start-state
    step-required
    h-suggested
    max-h
    continue
    done))
 '(generalized-corrector
   dimension
   lte-tolerance
   convergence-tolerance
   spice-mode?
   start-state
   step-required
   h-suggested
   max-h
   continue
   done))

(declare (usual-integrations))

(define (gear-advance-generator f&df dimension lte
	                #!optional convergence-tolerance implicit? spice-mode?)
  (let* ((lte-measure (parse-error-measure lte))
	 (convergence-tolerance
	  (parse-error-measure lte *gear-fixed-point-margin*))
	 (convergence-measure (parse-error-measure convergence-tolerance))
	 (implicit? (if (default-object? implicit?) false implicit?))
	 (spice-mode? (if (default-object? spice-mode?) false spice-mode?))
	 (stepper
	  (gear-integrator f&df
			   lte-measure
			   convergence-measure
			   spice-mode?
			   dimension
			   implicit?)))
    (define (advance start-state step-required h-suggested max-h continue done)
      ;; done = (lambda (end-state step-achieved h-suggested) ... )
      ;; continue = (lambda (state step-achieved h-taken next)
      ;;              ;; next = (lambda () ...)
      ;;              )
      (let lp ((step-achieved 0.0)
	       (states (list start-state))
	       (fx
		(if implicit? 'ignore (f&df start-state (lambda (fx dfx) fx))))
	       (h (min-step-size step-required h-suggested max-h))
	       (order 1)
	       (wins 1)
	       (accum-error 0.0))
	(if advance-wallp?
	    (pp `(advance: ,step-achieved ,(car states))))
	(continue (car states) step-achieved h
	  (lambda ()
	    (stepper states fx h order wins accum-error
		     (lambda (step-obtained
			      new-states
			      nfx
			      h-suggested
			      order-suggested
			      wins
			      accum-error)
		       (let ((nstep (+ step-achieved step-obtained)))
			 (if (close-enuf? step-required
					  nstep
					  *independent-variable-tolerance*)
			     (done (car new-states) nstep h-suggested)
			     (lp nstep
				 new-states
				 nfx
				 (min-step-size (- step-required nstep)
						h-suggested
						max-h)
				 order-suggested
				 wins
				 accum-error)))))))))
    advance))

(define (gear-stepper-generator f&df dimension lte
	 #!optional convergence-tolerance implicit? spice-mode?)
  (let ((gear-advancer
	 (gear-advance-generator f&df dimension
				 lte convergence-tolerance
				 implicit? spice-mode?)))
    (define (gear-stepper state dt-requested continue)
      ;; continue = (lambda (new-state dt-obtained dt-suggested) ...)
      (gear-advancer state
		     dt-requested
		     (/ dt-requested 2.0)
		     dt-requested
		     (lambda (state step-achieved h-taken next)
		       (next))
		     continue))
    gear-stepper))

#|
((gear-stepper-generator
  (lambda (x cont)
    (cont (vector 1.0 (vector-ref x 1))
	  (array->matrix
	   #(#(0.0 0.0)			;jacobian
	     #(0.0 1.0)))))
  2					;simple dimension
  .000001)				;lte
 #(0.0 1.0)				;initial conditions
 1.0					;target advance
 'foo					;ignored eps
 list)
;Value: (#(.9999999999999964 2.7182821316457484) 1. 2.7162473311300284e-2)
|#

(define (gear-integrator f&df
			 lte-measure convergence-measure spice-mode? dimension
			 implicit?)
  ;; lte = local-truncation-error tolerance
  (define gear-solve
    (gear-solve-maker f&df
		      lte-measure convergence-measure dimension implicit?))
  (define gear-predict (gear-predict-maker dimension implicit?))
  (define (gear-step xs fx h k wins err-accum cont)
    ;; The gear stepper takes a set of xs.
    ;; It returns by calling a continuation
    ;;   cont = (lambda (h nxs nfx newh newk nerr) ...)
    (wallp-pp gear-wallp? `(gear-step h= ,h k= ,k))
      (gear-predict xs h k
	(lambda (xp)		               ;predicted x
	  (wallp-pp gear-wallp? `(gear-predict: ,xp))
	  (gear-solve xs h k xp
		      (lambda (xc nfx err niter) ;converged in NITER
			(wallp-pp gear-wallp?
				  `(gear-solve: ,xc)
				  `(err=   ,err
					   accum= ,err-accum
					   niter= ,niter))
			(let ((naccum (+ err err-accum)))
			  (gear-control err wins h k naccum niter spice-mode?
			   (lambda (nh nk) ;good step
			     (cont h (update-table xc xs)
				   nfx nh nk (fix:+ wins 1) naccum))
			   (lambda (nh nk) ;careful step
			     (cont h (update-table xc xs)
				   nfx nh nk 1 0.0))
			   (lambda (nh nk) ;bad step
			     (gear-step xs fx nh nk 0 0.0 cont)))))
		      (lambda ()	;failed to converge
			(gear-step xs
				   fx
				   (/ h *gear-fixed-point-failure-contraction*)
				   (max (fix:- k 1) *gear-min-order*) ; was k
				   1
				   0.0
				   cont))))))
  gear-step)

(define (update-table new table)
  (if (fix:< (length table) *gear-max-order*)
      (cons new table)
      (cons new (reverse (cdr (reverse table))))))

(define (gear-predict-maker dimension implicit?)
  (let ((clip-vector
	 (if implicit?
	     (vector-clipper dimension)
	     (lambda (v) v))))
    (define (gear-predict xs h k cont)
      ;; continuation cont = (lambda (xp) ...)
      (let ((tn (vector-ref (car xs) 0)))
	(cont ((vector-ref lagrange-extrapolators k)
	       (map clip-vector xs)
	       (+ tn h)))))
    gear-predict))

(define (gear-solve-maker f&df lte-measure convergence-measure dimension implicit?)
  (let ((clip-vector (vector-clipper dimension))
	(pad-vector (vector-padder dimension))
	(I (m:make-identity (J-dimension dimension))))
    (define (gear-solve xs h order xp succeed fail)
      (let* ((ts (map (lambda (v) (vector-ref v 0)) xs))
	     (gear-coeffs ((vector-ref gear-correctors order) h ts))
	     (alpha (car gear-coeffs))
	     (alpha*I (scalar*matrix alpha I))
	     (b
	      (let lp ((i 1) (gc (cdr gear-coeffs)) (xs xs))
		(let ((x (clip-vector (car xs))))
		  (if (fix:= i order)
		      (scalar*vector (car gc) x)
		      (vector+vector (scalar*vector (car gc) x)
				     (lp (fix:+ i 1) (cdr gc) (cdr xs)))))))
	     (last-fx 0.0))
	(vector-fixed-point-with-failure
	 (lambda (x continue)
	   ;; continue = (lambda (newx newfx) ...)
	   (f&df x
		 (lambda (fx dfx)
		   (full-pivot-solve	            ; A*ds = b
		    (matrix-matrix alpha*I Dfx)     ;A
		    (vector-vector (clip-vector fx) ;b
				   (vector+vector (scalar*vector alpha
						                 (clip-vector x))
						  b))
		    (lambda (ds . rest)	;succeed
		      (continue (vector+vector (pad-vector ds) x) fx))
		    (lambda ()
		      (wallp-pp gear-wallp? '(gear-solve: singular matrix))
		      (fail))))))
	 xp
	 convergence-measure
	 (lambda (x-corr last-fx niter)
	   (let ((err (lte-measure x-corr xp)))
	     (succeed x-corr last-fx err niter)))
	 (lambda (last-x last-fx)
	   (wallp-pp gear-wallp? '(gear-solve: did not converge))
	   (fail)))))
    (define (implicit-gear-solve xs h order clipped-xp succeed fail)
      (let* ((ts (map (lambda (v) (vector-ref v 0)) xs))
	     (gear-coeffs ((vector-ref gear-correctors order) h ts))
	     (alpha (car gear-coeffs))
	     (b
	      (let lp ((i 1) (gc (cdr gear-coeffs)) (xs xs))
		(let ((x (clip-vector (car xs))))
		  (if (fix:= i order)
		      (scalar*vector (car gc) x)
		      (vector+vector (scalar*vector (car gc) x)
				     (lp (fix:+ i 1) (cdr gc) (cdr xs))))))))
	(f&df b alpha clipped-xp			
	      convergence-measure
	      (lambda (x-corr count)	;assumes x-corr is bigger than clipped.
		(let ((err (lte-measure (clip-vector x-corr) clipped-xp)))
		  (succeed x-corr 'ignore err count)))
	      (lambda args
		(wallp-pp gear-wallp? '(gear-solve: did not converge))
		(fail)))))
    (if implicit? implicit-gear-solve gear-solve)))

(define (gear-control err wins h k err-accum niter
		      spice-mode good-step careful-step bad-step)
  ;;  good-step = (lambda (nh nk) ...)
  ;;  bad-step  = (lambda (nh nk) ...)
  (cond (spice-mode
	 (cond ((fix:> niter *spice-order-too-big*)
		(bad-step (spice-contract h niter)
			  (max (fix:- k 1) *gear-min-order*)))
	       ((fix:> niter *spice-step-too-big*)
		(bad-step (spice-contract h niter) k))
	       ((fix:> niter *spice-good-step*)
		(careful-step (spice-contract h niter) k))
	       ((fix:> niter *spice-step-too-small*)
		(good-step h k))
	       ((fix:> niter *spice-order-too-small*)
		(good-step (spice-expand h niter) k))
	       (else
		(good-step (spice-expand h niter)
			   (min (fix:+ k 1) *gear-max-order*)))))
	((> err *gear-error-too-big*)			; Have to reduce step.
	 (let ((contract (expt (+ err *gear-protect*)
			       (/ -1.0 (exact->inexact (fix:+ k 1))))))
	   (cond ((< contract (* (exact->inexact k) *gear-decrease-order*))
		  ;;(write-line `(order-down: ,(max (- k 1) *gear-min-order*)))
		  (bad-step (* contract *contract-order* h)
			    (max (fix:- k 1) *gear-min-order*)))
		 (else (bad-step (* contract h) k)))))
	;; Step is acceptable.
	((<= wins (fix:* *gear-step-refractory-period* k)) ;do nothing
	 (good-step h k))
	(else
	 (let ((expand (expt (+ (/ err-accum (exact->inexact wins)) *gear-protect*)
			     (/ -1.0 (exact->inexact (fix:+ k 1))))))
	   (cond ((fix:> wins (fix:* *gear-order-refractory-period* k))
		  (careful-step (min (* *gear-damping* expand h)
				     (* *gear-max-step-increase* h))
				(min (fix:+ k 1) *gear-max-order*)))
		 ((and (< *gear-dead-zone-low* expand)
		       (< expand *gear-dead-zone-high*))
		  (good-step h k))
		 (else
		  (careful-step (min (* *gear-damping* expand h)
				     (* *gear-max-step-increase* h))
				k)))))))


(define (spice-expand h niter)
  (let* ((m (/ (- 1.0 *spice-step-expansion*)
	       (exact->inexact
		(fix:- *spice-step-too-small* *spice-order-too-small*))))
	 (b (- 1.0 (* m (exact->inexact *spice-step-too-small*)))))
    (* (+ (* m (exact->inexact niter)) b) h)))

(define (spice-contract h niter)
  (let* ((m (/ (- *spice-step-reduction* 1.0)
	       (exact->inexact
		(fix:- *spice-step-too-big* *spice-good-step*))))
	 (b (- 1.0 (* m (exact->inexact *spice-good-step*)))))
    (/ h (+ (* m (exact->inexact niter)) b))))

(define gear-wallp? false)

;;;       Stepsize and order control parameters

;;; The available orders are:

(define *gear-max-order* 4)		;hmmmmm
(define *gear-min-order* 1)


;;; Define a dead zone for the bang-bang controller

(define *gear-dead-zone-low* .9)

(define *gear-dead-zone-high* 1.1)


;;; Any error larger than this is unacceptable

(define *gear-error-too-big* 2.0)


;;; Acceptable steps are not even touched in the refractory period

(define *gear-step-refractory-period* 2)

(define *gear-order-refractory-period* 3)


;;; When step tries to shrink faster than this, try lower order.

(define *gear-decrease-order* .2)


;;; When order decreases, also cut step by this.

(define *contract-order* .5)


;;; To prevent step size from growing too fast.

(define *gear-damping* 1.0)

;;; On any one step, the step will not increase by more than this factor.

(define *gear-max-step-increase* 2.0)

;;; To prevent divide-by-zero errors in step-size adjust.

(define *gear-protect* 1e-20)


;;; Corrector must converge to within this fraction of the allowed
;;; local-truncation error.

(define *gear-fixed-point-margin* .1)	

;;; If it does not do so in *vector-fixed-point-iteration-loss*
;;; step size will be reduced by this factor

(define *gear-fixed-point-failure-contraction* 8.0) ;see SPICE

(define *spice-order-too-big*  12)

(define *spice-step-too-big*   10)

(define *spice-good-step*       6)

(define *spice-step-too-small*  4)

(define *spice-order-too-small* 2)


(define *spice-step-reduction* 8.0)	;SPICE uses 8

(define *spice-step-expansion* 2.0)

#|
;;; To generate gear-type correctors (and predictors!):

(define (gear-generator order deriv-index) 
  ;; deriv-index for corrector = 0, predictor = 1
  (let ((n (1+ order)))
    (let ((dtns (list-head '(dt1 0 dt3 dt4 dt5 dt6 dt7) n)))
      (let ((C (generate-vector n
		 (lambda (i)
		   (if (= i 0)
		       0
		       (* i (expt (list-ref dtns deriv-index)
				  (- i 1)))))))
	    (B (m:generate n n
		 (lambda (i j)
		   (if (= i 0)
		       1
		       (expt (list-ref dtns j) i))))))
      (let ((coeffs (g:simplify (m:rsolve C B)))
	    (tnames (list-head '(tn-1 tn-2 tn-3 tn-4 tn-5 tn-6) order)))
	(text/cselim
	 `(lambda (dt1 ts)		;dt1=step
	    (let* (,@(generate-list order
		        (lambda (i)
			  `(,(list-ref tnames i) (list-ref ts ,i))))
		   ,@(map (lambda (dtn-k+1 tn-k) `(,dtn-k+1 (- ,tn-k tn-1)))
			  (cddr dtns)
			  (cdr tnames)))
	      (list ,@(map expression (cdr coeffs)))))))))))


;;; For example, we can produce the standard stiffly-stable
;;;   integrators for equally-spaced intervals (see Gear.)

(pp (map (compose expression rcf:simplify (lambda (x) (* 'h x)))
     ((lambda->interpreted-generic-procedure (gear-generator 1 0))
      'h '(0))))
(1 -1)
;No value

(pp (map (compose expression rcf:simplify (lambda (x) (* 'h x)))
     ((lambda->interpreted-generic-procedure (gear-generator 2 0))
      'h (list 0 (- 'h)))))
(3/2 -2 1/2)
;No value

(pp (map (compose expression rcf:simplify (lambda (x) (* 'h x)))
     ((lambda->interpreted-generic-procedure (gear-generator 3 0))
      'h (list 0 (- 'h) (* -2 'h)))))
(11/6 -3 3/2 -1/3)
;No value

(pp (map (compose expression rcf:simplify (lambda (x) (* 'h x)))
     ((lambda->interpreted-generic-procedure (gear-generator 4 0))
      'h (list 0 (- 'h) (* -2 'h) (* -3 'h)))))
(25/12 -4 3 -4/3 1/4)
;No value

;;; More simply, for h=1:

(pp ((eval (gear-generator 4 0) generic-environment)
     '1 '(0 -1 -2 -3)))
(25/12 -4 3 -4/3 1/4)
;No value

(pp ((eval (gear-generator 5 0) generic-environment)
     '1 '(0 -1 -2 -3 -4)))
(137/60 -5 5 -10/3 5/4 -1/5)
;No value

;;; The preceeding is run in the symbolic environment to produce 
;;;  the next hairy frobs:

(define gc1
  (lambda (dt1 ts)
    (list (/ 1 dt1) (/ -1 dt1))))

(define gc2
  (lambda (dt1 ts)
    (let ((dt3 (- (list-ref ts 1) (list-ref ts 0))))
      (let ((V-32 (* dt1 dt3)) (V-31 (* -1 dt3)))
	(list
	 (/ (+ (* 2 dt1) V-31) (+ (expt dt1 2) (* V-31 dt1)))
	 (/ (+ dt1 V-31) V-32)
	 (/ (* -1 dt1) (+ V-32 (* -1 (expt dt3 2)))))))))

(define gc3
  (lambda (dt1 ts)
    (let ((tn-1 (list-ref ts 0)) (V-33 (expt dt1 2)))
      (let ((dt4 (- (list-ref ts 2) tn-1)) (dt3 (- (list-ref ts 1) tn-1)))
	(let ((V-42 (expt dt4 2))
	      (V-41 (expt dt3 2))
	      (V-38 (* -1 dt4))
	      (V-37 (* -1 dt3))
	      (V-36 (* dt3 dt4))
	      (V-35 (* dt1 dt4))
	      (V-34 (* dt1 dt3)))
	  (let ((V-40 (* V-36 dt1)) (V-39 (+ (* -1 V-33) V-35)))
	    (list
	     (/ (+ (* 3 V-33) (* V-34 -2) (* V-35 -2) V-36)
		(+ (expt dt1 3) (* V-37 V-33) (* V-38 V-33) (* V-35 dt3)))
	     (/ (+ V-39 V-34 (* V-37 dt4)) V-40)
	     (/ V-39
		(+ (* dt1 V-41)
		   (* V-34 V-38)
		   (* -1 (expt dt3 3))
		   (* V-41 dt4)))
	     (/ (+ V-33
		   (* V-34 -1))
		(+ V-40
		   (* -1 dt1 V-42)
		   (* V-37 V-42)
		   (expt dt4 3))))))))))

(define gc4
  (lambda (dt1 ts)
    (let ((V-63 (* -1 dt1))
	  (V-46 (* 2 dt1))
	  (tn-1 (list-ref ts 0))
	  (V-44 (expt dt1 2))
	  (V-43 (expt dt1 3)))
      (let ((dt5 (- (list-ref ts 3) tn-1))
	    (dt4 (- (list-ref ts 2) tn-1))
	    (dt3 (- (list-ref ts 1) tn-1))
	    (V-45 (* -3 V-44)))
	(let ((V-71 (expt dt5 3))
	      (V-69 (expt dt5 2))
	      (V-68 (expt dt4 3))
	      (V-66 (expt dt4 2))
	      (V-64 (expt dt3 2))
	      (V-62 (expt dt3 3))
	      (V-61 (+ (* -1 V-43) (* V-44 dt4)))
	      (V-58 (* dt1 dt5))
	      (V-56 (* dt1 dt4))
	      (V-53 (* dt3 dt5))
	      (V-52 (* -1 dt5))
	      (V-51 (* -1 dt4))
	      (V-50 (* -1 dt3))
	      (V-49 (* dt4 dt5))
	      (V-48 (* V-46 dt5))
	      (V-47 (* dt3 dt4)))
	  (let ((V-70 (* dt3 V-69))
		(V-67 (* V-50 V-56))
		(V-65 (* V-63 V-64))
		(V-59 (* V-58 dt3))
		(V-57 (* V-49 -1))
		(V-55 (+ V-43 (* V-50 V-44) (* V-52 V-44)))
		(V-54 (* V-49 dt1)))
	    (let ((V-60 (* V-59 dt4)))

	      (list
	       (/ (+ (* 4 V-43)
		     (* V-45 dt3)
		     (* V-45 dt4)
		     (* V-45 dt5)
		     (* V-46 V-47)
		     (* V-48 dt3)
		     (* V-48 dt4)
		     (* V-49 V-50))
		  (+ (expt dt1 4)
		     (* V-50 V-43)
		     (* V-51 V-43)
		     (* V-52 V-43)
		     (* V-47 V-44)
		     (* V-53 V-44)
		     (* V-49 V-44)
		     (* V-54 V-50)))
	       (/ (+ V-55
		     (* V-51 V-44)
		     (* V-56 dt3)
		     (* V-53 dt1)
		     V-54
		     (* V-57 dt3))
		  V-60)
	       (/ (+ V-61
		     (* V-44 dt5)
		     (* V-57 dt1))
		  (+ (* dt1 V-62)
		     (* V-65 dt4)
		     (* V-65 dt5)
		     V-60
		     (* -1 (expt dt3 4))
		     (* V-62 dt4)
		     (* V-62 dt5)
		     (* V-52 V-64 dt4)))
	       (/ (+ V-55 V-59)
		  (+ (* dt1 dt3 V-66)
		     (* V-67 dt5)
		     (* V-63 V-68)
		     (* V-58 V-66)
		     (* V-50 V-68)
		     (* V-53 V-66)
		     (expt dt4 4)
		     (* V-52 V-68)))
	       (/ (+ V-61 (* V-44 dt3) V-67)
		  (+ (* V-56 V-53)
		     (* V-70 V-63)
		     (* V-51 dt1 V-69)
		     (* dt1 V-71)
		     (* V-70 V-51)
		     (* dt3 V-71)
		     (* dt4 V-71)
		     (* -1 (expt dt5 4))))))))))))

#|
(pp (gear-generator 5 0))
|#

(define gc5
  (lambda (dt1 ts)
    (let ((V-569 (* -1 dt1)) (tn-1 (list-ref ts 0)))
      (let ((dt6 (- (list-ref ts 4) tn-1))
	    (dt5 (- (list-ref ts 3) tn-1))
	    (dt4 (- (list-ref ts 2) tn-1))
	    (dt3 (- (list-ref ts 1) tn-1)))
	(let ((V-585 (expt dt6 2))
	      (V-583 (expt dt6 4))
	      (V-582 (expt dt6 3))
	      (V-581 (* dt5 dt4))
	      (V-573 (* dt3 dt4))
	      (V-572 (* dt1 dt3))
	      (V-562 (* -1 dt4))
	      (V-560 (* -1 dt6))
	      (V-558 (* -1 dt3))
	      (V-557 (* -1 dt5))
	      (V-554 (+ dt5 dt6))
	      (V-550 (* -2 dt5))
	      (V-549 (* dt6 dt5))
	      (V-548 (+ (* 3 dt5) (* 3 dt6))))
	  (let ((V-586 (+ (* V-557 V-585) V-582))
		(V-584 (+ (* V-582 dt5) (* -1 V-583)))
		(V-578 (+ dt5 V-560))
		(V-576 (+ V-557 dt6))
		(V-571 (* V-558 dt4))
		(V-567 (+ V-562 V-560))
		(V-566 (* V-549 V-562))
		(V-564 (* V-557 dt6))
		(V-563 (+ V-557 V-560))
		(V-559 (+ V-557 V-558 dt1))
		(V-556 (+ dt4 V-554))
		(V-555 (+ (* V-554 dt4) V-549))
		(V-552 (* V-549 dt4))
		(V-551 (* V-550 dt6)))
	    (let ((V-587 (+ V-584 (* V-586 dt4)))
		  (V-579 (* V-578 (expt dt5 3)))
		  (V-577 (* V-576 (expt dt5 2)))
		  (V-575 (* (+ (* (+ V-554 V-562) dt4) V-564) (expt dt4 2)))
		  (V-574 (+ (* (+ V-563 dt4) dt4) V-549))
		  (V-570 (+ V-556 V-569))
		  (V-568 (+ V-567 V-557))
		  (V-565 (+ (* V-563 dt4) V-564))
		  (V-561 (+ V-559 V-560))
		  (V-553 (* V-552 dt3)))
	      (let ((V-580 (+ (* V-577 dt4) V-579)))
		(list
		 (/ (+ (* (+ (* (+ (* (+ (* 5 dt1)
					 (* -4 dt3)
					 (* -4 dt4)
					 (* -4 dt5)
					 (* -4 dt6))
				      dt1)
				   (* (+ V-548 (* 3 dt4)) dt3)
				   (* V-548 dt4)
				   (* V-549 3))
				dt1)
			     (* (+ (* (+ V-550 (* -2 dt6)) dt4) V-551) dt3)
			     (* V-551 dt4))
			  dt1)
		       V-553)
		    (* (+ (* (+ (* (+ V-555
				      (* V-556 dt3)
				      (* (+ V-561 V-562) dt1)
				      ) dt1)
			  (* V-565 dt3)
			  V-566)
		       dt1)
		      V-553)
		     dt1))
		 (/ (+ (* (+ (* (+ V-565 (* V-568 dt3) (* (+ V-570 dt3) dt1))
				dt1)
		       (* V-555 dt3)
		       V-552)
		    dt1)
		   (* V-571 V-549))
		  (* V-572 V-552))
		 (/ (* (+ (* (+ V-565 (* V-570 dt1)) dt1) V-552) dt1)
		    (+ (* V-572
			  (+ (* (+ V-555 (* (+ V-568 dt3) dt3)) dt3) V-566))
		       (* (+ (* (+ V-565 (* (+ V-556 V-558) dt3)) dt3) V-552)
			  (expt dt3 2))))
		 (/ (* (+ (* (+ (* V-561 dt1) (* V-554 dt3) V-549) dt1)
			  (* V-549 V-558))
		       dt1)
		    (+ (* (+ (* V-573 V-574) V-575) dt1)
		       (* V-575 dt3)
		       (* V-574 (expt dt4 3))))
		 (/ (* (+ (* (+ (* (+ V-569 dt3 dt4 dt6) dt1)
				(* V-567 dt3)
				(* V-562 dt6))
			     dt1)
			  (* V-573 dt6))
		       dt1)
		    (+ (* (+ V-580 (* (+ (* V-581 V-578) V-577) dt3)) dt1)
		       (* V-580 dt3)
		       (* V-579 dt4)
		       (* V-576 (expt dt5 4))))
		 (/ (* (+ (* (+ (* (+ V-559 V-562) dt1)
				(* (+ dt4 dt5) dt3) V-581) dt1)
		    (* V-571 dt5))
		   dt1)
		  (+ (* (+ V-587
			   (* (+ V-586 (* (+ V-549 (* -1 V-585)) dt4)) dt3))
		      dt1)
		   (* V-587 dt3)
		   (* V-584 dt4)
		   (* V-557 V-583)
		   (expt dt6 5))))))))))))
|#

;;; A bit of further hacking yields:

;;; (pp (flonumize (gear-generator 1 0)))

(define (gc1 dt1 ts)
  (list (flo:/ 1. dt1) (flo:/ -1. dt1)))

;;; (pp (flonumize (gear-generator 2 0)))

(define (gc2 dt1 ts)
  (let ((dt3 (flo:- (list-ref ts 1) (list-ref ts 0))))
    (let ((V-106 (flo:* dt1 dt3)) (V-105 (flo:* -1. dt3)))
      (list (flo:/ (flo:+ (flo:* 2. dt1) V-105)
		   (flo:+ (flo:* dt1 dt1) (flo:* V-105 dt1)))
	    (flo:/ (flo:+ dt1 V-105) V-106)
	    (flo:/ (flo:* -1. dt1)
		   (flo:+ V-106 (flo:* -1. (flo:* dt3 dt3))))))))

;;; (pp (flonumize (gear-generator 3 0)))

(define (gc3 dt1 ts)
  (let ((tn-1 (list-ref ts 0)) (V-86 (flo:* dt1 dt1)))
    (let ((dt4 (flo:- (list-ref ts 2) tn-1))
	  (dt3 (flo:- (list-ref ts 1) tn-1)))
      (let ((V-95 (flo:* dt4 dt4))
	    (V-94 (flo:* dt3 dt3))
	    (V-91 (flo:* dt4 -1.))
	    (V-90 (flo:* dt3 -1.))
	    (V-89 (flo:* dt3 dt4))
	    (V-88 (flo:* dt1 dt4))
	    (V-87 (flo:* dt1 dt3)))
	(let ((V-93 (flo:* V-89 dt1)) (V-92 (flo:+ (flo:* -1. V-86) V-88)))
	  (list (flo:/ (flo:+ (flo:* 3. V-86)
			      (flo:+ (flo:+ (flo:* V-87 -2.)
					    (flo:* V-88 -2.))
				     V-89))
		       (flo:+ (flo:* V-86 dt1)
			      (flo:+ (flo:+ (flo:* V-90 V-86)
					    (flo:* V-91 V-86))
				     (flo:* V-88 dt3))))
		(flo:/ (flo:+ (flo:+ V-92 (flo:* V-90 dt4)) V-87) V-93)
		(flo:/ V-92
		       (flo:+ (flo:* dt1 V-94)
			      (flo:+ (flo:* V-87 V-91)
				     (flo:+ (flo:* -1. (flo:* V-94 dt3))
					    (flo:* V-94 dt4)))))
		(flo:/ (flo:+ V-86 (flo:* V-87 -1.))
		       (flo:+ V-93
			      (flo:+ (flo:+ (flo:* -1. (flo:* dt1 V-95))
					    (flo:* V-90 V-95))
				     (flo:* V-95 dt4))))))))))

;;;(pp (flonumize (gear-generator 4 0)))

(define (gc4 dt1 ts)
  (let ((V-137 (flo:* dt1 -1.))
	(V-119 (flo:* 2. dt1))
	(tn-1 (list-ref ts 0))
	(V-114 (flo:* dt1 dt1)))
    (let ((dt5 (flo:- (list-ref ts 3) tn-1))
	  (dt4 (flo:- (list-ref ts 2) tn-1))
	  (dt3 (flo:- (list-ref ts 1) tn-1))
	  (V-115 (flo:* V-114 dt1)))
      (let ((V-142 (flo:* dt5 dt5))
	    (V-139 (flo:* dt4 dt4))
	    (V-135 (flo:* dt3 dt3))
	    (V-131 (flo:* dt5 dt1))
	    (V-130 (flo:* dt1 dt4))
	    (V-126 (flo:* dt5 dt3))
	    (V-125 (flo:* dt5 -1.))
	    (V-124 (flo:* dt4 -1.))
	    (V-123 (flo:* dt3 -1.))
	    (V-122 (flo:* dt5 dt4))
	    (V-121 (flo:* V-119 dt5))
	    (V-120 (flo:* dt4 dt3))
	    (V-118 (flo:* V-114 dt5))
	    (V-117 (flo:* V-114 dt4))
	    (V-116 (flo:* V-114 dt3)))
	(let ((V-144 (flo:* V-142 dt5))
	      (V-143 (flo:* dt3 V-142))
	      (V-141 (flo:* V-139 dt4))
	      (V-140 (flo:* V-123 V-130))
	      (V-138 (flo:* V-135 V-137))
	      (V-136 (flo:* V-135 dt3))
	      (V-134 (flo:+ V-117 (flo:* -1. V-115)))
	      (V-132 (flo:* V-131 dt3))
	      (V-129 (flo:* V-122 -1.))
	      (V-128 (flo:+ (flo:+ (flo:* V-125 V-114) (flo:* V-123 V-114))
			    V-115))
	      (V-127 (flo:* V-122 dt1)))
	  (let ((V-133 (flo:* V-132 dt4)))

	    (list (flo:/ (flo:+ (flo:+ (flo:+ (flo:* 4. V-115)
					      (flo:* V-116 -3.))
				       (flo:+ (flo:* V-117 -3.)
					      (flo:* V-118 -3.)))
				(flo:+ (flo:+ (flo:* V-119 V-120)
					      (flo:* V-121 dt3))
				       (flo:+ (flo:* V-121 dt4)
					      (flo:* V-122 V-123))))
			 (flo:+ (flo:+ (flo:+ (flo:* V-114 V-114)
					      (flo:* V-123 V-115))
				       (flo:+ (flo:* V-124 V-115)
					      (flo:* V-125 V-115)))
				(flo:+ (flo:+ (flo:* V-120 V-114)
					      (flo:* V-126 V-114))
				       (flo:+ (flo:* V-122 V-114)
					      (flo:* V-127 V-123)))))
		  (flo:/ (flo:+ (flo:+ V-128
				       (flo:* V-129 dt3))
				(flo:+ (flo:+ V-127
					      (flo:* V-126 dt1))
				       (flo:+ (flo:* V-130 dt3)
					      (flo:* V-124 V-114))))
			 V-133)
		  (flo:/ (flo:+ (flo:+ V-134 (flo:* V-129 dt1)) V-118)
			 (flo:+ (flo:+ (flo:+ (flo:* dt1 V-136)
					      (flo:* V-138 dt4))
				       (flo:+ (flo:* V-138 dt5)
					      V-133))
				(flo:+ (flo:+ (flo:* -1. (flo:* V-135 V-135))
					      (flo:* V-136 dt4))
				       (flo:+ (flo:* V-136 dt5)
					      (flo:* V-125
						     (flo:* dt4 V-135))))))
		  (flo:/ (flo:+ V-128 V-132)
			 (flo:+ (flo:+ (flo:+ (flo:* dt1 (flo:* dt3 V-139))
					      (flo:* V-140 dt5))
				       (flo:+ (flo:* V-137 V-141)
					      (flo:* V-131 V-139)))
				(flo:+ (flo:+ (flo:* V-123 V-141)
					      (flo:* V-126 V-139))
				       (flo:+ (flo:* V-139 V-139)
					      (flo:* V-125 V-141)))))
		  (flo:/ (flo:+ (flo:+ V-134 V-140) V-116)
			 (flo:+ (flo:+ (flo:+ (flo:* V-130 V-126)
					      (flo:* V-143 V-137))
				       (flo:+ (flo:* V-124 (flo:* V-142 dt1))
					      (flo:* dt1 V-144)))
				(flo:+ (flo:+ (flo:* V-143 V-124)
					      (flo:* dt3 V-144))
				       (flo:+ (flo:* dt4 V-144)
					      (flo:* -1.
						     (flo:* V-142
							    V-142)))))))))))))

(define gear-correctors
  (vector 0 gc1 gc2 gc3 gc4 ;gc5
	  ))

;;; The following are the coefficients of the LTE term for fixed
;;; stepsize gear correctors.

(define gear-corrector-errors
  (vector 0 -1/2 -2/9 -3/22 -12/125 -10/137 -60/1029))

(define (gear-error k)
  (let ((C (vector-ref gear-corrector-errors k))
	(k+1 (fix:+ k 1))
	(dfk 1))			;ugh!
    (lambda (h)
      (real:* C (expt h k+1) dfk))))

;;; Ugbletchreous Lagrange polynomial extrapolators.

(define (extrap1 xs t)
  (let ((s1 (car xs)))
    (generate-vector (vector-length s1)
		     (lambda (i)
		       (if (fix:= i 0)
			   t
			   (vector-ref s1 i))))))

(define (lag1 t1 t)
  (lambda (x1) x1))


(define (extrap2 xs t)
  (let* ((s1 (car xs)) (r1 (cdr xs))
	 (s2 (car r1)))
    (let ((e (lag2 (vector-ref s1 0)
		   (vector-ref s2 0)
		   t)))
      (generate-vector (vector-length s1)
		       (lambda (i)
			 (e (vector-ref s1 i)
			    (vector-ref s2 i)))))))

(define (lag2 t1 t2 t)
  (let ((t21 (- t2 t1)))
    (let ((s89 (- t t1)) (s88 (- t t2)))
      (lambda (x1 x2)
	(/ (- (* x2 s89) (* x1 s88)) t21)))))


(define (extrap3 xs t)
  (let* ((s1 (car xs)) (r1 (cdr xs))
	 (s2 (car r1)) (r2 (cdr r1))
	 (s3 (car r2)))
    (let ((e (lag3 (vector-ref s1 0)
		   (vector-ref s2 0)
		   (vector-ref s3 0)
		   t)))
      (generate-vector (vector-length s1)
		       (lambda (i)
			 (e (vector-ref s1 i)
			    (vector-ref s2 i)
			    (vector-ref s3 i)))))))

(define (lag3 t1 t2 t3 t)
  (let ((t32 (- t3 t2)) (t21 (- t2 t1)) (t31 (- t3 t1)))
    (let ((s86 (- t t1)) (s85 (- t t2)) (s84 (- t t3)))
      (lambda (x1 x2 x3)
	(/ (- (* (/ (- (* x3 s85) (* x2 s84))
		    t32)
		 s86)
	      (* (/ (- (* x2 s86) (* x1 s85))
		    t21)
		 s84))
	   t31)))))

(define (extrap4 xs t)
  (let* ((s1 (car xs)) (r1 (cdr xs))
	 (s2 (car r1)) (r2 (cdr r1))
	 (s3 (car r2)) (r3 (cdr r2))
	 (s4 (car r3)))
    (let ((e (lag4 (vector-ref s1 0)
		   (vector-ref s2 0)
		   (vector-ref s3 0)
		   (vector-ref s4 0)
		   t)))
      (generate-vector (vector-length s1)
		       (lambda (i)
			 (e (vector-ref s1 i)
			    (vector-ref s2 i)
			    (vector-ref s3 i)
			    (vector-ref s4 i)))))))

(define (lag4 t1 t2 t3 t4 t)
  (let ((t41 (- t4 t1)) (t31 (- t3 t1))
	(t21 (- t2 t1)) (t42 (- t4 t2))
	(t32 (- t3 t2)))
    (let ((s96 (- t t1)) (s95 (- t t2))
	  (s94 (- t t3)) (s93 (- t t4)))
      (lambda (x1 x2 x3 x4)
	(let ((s97 (/ (- (* x3 s95) (* x2 s94)) t32)))
	  (/ (- (* (/ (- (* (/ (- (* x4 s94) (* x3 s93))
			       (- t4 t3))
			    s95)
			 (* s97 s93))
		      t42)
		   s96)
		(* (/ (- (* s97 s96)
			 (* (/ (- (* x2 s96) (* x1 s95)) t21)
			    s94))
		      t31)
		   s93))
	     t41))))))


(define (extrap5 xs t)
  (let* ((s1 (car xs)) (r1 (cdr xs))
	 (s2 (car r1)) (r2 (cdr r1))
	 (s3 (car r2)) (r3 (cdr r2))
	 (s4 (car r3)) (r4 (cdr r3))
	 (s5 (car r4)))
    (let ((e (lag5 (vector-ref s1 0)
		   (vector-ref s2 0)
		   (vector-ref s3 0)
		   (vector-ref s4 0)
		   (vector-ref s5 0)
		   t)))
      (generate-vector (vector-length s1)
		       (lambda (i)
			 (e (vector-ref s1 i)
			    (vector-ref s2 i)
			    (vector-ref s3 i)
			    (vector-ref s4 i)
			    (vector-ref s5 i)))))))  

(define (lag5 t1 t2 t3 t4 t5 t)
  (let ((s103 (- t t1))
	(s102 (- t t2))
	(s101 (- t t3))
	(s100 (- t t4))
	(s99 (- t t5)))
    (let ((t51 (- t5 t1))
	  (t41 (- t4 t1))
	  (t31 (- t3 t1))
	  (t21 (- t2 t1))
	  (t52 (- t5 t2))
	  (t53 (- t5 t3))
	  (t54 (- t5 t4))
	  (t42 (- t4 t2))
	  (t43 (- t4 t3))
	  (t32 (- t3 t2)))
      (lambda (x1 x2 x3 x4 x5)
	(let ((s105 (/ (- (* x3 s102) (* x2 s101)) t32))
	      (s104 (/ (- (* x4 s101) (* x3 s100)) t43)))
	  (let ((s106 (/ (- (* s104 s102) (* s105 s100)) t42)))
	    (/ (- (* (/ (- (* (/ (- (* (/ (- (* x5 s100) (* x4 s99))
					  t54)
				       s101)
				    (* s104 s99))
				 t53)
			      s102)
			   (* s106 s99))
			t52)
		     s103)
		  (* (/ (- (* s106 s103)
			   (* (/ (- (* s105 s103)
				    (* (/ (- (* x2 s103) (* x1 s102))
					  t21)
				       s101))
				 t31)
			      s100))
			t41)
		     s99))
	       t51)))))))


(define lagrange-extrapolators
  (vector 0 extrap1 extrap2 extrap3 extrap4 extrap5))


