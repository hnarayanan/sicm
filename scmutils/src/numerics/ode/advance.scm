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

;;; The advance-generator is used with 1-step adaptive integrators to
;;;  to advance a state by a given increment in the independent
;;;  variable, in the face of variable-stepsize advancers.

(declare (usual-integrations))

#| For examples

((advance-generator
  ((quality-control rk4 4)		;integration method
   (lambda (v) v)			;x' = x
   .0001))				;error tolerated
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

((advance-generator
  (bulirsch-stoer-lisptran		;integrator
   (lambda (vin vout)			;x'= x
     (vector-set! vout 0
		  (vector-ref vin 0)))
   1					;1 dimension
   .0001))				;error tolerated
 #(1.0)
 1.0
 0.1
 0.5
 (lambda (ns dt h cont)
   (pp (list dt ns))
   (cont))
 (lambda (ns dt sdt)
   ;; assert ns = #(2.718...)
   ;; assert dt = 1.000...+-
   (list ns dt sdt)))
|#

(define (advance-generator advancer)
  (define (advance start-state goal-increment h-suggested max-h 
		   continue done)
    ;; done = (lambda (end-state current-increment h-suggested) ... )
    ;; continue = (lambda (state current-increment h-taken next)
    ;;     where next = (lambda () ...))
    (let lp ((state start-state)
	     (current-increment 0.0)
	     (h (min-step-size goal-increment h-suggested max-h))
             (h-suggested h-suggested))
      (if advance-wallp?
	  (pp `(advance: ,current-increment ,state ,h)))
      (if (close-enuf? goal-increment current-increment
		       *independent-variable-tolerance*)
	  (done state current-increment h-suggested)
	  (continue state current-increment h
	    (lambda ()
	      (advancer state h
		(lambda (new-state step-obtained h-suggested)
		  (let ((ndt (+ current-increment step-obtained)))
		    (lp new-state
                        ndt
                        (min-step-size (- goal-increment ndt)
                                       h-suggested
                                       max-h)
                        h-suggested)))))))))
  advance)


(define advance-wallp? false)

(define-integrable (first-with-sign-of-second x y)
  (if (> y 0.0)
      (abs x)
      (- (abs x))))

(define (min-step-size step-required h-suggested max-h)
  (let ((h-allowed (min (abs h-suggested) (abs max-h))))
    (if (<= (abs step-required) h-allowed)
	step-required
	(first-with-sign-of-second h-allowed step-required))))


(define *independent-variable-tolerance* 1.0e-100)

#| For making a stream of states

((stream-of-states
  (advance-generator
   ((quality-control rk4 4)		;integration method
    (lambda (v) v)			;x' = x
    .0001)))				;error tolerated
 #(1.0)					;initial state (at t = t0)
 1.0					;proceed to t = t0 + 1
 0.1					;first step no larger than .1
 0.5					;no step larger than .5
 )
|#

(define (stream-of-states advance)
  (lambda (state step-required h-suggested max-h)
    (advance state step-required h-suggested max-h
	     (lambda (state step-achieved h cont)
	       (cons-stream state (cont)))
	     (lambda (state step-achieved h-suggested)
	       (cons-stream state the-empty-stream)))))


;;; Utilities for ODE integrators.

(define (vector-fixed-point-with-failure update start measure succeed fail)
  ;; update  = (lambda (x cont)
                 ;; cont = (lambda (nx fx) ...)
                 ;; ...)
  ;; succeed = (lambda (nx fx count) ...)
  ;; fail    = (lambda () ...)
  (let improve ((last-value start) (count 1))
    (if (> (maxnorm last-value) *vector-fixed-point-ridiculously-large*)
	(fail last-value last-value)
	(update last-value
		(lambda (value fvalue)
		  (let ((d (measure value last-value)))
		    (if *fixed-point-wallpaper*
			(write-line
			 `(vector-fixed-point ,count d ,d ,start ,value)))
		    (if (< d 2.0)
			(succeed value fvalue count)
			(if (fix:> count *vector-fixed-point-iteration-loss*)
			    (fail value fvalue)
			    (improve value (1+ count))))))))))



(define *vector-fixed-point-iteration-loss* 20)

(define *vector-fixed-point-ridiculously-large* 1.0e50)

(define *fixed-point-wallpaper* false)

;;; Error measures

;;; Can specify for each component the breakpoints between relative
;;; error and absolute error measure and can specify the weights.

(define (vector-metric summarize accumulate each-component)
  (define (the-metric v1 v2)
    (let ((n (vector-length v1)))
      (assert (fix:= (vector-length v2) n))
      (let lp ((i 0) (accumulation 0.0))
	(if (fix:= i n)
	    (summarize accumulation n)
	    (lp (fix:1+ i)
		(accumulate (each-component (vector-ref v1 i)
					    (vector-ref v2 i)
					    i)
			    accumulation))))))
  the-metric)

(define *norm-breakpoint* 1e-10)

(define (lp-norm p #!optional tolerance breakpoints weights)
  (if (default-object? tolerance)
      (set! tolerance *machine-epsilon*))
  (if (default-object? breakpoints)
      (set! breakpoints (lambda (i) *norm-breakpoint*)))
  (if (default-object? weights)
      (set! weights (lambda (i) 1.0)))
  (vector-metric (lambda (a n)
		   (* (expt a (/ 1 p))
		      (/ 2
			 (* (n:sigma weights 0 (fix:- n 1))
			    tolerance))))
		 +
		 (lambda (x y i)
		   (* (expt (/ (magnitude (- x y))
			       (+ (+ (magnitude x) (magnitude y))
				  (* 2.0 (breakpoints i))))
			    p)
		      (weights i)))))

(define (max-norm #!optional tolerance breakpoints weights)
  (if (default-object? tolerance)
      (set! tolerance *machine-epsilon*))
  (if (default-object? breakpoints)
      (set! breakpoints (lambda (i) *norm-breakpoint*)))
  (if (default-object? weights)
      (set! weights (lambda (i) 1.0)))
  (vector-metric (lambda (a n)
		   (* a (/ 2 tolerance)))
		 max
		 (lambda (x y i)
		   (* (/ (magnitude (- x y))
			 (+ (+ (magnitude x) (magnitude y))
			    (* 2.0 (breakpoints i))))
		      (weights i)))))

(define (parse-error-measure tolerance-specification #!optional multiplier)
  (if (default-object? multiplier) (set! multiplier 1.0))
  (cond ((number? tolerance-specification) ;uniform relative error -- scale = 1
	 (max-norm (* multiplier tolerance-specification)))
	((procedure? tolerance-specification) ;arbitrary user-supplied procedure
	 tolerance-specification)
	(else
	 (error "Unknown tolerance specification -- PARSE-ERROR-MEASURE"))))



;;; For integrators that need a partial Jacobian, we need to be able
;;; to clip and pad vectors.

;;; A dimension is either single, in which case no clipping and
;;; padding is necessary, or it is a triplet of 
;;;   (state-size start-index end-index)
;;; such that the Jacobian only applies to the coordinates from
;;; start-index (inclusive) to end-index (exclusive).

(define (vector-clipper dimension)
  (cond ((number? dimension)
	 (lambda (x) x))
	((list? dimension)
	 (let ((start (cadr dimension)) (end (caddr dimension)))
	   (lambda (v) (subvector v start end))))
	(else (error "Bad dimension -- VECTOR-CLIPPER" dimension))))

(define (vector-padder dimension)
  (cond ((number? dimension)
	 (lambda (x) x))
	((list? dimension)
	 (let* ((state-size (car dimension))
		(start (cadr dimension))
		(end (caddr dimension))
		(j-size (fix:- end start)))
	   (lambda (v)
	     (make-initialized-vector state-size
	       (lambda (i)
		 (if (and (or (fix:< start i)
			      (fix:= start i))
			  (fix:< i end))
		     (vector-ref v (fix:- i start))
		     0.0))))))
	(else (error "Bad dimension -- VECTOR-PADDER" dimension))))

(define (J-dimension dimension)
  (cond ((number? dimension) dimension)
	((list? dimension) (fix:- (caddr dimension) (cadr dimension)))
	(else (error "Bad dimension -- J-DIMENSION" dimension))))




(define integrator-table (make-table "integrator-table" assq))

(define (add-integrator! name maker-procedure needs)
  (adjoin-to-list! name integrator-table 'integrators)
  (put! integrator-table maker-procedure name 'maker)
  (put! integrator-table needs name 'needs))
	
