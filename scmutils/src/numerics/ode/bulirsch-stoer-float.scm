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

;;;; Bulirsch-Stoer integration: Send bug reports to gjs@mit.edu
;;;    Ideas from Jack Wisdom, from Michel Henon, from B&S

#|
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
 0.5					;no step larger than .5
 (lambda (ns dt h cont)
   (pp (list dt ns))
   (cont))
 (lambda (ns dt sdt)
   ;; assert ns = #(2.718...)
   ;; assert dt = 1.000...+-
   (list ns dt sdt)))
(0. #(1.))
(.1 #(1.1051708858929685))
(.25 #(1.2840251054195329))
(.47500000000000003 #(1.6080138082200066))
(.8125 #(2.2535342510080656))
;Value: (#(2.7182794600110927) 1. .28125)

((advance-generator
  (bulirsch-stoer-lisptran		;integrator
   (lambda (vin vout)
     (vector-set! vout 0 1.0)
     (vector-set! vout 1 (flo:- 0.0 (vector-ref vin 2)))
     (vector-set! vout 2  (vector-ref vin 1)))
   3					;3 dimensions
   1e-12))				;error tolerated
 #(0.0 1.0 0.0)
 2pi
 0.1
 1.0					;no step larger than 1.0
 (lambda (ns dt h cont)
   (pp (list dt ns))
   (cont))
 (lambda (ns dt sdt)
   ;; assert ns = #(2.718...)
   ;; assert dt = 1.000...+-
   (list ns dt sdt)))
(0. #(0. 1. 0.))
(.1 #(.1 .9950041652780256 .09983341664682824))
(.25 #(.25 .9689124217106447 .24740395925452305))
(.47500000000000003 #(.4750000000000001 .8892927216231684 .4573384471789555))
(.8125 #(.8125 .6876855622205039 .7260086552607131))
(1.31875 #(1.31875 .2493861513251363 .9684041240759129))
...
(5.328125 #(5.328125 .5775595272329195 -.8163485729163036))
;Value: (#(6.283185307179586 .9999999999999994 1.3276830294967203e-15)
;;;;     6.283185307179586 1.4325904607693793)


(define (f x) (sin (/ 1.0 x)))
;Value: f

((advance-generator
  (bulirsch-stoer-lisptran		;integrator
   (lambda (vin vout)
     (let ((x (vector-ref vin 0)))
       (vector-set! vout 0 1.0)
       (vector-set! vout 1
		    (flo:- 0.0
			   (flo:/ (flo:cos (flo:/ 1.0 x))
				  (flo:* x x))))))
   2
   1e-14))				;error tolerated
 (vector -2.0 (f -2.0))
 1.9
 0.1
 1.0
 (lambda (ns dt h cont)
   (pp (list dt ns))
   (cont))
 (lambda (ns dt sdt)
   ;; assert ns = #(2.718...)
   ;; assert dt = 1.000...+-
   (list ns dt sdt)))
(0. #(-2. -.479425538604203))
(.1 #(-1.9 -.5023511546035125))
(.25 #(-1.75 -.5408342133588315))
(.47500000000000003 #(-1.525 -.6097441128783215))
(.8125 #(-1.1875 -.7460466536513232))
(1.31875 #(-.6812499999999999 -.9947098054628543))
(1.5465625 #(-.45343749999999994 -.8053211890895082))
(1.6319921875 #(-.3680078124999999 -.4116456228081236))
(1.76013671875 #(-.2398632812499999 .8559828752025934))
(1.8178017578125 #(-.18219824218749991 .7136241717900977))
(1.86970029296875 #(-.13029970703124993 -.9839567688364803))
;Value: (#(-.10000000000000009 .5440211108893656) 1.9 2.7269736328124856e-2)
|#

(declare (usual-integrations))

(declare (integrate-operator for less-than))

(define (for initial test increment to-do)
  (declare (integrate initial test increment to-do))
  (let loop ((x initial))
    (declare (integrate x))
    (if (test x)
      (begin (to-do x)
	     (loop (increment x)))
      'done)))

(define (less-than n)
  (declare (integrate n))
  (lambda (i)
    (declare (integrate i))
    (fix:< i n)))

(define *max-tableau-depth*)
(define *max-tableau-width*)
(define bulirsch-stoer-steps)
(define bulirsch-stoer-magic-vectors)

(define (bulirsch-stoer-setup max-depth max-width)
  (define (bsi n)
    (cons-stream (expt 2 (+ n 1))
		 (cons-stream (* 3 (expt 2 n))
			      (bsi (+ n 1)))))
  (set! *max-tableau-depth* max-depth)
  (set! *max-tableau-width* max-width)
  (let ((bulirsch-stoer-integers (cons-stream 1 (bsi 0))))
    (pp (stream-head bulirsch-stoer-integers max-depth))
    (set! bulirsch-stoer-steps
	  (list->vector
	   (map (lambda (x) (fix:* 2 x))
		(stream-head bulirsch-stoer-integers max-depth))))
    (set! bulirsch-stoer-magic-vectors
	  (make-initialized-vector *max-tableau-depth*
	    (lambda (m)
	      (flo:make-initialized-vector (min m *max-tableau-width*)
		(lambda (k)
		  (exact->inexact
		   (square (/ (stream-ref bulirsch-stoer-integers m)
			      (stream-ref bulirsch-stoer-integers
					  (fix:- m (fix:1+ k)))))))))))
    'done))

(bulirsch-stoer-setup 10 6)
;;; (1 2 3 4 6 8 12 16 24 32)

#|
;; bulirsch-stoer-integers = #(1 repeated{2^n+1 3*2^n})

(define bulirsch-stoer-integers
  #(1 2 3 4 6 8 12 16 24 32 48 64 96))	       
|#

(define-integrable (vector-copy-into-vector dim v1 v2)
  (for 0 (less-than dim) fix:1+
       (lambda (i)
	 (declare (integrate i))
	 (vector-set! v2 i (vector-ref v1 i)))))

(define-integrable (flo:vector-copy-into-vector dim v1 v2)
  (for 0 (less-than dim) fix:1+
       (lambda (i)
	 (declare (integrate i))
	 (flo:vector-set! v2 i (flo:vector-ref v1 i)))))

(define-integrable (flo:vector-copy source)
  (flo:make-initialized-vector (flo:vector-length source)
			       (lambda (i)
				 (declare (integrate i))
				 (flo:vector-ref source i))))


(define-integrable (c*v+v dim c v1 v2 ans)
  (for 0 (less-than dim) fix:1+
       (lambda (i)
	 (declare (integrate i))
	 (flo:vector-set! ans i
			  (flo:+ (flo:* c (flo:vector-ref v1 i))
				 (flo:vector-ref v2 i))))))

(define-integrable (c*v+v+v*c dim c1 v1 v2 v3 c2 ans)
  (for 0 (less-than dim) fix:1+
       (lambda (i)
	 (declare (integrate i))
	 (flo:vector-set! ans i
		      (flo:* c2
			     (flo:+
			      (flo:+ (flo:* c1 (flo:vector-ref v1 i))
				     (flo:vector-ref v2 i))
			      (flo:vector-ref v3 i)))))))


(define-integrable (vector-copy-into-floating-vector dim v1 v2)
  (for 0 (less-than dim) fix:1+
       (lambda (i)
	 (declare (integrate i))
	 (flo:vector-set! v2 i (->flonum (vector-ref v1 i))))))

(define-integrable (floating-vector-copy-into-vector dim v1 v2)
  (for 0 (less-than dim) fix:1+
       (lambda (i)
	 (declare (integrate i))
	 (vector-set! v2 i (flo:vector-ref v1 i)))))

(define vector-Gragg
  (lambda (g dim)
    (let ((temp0 (flo:make-vector dim 0.0))
	  (temp1 (flo:make-vector dim 0.0))
	  (temp2 (flo:make-vector dim 0.0))
	  (temp3 (flo:make-vector dim 0.0)))
      (let ((g$y0 temp0)
	    (eta_1 temp1)
	    (eta_j+2 temp1)
	    (eta_j+1 temp2)
	    (g$eta_j temp3)
	    (g$eta_j+1 temp3))
	(lambda (y0 HH)
	  (g y0 g$y0)
	  (lambda (n yn)
	    (let* ((h (flo:/ HH (exact->inexact n)))
		   (2h (flo:* 2.0 h)))
	      ;; Fortran
	      (c*v+v dim h g$y0 y0 eta_1)
	      (let lp ((j 2) (eta_j-1 y0) (eta_j eta_1))
		(if (fix:< j n)
		    (begin (g eta_j g$eta_j)
			   (c*v+v dim 2h g$eta_j eta_j-1 eta_j+1)
			   (g eta_j+1 g$eta_j+1)
			   (c*v+v dim 2h g$eta_j+1 eta_j eta_j+2)
			   (lp (fix:+ j 2) eta_j+1 eta_j+2))
		    (begin (g eta_j g$eta_j)
			   (c*v+v dim 2h g$eta_j eta_j-1 eta_j+1)
			   (g eta_j+1 g$eta_j+1)
			   (c*v+v+v*c dim h g$eta_j+1 eta_j eta_j+1 0.5 yn)))))))))))


;;;;                  (bulirsch-stoer-lisptran f n tolerance)

;;;    f is a system derivative, 
;;;    n is the system dimension, 
;;;    tolerance is the maximum allowable relative error.
;;;
;;;  As in FORTRAN, f takes an n-dimensional state vector, 
;;;    and an answer vector to clobber
;;;    it clobbers the answer to be the state derivative vector.
;;;
;;;  (bulirsch-stoer-lisptran f n tolerance) returns a procedure 
;;;    that takes 
;;;          a state and 
;;;          a requested advance,
;;;      it calls a continuation 
;;;               that takes a new state, 
;;;                          the advance achieved, and
;;;                          a guestimate of the achievable advance.


(define (lisptran-derivative->floating-lisptran-derivative f n) ; f!(y y')
  (let ((vy (make-vector n 0.0)) (vyp (make-vector n 0.0)))
    (define (floating-lisptran-derivative fvy fvyp)
      (floating-vector-copy-into-vector n fvy vy)
      (f vy vyp)
      (vector-copy-into-floating-vector n vyp fvyp))
    floating-lisptran-derivative))

(define (error-measure->floating-error-measure error-measure n)
  (let ((old-state-estimate-lisp (make-vector n 0.0))
	(new-state-estimate-lisp (make-vector n 0.0)))
    (define (floating-error-measure new-state-estimate old-state-estimate)
      (floating-vector-copy-into-vector n new-state-estimate new-state-estimate-lisp)
      (floating-vector-copy-into-vector n old-state-estimate old-state-estimate-lisp)
      (error-measure new-state-estimate-lisp old-state-estimate-lisp))
    floating-error-measure))

(define (bulirsch-stoer-lisptran f n tolerance)
  (let ((floating-stepper
	 (bulirsch-stoer-floating-lisptran
	  (lisptran-derivative->floating-lisptran-derivative f n)
	  n
	  (error-measure->floating-error-measure
	   (parse-error-measure tolerance)
	   n))))
    (lambda (state delta-t-suggested continuation) ;lisp vector states
      (floating-stepper (vector->flonum-vector state)
			delta-t-suggested
			(lambda (floating-new-state actual-delta-t suggested-delta-t)
			  (continuation (flonum-vector->vector floating-new-state)
					actual-delta-t suggested-delta-t))))))

(define (bulirsch-stoer-floating-lisptran f n error-measure)
  (let ((mm (vector-Gragg f n))
	(state-estimate1 (flo:make-vector n 0.0))
	(state-estimate2 (flo:make-vector n 0.0))
	(gragg-output1 (flo:make-vector n 0.0))
	(gragg-output2 (flo:make-vector n 0.0))
	(tableau
	 (make-initialized-vector n
	    (lambda (i) (flo:make-vector *max-tableau-width* 0.0)))))
    (lambda (state delta-t-suggested continuation)
      ;; continuation = (lambda (new-state actual-delta-t suggested-delta-t) ...)
      (if bulirsch-stoer-state-wallp
	  (pp `(bulirsch-stoer-state ,state ,delta-t-suggested)))
      (let outside ((delta-t delta-t-suggested))
	(let ((modified-midpoint (mm state delta-t)))
	  (modified-midpoint 2 state-estimate1)
	  (flo:vector-copy-into-vector n state-estimate1 gragg-output1)
	  (let m-loop ((m 1)
		       (old-verr)
		       (old-state-estimate state-estimate1)
		       (new-state-estimate state-estimate2)
		       (old-out gragg-output1)
		       (new-out gragg-output2)
		       (fail #f))	;zero divide would have happened
	    (if (fix:< m *max-tableau-depth*)
		(let ((m1 (min m *max-tableau-width*))
		      (d (vector-ref bulirsch-stoer-magic-vectors m)))
		  (modified-midpoint (vector-ref bulirsch-stoer-steps m) new-out)

		  (for 0 (less-than n) fix:1+
		       (lambda (i)	;coordinates
			 (declare (integrate i))
			 (let* ((dta (flo:vector-ref old-out i))
				(yb (flo:vector-ref new-out i))
				(c yb))
			   (for 0 (less-than m1) fix:1+
				(lambda (k) ;width of tableau
				  (declare (integrate k))
				  (let* ((b1 (flo:* (flo:vector-ref d k) dta))
					 (den (flo:- b1 c))
					 (dtn dta))
				    (if (not (flo:= den 0.0))
					(let ((b (flo:/ (flo:- c dta) den)))
					  (set! dtn (flo:* c b))
					  (set! c (flo:* b1 b)))
					(set! fail #t))
				    (set! dta (flo:vector-ref (vector-ref tableau i) k))
				    (flo:vector-set! (vector-ref tableau i) k dtn)
				    (set! yb (flo:+ yb dtn)))))
			   (flo:vector-set! new-state-estimate i yb))))

		  (let ((verr (error-measure new-state-estimate old-state-estimate)))
		    (if bulirsch-stoer-error-wallp
			(pp `(bulirsch-stoer-error level: ,m error: ,verr h: ,delta-t)))
		    ;; In Jack's C program the first two conditions
		    ;; below are interchanged and the minimum number
		    ;; of iterations is set to (fix:< m 4)
		    (cond ;;(fail) 
                          ;;not good to (outside (* 0.9 delta-t)) or to m-loop with m+1
			  ((flo:< verr 2.0)
			   (continuation (flo:vector-copy new-state-estimate)
					 delta-t
					 (flo:* (flo:* delta-t bulirsch-stoer-magic-multiplier)
					    (flo:expt bulirsch-stoer-magic-base
						      (exact->inexact (fix:- m m1))))))
			  ((fix:< m 2)
			   (m-loop (fix:1+ m) verr
				   new-state-estimate old-state-estimate
				   new-out old-out #f))
			  ((not (flo:< verr old-verr))
			   (outside (flo:* 0.5 delta-t)))
			  (else
			   (m-loop (fix:1+ m) verr
				   new-state-estimate old-state-estimate
				   new-out old-out #f)))))

		(outside (flo:* 0.5 delta-t)))))))))

(define bulirsch-stoer-magic-multiplier 1.5)
(define bulirsch-stoer-magic-base 0.6)

(define bulirsch-stoer-error-wallp false)
(define bulirsch-stoer-state-wallp false)

(add-integrator!
 'bulirsch-stoer-lisptran
 (lambda (lisptran-derivative
	  dimension
	  lte-tolerance
	  start-state
	  step-required
	  h-suggested
	  max-h
	  continue
	  done)
   ((advance-generator
     (bulirsch-stoer-lisptran lisptran-derivative dimension lte-tolerance))
    start-state
    step-required
    h-suggested
    max-h
    continue
    done))
 '(lisptran-derivative
	  dimension
	  lte-tolerance
	  start-state
	  step-required
	  h-suggested
	  max-h
	  continue
	  done))


;;; A convenience for interchangeable use of lisptran and functional
;;;  forms of system derivative.


(define (system-derivative->lisptran-derivative f) ; y' = f(y)
  (define (lisptran-derivative y yprime)
    (subvector-move-left! (f y) 0 (vector-length y) yprime 0))
  lisptran-derivative)

(define (lisptran-derivative->system-derivative f!) ; f!(y y')
  (define (system-derivative y)
    (let ((ans (make-vector (vector-length y))))
      (f! y ans)
      ans))
  system-derivative)
