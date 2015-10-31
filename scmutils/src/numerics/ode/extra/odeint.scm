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

;;;; Interface system for ODE integrators

;;; Assume all system derivatives are of the form
;;;  [ t, x1, ..., xn ] --> [ t, dx1, ..., dxn ]

;;; Given an initial state and a system derivative, we 
;;; have a program, the system integral, that will give
;;; us the state at any time.

(declare (usual-integrations))

#|
(define foo
  (make-ode-integrator (lambda (v) (vector 1.0 (vector-ref v 1)))
		       (vector 0.0 1.0)))

(define bar (foo 'system-integral))

(bar 1.0)
;Value 26: #(1.0000000000000002 2.718281828451085)

(bar 2.0)
;Value 27: #(1.9999999999999976 7.389056098893019)

(bar 1.1)
;Value 28: #(1.0999999999999996 3.0041660239255084)

(bar 1.0)
;Value 29: #(.9999999999999998 2.718281828439487)

(foo 'set-integration-method! 'bulirsch-stoer)

(bar .75)
;Value 38: #(.75 2.117000016612677)

(bar 1.0)
;Value 39: #(1. 2.7182818284590478)

(bar .75)
;Value 40: #(.75 2.1170000166126766)

(bar 1.0)
;Value 41: #(1. 2.7182818284590455)
|#

(define (make-ode-integrator #!optional
			     system-derivative
			     initial-state
			     local-truncation-error
			     integration-method)
  (let* ((system-derivative
	  (if (default-object? system-derivative) #f system-derivative))
	 (initial-state
	  (if (default-object? initial-state) #f initial-state))
	 (local-truncation-error
	  (if (default-object? local-truncation-error) 1.0e-10 local-truncation-error))
	 (integration-method
	  (if (default-object? integration-method) 'QCRK4 integration-method))

	 (startup-safety 0.1)

	 (current-state initial-state)
	 (current-dt #f)
	 (make-integrator #f)
	 (integrator #f)
	 )

    (define (make-qc-integrator)
      (if (not system-derivative)
	  (error "No system derivative -- INTEGRATOR"))
      (set! integrator
	    (advance-generator
	     ((quality-control rk4 4)
	      system-derivative
	      local-truncation-error))))

    (define (make-bs-integrator)
      (if (not system-derivative)
	  (error "No system derivative -- INTEGRATOR"))
      (set! integrator
	    (advance-generator
	     (bulirsch-stoer-lisptran
	      (system-derivative->lisptran-derivative system-derivative)
	      (vector-length initial-state)
	      local-truncation-error))))

    (define (system-integral t1)
      (if (not initial-state)
	  (error "No initial state -- SYSTEM-INTEGRAL"))
      (let* ((start-state
	      (if (<= (abs (- t1 (vector-ref initial-state 0)))
		      (abs (- t1 (vector-ref current-state 0))))
		  (begin (set! current-dt #f)
			 initial-state)
		  current-state))
	     (delta-t (- t1 (vector-ref start-state 0))))
	(primitive-advancer start-state delta-t)))

    (define (primitive-advancer start-state delta-t)
      (if (not integrator) (make-integrator))
      (integrator start-state
		  delta-t
		  (if (and current-dt (not (zero? current-dt)))
		      current-dt
		      (* delta-t startup-safety))	     
		  delta-t		;max
		  (lambda (ns dt h cont)
		    (cont))
		  (lambda (ns dt sdt)
		    (set! current-state ns)
		    (set! current-dt sdt)
		    ns)))

    (define (ode-advancer state delta-t success failure)
      (if (not initial-state) (set! initial-state state))
      (set! current-state state)
      (success (primitive-advancer state delta-t)))

    (define (the-integrator-control message . rest-of-arguments)

      (case message
	((SYSTEM-INTEGRAL) system-integral)

	((FAST-ADVANCER)
	 ode-advancer)

	((CURRENT-STATE) current-state)
	((SET-CURRENT-STATE!)
	 (set! current-state (car rest-of-arguments)))

	((CURRENT-DT) current-dt)
	((SET-CURRENT-DT!)
	 (set! current-dt (car rest-of-arguments)))
	
	((SYSTEM-DERIVATIVE) system-derivative)
	((SET-SYSTEM-DERIVATIVE!)
	 (let ((x (car rest-of-arguments)))
	   (set! system-derivative x)
	   (set! current-state initial-state)
	   (set! current-dt #f)
	   (set! integrator #f)))

	((INITIAL-STATE) initial-state)
	((SET-INITIAL-STATE!)
	 (let ((x (car rest-of-arguments)))
	   (set! initial-state x)
	   (set! current-state x)
	   (set! current-dt #f)))

	((LOCAL-TRUNCATION-ERROR) local-truncation-error)
	((SET-LOCAL-TRUNCATION-ERROR!)
	 (let ((x (car rest-of-arguments)))
	   (set! local-truncation-error x)
	   (set! integrator #f)))
	
	((INTEGRATION-METHOD) integration-method)
	((SET-INTEGRATION-METHOD!)
	 (let ((x (car rest-of-arguments)))
	   (set! integration-method x)
	   (set! make-integrator 
		 (case x
		   ((QCRK4) make-qc-integrator)
		   ((BULIRSCH-STOER) make-bs-integrator)
		   (else
		    (lambda ()
		      (error "Unknown integrator" message)))))
	   (set! integrator #f)))

	((STARTUP-SAFETY) startup-safety)
	((SET-STARTUP-SAFETY!)
	 (let ((x (car rest-of-arguments)))
	   (set! startup-safety x)))
	(else
	 (if (and (vector? message)
		  (not (null? rest-of-arguments))
		  (number? (car rest-of-arguments)))
	     (ode-advancer message
			   (car rest-of-arguments)
			   (lambda (new-state) new-state)
			   (lambda args
			     (error "Could not advance"
				    args
				    message
				    (car rest-of-arguments))))
	     (error "Unknown message -- ODE-INTEGRATOR" message)))
	))

    ;;Initialization
    (set! make-integrator make-qc-integrator)
    the-integrator-control))

