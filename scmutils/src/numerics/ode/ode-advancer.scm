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

(declare (usual-integrations))

;;; Default settings

(define *ode-integration-method* 'BULIRSCH-STOER)

;;; (set! *ode-integration-method* 'QCRK4)
;;; (set! *ode-integration-method* 'BULIRSCH-STOER)
;;; (set! *ode-integration-method* 'QCCTRAP2)
;;; (set! *ode-integration-method* 'GEAR)

(define *first-step-scale* 1.0)

(define *corrector-convergence-margin* 1.0e-1)

(define *progress-monitor* #f)

(define *last-state*)

;;; ode-advancer returns a procedure of the form
;;; (lambda (state dt continue) ...)
;;;   where
;;;     continue=(lambda (new-state dt-obtained dt-suggested) ...)

#|
(pe ((ode-advancer
      (lambda (s) (up 1 (ref s 1)))
      1.e-12
      2)
     (up 0 1)
     1
     list))
((up 1. 2.718281828459047) 1 1.5)
|#

(define (ode-advancer sysder local-error-tolerance dimension)
  (case *ode-integration-method*
    ((bulirsch-stoer)
     (bs-advancer sysder local-error-tolerance dimension))
    ((qcrk4)
     (qcrk4-advancer sysder local-error-tolerance))
    ((qcctrap2)
     (qc-ctrap-advancer sysder local-error-tolerance))
    ((qcceuler)
     (qc-ceuler-advancer sysder local-error-tolerance))
    ((explicit-gear)
     ;; actually sysder here is f&df
     (gear-advancer sysder local-error-tolerance dimension))
    (else
     (write-line `(methods: bulirsch-stoer qcrk4 qcctrap2 qcceuler explicit-gear))
     (error "Unknown ode integrator" *ode-integration-method*))))

(define (set-ode-integration-method! method)
  (case method
    ((BULIRSCH-STOER bulirsch-stoer Bulirsch-Stoer)
     (set! *ode-integration-method* 'bulirsch-stoer))
    ((QCRK4 qcrk4)
     (set! *ode-integration-method* 'qcrk4))
    ((QCCTRAP2 qcctrap2)
     (set! *ode-integration-method* 'qcctrap2))
    ((QCCEULER qcceuler)
     (set! *ode-integration-method* 'qcceuler))
    ((Gear Explicit-Gear gear explicit-gear GEAR)
     ;; actually sysder here is f&df
     (set! *ode-integration-method* 'explicit-gear))
    (else
     (write-line
      `(available methods: bulirsch-stoer qcrk4 qcctrap2 qcceuler explicit-gear))
     (display
      "Note: for x' = f(x), Gear needs f&df, all others need only f.")
     (newline)
     `(currently: ,*ode-integration-method*))))

(define (advance-monitor ns step-achieved step-suggested cont)
  (if *progress-monitor* (pp `(,ns ,step-achieved ,step-suggested)))
  (set! *last-state* ns)
  (cont))

(define (final-step-monitor ns step-achieved step-suggested)
  (if *progress-monitor* (pp `(,ns ,step-achieved ,step-suggested)))
  (set! *last-state* ns)
  ns)

(define (bs-advancer sysder local-error-tolerance dimension)
  (bulirsch-stoer-lisptran		;integrator
   (system-derivative->lisptran-derivative sysder)
   dimension
   local-error-tolerance))

(define (qcrk4-advancer sysder local-error-tolerance)
  ((quality-control rk4 4)
   sysder	
   local-error-tolerance))

(define (qc-ctrap-advancer sysder local-error-tolerance)
  ((quality-control c-trapezoid 2)
   sysder			
   local-error-tolerance 
   (* *corrector-convergence-margin*
      local-error-tolerance)))

(define (qc-ceuler-advancer sysder local-error-tolerance)
  ((quality-control c-euler 1)
   sysder			
   local-error-tolerance 
   (* *corrector-convergence-margin*
      local-error-tolerance)))

(define (gear-advancer f&df local-error-tolerance dimension)
  (gear-stepper-generator
   f&df
   dimension
   local-error-tolerance))

(define (gear? method)
  (memq method '(Gear Explicit-Gear gear explicit-gear GEAR)))