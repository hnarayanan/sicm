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

;;;; Scheme evaluator extensions for algebra.

;;; Extension of Scheme for application of non-procedures in operator position.

(define *enable-generic-apply* true)

(define inapplicable-object/operator
  (condition-accessor condition-type:inapplicable-object 'DATUM))

(define (apply-extension-init)
  (bind-default-condition-handler
   (list condition-type:inapplicable-object)
   (lambda (condition)
     (if *enable-generic-apply*
	 (use-value
	  (lambda args
	    (g:apply (inapplicable-object/operator condition) args)))))))

(define (once-only! thunk name)
  (if (lexical-unbound? system-global-environment name)
      (begin (thunk)
	     ;; Create NAME in SGE
	     (eval `(define ,name #t) system-global-environment)
	     'done)
      'already-done))

(once-only! apply-extension-init 'apply-extension-init)


#|
;;; Example of the way it should not be done!

(define (apply-extension-init)
  (bind-default-condition-handler
   (list condition-type:inapplicable-object)
   (lambda (condition)
     (if *enable-generic-apply*
	 ((stack-frame->continuation
	   (stack-frame/next
	    (stack-frame/next
	     (stack-frame/next
	      (continuation->stack-frame
	       (condition/continuation condition))))))
	  (lambda args
	    (g:apply (inapplicable-object/operator condition) args)))))))
|#

;;; Extension of Scheme for self-evaluating unbound variables

(define (with-self-evaluating-unbound-variables thunk)
  (bind-condition-handler
      (list condition-type:unbound-variable)
      (lambda (condition)
	(let ((variable-name
	       (access-condition condition 'location)))
	  (use-value variable-name)))
    thunk))

#|
(pe (with-self-evaluating-unbound-variables
     (lambda ()
       (+ a 1))))
(+ 1 a)
|#

;;; Extension of Scheme for allowing symbolic literals to be applied

;;; *enable-generic-apply* is tested in applicable-literal?, used in
;;; g:apply.  See generic.scm.

(define *enable-literal-apply* #f)

(define (with-literal-apply-enabled thunk)
  (fluid-let ((*enable-literal-apply* #t))
    (thunk)))

#|
(pe (+ (f 'a) 3))
;Unbound variable: f

(pe (with-literal-apply-enabled
	(lambda ()
	  (+ (f 'a) 3))))
;Unbound variable: f

(pe (with-self-evaluating-unbound-variables
     (lambda ()
       (+ (f 'a) 3)) ))
;Application of a number not allowed f ((a))

(pe (with-literal-apply-enabled
	(lambda ()
	  (with-self-evaluating-unbound-variables
	   (lambda ()
	     (+ (f 'a) 3)) ))))
(+ 3 (f a))
|#

;;; (define *numbers-are-constant-functions* #f) in numbers.scm.  If
;;; this is set to #t then numbers are applied as constant functions.


;;; This allows literal functions to be reconstructed.

(define (with-literal-reconstruction-enabled thunk)
  (fluid-let ((*literal-reconstruction* #t))
    (thunk)))


;;; Sometimes this saves the butt of a number jockey.

(define (with-underflow->zero thunk)
  (bind-condition-handler
      (list condition-type:floating-point-underflow)
      (lambda (condition)
	(use-value 0.))
    thunk))
