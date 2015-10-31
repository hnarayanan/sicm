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

(define *opaque-procedure-table* '())

(define (make-opaque name #!optional function-type)
  (if (default-object? function-type)
      (set! function-type (default-function-type 1)))
  (cond ((assq name *opaque-procedure-table*)
	 =>
	 (lambda (entry)
	   (let ((value (environment-lookup generic-environment name)))
	     (if (and (not (literal-function? value))
		      (or (not (eq? value (cadr entry)))
			  (not (equal? function-type (caddr entry)))))
		 (set-car! (cdr entry) value)
		 (set-car! (cddr entry) function-type)))))
	((environment-bound? generic-environment name)
	 (set! *opaque-procedure-table*
	       (cons (list name
			   (environment-lookup generic-environment name)
			   function-type)
		     *opaque-procedure-table*)))
	(else (error "Cannot find procedure to opaqify" name)))
  (environment-define generic-environment
		      name
		      (literal-function name function-type))
  (environment-define numerical-environment
		      name
		      (cadr (assq name *opaque-procedure-table*)))
  name)

(define (make-transparent name)
  (cond ((assq name *opaque-procedure-table*)
	 =>
	 (lambda (entry)
	   (environment-assign! generic-environment name (cadr entry))))
	(else 'done)))
	 
(define (compile-opaque name)
  (cond ((assq name *opaque-procedure-table*)
	 =>
	 (lambda (entry)
	   (let ((procedure (cadr entry))
		 (function-type (caddr entry)))
	     ;; Must use function type
	     (let ((arity (procedure-arity procedure)))
	       (if (not (eq? (car arity) (cdr arity)))
		   (error "I don't know how to compile this kind of procedure"
			  name))
	       (let ((cproc
		      (lambda->numerical-procedure
		       (lambdafy (car arity)
			 (lambda names
			   (g:simplify (apply procedure names)))))))
		 (environment-assign! numerical-environment name cproc))))))
	(else (error "No opaque definition for procedure" name))))

#|
;;; For example ...

Image saved on Friday October 1, 2004 at 4:11:23 PM
  Release 7.7.91.pre               || Microcode 14.11 || Runtime 15.5
  SF 4.41                          || LIAR 4.117      || Edwin 3.116
  ScmUtils Mechanics . Summer 2004
;You are in an interaction window of the Edwin editor.
;Type C-h for help.  C-h m will describe some commands.


(define (foo x) (- x))

(make-opaque 'foo)
(compile-opaque 'foo)

(pp foo)
#| (lambda (x) (literal-apply apply-hook (list x))) |#

(pp (access foo numerical-environment))
#| (lambda (x19) (&* -1 x19)) |#

(define ((circle) state)
  (let ((t (ref state 0))
	(x (ref state 1))
	(y (ref state 2)))
    (up 1 y (foo x))))

(define ((mon-xy win) state)
  (let ((t (ref state 0))
	(x (ref state 1))
	(y (ref state 2)))
    (plot-point win x y)))

(define win (frame -2 2 -2 2))

(begin (graphics-clear win)
       ((evolve circle) (up 0 1 0) (mon-xy win) .001 10))
;Value: #(10.000000000000728 -.8390715290764753 .5440211108893664)
;;; Works

;;; Just to test that we are really accessing the right foo...
(set! (access foo numerical-environment))
;Value: #[compiled-procedure 17 (lambda) #xF #xADF75F]

(begin (graphics-clear win)
       ((evolve circle) (up 0 1 0) (mon-xy win) .001 10))
;Anomalous microcode error unassigned-variable -- get a wizard.

;;; And I can restore it.
(compile-opaque 'foo)

(begin (graphics-clear win)
       ((evolve circle) (up 0 1 0) (mon-xy win) .001 10))
;Value: #(10.000000000000728 -.8390715290764753 .5440211108893664)
|#

#|
;;; No trouble with multiple input arguments
(define (baz x y) (- x))

(make-opaque 'baz (-> (X Real Real) Real))

(compile-opaque 'baz)

(pp baz)
#| (lambda (x y) (literal-apply apply-hook (list x y))) |#

(pp (access baz numerical-environment))
#| (lambda (x6 x5) (&* -1 x6)) |#

(define ((circ2) state)
  (let ((t (ref state 0))
	(x (ref state 1))
	(y (ref state 2)))
    (up 1 y (baz x y))))

(define ((mon-xy win) state)
  (let ((t (ref state 0))
	(x (ref state 1))
	(y (ref state 2)))
    (plot-point win x y)))

(define win (frame -2 2 -2 2))

(begin (graphics-clear win)
       ((evolve circ2) (up 0 1 0) (mon-xy win) .001 10))
;Value: #(10.000000000000728 -.8390715290764753 .5440211108893664)
|#

#|
;;; Problem with structured values.

(define ((circ1) state)
  (let ((t (ref state 0))
	(x (ref state 1))
	(y (ref state 2)))
    (bar x y)))

(define (bar x y)
  (up 1 y (- x)))

(begin (graphics-clear win)
       ((evolve circ1) (up 0 1 0) (mon-xy win) .001 10))
;Value: #(10.000000000000728 -.8390715290764753 .5440211108893664)
;;; Works without opacity, but opacity in compiling screws it up

(define ((circ2) state)
  (let ((t (ref state 0))
	(x (ref state 1))
	(y (ref state 2)))
    (bar x y)))

(make-opaque 'bar
	     (-> (X Real Real)
		 (UP Real Real Real)))

(compile-opaque 'bar)

(begin (graphics-clear win)
       ((evolve circ2) (up 0 1 0) (mon-xy win) .001 10))
;The object 3, passed as the third argument to subvector-move-left!, 
; is not in the correct range.
;;; problem is (subvector-move-left! (quote #(#(1 0 -1))) 0 3 (quote #(0. 0. 0.)) 0)
;;; in (g y0 g$y0), where g is
;;; (named-lambda (lisptran-derivative y yprime)
;;;  (subvector-move-left! (f y) 0 (vector-length y) yprime 0))

;;; Stopped inside compile-parametric at call to compiler
;;; Output of the state-procedure is already screwed up.

(pp (state-procedure (list->vector state-var-names)))
#((*vector*
   (expression (bar x1 x2))
   (literal-function #[apply-hook 41])
   (type-expression (UP Real Real Real))))

(pp state-procedure)
(lambda (fstate)
  (flatten
   (((access apply ()) parametric-sysder params) (unflatten fstate))))

(where state-procedure)
Environment created by the procedure: PARAMETRIC-FLAT-SYSDER
Depth (relative to initial environment): 0
 has bindings:

params = ()

;;; Parametic-sysder looks good
(pp parametric-sysder)
(named-lambda (circ2)
  (lambda (state)
    (let ((t (ref state 0)) (x (ref state 1)) (y (ref state 2)))
      (bar x y))))

;;; So problem is with flatten
(pp flatten)
(named-lambda (flatten state)
  (list->vector (ultra-flatten state)))

(pp ultra-flatten)
(named-lambda (ultra-flatten s)
  (if (structure? s)
      ((access apply ())
       append
       (map ultra-flatten (vector->list (s:->vector s))))
      (cons s ())))

;;; Analysis to follow.
|#

#|
;;; Things are even worse when we have structured components
;;; Work fine when acc is transparent, but fails when it is opaque.

(define ((test) state)
  (let ((t (ref state 0))
	(q (ref state 1))
	(v (ref state 2)))
    (let ((x (ref q 0))
	  (y (ref q 1))
	  (vx (ref v 0))
	  (vy (ref v 1)))
      (up 1
	  (up vx vy)
	  (acc x y)))))

(define (acc x y)
  (up (* x y) (+ x y)))

(make-opaque 'acc (-> (X Real Real) (UP Real Real)))

(compile-opaque 'acc)

((evolve test)
 (up 0 (up 1 2) (up 3 4))
 (lambda (state)
   state)
 .1
 1)

;;; At compilation entry point in compile-parametric:
(pp lexp)
(lambda (params)
  (let ()
    (lambda (state)
      (let ((x0 (vector-ref state 0)) (x1 (vector-ref state 1))
                                      (x2 (vector-ref state 2))
                                      (x3 (vector-ref state 3))
                                      (x4 (vector-ref state 4)))
        (vector 1 x3 x4 (acc x1 x2))))))
|#

#|
;;; Hmmm? maybe..., but probably not...

(define (flatten s)
  (let ((exps '()))
    (define (flatten-helper s)
      (cond ((structure? s)
	     (apply append (map flatten-helper (vector->list (s:->vector s)))))
	    ((list? s)
	     (cond ((assq (car s) *opaque-procedure-table*)
		    =>
		    (lambda (entry)
		      (let ((expname (generate-uninterned-symbol)))
			(set! exps (cons (list expname s) exps))
			(let ((function-type (caddr entry)))
			  (let ((range (type->range-type function-type)))
			    (assert (or (eq? (car range) up-tag)
					(eq? (car range) down-tag)))
			    (let lp ((stuff (cdr range)) (path '()))
			      (cond ((null? stuff) '())
				    ((symbol? stuff)
				     `(ref expname ,@path))
				    ((pair? stuff)
				     



))))
		   (else (list s))))
	    (else (list s))))
    (let ((l (flatten-helper s)))
      ...)))
      

|#

#|

(define ((test) state)
  (let ((t (ref state 0))
	(q (ref state 1))
	(v (ref state 2)))
    (let ((x (ref q 0))
	  (y (ref q 1))
	  (vx (ref v 0))
	  (vy (ref v 1)))
      (acc x y
	   (lambda (ax ay)
	     (up 1
		 (up vx vy)
		 (up ax ay)))))))

(define (acc x y cont)
  (cont (* x y) (+ x y)))

(make-opaque 'acc
	     (-> (X Real Real
		    (-> (X Real Real)
			(UP Real
			    (UP Real Real)
			    (UP Real Real))))
		 (UP Real
		     (UP Real Real)
		     (UP Real Real))))

((evolve test)
 (up 0 (up 1 2) (up 3 4))
 (lambda (state)
   state)
 .1
 1)




;Value:
#(1.0000000000000004
  #(9.64744500576115 9.101859747198148)
  #(25.802395793187287 12.616331310363636))

;;; To make sure that acc is really used:

(pp (access acc numerical-environment))
(named-lambda (acc x y cont)
  (cont (* x y) (+ x y)))

(environment-assign! numerical-environment 'acc
		     (lambda (x y cont)
		       (cont (* x y) (+ x y))))

((evolve test)
 (up 0 (up 1 2) (up 3 4))
 (lambda (state)
   state)
 .1
 1)
;Unbound variable: v
;To continue, call RESTART with an option number:
; (RESTART 3) => Specify a value to use instead of v.
; (RESTART 2) => Define v to a given value.
; (RESTART 1) => Return to read-eval-print level 1.
;Start debugger? (y or n): 




(make-transparent 'acc)

((evolve test)
 (up 0 (up 1 2) (up 3 4))
 (lambda (state)
   state)
 .1
 1)
;Value:
#(1.0000000000000004
  #(9.64744500576115 9.101859747198148)
  #(25.802395793187287 12.616331310363636))

|#