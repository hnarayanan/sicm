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
;;; A signal function has a span, the domain over which it may be
;;; nonzero.  The spans are ordered pairs of numbers.

(define (sigfun? x)
  (and (pair? x)
       (eq? (car x) '*signal-function*)))

(define (sigfun:make procedure span)
  (list '*signal-function*
	span
	(lambda (x)
	  (cond ((< x (sigfun:min span)) 0)
		((< x (sigfun:max span))
		 (procedure x))
		(else 0)))))

(define (sigfun:span signal-function)
  (cadr signal-function))

(define (sigfun:procedure signal-function)
  (caddr signal-function))				   


;;; For convenience, the span can be specified as either a single
;;; number, indicating the maxx, or a minimum and a maximum.  I hope
;;; to eventually improve this system to allow non-symmetric spans,
;;; but right now only symmetric spans are allowed.

(define (sigfun:make-span minx #!optional maxx)
  (if (default-object? maxx)
      (begin (set! maxx minx)
	     (set! minx (- maxx))))
  (assert (< minx maxx))
  (cons minx maxx))

(define (sigfun:min span) (car span))
(define (sigfun:max span) (cdr span))


(define ((sigfun:unary-op op) sigfun)
  (sigfun:make (op (sigfun:procedure sigfun))
	       (sigfun:span sigfun)))

(define ((sigfun:binary-op op) sigfun1 sigfun2)
  (let ((span1 (sigfun:span sigfun1))
	(span2 (sigfun:span sigfun2)))
    (sigfun:make (lambda (x)
		   (op ((sigfun:procedure sigfun1) x)
		       ((sigfun:procedure sigfun2) x)))
		 (sigfun:make-span
		  (min (sigfun:min span1) (sigfun:min span2))
		  (max (sigfun:max span1) (sigfun:max span2))))))
	


(define sigfun:make-rectangular (sigfun:binary-op g:make-rectangular))
(define sigfun:make-polar       (sigfun:binary-op g:make-polar))

(define sigfun:real-part (sigfun:unary-op g:real-part))
(define sigfun:imag-part (sigfun:unary-op g:imag-part))
(define sigfun:magnitude (sigfun:unary-op g:magnitude))
(define sigfun:angle     (sigfun:unary-op g:angle))

(define sigfun:conjugate (sigfun:unary-op g:conjugate))


(define sigfun:negate    (sigfun:unary-op g:negate))
(define sigfun:invert    (sigfun:unary-op g:invert))

(define sigfun:sqrt      (sigfun:unary-op g:sqrt))
(define sigfun:square    (sigfun:unary-op g:square))

(define sigfun:exp       (sigfun:unary-op g:exp))
(define sigfun:log       (sigfun:unary-op g:log))

(define sigfun:sin       (sigfun:unary-op g:sin))
(define sigfun:cos       (sigfun:unary-op g:cos))

(define sigfun:asin      (sigfun:unary-op g:asin))
(define sigfun:acos      (sigfun:unary-op g:acos))

(define sigfun:sinh      (sigfun:unary-op g:sinh))
(define sigfun:cosh      (sigfun:unary-op g:cosh))

(define sigfun:+         (sigfun:binary-op g:+))
(define sigfun:-         (sigfun:binary-op g:-))
(define sigfun:*         (sigfun:binary-op g:*))
(define sigfun:/         (sigfun:binary-op g:/))

(define sigfun:expt      (sigfun:binary-op g:expt))

(define (sigfun:expt2 sigfun n)
  ((sigfun:unary-op (lambda (x) (g:expt x a))) sigfun))

(define (sigfun:expt3 n sigfun)
  ((sigfun:unary-op (lambda (x) (g:expt a x))) sigfun))


(define sigfun:atan2     (sigfun:binary-op g:atan2))
(define sigfun:atan      (sigfun:unary-op g:atan))

(define (sigfun:scale a sigfun)
  ((sigfun:unary-op (lambda (x) (g:* a x))) sigfun))

(define (sigfun:scale2 sigfun a)
  ((sigfun:unary-op (lambda (x) (g:* x a))) sigfun))

(define (sigfun:scale3 a sigfun)
  ((sigfun:unary-op (lambda (x) (g:/ a x))) sigfun))

(define (sigfun:scale4 sigfun a)
  ((sigfun:unary-op (lambda (x) (g:/ x a))) sigfun))


(define (sigfun:shift sigfun shift)
  (sigfun:make (g:arg-shift (sigfun:procedure sigfun) shift)
	       (sigfun:span sigfun)))


(define (sigfun:reverse sigfun)
  (let ((span (sigfun:span sigfun)))
    (sigfun:make (lambda (x) ((sigfun:procedure sigfun) (- x)))
		 (sigfun:make-span (sigfun:max span) (sigfun:min span)))))


(define (sigfun:apply sigfun args)
  (g:apply (sigfun:procedure sigfun) args))
  


(assign-operation 'negate          sigfun:negate         sigfun?)
(assign-operation 'invert          sigfun:invert         sigfun?)

(assign-operation 'sqrt            sigfun:sqrt           sigfun?)
(assign-operation 'square          sigfun:square         sigfun?)

(assign-operation 'exp             sigfun:exp            sigfun?)
(assign-operation 'log             sigfun:log            sigfun?)

(assign-operation 'sin             sigfun:sin            sigfun?)
(assign-operation 'cos             sigfun:cos            sigfun?)

(assign-operation 'asin            sigfun:asin           sigfun?)
(assign-operation 'acos            sigfun:acos           sigfun?)

(assign-operation 'sinh            sigfun:sinh           sigfun?)
(assign-operation 'cosh            sigfun:cosh           sigfun?)

(assign-operation '+               sigfun:+              sigfun?  sigfun?)
(assign-operation '-               sigfun:-              sigfun?  sigfun?)
(assign-operation '*               sigfun:*              sigfun?  sigfun?)
(assign-operation '*               sigfun:scale          number?  sigfun?)
(assign-operation '*               sigfun:scale2         sigfun?  number?)

(assign-operation '/               sigfun:/              sigfun?  sigfun?)
(assign-operation '/               sigfun:scale3         number?  sigfun?)
(assign-operation '/               sigfun:scale4         sigfun?  number?)

(assign-operation 'expt            sigfun:expt           sigfun?  sigfun?)
(assign-operation 'expt            sigfun:expt2          number?  sigfun?)
(assign-operation 'expt            sigfun:expt3          sigfun?  number?)


(assign-operation 'make-rectangular sigfun:make-rectangular  sigfun? sigfun?)
(assign-operation 'make-polar       sigfun:make-polar        sigfun? sigfun?)

(assign-operation 'real-part        sigfun:real-part      sigfun?)
(assign-operation 'imag-part        sigfun:imag-part      sigfun?)
(assign-operation 'magnitude        sigfun:magnitude      sigfun?)
(assign-operation 'angle            sigfun:angle          sigfun?)

(assign-operation 'conjugate        sigfun:conjugate      sigfun?)

(assign-operation 'atan1            sigfun:atan           sigfun?)
(assign-operation 'atan2            sigfun:atan2          sigfun? sigfun?)

(assign-operation 'apply            sigfun:apply          sigfun? any?)


#|
;;; See show.scm

(define (sigfun:dB cutoff return sigfun)
  (let ((span (sigfun:span sigfun))
	(epsilon (expt 10 (/ cutoff 20))))
    (sigfun:make (compose (safe-dB epsilon return)
			  (sigfun:procedure sigfun))
		 span)))


(define ((safe-dB epsilon return) x)
  (if (< x epsilon)
      return
      (* 20 (log10 x))))
|#