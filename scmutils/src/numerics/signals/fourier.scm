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

;;;; Fourier transform, approximated as DFT
(declare (usual-integrations))

#|
;;;  These take signal functions, as defined in sigfun.scm
;;; A signal function has a span, the domain over which it is nonzero.

(define (sigfun? x)                        ...)

(define (sigfun:make procedure span)       ...)

(define (sigfun:span signal-function)      ...)
(define (sigfun:procedure signal-function) ...)

(define (sigfun:make-span xmin xmax)       ...)
(define (sigfun:min signal-function)       ...)
(define (sigfun:max signal-function)       ...)
|#

#|
;;; We would like to define things in these terms.
(define *time-domain-min*)
(define *time-domain-max*)

(define *frequency-domain-min)
(define *frequency-domain-max)
|#

;;; Today we will just specify the total number of samples.  This can
;;; be changed to any power of two. 

(define *nsamples*)

;;; And we can specify the interval of time around zero, we are not
;;; allowing non-symmetric spans. (No shifts today!)

(define (symmetric-span? span)
  (= (- (sigfun:min span)) (sigfun:max span)))

;;; The following global variables are assigned to help in debugging.
;;; They will be reassigned for each transform done.

(define *time-half-width*)
(define *time-domain-sampling-interval*)

(define *frequency-half-width*)
(define *frequency-domain-sampling-interval*)

;;; This sets the defaults to DFT standard.

(define (set-nsamples! new)
  (assert (power-of-2? new))
  (set! *nsamples* new)
  (set! *frequency-domain-sampling-interval* 1)
  (set! *frequency-half-width* (/ *nsamples* 2))
  (set! *time-domain-sampling-interval* (/ 1 *nsamples*))
  (set! *time-half-width* 1/2)
  'done)
  
(set-nsamples! 1024)

(define (set-time-period! new)
  (set! *time-half-width* new)
  (set! *time-domain-sampling-interval*
	(/ (* 2 *time-half-width*) *nsamples*))
  (set! *frequency-half-width*
	(/ *nsamples* (* 4 *time-half-width*)))
  (set! *frequency-domain-sampling-interval*
	(/ (* 2 *frequency-half-width*) *nsamples*))
  'done)

(define (set-frequency-period! new)
  (set! *frequency-half-width* new)
  (set! *frequency-domain-sampling-interval*
	(/ (* 2 *frequency-half-width*) *nsamples*))
  (set! *time-half-width*
	(/ *nsamples* (* 4 *frequency-half-width*)))
  (set! *time-domain-sampling-interval*
	(/ (* 2 *time-half-width*) *nsamples*))
  'done)

(define (Fourier-transform f)
  (assert (sigfun? f))
  (assert (symmetric-span? (sigfun:span f)))
  (set-time-period! (sigfun:max (sigfun:span f)))
  (let ((period-span
	 (sigfun:make-span (- *frequency-half-width*)
			   (+ *frequency-half-width*))))
    (sigfun:make (g:* (* 2 *time-half-width*) ;=N*dt (flush 1/N in dft)
		      (circular-interpolate
		       (dft (samples *nsamples*
				     (sigfun:procedure f)
				     *time-half-width*))
		       period-span))
		 period-span)))

(define (inverse-Fourier-transform F)
  (assert (sigfun? F))
  (assert (symmetric-span? (sigfun:span F)))
  (set-frequency-period! (sigfun:max (sigfun:span F)))
  (let ((period-span (sigfun:make-span (- *time-half-width*)
				       (+ *time-half-width*))))
    (sigfun:make (g:* *frequency-domain-sampling-interval*
		      (circular-interpolate
		       (idft (samples *nsamples*
				      (sigfun:procedure F)
				      *frequency-half-width*))
		       period-span))
		 period-span)))

(define (circular-interpolate samples period-span)
  (let* ((nsamples (length samples))
	 (minx (sigfun:min period-span))
	 (maxx (sigfun:max period-span))
	 (period (- maxx minx))
	 (dx (/ period nsamples)))
    (define (interpolation x)
      (let* ((xpos (/ (- x minx) dx))
	     (ixpos (floor->exact xpos))
	     (xoffset (- xpos ixpos))
	     (ilo (modulo ixpos nsamples))
	     (stuff (list-tail samples ilo))
	     (flo (car stuff))
	     (fhi (if (null? (cdr stuff)) (car samples) (cadr stuff))))
	(+ flo (* xoffset (- fhi flo)))))
    interpolation))

#|
;;; from dft.scm

(define (samples n f a)
  (assert (power-of-2? n))
  (map f (iota n (- a) (/ (* 2 a) n))))
|#

#|
;;; Scale factor test...

(define fdelta
  (signal->frequency-function
   (time-function->signal
    (sigfun:make (constant 1)
		 (sigfun:make-span -10 10)))))

(define mfdelta (magnitude fdelta))

(show-signal-function mfdelta 0 40)
(wmfdelta -25.6 25.6 0 40)
(graphics-close wmfdelta)

((sigfun:procedure mfdelta) 0)
;Value: 20

((sigfun:procedure mfdelta) .1)

(definite-integral (sigfun:procedure mfdelta) -.1 +.1 .001 #f)
;Value: 1.0000000000000835

(define tdelta
  (signal->time-function
   (frequency-function->signal
    (sigfun:make (constant 1)
		 (sigfun:make-span -25.6 25.6)))))

(define mtdelta (magnitude tdelta))

((sigfun:procedure mtdelta) 0)
;Value: 51.2

(show-signal-function mtdelta 0 100)
(wmtdelta -10. 10. 0 100)
(graphics-close wmtdelta)

(definite-integral (sigfun:procedure mtdelta) -.03 +.03 .001 #f)
;Value: .9997816051629261
|#

(define (time-domain-energy f)
  (assert (sigfun? f))
  (assert (symmetric-span? (sigfun:span f)))
  (set-time-period! (sigfun:max (sigfun:span f)))
  (let ((period-span
	 (sigfun:make-span (- *frequency-half-width*)
			   (+ *frequency-half-width*))))
    (/ (apply +
	      (map (compose square magnitude)
		   (samples *nsamples*
			    (sigfun:procedure f)
			    *time-half-width*)))
       *frequency-half-width*)))

(define (frequency-domain-energy F)
  (assert (sigfun? F))
  (assert (symmetric-span? (sigfun:span F)))
  (set-frequency-period! (sigfun:max (sigfun:span F)))
  (let ((period-span (sigfun:make-span (- *time-half-width*)
				       (+ *time-half-width*))))
    (/ (apply +
	      (map (compose square magnitude)
		   (samples *nsamples*
			    (sigfun:procedure F)
			    *frequency-half-width*)))
       *time-half-width*)))

#|
(define ((test-cos a f phi) t)
  (* a (cos (+ (* :2pi f t) phi))))

(define tc
  (sigfun:make (test-cos 2 8.2 1) (sigfun:make-span -2 +2)))

(time-domain-energy tc)
;Value: 16.027322899638975

(define fc (Fourier-transform tc))

(frequency-domain-energy fc)
;Value: 16.027322899639
|#
