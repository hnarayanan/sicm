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

;;;; Popular filters

;;;   The response functions are the defining squared magnitude
;;;   responses of filters.  To compute the actual pole and zero
;;;   positions and thus the full properties of the filters we need to
;;;   compute the roots of the rational function that fits these
;;;   responses and take only half of them.

;;; The following is useful for working in Hertz.

(define (f->s f) (* +i 2pi f))

(define (f->omega f) (* 2pi f))


(define ((group-delay H) omega)
  (let* ((z (H omega))
	 (x (real-part z))
	 (y (imag-part z))
	 (r (+ (square x) (square y)))
	 (Hp (derivative H))
	 (dz (Hp omega))
	 (dx (real-part dz))
	 (dy (imag-part dz)))
    (/ (- (* y dx) (* x dy)) r)))

;;; Butterworth filter

(define ((Butterworth-response omega-cutoff N) omega)
  (/ 1
     (+ 1
	(expt (/ omega omega-cutoff) (* 2 N)))))


(define (Butterworth-poles omega-cutoff n) 
  (let* ((dtheta (/ pi n))
	 (dtheta/2 (/ dtheta 2))
	 (poles
	  (if (even? n)
	      (apply append
		     (map (lambda (i)
			    (list (make-polar omega-cutoff
					      (+ -pi
						 (+ dtheta/2 (* i dtheta))))
				  (make-polar omega-cutoff
					      (- -pi
						 (+ dtheta/2 (* i dtheta))))))

			  (iota (/ n 2))))
	      (cons (- omega-cutoff)	; odd number of poles
		    (apply append
			   (map (lambda (i)
				  (list (make-polar omega-cutoff
						    (+ -pi (* i dtheta)))
					(make-polar omega-cutoff
						    (- -pi (* i dtheta)))))
				(cdr (iota (ceiling->exact (/ n 2))))))))))
    poles))


(define (H-Butterworth omega-cutoff n)
  (let* ((poles (Butterworth-poles omega-cutoff n))
	 (scale (apply * (map magnitude poles))))
    (define (H s)
      (/ scale
	 (apply *
		(map (lambda (sp) (- s sp))
		     poles))))
    H))

#| 
(make-scope)

(plot-trace 1
	    (sigfun:make (compose sqrt (Butterworth-response 2pi 8) f->omega)
			 (sigfun:make-span -2 +2))
	    #t)
;Value: (1 (-2. 2. 3.9062201980186685e-3 1.))

(plot-trace 2
	    (magnitude
	     (sigfun:make (compose (H-Butterworth 2pi 8) f->s)
			  (sigfun:make-span -2 +2)))
	    #t)
;Value: (2 (-2. 2. 3.906220198018671e-3 1.0000000000000007))
	     
(plot-trace 3
	    (angle
	     (sigfun:make (compose (H-Butterworth 2pi 8) f->s)
			  (sigfun:make-span -2 +2)))
	    #t)
;Value: (3 (-2. 2. -3.1383164803632173 3.1383164803632178))

(plot-trace 4
	    (sigfun:make
	      (compose (group-delay
			(compose (H-Butterworth 2pi 8)
				 (lambda (omega) (* +i omega))))
		       f->omega)
	      (sigfun:make-span -2 +2))
	    #t)
|# 

;;; Chebyshev filters

(define (Chebyshev-poly n)
  (define (T n)
    (cond ((= n 0) T_0)
	  ((= n 1) T_1)
	  (else
	   (- (* 2 identity (T (- n 1)))
	      (T (- n 2))))))
  (define T_1 identity)
  (define T_0 (constant 1))
  (T n))
    

;;; epsilon=1 ==> 3dB ripple in passband

(define (Chebyshev-response omega-cutoff epsilon n)
  (let ((chebn (Chebyshev-poly n)))
    (define (response omega)
      (/ 1
	 (+ 1 (* (square epsilon)
		 (square (chebn (/ omega omega-cutoff)))))))
    response))



(define (Chebyshev-poles omega-cutoff epsilon n)
  (let* ((beta (/ (asinh (/ 1 epsilon)) n))
	 (xk (sinh beta))
	 (yk (cosh beta)))
    (map (lambda (sp)
	   (make-rectangular
	    (* (real-part sp) xk)
	    (* (imag-part sp) yk)))
	 (Butterworth-poles omega-cutoff n))))


(define (H-Chebyshev omega-cutoff epsilon n)
  (let* ((poles (Chebyshev-poles omega-cutoff epsilon n))
	 (scale
	  (* (/ 1 (sqrt (+ 1 (square epsilon))))
	     (apply * (map magnitude poles)))))
    (define (H s)
      (/ scale
	 (apply *
		(map (lambda (sp) (- s sp))
		     poles))))
    H))

#|
(flush-scope)

(make-scope 2)

(plot-trace 1
	    (sigfun:make (compose sqrt (Chebyshev-response 2pi .4 8) f->omega)
			 (sigfun:make-span -10 +10))
	    #t)

(plot-trace 2
	    (magnitude
	     (sigfun:make (compose (H-Chebyshev 2pi .4 8) f->s)
			  (sigfun:make-span -10 +10)))
	    #t)
	     
(flush-scope)

(make-scope 4)

(plot-trace 1
	    (magnitude
	     (sigfun:make (compose (H-Chebyshev 2pi .4 8) f->s)
			  (sigfun:make-span -10 +10))))
;Value: (1 (-10. 10. 1.9927310271925264e-10 .9999997372498004))

(plot-trace 2
	    (angle
	     (sigfun:make (compose (H-Chebyshev 2pi .4 8) f->s)
			  (sigfun:make-span -10 +10))))
;Value: (2 (-10. 10. -3.0893393766264996 3.0893393766264974))

(plot-trace 3
	    (inverse-Fourier-transform
	     (sigfun:make (compose (H-Chebyshev 2pi .4 8) f->s)
			  (sigfun:make-span -10 +10))))
;Value: (3 (-25.6 25.6 -.817767132981212 1.797939938853742))

(plot-trace 4
	    (sigfun:make
	      (compose (group-delay
			(compose (H-Chebyshev 2pi .4 8)
				 (lambda (omega) (* +i omega))))
		       f->omega)
	      (sigfun:make-span -10 +10)))
;Value: (4 (-10. 10. 1.7092404439324129e-3 4.6037551667380185))







|#

