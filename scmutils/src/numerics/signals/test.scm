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

(load '("fft" "dft" "sigfun" "fourier" "signals" "useful" "show"))

(make-scope 7)

(define x1
  (sigfun:make (lambda (t) (sin (* 2 pi 1/2 t)))
	       (sigfun:make-span -20 20)))

(plot-trace 1 x1 #t)
;Value: (1 (-20. 20. -1. 1.))

(define s1
  (time-function->signal x1))

(define f1
  (signal->frequency-function s1))

(plot-trace 2 (magnitude f1))
;Value: (2 (-12.8 12.8 0. 20.))

(plot-trace 3 (angle f1))
;Value: (3 (-12.8 12.8 -1.5707963267948966 1.5707963267948966))


(define ((unit-boxcar half-width) t)
  (if (< (abs t) half-width) 1 0))

(define x2
  (sigfun:make (unit-boxcar 3)
	       (sigfun:make-span -20 20)))

(plot-trace 4 x2 #t)
;Value: (4 (-20. 20. 0 1))

(plot-trace 5
	    (magnitude (signal->frequency-function
			(time-function->signal x2)))
	    #t)
;Value: (5 (-12.8 12.8 0. 5.9765625))

(define p1 (time-function->signal x2))

(define x1*x2 (* x1 x2))

(plot-trace 6 x1*x2 #t)
;Value: (6 (-20. 20. -.9996988186962042 .9996988186962042))

(plot-trace 7
	    (imag-part (signal->frequency-function
			(time-function->signal x1*x2)))
	    #t)
;Value: (7 (-12.8 12.8 -3.0000188527905056 3.0000188527905056))
