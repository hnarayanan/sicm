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

;;; Approach to a train of impulses by Fourier Series

(define ((delta-train-1 T n) f)
  (+ 1
     (* 1/2
	(sigma (lambda (k) (cos (* :2pi k f T)))
	       1 n))))

(plot-trace 1
	    (sigfun:make (delta-train-1 4/20 1)
			 (sigfun:make-span -10 10))
	    #t)
;Value: (1 (-10. 10. .5 1.5))

(plot-trace 2
	    (sigfun:make (delta-train-1 4/20 2)
			 (sigfun:make-span -10 10))
	    #t)
;Value: (2 (-10. 10. .4375492778741905 2.))

(plot-trace 3
	    (sigfun:make (delta-train-1 4/20 3)
			 (sigfun:make-span -10 10))
	    #t)
;Value: (3 (-10. 10. .34236853226157904 2.5))

(plot-trace 4
	    (sigfun:make (delta-train-1 4/20 4)
			 (sigfun:make-span -10 10))
	    #t)
;Value: (4 (-10. 10. .24028485384722975 3.))

(plot-trace 5
	    (sigfun:make (delta-train-1 4/20 5)
			 (sigfun:make-span -10 10))
	    #t)
;Value: (5 (-10. 10. .13642385332517026 3.5))

(plot-trace 6
	    (sigfun:make (delta-train-1 4/20 20)
			 (sigfun:make-span -10 10))
	    #t)
;Value: (6 (-10. 10. -1.4798262563595368 11.))

(plot-trace 7
	    (sigfun:make (delta-train-1 1/10 100)
			 (sigfun:make-span -10 10))
	    #t)
;Value: (7 (-10. 10. -9.189418162456645 51.))





;;; Fourier transform of the approximate train

(plot-trace 1
	    (sigfun:make (delta-train-1 6 127)
			 (sigfun:make-span -12 +12)))
;Value: (1 (-10. 10. .49999999999917444 64.5))

(plot-trace 2
	    (Fourier-transform
	     (sigfun:make (delta-train-1 6 127)
			  (sigfun:make-span -12 +12))))
;Value: (2 (-21.333333333333332 21.333333333333332 0. 24.))

(plot-trace 3
	    (sigfun:make (delta-train-1 4 85)
			 (sigfun:make-span -12 +12)))
;Value: (3 (-12. 12. -8.113523175871117 43.5))

(plot-trace 4
	    (Fourier-transform
	     (sigfun:make (delta-train-1 4 85)
			  (sigfun:make-span -12 +12))))
;Value: (4 (-21.333333333333332 21.333333333333332 0. 24.))

(plot-trace 5
	    (sigfun:make (delta-train-1 2 42)
			 (sigfun:make-span -12 +12)))
;Value: (5 (-12. 12. -3.620130751851196 22.))

(plot-trace 6
	    (Fourier-transform
	     (sigfun:make (delta-train-1 2 42)
			  (sigfun:make-span -12 +12))))
;Value: (6 (-21.333333333333332 21.333333333333332 0. 24.))



;;; Clear out trace 7

(plot-trace 7
	    (sigfun:make (constant 0) (sigfun:make-span -12 +12)))
;Value: (7 (-12. 12. 0 0))
