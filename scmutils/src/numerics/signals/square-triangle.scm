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

;;; Approach to a square wave by Fourier Series

(define (sinc x) (/ (sin x) x))
(define ((square-wave W T n) t)
  (* 2 (/ W T) (+ 1 (* 2 (sigma (lambda (k)
				  (let ((f (/ k T)))
				    (* (sinc (* :2pi f W))
				       (cos (* :2pi f t)))))
				1 n)))))

;;; Approximate a square wave that is 2.5 wide and repeats every 4.
(plot-trace 1
	    (sigfun:make (square-wave 1.25 4 1)
			 (sigfun:make-span -12 +12))
	    #t)
;Value: (1 (-12. 12. .03684002231759714 1.213159977682403))

(plot-trace 2
	    (sigfun:make (square-wave 1.25 4 2)
			 (sigfun:make-span -12 +12))
	    #t)
;Value: (2 (-12. 12. -.18823905672167934 1.0421960847918539))

(plot-trace 3
	    (sigfun:make (square-wave 1.25 4 3)
			 (sigfun:make-span -12 +12))
	    #t)
;Value: (3 (-12. 12. -.10703111018797645 1.117760466174441))

(plot-trace 4
	    (sigfun:make (square-wave 1.25 4 4)
			 (sigfun:make-span -12 +12))
	    #t)
;Value: (4 (-12. 12. -.07905030735568391 1.1093709156730842))

(plot-trace 5
	    (sigfun:make (square-wave 1.25 4 5)
			 (sigfun:make-span -12 +12))
	    #t)
;Value: (5 (-12. 12. -.12634214226216883 1.0649406036869744))

(plot-trace 6
	    (sigfun:make (square-wave 1.25 4 6)
			 (sigfun:make-span -12 +12))
	    #t)
(6 (-12. 12. -.06425731676601931 1.1134308652830036))

(plot-trace 7
	    (sigfun:make (square-wave 1.25 4 100)
			 (sigfun:make-span -12 +12))
	    #t)
;Value: (7 (-12. 12. -.07429889763104741 1.075430034484893))

;;; Triangles are produced by convolving pulses with themselves.

(define ((triangle-wave W T n) t)
  (* (square (/ (* 2 W) T))
     (+ 1 (* 4 (sigma (lambda (k)
			(let ((f (/ k T)))
			  (* (square (sinc (* :2pi f W)))
			     (cos (* :2pi f t)))))
		      1 n)))))

;;; Approximate a triangle wave that is 2 wide and repeats every 4.
(plot-trace 1
	    (sigfun:make (triangle-wave .5 4 1)
			 (sigfun:make-span -12 +12))
	    #t)
;Value: (1 (-12. 12. -.14014236728467552 .26514236728467555))

(plot-trace 2
	    (sigfun:make (triangle-wave .5 4 2)
			 (sigfun:make-span -12 +12))
	    #t)
;Value: (2 (-12. 12. -.089479238348769 .3664635509270133))

(plot-trace 3
	    (sigfun:make (triangle-wave .5 4 3)
			 (sigfun:make-span -12 +12))
	    #t)
;Value: (3 (-12. 12. -.0669651151613126 .3889793695141995))

(plot-trace 4
	    (sigfun:make (triangle-wave .5 4 4)
			 (sigfun:make-span -12 +12))
	    #t)
;Value: (4 (-12. 12. -.0669651151613126 .3889793695141995))

(plot-trace 5
	    (sigfun:make (triangle-wave .5 4 5)
			 (sigfun:make-span -12 +12))
	    #t)
;Value: (5 (-12. 12. -.07338396955037621 .3970850642055865))

(plot-trace 6
	    (sigfun:make (triangle-wave .5 4 6)
			 (sigfun:make-span -12 +12))
	    #t)
;Value: (6 (-12. 12. -.06971002244994887 .4083429734991796))

(plot-trace 7
	    (sigfun:make (triangle-wave .5 4 100)
			 (sigfun:make-span -12 +12))
	    #t)
;Value: (7 (-12. 12. -.06278847889885619 .43547374511550796))





