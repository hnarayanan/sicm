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

;;; Combining impedances -- simple stuff -- see MARTHA for more stuff

(define (series z1 z2) (+ z1 z2))

(define (parallel z1 z2) (/ (* z1 z2) (+ z1 z2)))

(define (divider z1 z2) (/ z2 (+ z1 z2)))

(define (resistor R)			;R
  (lambda (s) R))

(define (capacitor C)			;C
  (lambda (s) (/ 1 (* C s))))

(define (inductor L)			;L
  (lambda (s) (* L s)))


(define (dB V)
  (* 20 (logmag V)))

(define (logmag V)
  (log10 (magnitude V)))

(define (freq->s f)
  (* :2pi f +i))

(define (phase V)
  (atan (imag-part V) (real-part V)))

(define (logf->f logf)
  (expt 10 logf))

#|
(define ((H R L C) s)
  (/ (+ (* L s) R)
     (+ (* C L (expt s 2)) (* C R s) 1)))

(define win (frame 0 6 -2 4))

(define mag
  (compose logmag
	   (H 1 1e-3 1e-6)
	   freq->s
	   logf->f))

(graphics-clear win)

(plot-function win mag -1 7 .01)

(graphics-close win)

;;; At 1 Hz we see 10 Ohm resistor
(mag 0)
#| 1.000000102014274 |#


;;; Peak at 5041 Hz is about 972 Ohms
(pointer-coordinates win list)
#| (3.702512741088867 2.987593173980713 0) |#

(logf->f 3.702512741088867)
#| 5040.954077551694 |#

(expt 10 2.987593173980713)
#| 971.836427990541 |#

;;; Large f slope=-1, so 1 pole rolloff

;;; Zero at omega=-R/L=-1/1e-3=1000
;;; So zero is at omega/2pi = about 160Hz
;;; We see the zero rise at about that point.



(define (phase V)
  (atan (imag-part V) (real-part V)))


(define angwin (frame -1 7 :-pi :pi))

(define ang
  (compose phase
	   (H 1 1e-3 1e-6)
	   freq->s
	   logf->f))

(start-gnuplot "phase.data")
(plot-function angwin ang -1 7 .01)
(stop-gnuplot)
|#
