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

;;;; Graphs
;;; For quick-and-dirty graphs of functions use MAKE-FUNCTION-GRAPH. 
;;; For anything more complex, use MAKE-PARAMETRIC-GRAPH (next page).

(define (make-function-graph pspec f xlo xhi #!optional n)
  (let* ((plotter (if (pair? pspec) (car pspec) pspec))
	 (trace-number (if (pair? pspec) (cdr pspec) 0))
	 (n (if (default-object? n) (plotter 'HORIZONTAL-RESOLUTION) n)))
    (let ((dx (/ (- xhi xlo) n)))
      (let lp ((i 0))
	(if (= i n)
	    'done
	    (let ((x (+ xlo (* i dx))))
	      (plotter 'PLOT-A-POINT x (f x) trace-number)
	      (lp (+ i 1))))))))
	          
#|
(define p (make-plotter))

(make-function-graph p cos 0 7)

(p 'AUTOSCALE)
;Value 23: (0. 6.982500000000001 -.999964658471342 1.)

(p 'REDRAW)

(make-function-graph (p 1) sin 0 7 20)

(p 'AUTOSCALE 1)
;Value 24: (0. 6.65 -.9868438585032365 .9854497299884601)

(p 'REDRAW)

(p 'SET-COLOR! "red" 0)
(p 'SET-COLOR! "blue" 1)

(p 'REDRAW)

(p 'CLOSE-GRAPHICS)
|#

;;; Here each call to F is expected to yield a scheme vector of values.
;;;  The trace specification maps a horizontal and a vertical for each 
;;;  trace to two vector indices.  Each spec is a pair of the two indices
;;;  the CAR is the vector index mapped to the horizontal, and the CDR is
;;;  vector index mapped to the vertical.  One can skip a trace by placing 
;;;  a #f in that position.

(define (make-parametric-graph plotter f tlo thi trace-specs #!optional n)
  (plotter (- (length trace-specs) 1))
  (let* ((n (if (default-object? n) (plotter 'HORIZONTAL-RESOLUTION) n))
	 (dt (/ (- thi tlo) n))
	 (the-plotters
	  (let sloop ((specs trace-specs) (trace-number 0))
	    (if (null? specs)
		'()
		(cons (plotter 'FAST-POINT-PLOTTER trace-number)
		      (sloop (cdr specs) (+ trace-number 1)))))))
    (let lp ((i 0))
      (if (= i n)
	  'done
	  (let ((t (+ tlo (* i dt))))
	    (let ((v (f t)))
	      (let sloop ((specs trace-specs) (plotters the-plotters))
		(if (null? specs)
		    'done
		    (let ((trace-spec (car specs)))
		      (if trace-spec
			  ((car plotters)
			   (vector-ref v (car trace-spec))
			   (vector-ref v (cdr trace-spec))))
		      (sloop (cdr specs) (cdr plotters))))))
	    (lp (+ i 1)))))))

#|
(define p (make-plotter))

(make-parametric-graph p
		       (lambda (t) (vector t (cos t) (sin t)))
		       0.0 7.0
		       '((1 . 2)
			 (0 . 1)))

(p 'AUTOSCALE 0)
;Value 24: (-.999964658471342 1. -.9999880489592037 .9999911645788031)

(p 'REDRAW)

(p 'AUTOSCALE 1)
;Value 25: (0. 6.982500000000001 -.999964658471342 1.)

(p 'REDRAW)
(p 'CLEAR-TRACES! 1)

(p 'SET-ABSCISSA-MARKS-STYLE! 'TICKS)
(p 'SET-ORDINATE-MARKS-STYLE! 'TICKS)
(p 'SET-ABSCISSA-MARKS-POSITION! 0.0)
(p 'SET-ORDINATE-MARKS-POSITION! 0.0)

(p 'REDRAW)

(p 'POINTER-COORDINATES list)
;Value 33: (-.07767519355132335 -.07769187192825389 0)

(p 'CLOSE-GRAPHICS)
|#

#|
(define p (make-plotter))

(p 'SET-PLOT-TYPE! 'POLAR)

;;; A three-leaved rose
(make-parametric-graph p
		       (lambda (t) (vector t (sin (* 3 t))))
		       0.0 7.0
		       '((1 . 0)))

(p 'CLEAR-TRACES! 0)

;;; A Cardioid
(make-parametric-graph p
		       (lambda (t) (vector t (- 1 (cos t))))
		       0.0 7.0
		       '((1 . 0)))

(p 'AUTOSCALE 0)

(p 'REDRAW)

(p 'CLOSE-GRAPHICS)
|#
