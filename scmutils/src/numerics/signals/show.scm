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

;;;; This makes the "oscilliscope"
(declare (usual-integrations))

;;; Needs space between traces, and way to undefault the scaling.

(define *the-scope*)

(define *ntraces* 5)
(define *trace-width-pixels* 512)
(define *trace-height-pixels* 100)
(define *trace-separation-pixels* 5)
(define *trace-shrink* .9)

(define *scope-left-edge* 5)
(define *scope-top-edge* 105)



(define (make-scope #!optional ntraces)
  (if (not (default-object? ntraces)) (set! *ntraces* ntraces))
  (set! *the-scope*
	(frame 0 +1 (- *ntraces*) 0
	       *trace-width-pixels*
	       (+ (* *trace-height-pixels* *ntraces*)
		  (* *trace-separation-pixels* (- *ntraces* 1)))
	       *scope-left-edge* *scope-top-edge*))
  (list 'new-scope *ntraces*))

(define (flush-scope)
  (graphics-close *the-scope*))


(define (plot-trace trace signal-function #!optional dots?)
  (if (default-object? dots?) (set! dots? #f))
  (let* ((span (sigfun:span signal-function))
	 (minx (exact->inexact (sigfun:min span)))
	 (maxx (exact->inexact (sigfun:max span)))
	 (xspan (- maxx minx))
	 (dx (/ xspan *nsamples*))
	 (xdata (generate-list *nsamples* (lambda (i) (+ minx (* i dx)))))
	 (ydata
	  (map (lambda (x)
		 (real-part ((sigfun:procedure signal-function) x)))
	       xdata))
	 (miny (apply min ydata))
	 (maxy (apply max ydata))
	 (yspan (/ (- maxy miny) *trace-shrink*))
	 (yoffset (/ (- 1 *trace-shrink*) 2)))
    (graphics-set-clip-rectangle *the-scope* 0 (- trace) 1 (- (- trace 1)))
    (graphics-clear *the-scope*)
    (graphics-bind-line-style *the-scope* 1 ; dashs
     (lambda ()
       (graphics-draw-line *the-scope* 0 (- trace) 1 (- trace))
       (graphics-draw-line *the-scope* 0 (- (- trace 1)) 1 (- (- trace 1)))))
    (cond ((= yspan 0)
	   (graphics-bind-line-style *the-scope* 7 ; dash dot dot
	    (lambda ()
	      (graphics-draw-line *the-scope* 0 (- .5 trace) 1 (- .5 trace)))))
	  ((= xspan 0)
	   (graphics-bind-line-style *the-scope* 6 ; center dash
	    (lambda ()
	      (graphics-draw-line *the-scope* 0 (- .5 trace) 1 (- .5 trace)))))
	  (else
	   (if (<= minx 0 maxx)
	       (graphics-bind-line-style *the-scope* 2 ; dots
		 (lambda ()
		   (let ((x0 (- (/ minx xspan))))
		     (graphics-draw-line *the-scope*
					 x0 (- trace) x0 (- (- trace 1)))))))
	   (let ((y0 (- yoffset (/ miny yspan) trace)))
	     (if (<= miny 0 maxy)
		 (graphics-bind-line-style *the-scope* 2 ; dots
		   (lambda ()
		     ;;(pp `(drawing horizontal dotted line at ,y0))
		     (graphics-draw-line *the-scope* 0 y0 1 y0))))
	     (for-each
	      (lambda (x y)
		(let ((xp (/ (- x minx) xspan))
		      (yp (- (+ (/ (- y miny) yspan) yoffset) trace)))
		  (if dots?
		      (graphics-draw-point *the-scope* xp yp)
		      (graphics-draw-line *the-scope* xp y0 xp yp))))
	      xdata
	      ydata))))
    (graphics-draw-text *the-scope* 0 (- .6 trace) (number->string trace))
    (list trace (list minx maxx miny maxy))))				  


;;; Simple plots in specified windows.

(define (plot-sigfun win signal-function #!optional offset)
  (if (default-object? offset) (set! offset 0.0))
  (let* ((span (sigfun:span signal-function))
	 (minx (exact->inexact (sigfun:min span)))
	 (maxx (exact->inexact (sigfun:max span)))
	 (xspan (- maxx minx))
	 (dx (/ xspan *nsamples*)))
    (plot-function win
	(lambda (x)
	  (- (real-part ((sigfun:procedure signal-function) x)) offset))
	minx maxx dx)))

(define (safe-dB cdb)
  (let ((cutoff (expt 10 (/ cdb 20))))
    (define (dB v)
      (if (< v cutoff)
	  cdb
	  (* 20 (log10 v))))
      (sigfun:unary-op
       (lambda (f)
	 (compose dB f)))))
