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

(define (dds-wave phase-increment phase-accumulator-bits dac-bits n-clocks)
  (let ((result (flo:vector-cons n-clocks))
	(dphi/dt
	 (flo:* (->flonum phase-increment)
		(flo:/ 2pi (->flonum (expt 2 phase-accumulator-bits)))))
	(dac-scale (->flonum (- (expt 2 (- dac-bits 1)) 1))))
    (do ((i 0 (fix:+ i 1)))
	((fix:= i n-clocks))
      (flo:vector-set! result i
		       (flo:/ (flo:round
			       (flo:* (flo:sin (flo:* dphi/dt (->flonum i)))
				      dac-scale))
			      dac-scale)))
    result))

(define (plot-dds-wave device f-clock f-out phase-bits dac-bits n-clocks fft-size)
  (let ((data
	 (magnitude-squared->log-magnitude!
	  (fft-results->magnitude-squared!
	   (halve-fft-results!
	    (flo:real-fft
	     (flo:vector-elementwise-product
	      (dds-wave (round->exact (* (/ f-out f-clock)
					 (expt 2 phase-bits)))
			phase-bits dac-bits n-clocks)
	      (get-kaiser-window 140 n-clocks))
	     fft-size))))))
    (let ((length (flo:vector-length data)))
      (let ((xl 0)
	    (xu (fix:- length 1))
	    (yl -140)
	    (yu 0))
	(graphics-clear device)
	(graphics-set-coordinate-limits device xl yl xu yu)
	(graphics-set-line-style device 2)
	(for-each (lambda (y)
		    (graphics-draw-line device xl y xu y))
		  '(-20 -40 -60 -80 -100 -120))
	(graphics-set-line-style device 0)
	(let ((max-value #f))
	  (flo:vector-for-each data
	    (lambda (point)
	      (if (or (not max-value)
		      (flo:> point max-value))
		  (set! max-value point))))
	  (if plot-lines?
	      (begin
		(graphics-move-cursor
		 device 0
		 (flo:- (flo:vector-ref data 0) max-value))
		(do ((i 1 (fix:+ i 1)))
		    ((fix:= i length))
		  (graphics-drag-cursor device i
					(flo:- (flo:vector-ref data i) max-value))))
	      (do ((i 0 (fix:+ i 1)))
		  ((fix:= i length))
		(graphics-draw-point device i
				     (flo:- (flo:vector-ref data i) max-value)))))
	(graphics-flush device)))))

(define plot-lines? #t)

(define (get-kaiser-window asl n-clocks)
  (let ((key (cons asl n-clocks)))
    (or (hash-table/get kaiser-window-table key #f)
	(let ((table
	       (normalize-kaiser-window
		(make-kaiser-window (kaiser-beta-from-asl asl)
				    (- n-clocks 1)))))
	  (hash-table/put! kaiser-window-table key table)
	  table))))

(define kaiser-window-table
  (make-equal-hash-table))