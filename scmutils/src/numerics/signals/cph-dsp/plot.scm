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

;;; -*-Scheme-*-
;;;
;;; $Id: plot.scm,v 1.3 1998/06/09 03:43:42 cph Exp $
;;;
;;; Copyright (c) 1993-98 Massachusetts Institute of Technology
;;;
;;; This material was developed by the Scheme project at the
;;; Massachusetts Institute of Technology, Department of Electrical
;;; Engineering and Computer Science.  Permission to copy this
;;; software, to redistribute it, and to use it for any purpose is
;;; granted, subject to the following restrictions and understandings.
;;;
;;; 1. Any copy made of this software must include this copyright
;;; notice in full.
;;;
;;; 2. Users of this software agree to make their best efforts (a) to
;;; return to the MIT Scheme project any improvements or extensions
;;; that they make, so that these may be included in future releases;
;;; and (b) to inform MIT of noteworthy uses of this software.
;;;
;;; 3. All materials developed as a consequence of the use of this
;;; software shall duly acknowledge such use, in accordance with the
;;; usual standards of acknowledging credit in academic research.
;;;
;;; 4. MIT has made no warrantee or representation that the operation
;;; of this software will be error-free, and MIT is under no
;;; obligation to provide any services, by way of maintenance, update,
;;; or otherwise.
;;;
;;; 5. In conjunction with products arising from the use of this
;;; material, there shall be no use of the name of the Massachusetts
;;; Institute of Technology nor of any adaptation thereof in any
;;; advertising, promotional, or sales literature without prior
;;; written consent from MIT in each case.

;;;; Impulse- and Frequency-Response Plotting

(declare (usual-integrations))

(define (plot-impulse-response impulse-response #!optional marks device)
  (plot-log-frequency-response
   (plot-response-map impulse-response
     (lambda (impulse-response)
       (magnitude-squared->log-magnitude!
	(fft-results->magnitude-squared!
	 (halve-fft-results!
	  (flo:real-fft
	   impulse-response
	   (and (fix:< (flo:vector-length impulse-response) 1024)
		1024)))))))
   (if (default-object? marks) #f marks)
   (if (default-object? device) #f device)))

(define (plot-frequency-response frequency-response #!optional marks device)
  (plot-log-frequency-response
   (plot-response-map frequency-response
     (lambda (frequency-response)
       (flo:vector-map frequency-response
		       (lambda (x)
			 (if (= x 0)
			     -100.
			     (10log10 (* x x)))))))
   (if (default-object? marks) #f marks)
   (if (default-object? device) #f device)))

(define plot-y-min #f)
(define plot-y-max #f)

(define (plot-log-frequency-response responses #!optional marks device)
  (let ((responses (plot-response-map responses identity-procedure))
	(marks (if (or (default-object? marks) (not marks)) '() marks))
	(device (get-plotting-device (if (default-object? device) #f device))))
    (let ((xl 0)
	  (xu pi)
	  (yl (flo:vector-ref (car responses) 0))
	  (yu (flo:vector-ref (car responses) 0)))
      (for-each (lambda (response)
		  (let ((l (flo:vector-length response)))
		    (do ((i 0 (fix:+ i 1)))
			((fix:= i l))
		      (cond ((> (flo:vector-ref response i) yu)
			     (set! yu (flo:vector-ref response i)))
			    ((< (flo:vector-ref response i) yl)
			     (set! yl (flo:vector-ref response i)))))))
		responses)
      (set! yl (* (floor (/ yl 10.)) 10.))
      (set! yu (* (ceiling (/ yu 10.)) 10.))
      (if plot-y-min (set! yl plot-y-min))
      (if plot-y-max (set! yu plot-y-max))
      (graphics-clear device)
      (graphics-set-coordinate-limits device xl yl xu yu)
      (if (graphics-operation device 'COLOR?)
	  (graphics-operation device 'SET-FOREGROUND-COLOR "green")
	  (graphics-set-line-style device 2))
      (let ((yd (plot-floor-10 (round (/ (- yu yl) 10)))))
	(do ((y yl (+ y yd)))
	    ((>= y yu))
	  (if (> y yl)
	      (graphics-draw-line device xl y xu y))
	  (graphics-draw-text device xl y
			      (number->string (inexact->exact y)))))
      (if (not (graphics-operation device 'COLOR?))
	  (graphics-set-line-style device 0))
      (if (not (null? marks))
	  (begin
	    (graphics-set-coordinate-limits device xl yl xu yu)
	    (for-each
	     (let* ((y-delta 1)
		    (x-delta (/ (* y-delta pi) (- yu yl))))
	       (lambda (mark)
		 (let ((x (car mark)) (y (cdr mark)))
		   (graphics-draw-line device
				       (- x x-delta) (- y y-delta)
				       (+ x x-delta) (+ y y-delta))
		   (graphics-draw-line device
				       (- x x-delta) (+ y y-delta)
				       (+ x x-delta) (- y y-delta)))))
	     marks)))
      (if (graphics-operation device 'COLOR?)
	  (graphics-operation device 'SET-FOREGROUND-COLOR "red"))
      (for-each (lambda (response)
		  (let ((l (flo:vector-length response)))
		    (graphics-set-coordinate-limits device 0 yl (- l 1) yu)
		    (graphics-move-cursor device
					  0
					  (flo:vector-ref response 0))
		    (do ((i 1 (fix:+ i 1)))
			((fix:= i l))
		      (graphics-drag-cursor device
					    i
					    (flo:vector-ref response i)))))
		responses)
      (graphics-flush device))))

(define (plot-response-map response procedure)
  (let ((do-it
	 (lambda (response)
	   (procedure (if (vector? response)
			  (vector->flonum-vector response)
			  response)))))
    (if (list? response)
	(map do-it response)
	(list (do-it response)))))

(define plot-floor-10
  (let ((l2 (log10 2))
	(l5 (log10 5)))
    (lambda (x)
      (round
       (let ((lx (log10 x)))
	 (let ((flx (floor lx)))
	   (expt 10
		 (+ (let ((d0 (- lx flx)))
		      (cond ((< d0 l2) 0.)
			    ((< d0 l5) l2)
			    (else l5)))
		    flx))))))))

(define default-plotting-device #f)

(define (open-plotting-device)
  (let ((device (make-graphics-device #f)))
    (graphics-enable-buffering device)
    (if (not default-plotting-device)
	(set! default-plotting-device device))
    device))

(define (close-plotting-device #!optional device)
  (let ((device
	 (if (or (default-object? device) (not device))
	     default-plotting-device
	     device)))
    (graphics-close device)
    (if (eq? device default-plotting-device)
	(set! default-plotting-device #f))
    unspecific))

(define (get-plotting-device #!optional device)
  (if (or (default-object? device) (not device))
      (or default-plotting-device (open-plotting-device))
      device))

(define (plot-time-response responses #!optional device)
  (let ((responses (plot-response-map responses identity-procedure))
	(device (get-plotting-device (if (default-object? device) #f device))))
    (let ((xl 0)
	  (xu (- (apply max (map flo:vector-length responses)) 1))
	  (yl (flo:vector-ref (car responses) 0))
	  (yu (flo:vector-ref (car responses) 0)))
      (for-each (lambda (response)
		  (let ((l (flo:vector-length response)))
		    (do ((i 0 (fix:+ i 1)))
			((fix:= i l))
		      (cond ((> (flo:vector-ref response i) yu)
			     (set! yu (flo:vector-ref response i)))
			    ((< (flo:vector-ref response i) yl)
			     (set! yl (flo:vector-ref response i)))))))
		responses)
      (if plot-y-min (set! yl plot-y-min))
      (if plot-y-max (set! yu plot-y-max))
      (graphics-clear device)
      (graphics-set-coordinate-limits device xl yl xu yu)
      #|
      (if (graphics-operation device 'COLOR?)
	  (graphics-operation device 'SET-FOREGROUND-COLOR "green"))
      (let ((xd (integer-floor (fix:- xu xl) 32)))
	(do ((x (fix:+ xl xd) (fix:+ x xd)))
	    ((fix:>= x xu))
	  (graphics-draw-line device x yl x yu)))
      (if (graphics-operation device 'COLOR?)
	  (graphics-operation device 'SET-FOREGROUND-COLOR "red"))
      |#
      (for-each
       (lambda (response)
	 (let ((l (flo:vector-length response)))
	   (graphics-move-cursor device
				 0
				 (flo:vector-ref response 0))
	   (do ((i 1 (fix:+ i 1)))
	       ((fix:= i l))
	     (graphics-drag-cursor device
				   i
				   (flo:vector-ref response (fix:- i 1)))
	     (graphics-drag-cursor device
				   i
				   (flo:vector-ref response i)))))
       responses)
      (graphics-flush device))))

#|
(define (generate-graph-data nx ny fx fy fd)
  ;; NX is the number of x pixels
  ;; NY is the number of y pixels
  ;; FX maps a pixel index to an x value
  ;; FY maps a y value to a pixel index
  ;; FD maps an x value to a y value or #F
  (let ((result (make-vector nx))
	(fx (fx nx)))
    (do ((ix 0 (fix:+ ix 1)))
	((fix:= ix nx))
      (vector-set! result ix
		   (let ((y (fd (fx ix))))
		     (and y
			  (fy y)))))
    result))

(define (subvector->linear-map vector start end x-min x-max)
  (let ((end (- end 1)))
    (let ((delta (/ (- x-max x-min) (- end start))))
      (lambda (x)
	(let ((index. (+ start (/ (- x x-min) delta))))
	  (let ((index (round->exact index)))
	    (and (<= start index end)
		 (if (integer? index.)
		     (vector-ref vector index)
		     (let ((interpolate
			    (lambda (xl xh)
			      (let ((yl (vector-ref vector xl)))
				(+ (* (- (vector-ref vector xh) yl)
				      (- index. xl))
				   yl)))))
		       (if (< index. index)
			   (if (= index start)
			       (interpolate index (+ index 1))
			       (interpolate (- index 1) index))
			   (if (= index end)
			       (interpolate (- index 1) index)
			       (interpolate index (+ index 1)))))))))))))

(define (linear-x-scale x-min x-max)
  (lambda (nx)
    (let ((delta (/ (- x-max x-min) (- nx 1))))
      (lambda (ix)
	(+ (* delta ix) x-min)))))

(define (log-x-scale x-min x-max)
  (lambda (nx)
    (let ((delta (/ (- x-max x-min) (- nx 1))))
      (lambda (ix)
	(+ (* delta (exp ix)) x-min)))))

(define log10-x-scale
  (let ((log10 (log 10)))
    (lambda (x-min x-max)
      (log-x-scale (/ x-min log10) (/ x-min log10)))))

(define (linear-y-scale y-min y-max)
  (lambda (ny)
    (let ((delta (/ (- y-max y-min) (- ny 1))))
      (lambda (y)
	(let ((iy (round->exact (/ (- y y-min) delta))))
	  (if (or (fix:< iy 0) (fix:>= iy ny))
	      #f
	      iy))))))

(define (log-y-scale y-min y-max)
  (lambda (ny)
    (let ((delta (/ (- y-max y-min) (- ny 1))))
      (lambda (y)
	(let ((iy (round->exact (/ (- y y-min) delta))))
	  (if (or (fix:< iy 0) (fix:>= iy ny))
	      #f
	      iy))))))

(define (plot-16-data device data start end bound)
  (graphics-clear device)
  (graphics-set-coordinate-limits device start (- bound) (fix:- end 1) bound)
  (do ((i start (fix:+ i 1)))
      ((fix:= i end))
    (graphics-draw-point device i (vector-ref data i)))
  (graphics-flush device))

(define (plot-point-data device data start end lower upper)
  (graphics-clear device)
  (graphics-set-coordinate-limits device start lower (fix:- end 1) upper)
  (do ((i start (fix:+ i 1)))
      ((fix:= i end))
    (graphics-draw-point device i (flo:vector-ref data i)))
  (graphics-flush device))

(define (plot-line-data device data start end lower upper)
  (graphics-clear device)
  (graphics-set-coordinate-limits device start lower (fix:- end 1) upper)
  (graphics-move-cursor device start (flo:vector-ref data start))
  (do ((i start (fix:+ i 1)))
      ((fix:= i end))
    (graphics-drag-cursor device i (flo:vector-ref data i)))
  (graphics-flush device))

(define (plot-16-data-bar device data start end bound)
  (graphics-clear device)
  (graphics-set-coordinate-limits device start (- bound) (fix:- end 1) bound)
  (do ((i start (fix:+ i 1)))
      ((fix:= i end))
    (graphics-draw-line device i (vector-ref data i) i 0))
  (graphics-flush device))

(define (plot-16-ln device data start end)
  (graphics-clear device)
  (graphics-set-coordinate-limits device start -1 (fix:- end 1) 1)
  (do ((i start (fix:+ i 1)))
      ((fix:= i end))
    (graphics-draw-point device i (strpcm->mulaw (vector-ref data i))))
  (graphics-flush device))
|#