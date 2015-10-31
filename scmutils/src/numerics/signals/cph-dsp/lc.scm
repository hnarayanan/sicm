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
;;; $Id: lc.scm,v 1.1 1994/07/20 19:42:34 cph Exp $
;;;
;;; Copyright (c) 1993 Massachusetts Institute of Technology
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

;;;; Butterworth/Chebyshev Filter Models

(declare (usual-integrations))

(define 2pi
  (flo:* 8. (flo:atan2 1. 1.)))

(define flo:log10
  (let ((ln10 (flo:log 10.)))
    (lambda (x)
      (flo:/ (flo:log x) ln10))))

(define log10
  (let ((ln10 (flo:log 10.)))
    (lambda (x)
      (flo:/ (exact->inexact (log x)) ln10))))

(declare (integrate-operator flo:square))
(define (flo:square x)
  (flo:* x x))

(declare (integrate-operator square))
(define (square x)
  (* x x))

;;;; Various Filters

;;; Filters return the square of the voltage attenuation.

(define (butterworth-lowpass-filter order fc)
  (let ((2n (fix:* 2 order))
	(fc (exact->inexact fc)))
    (lambda (f)
      (flo:+ 1. (fast-expt (flo:/ f fc) 2n)))))

(define (butterworth-bandpass-filter order f0 bw-3db)
  (let ((2n (fix:* 2 order))
	(f0 (exact->inexact f0))
	(bw-3db (exact->inexact bw-3db)))
    (let ((f0^2/bw (flo:/ (flo:* f0 f0) bw-3db)))
      (lambda (f)
	(flo:+ 1.
	       (fast-expt (flo:- (flo:/ f bw-3db) (flo:/ f0^2/bw f))
			  2n))))))

(define (chebyshev-lowpass-filter order ripple fc)
  (let ((cp (chebyshev-poly order))
	(e (ripple->epsilon (exact->inexact ripple)))
	(fc (exact->inexact fc)))
    (lambda (f)
      (flo:+ 1. (chebyshev cp e (flo:/ f fc))))))

(define (chebyshev-bandpass-filter order ripple f0 bw-v)
  (let ((cp (chebyshev-poly order))
	(e (ripple->epsilon (exact->inexact ripple)))
	(f0 (exact->inexact f0)))
    (let ((xv (flo:/ (exact->inexact bw-v) f0)))
      (lambda (f)
	(flo:+ 1.
	       (chebyshev cp
			  e
			  (flo:/ (flo:abs (flo:- (flo:/ f f0) (flo:/ f0 f)))
				 xv)))))))

(define-integrable (ripple->epsilon ripple)
  (flo:- (flo:square (flo:expt 10. (flo:/ ripple 20.))) 1.))

(define-integrable (chebyshev p e x)
  (flo:* e (flo:square (poly/horner p x))))

#|
(define (lc-lowpass-filter rs l c rl)
  ;; This one doesn't return proper value.
  (lambda (f)
    (let ((xl (* 2pi l f))
	  (xc (/ 1 (* 2pi c f))))
      (let ((x1 (/ (* rl xc) (+ rl xc)))
	    (x2 (+ rs xl)))
	(/ x1 (+ x1 x2))))))
|#

;;;; Bandpass Filter Shape

(define (filter-shape filter f0 narrow wide)
  (call-with-values (lambda () (filter-points filter f0 narrow wide))
    (lambda (fc1 fc2 fs1 fs2)
      (let ((bw (flo:- fc2 fc1)))
	(values bw
		(flo:/ (flo:- fs2 fs1) bw))))))

(define (filter-points filter f0 narrow wide)
  ;; NARROW and WIDE are the attenuation values at which to measure
  ;; the shape.  NARROW is usually 6 dB, while WIDE is usually 60 dB.
  (let ((f0 (exact->inexact f0))
	(narrow (db->attn^2 (exact->inexact narrow)))
	(wide (db->attn^2 (exact->inexact wide))))
    (let ((search
	   (lambda (target sign)
	     (let loop
		 ((f f0)
		  (previous-step (flo:* sign (flo:/ f0 2.)))
		  (previous-outward? #t))
	       (let ((f (max f 1.)))
		 (if (flo:< (flo:abs previous-step) 1.)
		     f
		     (if (flo:< target (filter f))
			 ;; outside of target
			 (let ((step
				(if previous-outward?
				    (flo:/ previous-step 2.)
				    previous-step)))
			   (loop (flo:+ f step) step #f))
			 ;; inside of target
			 (if (flo:= f 1.)
			     f
			     (let ((step
				    (if previous-outward?
					previous-step
					(flo:/ previous-step 2.))))
			       (loop (flo:- f step) step #t))))))))))
      (values (search narrow 1.)
	      (search narrow -1.)
	      (search wide 1.)
	      (search wide -1.)))))

(define-integrable (db->attn^2 db)
  (flo:expt 10. (flo:/ db 10.)))

;;;; Magnitude Plotting

(define (show-response filter fl fh ymin ystep)
  (show-responses (list filter) fl fh ymin ystep))

(define (show-responses filters fl fh ymin ystep)
  (let ((device (initialize-graphics-device))
	(el (flo:log10 (exact->inexact fl)))
	(eh (flo:log10 (exact->inexact fh)))
	(ymin (exact->inexact ymin)))
    (call-with-values
	(lambda ()
	  (draw-axes device fl fh log10 ymin 0 ystep))
      (lambda (x-left y-bottom x-right y-top)
	(if (x-graphics/color? device)
	    (x-graphics/set-foreground-color device "green"))
	(let ((demax
	       (flo:/ (flo:- eh el)
		      (exact->inexact (abs (- x-right x-left)))))
	      (dymax
	       (flo:/ (flo:- 0. ymin)
		      (exact->inexact (abs (- y-top y-bottom))))))
	  (for-each
	   (lambda (filter)
	     (do ((points
		   (compute-points (lambda (e)
				     (flo:* -10.
					    (flo:log10
					     (filter (expt 10. e)))))
				   el eh ymin 0.
				   demax dymax)
		   (force (cdr points))))
		 ((null? points))
	       (graphics-draw-point device (caar points) (cdar points))))
	   filters))))))

(define (initialize-graphics-device)
  (if (not show-responses-device)
      (set! show-responses-device
	    (make-graphics-device x-graphics-device-type #f "512x512")))
  (graphics-clear show-responses-device)
  show-responses-device)

(define show-responses-device
  #f)

(define x-graphics/color?
  (let ((x-get-visual-info (make-primitive-procedure 'X-GET-VISUAL-INFO))
	(x-graphics-device/xw
	 (access x-graphics-device/xw (->environment '(runtime x-graphics)))))
    (lambda (device)
      (let ((info
	     (x-get-visual-info (x-graphics-device/xw device)
				#f #f #f #f #f #f #f #f #f)))
	(let ((n (vector-length info)))
	  (let loop ((index 0))
	    (and (not (fix:= index n))
		 (let ((info (vector-ref info index)))
		   (or (memv (vector-ref info 4) '(2 3 4 5))
		       (loop (fix:+ index 1)))))))))))

(define (draw-axes device fl fh f->x yl yh yd)
  (let ((el (exact->inexact (f->x fl)))
	(eh (exact->inexact (f->x fh)))
	(yl (exact->inexact yl))
	(yh (exact->inexact yh))
	(yd (exact->inexact yd)))
    (call-with-values
	(lambda ()
	  (compute-coordinate-limits device el eh yl yh))
      (lambda (el* eh* yl* yh* x-left* y-bottom* x-right* y-top*)
	(graphics-set-coordinate-limits device el* yl* eh* yh*)
	(if (x-graphics/color? device)
	    (x-graphics/set-foreground-color device "brown")
	    (graphics-set-line-style device 2))
	(let ((xdmin
	       (/ (- eh el)
		  (/ (- x-right* x-left*) 40))))
	  (draw-f-axis device fl fh f->x xdmin yl yh)
	  (label-f-axis device fl fh f->x xdmin yl*))
	(draw-y-axis device yl yh yd el eh)
	(label-y-axis device yl yh yd el*)
	(values x-left* y-bottom* x-right* y-top*)))))

(define (compute-coordinate-limits device el eh yl yh)
  (call-with-values (lambda () (x-graphics/device-coordinate-limits device))
    (lambda (x-left y-bottom x-right y-top)
      (let ((x-left* (+ x-left 30))
	    (y-bottom* (- y-bottom 30))
	    (x-right* (- x-right 30))
	    (y-top* (+ y-top 30)))
	(let ((ed
	       (/ (- (* (- x-right x-left)
			(/ (- eh el)
			   (- x-right* x-left*)))
		     (- eh el))
		  2))
	      (yd
	       (/ (- (* (- y-bottom y-top)
			(/ (- yh yl)
			   (- y-bottom* y-top*)))
		     (- yh yl))
		  2)))
	  (values (- el ed) (+ eh ed) (- yl yd) (+ yh yd)
		  x-left* y-bottom* x-right* y-top*))))))

(define (draw-y-axis dev yl yh yd xl xh)
  (graphics-draw-line dev xl yl xl yh)
  (let ((draw-tick
	 (lambda (y)
	   (graphics-draw-line dev xl y xh y))))
    (do ((y yl (+ y yd)))
	((> y yh))
      (draw-tick y))))

(define (label-y-axis dev yl yh yd x)
  (do ((y yl (+ y yd)))
      ((> y yh))
    (graphics-draw-text dev x y
			(let-fluid flonum-unparser-cutoff '(RELATIVE 2)
                          (lambda ()
                            (number->string
                             (if (integer? y)
                                 (inexact->exact y)
                                 y)))))))

(define (draw-f-axis dev fl fh f->x xdmin yl yh)
  (graphics-draw-line dev (f->x fl) yl (f->x fh) yl)
  (map-over-f fl fh f->x xdmin
    (lambda (f)
      (let ((x (f->x f)))
	(graphics-draw-line dev x yl x yh)))))

(define (label-f-axis device fl fh f->x xdmin y)
  (map-over-f fl fh f->x xdmin
    (lambda (f)
      (graphics-draw-text device
			  (f->x f)
			  y
			  (let-fluid flonum-unparser-cutoff '(RELATIVE 2)
                                     (lambda ()
                                       (number->string
                                        (let ((x (/ f (expt 10 (exponent-of f)))))
                                          (if (integer? x)
                                              x
                                              (exact->inexact x))))))))))

(define (map-over-f fl fh f->x xdmin procedure)
  (let ((fl
	 (let ((fmod (expt 10 (exponent-of fl))))
	   (let ((r (remainder fl fmod)))
	     (if (= r 0)
		 fl
		 (begin
		   (procedure fl)
		   (+ fl (- fmod r))))))))
    (let loop ((f fl) (step (find-f-step fl f->x xdmin)))
      (if (<= f fh)
	  (begin
	    (procedure f)
	    (let ((step
		   (if (integer? (/ f (expt 10 (exponent-of f))))
		       (find-f-step f f->x xdmin)
		       step)))
	      (loop (+ f step) step)))))))

(define (find-f-step f f->x xdmin)
  ;; assume F in form (* D (EXPT 10 N)) for D, N nonnegative integers
  ;; and D between 1 and 9 inclusive.
  (let ((try (lambda (step) (>= (- (f->x (+ f step)) (f->x f)) xdmin)))
	(n (exponent-of f)))
    (let ((10^n (expt 10 n)))
      (if (and (= f 10^n)
	       (try 10^n))
	  (let ((10^n-1 (/ 10^n 10)))
	    (if (try 10^n-1)
		10^n-1
		(let ((x (* 2 10^n-1)))
		  (if (try x)
		      x
		      (let ((x (* 5 10^n-1)))
			(if (try x)
			    x
			    10^n))))))
	  10^n))))

(define (exponent-of f)
  (let ((lf (log10 f)))
    (let ((n (rationalize->exact lf 1e-10)))
      (if (integer? n)
	  n
	  (floor->exact lf)))))

;;;; Compute Points for Magnitude Plot

(define (compute-points f xl xh yl yh dxmax dymax)
  (let ((visible?
	 (lambda (y)
	   (and (not (flo:< y yl)) (not (flo:> y yh))))))
    (let ((add-point
	   (lambda (x y tail)
	     (if (visible? y)
		 (cons (cons x y) tail)
		 (force tail)))))
      (let loop ((x xl) (y (f xl)))
	(add-point
	 x y
	 (delay
	   (if (flo:> x xh)
	       '()
	       (let ((x* (flo:+ x dxmax)))
		 (let ((y* (f x*)))
		   (if (and (flo:> (flo:abs (flo:- y* y)) dymax)
			    (or (visible? y) (visible? y*)))
		       (let fill
			   ((xl x)
			    (yl y)
			    (xh x*)
			    (yh y*)
			    (cont (lambda () (loop x* y*))))
			 (let ((xm (flo:/ (flo:+ xl xh) 2.)))
			   (let ((ym (f xm)))
			     (add-point
			      xm ym
			      (delay
				(let ((cont
				       (if (flo:> (flo:abs (flo:- ym yh))
						  dymax)
					   (lambda () (fill xm ym xh yh cont))
					   cont)))
				  (if (flo:> (flo:abs (flo:- ym yl)) dymax)
				      (fill xl yl xm ym cont)
				      (cont))))))))
		       (loop x* y*)))))))))))

;;;; MF8 filters

(define (mf8-butterworth-filter order clock kq index)
  (call-with-values (lambda () (mf8-parameters clock kq index))
    (lambda (f0 bw)
      (butterworth-bandpass-filter order f0 bw))))

(define (mf8-chebyshev-filter order ripple clock kq index)
  (call-with-values (lambda () (mf8-parameters clock kq index))
    (lambda (f0 bw)
      (chebyshev-bandpass-filter order ripple f0 bw))))

(define (mf8-butterworth-family order clock kq indexes)
  (map (lambda (index)
	 (mf8-butterworth-filter order clock kq index))
       indexes))

(define (mf8-chebyshev-family order ripple clock kq indexes)
  (map (lambda (index)
	 (mf8-chebyshev-filter order ripple clock kq index))
       indexes))

(define (mf8-parameters clock kq index)
  (let ((f0
	 (flo:/ (exact->inexact clock)
		(vector-ref mf8-clock-ratios index))))
    (values f0
	    (flo:/ (flo:* kq f0) (vector-ref mf8-qs index)))))

(define mf8-clock-ratios
  '#(94.0 95.8 96.8 98.4 98.7 98.9 99.2 99.3 99.4 99.4 99.5 99.6 99.6
	  99.6 99.7 99.7 99.7 99.7 99.7 99.7 99.8 99.8 99.8 99.8 99.8
	  99.8 99.8 99.9 99.9 99.9 99.9))

(define mf8-qs
  '#(0.47 0.73 0.98 2.0 2.5 3.0 4.0 5.0 5.7 6.4 7.6 8.5 10.6 11.7 12.5
	  13.6 14.7 15.8 16.5 17.0 19.0 22.0 27.0 30.0 33.0 40.0 44.0
	  57.0 68.0 79.0 90.0))

(define (enumerate-mf8-shapes make-filter f0 low high)
  (write-string "\nIndex\t6dB BW\t60:6 Shape")
  (write-string "\n-----\t------\t----------")
  (do ((i low (fix:+ i 1)))
      ((fix:= i high))
    (newline)
    (write i)
    (write-char #\tab)
    (call-with-values (lambda () (filter-shape (make-filter i) f0 6 60))
      (lambda (bw shape-factor)
	(let-fluid flonum-unparser-cutoff '(RELATIVE 2)
          (lambda ()
            (write bw)
            (write-char #\tab)
            (write shape-factor)))))))

;;;; Interesting Filters

#|
;;; MFJ CWF-2:
(list (butterworth-bandpass-filter 1 750 180)
      (butterworth-bandpass-filter 2 750 180)
      (butterworth-bandpass-filter 3 750 180))

;;; Heath SBA-301-2:
(list (butterworth-bandpass-filter 4 1000 350))

(enumerate-mf8-shapes (lambda (i) (mf8-butterworth-filter 8 1e5 1.5307 i))
		      1000 0 31)
Index	6dB BW	60:6 Shape
-----	------	----------
0	3700.	2.2
1	2300.	2.2
2	1700.	2.2
3	830.	2.2
4	660.	2.2
5	550.	2.2
6	410.	2.2
7	330.	2.2
8	290.	2.2
9	260.	2.2
10	220.	2.2
11	190.	2.2
12	160.	2.2
13	140.	2.2
14	130.	2.2
15	120.	2.2
16	110.	2.2
17	100.	2.2
18	100.	2.2
19	96.	2.2
20	86.	2.2
21	74.	2.2
22	61.	2.2
23	55.	2.2
24	49.	2.3
25	41.	2.2
26	37.	2.2
27	27.	2.4
28	23.	2.2
29	20.	2.4
30	20.	2.

SSB: 1 2
CW: 5 6 7 9 11 15 21
(mf8-butterworth-family 8 100e3 1.5307 '(1 2 5 6 7 9 11 15 21))

(enumerate-mf8-shapes (lambda (i) (mf8-chebyshev-filter 8 1 1e5 4.1981 i))
		      1000 3 31)
Index	6dB BW	60:6 Shape
-----	------	----------
3	2200.	1.5
4	1800.	1.5
5	1500.	1.5
6	1100.	1.5
7	870.	1.5
8	760.	1.5
9	680.	1.5
10	570.	1.5
11	510.	1.5
12	410.	1.5
13	370.	1.5
14	350.	1.5
15	320.	1.5
16	290.	1.5
17	270.	1.5
18	260.	1.5
19	250.	1.5
20	230.	1.5
21	200.	1.6
22	160.	1.5
23	140.	1.5
24	130.	1.6
25	110.	1.5
26	98.	1.6
27	76.	1.5
28	64.	1.5
29	55.	1.5
30	47.	1.6

SSB: 3 4
CW: 11 12 15 20 23 27
(mf8-chebyshev-family 8 1 100e3 4.1981 '(3 4 11 12 15 20 23 27))

|#