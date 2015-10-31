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

;;;; Plotting package for SCMUTILS, by GJS.

(declare (usual-integrations))

#|
;;; For standalone applications, uncomment these
(define ln2 (log 2.0))
(define ln10 (log 10.0))

(define +pi  (* 4.0 (atan 1.0 1.0)))
(define -pi (- +pi))
(define +2pi (* 2.0 +pi))


(define minlog -1000.0)

(define (safelog x)
  (if (and (real? x) (> x 0))
      (max (log x) minlog)
      (error "Out of range -- SAFELOG" x)))


(define (principal-value cuthigh)
  (let ((cutlow (- cuthigh +2pi)))
    (define (the-principal-value x)
      (if (and (<= cutlow x) (< x cuthigh))
	  x
	  (let ((y (- x (* +2pi (floor (/ x +2pi))))))
	    (if (< y cuthigh) 
		y
		(- y +2pi)))))
    the-principal-value))

(define principal-value-minus-pi-to-pi
  (principal-value +pi))
|#

#|
Drawing symbols are currently not implemented

   The procedure DRAW-THE-SYMBOL is not defined.

Need to define angle ranges to have principal values
   centered around other than zero.

|#

#|
(define p (make-plotter))

(p 'PLOT-A-POINT .1 .4)

(p 'PLOT-A-POINT .2 .4)

(p 'PLOT-A-POINT .3 .5)

(p 'PLOT-A-POINT .1 .6)

(p 'PLOT-A-POINT .1 .4)

(p 'REDRAW)

(p 'AUTOSCALE)

(p 'REDRAW)

(p 'NEW-TRACE)

(p 'PLOT-A-POINT .1 .4)

(p 'PLOT-A-POINT .2 .4)

(p 'PLOT-A-POINT .3 .5)

(p 'PLOT-A-POINT .1 .6)

(p 'PLOT-A-POINT .1 .4)

(p 'SET-ABSCISSA-TYPE! 'LOG10)

(p 'AUTOSCALE)

(p 'REDRAW)

(p 'SET-ABSCISSA-TYPE! 'LINEAR)

(p 'AUTOSCALE)

(p 'REDRAW)

(p 'CLOSE-GRAPHICS)
|#

;;; Default-values of trace description components:


(define (descriptor trace) (car trace))

(define-structure (trace-descriptor (constructor make-trace-descriptor ())
				    (type vector)
				    (conc-name #f))
  (plot-type 'X-Y)			;X-Y, POLAR
  (abscissa-type 'LINEAR)		;LINEAR, LN, LOG10, LOG2, ANGLE
  (ordinate-type 'LINEAR)		;LINEAR, LN, LOG10, LOG2, ANGLE
  (abscissa-min 0.0)
  (abscissa-max 1.0)
  (ordinate-min 0.0)
  (ordinate-max 1.0)
  (connect-the-dots #t)
  (drawing-mode #f)
  (line-style #f)
  (trace-color #f)
  (plotting-symbol #f)
  (last-x #f)
  (last-y #f)
  (abscissa-marks (make-marks-descriptor))
  (ordinate-marks (make-marks-descriptor)))


(define-structure (marks-descriptor (constructor make-marks-descriptor ())
				    (type vector)
				    (conc-name #f))
  (marks-style #f)			; #f, NONE, TICKS, GRID
  (marks-position 'LOW)			; LOW, HIGH, numerical-value
  (marks				; 10 spaces (11 marks)
   '((0.0 "0")
     0.1 0.2 0.3 0.4
     (0.5 "5")
     0.6 0.7 0.8 0.9
     (1.0 "10")))
  )

(define (coordinate->frame min max x)	;to [0,1]
  (/ (- x min) (- max min)))

(define (frame->coordinate min max x)
  (+ (* (- max min) x) min))

(define (transform-point x1 x2 d cont)
  ;; cont = (lambda (x1 x2 d) ...)
  (let ((x (case (abscissa-type d)
	     ((LINEAR) x1)
	     ((LN) (safelog x1))
	     ((LOG10) (/ (safelog x1) ln10))
	     ((LOG2) (/ (safelog x1) ln2))
	     ((ANGLE) ((principal-value (abscissa-max d)) x1))))
	(y (case (ordinate-type d)
	     ((LINEAR) x2)
	     ((LN) (safelog x2))
	     ((LOG10) (/ (safelog x2) ln10))
	     ((LOG2) (/ (safelog x2) ln2))
	     ((ANGLE) ((principal-value (ordinate-max d)) x2)))))
    (case (plot-type d)
      ((X-Y)
       (cont (exact->inexact x) (exact->inexact y) d))
      ((POLAR)
       (cont (exact->inexact (* x (cos y)))
	     (exact->inexact (* x (sin y)))
	     d)))))

(define (graphics->coords x1 y1 d cont)
  (define (undo-scale x y)
    (let ((x (case (abscissa-type d)
	       ((LINEAR) x)
	       ((LN) (exp x))
	       ((LOG10) (expt 10.0 x))
	       ((LOG2) (expt 2.0 x))
	       ((ANGLE) x)))
	  (y (case (ordinate-type d)
	       ((LINEAR) y)
	       ((LN) (exp y))
	       ((LOG10) (expt 10.0 y))
	       ((LOG2) (expt 2.0 y))
	       ((ANGLE) y))))
      (cont x y)))
  (case (plot-type d)
    ((X-Y)
     (let ((x (frame->coordinate (abscissa-min d) (abscissa-max d) x1))
	   (y (frame->coordinate (ordinate-min d) (ordinate-max d) y1)))
       (undo-scale x y)))
    ((POLAR)
     (let ((rmax (max (abs (abscissa-min d)) (abs (abscissa-max d)))))
       (let ((x (frame->coordinate (- rmax) rmax x1))
	     (y (frame->coordinate (- rmax) rmax y1)))
	 (let ((r (+ (* x x) (* y y)))
	       (theta (atan y x)))
	   (undo-scale r theta)))))))

(define ((make-point-plotter graphics-device) x y d)
  (define (do-it x y)
    (let* ((thunk3
	    (lambda ()
	      (cond ((and (eq? (abscissa-type d) 'ANGLE)
			  (eq? (ordinate-type d) 'ANGLE)
			  (> (abs (- x (last-x d))) *angle-max-connect*)
			  (> (abs (- y (last-y d))) *angle-max-connect*))
		     (if (< x (last-x d))
			 (begin
			   (graphics-draw-line graphics-device 0.0 0.0 x y)
			   (graphics-draw-line graphics-device (last-x d) (last-y d) 1.0 1.0))
			 (begin
			   (graphics-draw-line graphics-device 1.0 1.0 x y)
			   (graphics-draw-line graphics-device (last-x d) (last-y d) 0.0 0.0))))
		    ((and (eq? (abscissa-type d) 'ANGLE)
			  (> (abs (- x (last-x d))) *angle-max-connect*))
		     (if (< x (last-x d))
			 (let ((y2 (/ (- (+ y (* x (last-y d))) (* y (last-x d)))
				      (- (+ 1.0 x) (last-x d)))))
			   (graphics-draw-line graphics-device (last-x d) (last-y d) 1.0 y2)
			   (graphics-draw-line graphics-device 0.0 y2 x y))
			 (let ((y2
				(/ (- (* x (last-y d)) (* y (last-x d)) (last-y d))
				   (- x (last-x d) 1.0))))
			   (graphics-draw-line graphics-device (last-x d) (last-y d) 0.0 y2)
			   (graphics-draw-line graphics-device 1.0 y2 x y))))
		    ((and (eq? (ordinate-type d) 'ANGLE)
			  (> (abs (- y (last-y d))) *angle-max-connect*))
		     (if (< y (last-y d))
			 (let ((x2 (/ (- (+ x (* y (last-x d))) (* x (last-y d)))
				      (- (+ 1.0 y) (last-y d)))))
			   (graphics-draw-line graphics-device (last-x d) (last-y d) x2 1.0)
			   (graphics-draw-line graphics-device x2 0.0 x y))
			 (let ((x2
				(/ (- (* y (last-x d)) (* x (last-y d)) (last-x d))
				   (- y (last-y d) 1.0))))
			   (graphics-draw-line graphics-device (last-x d) (last-y d) x2 0.0)
			   (graphics-draw-line graphics-device x2 1.0 x y))))
		    (else (graphics-draw-line graphics-device (last-x d) (last-y d) x y)))))
	   (thunk1
	    (lambda ()
	      (if (connect-the-dots d)				 
		  (begin 
		    (if (last-x d)	; already one to start
			(if (line-style d)
			    (graphics-bind-line-style graphics-device (line-style d) thunk3)
			    (thunk3))
			(graphics-draw-point graphics-device x y))
		    (set-last-x! d x)
		    (set-last-y! d y))
		  (graphics-draw-point graphics-device x y))
	      (if (plotting-symbol d)
		  (draw-the-symbol graphics-device x y (plotting-symbol d)))))
	   (thunk2
	    (lambda ()
	      (if (drawing-mode d)
		  (graphics-bind-drawing-mode graphics-device
					      (drawing-mode d)
					      thunk)
		  (thunk1)))))
      (if (and *can-use-colors* (trace-color d))
	  (dynamic-wind (lambda ()
			  (graphics-operation graphics-device
					      'set-foreground-color
					      (trace-color d)))
			thunk2
			(lambda ()
			  (graphics-operation graphics-device
					      'set-foreground-color
					      *foreground-color*)))
	  (thunk2))))
  (case (plot-type d)
    ((X-Y)
     (do-it (coordinate->frame (abscissa-min d) (abscissa-max d) x)
	    (coordinate->frame (ordinate-min d) (ordinate-max d) y)))
    ((POLAR)
     (let ((rmax (max (abs (abscissa-min d)) (abs (abscissa-max d)))))
       (do-it (coordinate->frame (- rmax) rmax x)
	      (coordinate->frame (- rmax) rmax y))))))

(define *ticksize* 0.02)
(define *axis-line-style* 2)		;dotted lines for axes.

;;; These have to be computed from the font used... How?

(define *small-x-space* 0.01)
(define *character-x-space* 0.015)

(define *small-y-space* 0.01)
(define *large-y-space* 0.03)

(define *angle-max-connect* 0.75)


(define (show-labels graphics-device d)
  (define (draw-axis-line x1 y1 x2 y2)
    (graphics-bind-line-style
     graphics-device *axis-line-style*
     (lambda ()
       (graphics-draw-line graphics-device x1 y1 x2 y2))))
  (define (write-tick-label x y text axis position)
    (case axis
      ((ABSCISSA)
       (case position
	 ((LOW) (set! x (+ x *small-x-space*)) (set! y (+ y *small-y-space*)))
	 ((HIGH) (set! x (+ x *small-x-space*)) (set! y (- y *large-y-space*)))
	 (else (set! x (+ x *small-x-space*)) (set! y (- y *large-y-space*)))))
      ((ORDINATE)
       (case position
	 ((LOW) (set! x (+ x *small-x-space*)) (set! y (+ y *small-y-space*)))
	 ((HIGH)
	  (set! x
		(- x
		   (* (string-length text) *character-x-space*)
		   *small-x-space*))
	  (set! y (+ y *small-y-space*)))
	 (else
	  (set! x
		(- x
		   (* (string-length text) *character-x-space*)
		   *small-x-space*))
	  (set! y (+ y *small-y-space*))))))
    (graphics-draw-text graphics-device x y text))

  (case (plot-type d)
    ((X-Y)
     (let ((m (abscissa-marks d)))
       (if (marks-style m)
	   (let ((ypos
		  (case (marks-position m)
		    ((LOW) 0.0)		;frame coordinates
		    ((HIGH) 1.0)
		    (else (coordinate->frame (ordinate-min d)
					     (ordinate-max d)
					     (marks-position m))))))
	     (draw-axis-line 0.0 ypos 1.0 ypos)
	     (if (not (eq? (marks-style m) 'NONE))
		 (for-each (lambda (mspec)
			     (let* ((x (if (number? mspec) mspec (car mspec)))
				    (xpos (coordinate->frame (abscissa-min d)
							     (abscissa-max d)
							     x)))
			       (case (marks-style m)
				 ((TICKS)
				  (draw-axis-line xpos (- ypos *ticksize*)
						  xpos (+ ypos *ticksize*)))
				 ((GRID)
				  (draw-axis-line xpos 0.0 xpos 1.0)))
			       (if (pair? mspec)
				   (write-tick-label xpos ypos (cadr mspec)
						     'abscissa
						     (marks-position m)))))
			   (marks m))))))
     (let ((m (ordinate-marks d)))
       (if (marks-style m)
	   (let ((xpos
		  (case (marks-position m)
		    ((LOW) 0.0)
		    ((HIGH) 1.0)
		    (else (coordinate->frame (abscissa-min d)
					     (abscissa-max d)
					     (marks-position m))))))
	     (draw-axis-line xpos 0.0 xpos 1.0)
	     (if (not (eq? (marks-style m) 'NONE))
		 (for-each (lambda (mspec)
			     (let* ((y (if (number? mspec) mspec (car mspec)))
				    (ypos (coordinate->frame (ordinate-min d)
							     (ordinate-max d)
							     y)))
			       (case (marks-style m)
				 ((TICKS)
				  (draw-axis-line (- xpos *ticksize*) ypos
						  (+ xpos *ticksize*) ypos))
				 ((GRID)
				  (draw-axis-line 0.0 ypos 1.0 ypos)))
			       (if (pair? mspec)
				   (write-tick-label xpos ypos (cadr mspec)
						     'ordinate
						     (marks-position m)))))
			   (marks m)))))))
    ((POLAR)
     'foo
	 ;;;**** I dunno what to do here.
     )))

(define (make-plotter #!optional graphics-device)
  (let (;; Behavior of the  plotter
	(plot-points-as-delivered? #t)
	(collect-points? #t)

	;; State of plotter
	(the-default-trace
	  (cons (make-trace-descriptor) '()))

	(traces '())

	(graphics-device
	 (if (default-object? graphics-device)
	     #f
	     graphics-device)))

    (define (copy-default-trace)
	(let ((d (vector-copy (car the-default-trace))))
	  (set-abscissa-marks! d (vector-copy (abscissa-marks d)))
	  (set-ordinate-marks! d (vector-copy (ordinate-marks d)))
	  (list d)))

    (define (make-a-new-trace)
      (if (null? traces)
	  (set! traces (list (copy-default-trace)))
	  (set-cdr! (last-pair traces)
		    (list (copy-default-trace)))))

    (define (open-graphics-if-not-already-opened)
      (if (not graphics-device)
	  (set! graphics-device (make-display-frame))))

    (define (the-trace trace-number)
      (if (not (< trace-number (length traces)))
	  (let lp ()
	    (if (not (< trace-number (length traces)))
		(begin (make-a-new-trace)
		       (lp)))))
      (list-ref traces trace-number))


    (define (%plot-a-point x1 x2 trace-number)
      (let* ((trace (the-trace trace-number))
	     (d (descriptor trace)))
	(if plot-points-as-delivered?
	    (transform-point x1 x2 d
			     (make-point-plotter graphics-device)))
	(if collect-points?		;collects the raw data
	    (set-cdr! trace (cons (cons x1 x2) (cdr trace))))))


    (define (with-traces tracenums proc)
      (if (null? tracenums)
	  (list (proc (the-trace 0)))
	  (case (car tracenums)
	    ((DEFAULT)
	     (list (proc the-default-trace)))
	    ((ALL)
	     (map proc traces))
	    (else
	     (map (lambda (n)
		    (if (not (and (exact-integer? n) (>= n 0)))
			(error "Bad trace number -- PLOTTER" tracenums))
		    (proc (the-trace n)))
		  tracenums)))))


    (define (the-plotter message . rest-of-arguments)
      (let ((must-be-one-of
	     (lambda (keyword list)
	       (if (not (memq keyword list))
		   (error "Bad keyword value -- PLOTTER" message keyword))))
	    (must-be-number
	     (lambda (x)
	       (if (not (number? x))
		   (error "Bad numerical argument -- PLOTTER" message x))))
	    (must-be-boolean
	     (lambda (x)
	       (if (not (boolean? x))
		   (error "Bad numerical argument -- PLOTTER" message x)))))

	(case message
	  ((PLOT-TYPE)
	   (with-traces rest-of-arguments
			(lambda (trace)
			  (plot-type (descriptor trace)))))
	  ((SET-PLOT-TYPE!)
	   (let ((x (car rest-of-arguments)))
	     (must-be-one-of x '(X-Y POLAR))
	     (with-traces (cdr rest-of-arguments)
			  (lambda (trace)
			    (let ((d (descriptor trace)))
			      (if (eq? x 'POLAR)
				  (begin (set-ordinate-type! d 'ANGLE)
					 (set-ordinate-min! d -pi)
					 (set-ordinate-max! d +pi)))
			      (set-plot-type! d x))))))

	  ((ABSCISSA-TYPE) 
	   (with-traces rest-of-arguments
			(lambda (trace)
			  (abscissa-type (descriptor trace)))))
	  ((SET-ABSCISSA-TYPE!)
	   (let ((x (car rest-of-arguments))
		 (tracenums (cdr rest-of-arguments)))
	     (must-be-one-of x '(LINEAR LN LOG10 LOG2 ANGLE))
	     (with-traces tracenums
			  (lambda (trace)
			    (let ((d (descriptor trace)))
			      (set-abscissa-type! (descriptor trace) x)
			      (if (eq? x 'ANGLE)
				  (begin (set-abscissa-min! d -pi)
					 (set-abscissa-max! d +pi))))))))

	  ((ORDINATE-TYPE) 
	   (with-traces rest-of-arguments
			(lambda (trace)
			  (ordinate-type (descriptor trace)))))
	  ((SET-ORDINATE-TYPE!)
	   (let ((x (car rest-of-arguments))
		 (tracenums (cdr rest-of-arguments)))
	     (must-be-one-of x '(LINEAR LN LOG10 LOG2 ANGLE))
	     (with-traces tracenums
			  (lambda (trace)
			    (let ((d (descriptor trace)))
			      (set-ordinate-type! (descriptor trace) x)
			      (if (eq? x 'ANGLE)
				  (begin (set-ordinate-min! d -pi)
					 (set-ordinate-max! d +pi))))))))

	  ((ABSCISSA-MIN)
	   (with-traces rest-of-arguments
			(lambda (trace) (abscissa-min (descriptor trace)))))
	  ((SET-ABSCISSA-MIN!)
	   (let ((x (car rest-of-arguments))
		 (tracenums (cdr rest-of-arguments)))
	     (must-be-number x)
	     (with-traces tracenums
			  (lambda (trace)
			    (set-abscissa-min! (descriptor trace) x)
			    (if (eq? (abscissa-type (descriptor trace)) 'angle)
				(set-abscissa-max! (descriptor trace)
						   (+ x +2pi)))))))
	  ((ABSCISSA-MAX)
	   (with-traces rest-of-arguments
			(lambda (trace) (abscissa-max (descriptor trace)))))
	  ((SET-ABSCISSA-MAX!)
	   (let ((x (car rest-of-arguments))
		 (tracenums (cdr rest-of-arguments)))
	     (must-be-number x)
	     (with-traces tracenums
			  (lambda (trace)
			    (set-abscissa-max! (descriptor trace) x)
			    (if (eq? (abscissa-type (descriptor trace)) 'angle)
				(set-abscissa-min! (descriptor trace)
						   (- x +2pi)))))))

	  ((ORDINATE-MIN)
	   (with-traces rest-of-arguments
			(lambda (trace) (ordinate-min (descriptor trace)))))
	  ((SET-ORDINATE-MIN!)
	   (let ((x (car rest-of-arguments))
		 (tracenums (cdr rest-of-arguments)))
	     (must-be-number x)
	     (with-traces tracenums
			  (lambda (trace)
			    (set-ordinate-min! (descriptor trace) x)
			    (if (eq? (ordinate-type (descriptor trace)) 'angle)
				(set-ordinate-max! (descriptor trace)
						   (+ x +2pi)))))))
	  ((ORDINATE-MAX)
	   (with-traces rest-of-arguments
			(lambda (trace) (ordinate-max (descriptor trace)))))
	  ((SET-ORDINATE-MAX!)
	   (let ((x (car rest-of-arguments))
		 (tracenums (cdr rest-of-arguments)))
	     (must-be-number x)
	     (with-traces tracenums
			  (lambda (trace)
			    (set-ordinate-max! (descriptor trace) x)
			    (if (eq? (ordinate-type (descriptor trace)) 'angle)
				(set-ordinate-min! (descriptor trace)
						   (- x +2pi)))))))

	  ((LIMITS)
	   (with-traces rest-of-arguments
			(lambda (trace)
			  (let ((d (descriptor trace)))
			    (list (abscissa-min d) (abscissa-max d)
				  (ordinate-min d) (ordinate-max d))))))
	  ((SET-LIMITS!)
	   (let ((x (car rest-of-arguments))
		 (tracenums (cdr rest-of-arguments)))
	     (with-traces tracenums
			  (lambda (trace)
			    (let ((d (descriptor trace)))
			      (set-abscissa-min! d (car x))
			      (set-abscissa-max! d (cadr x))
			      (set-ordinate-min! d (caddr x))
			      (set-ordinate-max! d (cadddr x)))))))

	  ((CONNECT-THE-DOTS?)
	   (with-traces rest-of-arguments
			(lambda (trace) (connect-the-dots (descriptor trace)))))
	  ((SET-CONNECT-THE-DOTS!)
	   (let ((x (car rest-of-arguments))
		 (tracenums (cdr rest-of-arguments)))
	     (must-be-boolean x)
	     (with-traces tracenums
			  (lambda (trace)
			    (set-connect-the-dots! (descriptor trace) x)))))

	  ((DRAWING-MODE)
	   (with-traces rest-of-arguments
			(lambda (trace) (drawing-mode (descriptor trace)))))
	  ((SET-DRAWING-MODE!)
	   (let ((x (car rest-of-arguments))
		 (tracenums (cdr rest-of-arguments)))
	     (if (and x (not (and (exact-integer? x) (<= 0 x 15))))
		 (error "Bad drawing mode -- PLOTTER" x))
	     (with-traces tracenums
			  (lambda (trace)
			    (set-drawing-mode! (descriptor trace) x)))))

	  ((LINE-STYLE)
	   (with-traces rest-of-arguments
			(lambda (trace) (line-style (descriptor trace)))))
	  ((SET-LINE-STYLE!)
	   (let ((x (car rest-of-arguments))
		 (tracenums (cdr rest-of-arguments)))
	     (if (and x (not (and (exact-integer? x) (<= 0 x 7))))
		 (error "Bad line-style -- PLOTTER" x))
	     (with-traces tracenums
			  (lambda (trace)
			    (set-line-style! (descriptor trace) x)))))

	  ((COLOR)
	   (with-traces rest-of-arguments
			(lambda (trace) (trace-color (descriptor trace)))))
	  ((SET-COLOR!)
	   (let ((x (car rest-of-arguments))
		 (tracenums (cdr rest-of-arguments)))
	     (if (and x (not (string? x))) (error "Bad color -- PLOTTER" x))
	     (with-traces tracenums
			  (lambda (trace)
			    (set-trace-color! (descriptor trace) x)))))

	  ((PLOTTING-SYMBOL)
	   (with-traces rest-of-arguments
			(lambda (trace) (plotting-symbol (descriptor trace)))))
	  ((SET-PLOTTING-SYMBOL!)
	   (let ((x (car rest-of-arguments))
		 (tracenums (cdr rest-of-arguments)))
	     (if (and x (not (plotting-symbol? x)))
		  (error "Bad plotting-symbol -- PLOTTER" x))
	     (with-traces tracenums
			  (lambda (trace)
			    (set-plotting-symbol! (descriptor trace) x)))))

	  ((PLOT-A-POINT)
	   (open-graphics-if-not-already-opened)
	   (let ((x1 (car rest-of-arguments))
		 (x2 (cadr rest-of-arguments))
		 (trace-number
		  (if (null? (cddr rest-of-arguments))
		      0
		      (caddr rest-of-arguments))))
	     (if (not (and (exact-integer? trace-number)
			   (>= trace-number 0)))
		 (error "Bad trace number -- PLOTTER" trace-number))
	     (%plot-a-point x1 x2 trace-number)))
	  
	  ((PLOT-POINTS)
	   (open-graphics-if-not-already-opened)
	   (let ((list-of-points (car rest-of-arguments))
		 (trace-number
		  (if (null? (cdr rest-of-arguments))
		      0
		      (cadr rest-of-arguments))))
	     (for-each (lambda (xy)
			 (%plot-a-point (car xy) (cdr xy) trace-number))
		       list-of-points)))

	  ((FAST-POINT-PLOTTER)
	   (open-graphics-if-not-already-opened)
	   (let ((trace-number
		  (if (null? rest-of-arguments)
		      0
		      (car rest-of-arguments))))
	     (lambda (x1 x2)
	       (%plot-a-point x1 x2 trace-number))))

	  ((AUTOSCALE)
	   (with-traces rest-of-arguments
			(lambda (trace)
			  (let ((d (descriptor trace)))
			    (let ((points
				   (map (lambda (xy)
					  (transform-point (car xy) (cdr xy)
							   d
							   (lambda (x y d)
							     d
							     (cons x y))))
					(cdr trace))))
			      (let ((xs (map car points))
				    (ys (map cdr points)))
				(let ((xmin (apply min xs))
				      (xmax (apply max xs))
				      (ymin (apply min ys))
				      (ymax (apply max ys)))
				  (set-abscissa-min! d xmin)
				  (set-abscissa-max! d xmax)
				  (set-ordinate-min! d ymin)
				  (set-ordinate-max! d ymax)
				  (list xmin xmax ymin ymax))))))))

	  ((ABSCISSA-MARKS-STYLE)
	   (with-traces rest-of-arguments
			(lambda (trace)
			  (marks-style (abscissa-marks (descriptor trace))))))
	  ((SET-ABSCISSA-MARKS-STYLE!)
	   (let ((x (car rest-of-arguments))
		 (tracenums (cdr rest-of-arguments)))
	     (must-be-one-of x '(#f NONE TICKS GRID))
	     (with-traces tracenums
			  (lambda (trace)
			    (let ((d (descriptor trace)))
			      (set-marks-style! (abscissa-marks d) x))))))

	  ((ABSCISSA-MARKS-POSITION)
	   (with-traces rest-of-arguments
			(lambda (trace)
			  (marks-position (abscissa-marks (descriptor trace))))))
	  ((SET-ABSCISSA-MARKS-POSITION!)
	   (let ((x (car rest-of-arguments))
		 (tracenums (cdr rest-of-arguments)))
	     (if (not (or (memq x '(LOW HIGH)) (number? x)))
		 (error "Bad abscissa marks position" x))
	     (with-traces tracenums
			  (lambda (trace)
			    (let ((d (descriptor trace)))
			      (set-marks-position! (abscissa-marks d) x))))))

	  ((ABSCISSA-MARKS)
	   (with-traces rest-of-arguments
			(lambda (trace)
			  (marks (abscissa-marks (descriptor trace))))))
	  ((SET-ABSCISSA-MARKS!)
	   (let ((x (car rest-of-arguments))
		 (tracenums (cdr rest-of-arguments)))
	     (with-traces tracenums
			  (lambda (trace)
			    (let ((d (descriptor trace)))
			      (set-marks! (abscissa-marks d) x))))))

	  ((ORDINATE-MARKS-STYLE)
	   (with-traces rest-of-arguments
			(lambda (trace)
			  (marks-style (ordinate-marks (descriptor trace))))))
	  ((SET-ORDINATE-MARKS-STYLE!)
	   (let ((x (car rest-of-arguments))
		 (tracenums (cdr rest-of-arguments)))
	     (must-be-one-of x '(#f NONE TICKS GRID))
	     (with-traces tracenums
			  (lambda (trace)
			    (let ((d (descriptor trace)))
			      (set-marks-style! (ordinate-marks d) x))))))

	  ((ORDINATE-MARKS-POSITION)
	   (with-traces rest-of-arguments
			(lambda (trace)
			  (marks-position (ordinate-marks (descriptor trace))))))
	  ((SET-ORDINATE-MARKS-POSITION!)
	   (let ((x (car rest-of-arguments))
		 (tracenums (cdr rest-of-arguments)))
	     (if (not (or (memq x '(LOW HIGH)) (number? x)))
		 (error "Bad ordinate marks position" x))
	     (with-traces tracenums
			  (lambda (trace)
			    (let ((d (descriptor trace)))
			      (set-marks-position! (ordinate-marks d) x))))))

	  ((ORDINATE-MARKS)
	   (with-traces rest-of-arguments
			(lambda (trace)
			  (marks (ordinate-marks (descriptor trace))))))
	  ((SET-ORDINATE-MARKS!)
	   (let ((x (car rest-of-arguments))
		 (tracenums (cdr rest-of-arguments)))
	     (with-traces tracenums
			  (lambda (trace)
			    (let ((d (descriptor trace)))
			      (set-marks! (ordinate-marks d) x))))))
	  
	  ((CLEAR-TRACES!)
	   (with-traces rest-of-arguments
			(lambda (trace)
			  (set-cdr! trace '())))
	   (the-plotter 'REDRAW))

	  ((REDRAW-TRACES)
	   (open-graphics-if-not-already-opened)
	   (graphics-clear graphics-device)
	   (let ((%%plot-point (make-point-plotter graphics-device)))
	     (with-traces rest-of-arguments
			  (lambda (trace)
			    (let ((d (descriptor trace)))
			      (set-last-x! d #f)
			      (set-last-y! d #f)
			      (for-each (lambda (xy)
					  (transform-point (car xy) (cdr xy) d
							   %%plot-point))
					(cdr trace))
			      (show-labels graphics-device d))))))

	  ((REDRAW-TRACE-LABELS)
	   (open-graphics-if-not-already-opened)
	   (with-traces rest-of-arguments
			(lambda (trace)
			  (show-labels graphics-device (descriptor trace)))))

	  ((POINTER-COORDINATES)
	   (apply
	    (lambda (continuation #!optional trace-number)
	      ;; continuation = (lambda (x y b) ...)
	      (get-pointer-coordinates
	       graphics-device
	       (lambda (x0 y0 b)
		 (let ((trace-number
			(if (default-object? trace-number) 0 trace-number)))
		   (graphics->coords x0 y0
				     (descriptor (the-trace trace-number))
				     (lambda (x1 y1)
				       (continuation x1 y1 b)))))))
	    rest-of-arguments))

	  ;; The following are for the plot as a whole.

	  ((PLOT-POINTS-AS-DELIVERED?)
	   plot-points-as-delivered?)
	  ((SET-PLOT-POINTS-AS-DELIVERED!)
	   (let ((x (car rest-of-arguments)))
	     (must-be-boolean x)
	     (set! plot-points-as-delivered? x)))

	  ((COLLECT-POINTS?)
	   collect-points?)
	  ((SET-COLLECT-POINTS!)
	   (let ((x (car rest-of-arguments)))
	     (must-be-boolean x)
	     (set! collect-points? x)))

	  ((GET-TRACES)
	   traces)
	  ((SET-TRACES!)
	   (let ((x (car rest-of-arguments)))
	     (set! traces x)))

	  ((NEW-TRACE)
	   (set! traces
		 (cons (copy-default-trace)
		       traces)))
		   
	  ((REDRAW)
	   (the-plotter 'REDRAW-TRACES 'ALL))

	  ((SHOW-LABELS)
	   (the-plotter 'REDRAW-TRACE-LABELS 'ALL))

	  ((CLOSE-GRAPHICS)
	   (if graphics-device
	       (begin (graphics-close graphics-device)
		      (set! graphics-device #f)
		      'closed)
	       'not-opened))

	  ((CLEAR-GRAPHICS)
	   (open-graphics-if-not-already-opened)
	   (graphics-clear graphics-device))

	  ((HORIZONTAL-RESOLUTION)
	   (open-graphics-if-not-already-opened)
	   (call-with-values
	    (lambda ()
	      (graphics-device-coordinate-limits graphics-device))
	    (lambda (x-left y-bottom x-right y-top)
	      (+ 1 (- x-right x-left)))))
	  ((VERTICAL-RESOLUTION)
	   (open-graphics-if-not-already-opened)
	   (call-with-values
	    (lambda ()
	      (graphics-device-coordinate-limits graphics-device))
	    (lambda (x-left y-bottom x-right y-top)
	      (+ 1 (- y-bottom y-top)))))
	  
	  ((GRAPHICS-DEVICE)
	   (open-graphics-if-not-already-opened)
	   graphics-device)

	  (else
	   (if (exact-integer? message)
	       (let lp ()
		 (if (not (> (length traces) message))
		     (begin (make-a-new-trace)
			    (lp))
		     (cons the-plotter message)))		 
	       (error "Unknown message -- PLOTTER" message))))))
    the-plotter))
