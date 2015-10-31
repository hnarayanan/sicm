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

;;;; Graphics Windows

;;; The "hardware" OPEN

(define *foreground-color* "white")
(define *background-color* "black")
(define *can-use-colors* #t)

;;; Defaults for X windows
; (define *frame-x-position* (if (eq? 'unix microcode-id/operating-system) 750 532))
(define *frame-x-position* (if (eq? 'unix microcode-id/operating-system) -10 532))
(define *frame-y-position* 0)
(define *frame-width* (if (eq? 'unix microcode-id/operating-system) 400 100))
(define *frame-height* (if (eq? 'unix microcode-id/operating-system) 400 100))


(define (make-display-frame #!optional xmin xmax ymin ymax
			    frame-width frame-height
			    frame-x-position frame-y-position
			    display)
  (let ((xmin (if (default-object? xmin) 0.0 xmin))
	(xmax (if (default-object? xmax) 1.0 xmax))
	(ymin (if (default-object? ymin) 0.0 ymin))
	(ymax (if (default-object? ymax) 1.0 ymax))
	(frame-x
	 (if (default-object? frame-x-position)
	     *frame-x-position*
	     frame-x-position))
	(frame-y
	 (if (default-object? frame-y-position)
	     *frame-y-position*
	     frame-y-position))
	(frame-width
	 (if (default-object? frame-width)
	     *frame-width*
	     frame-width))
	(frame-height
	 (if (default-object? frame-height)
	     *frame-height*
	     frame-height)))
    (if (not (and (integer? frame-width) (> frame-width 0)
		  (integer? frame-height) (> frame-height 0)))
	(error "Bad frame width or height"))
    (let ((window
	   (if (default-object? display)
	       (make-window frame-width frame-height frame-x frame-y)
	       (make-window frame-width frame-height frame-x frame-y display))))
      (graphics-set-coordinate-limits window xmin ymin xmax ymax)
      (graphics-set-clip-rectangle window xmin ymin xmax ymax)
      (let ((name (graphics-type-name (graphics-type #f))))
	     (case name
	       ((X)
		(graphics-operation window 'set-border-color "green")
		(graphics-operation window 'set-mouse-color "green"))
	       ((WIN32) 'nothing-to-do )
	       ((OS/2) 'nothing-to-do )
	       (else (error "Unsupported graphics type:" name))))
      (graphics-operation window 'set-background-color *background-color*)
      (graphics-operation window 'set-foreground-color *foreground-color*)
      (graphics-clear window)
      window)))

;;; By CPH, frame maker in display coordinates.

(define (make-window width height x y #!optional display)
  (let ((window
	 (let ((name (graphics-type-name (graphics-type #f))))
	   (case name
	     ((X)
	      (if (default-object? display) (set! display #f))
	      (make-window/X11 width height x y display))
	     ((WIN32)
	      (if (not (default-object? display))
		  (error "No remote Win32 display"))
	      (make-window/win32 width height x y))
	     ((OS/2)
	      (if (not (default-object? display))
		  (error "No remote OS/2 display"))
	      (make-window/OS2 width height x y))
	     (else
	      (error "Unsupported graphics type:" name))))))
    (graphics-set-coordinate-limits window 0 (- (- height 1)) (- width 1) 0)
    ;;(restore-focus-to-editor)
    window))

(define (make-window/X11 width height x y #!optional display)
  (if (default-object? display) (set! display #f))
  (let ((window
	 (make-graphics-device 'X 
			       display
			       (x-geometry-string x y width height)
			       true)))
    ;; Prevent this window from receiving the keyboard focus.
    (if (not (string-ci=? "MacOSX" microcode-id/operating-system-variant))
	(x-graphics/disable-keyboard-focus window))
    ;; Inform the window manager that this window does not do any
    ;; keyboard input.
    (x-graphics/set-input-hint window false)
    ;; OK, now map the window onto the screen.
    (x-graphics/map-window window)
    (x-graphics/flush window)
    window))

(define (make-window/win32 width height x y)
  (let ((window (make-graphics-device 'WIN32 width height 'GRAYSCALE-128)))
    (graphics-operation window 'MOVE-WINDOW x y)
    window))

(define (make-window/OS2 width height x y)
  (let ((window (make-graphics-device 'OS/2 width height)))
    ;; X, Y specify the position of the upper-left corner of the
    ;; window, in coordinates relative to the upper-left corner of the
    ;; display with Y growing down; the OS/2 SET-WINDOW-POSITION
    ;; operation specifies the position of the lower-left corner of
    ;; the window, in coordinates relative to the lower left corner of
    ;; the display, with Y growing up.
    (call-with-values (lambda () (graphics-operation window 'DESKTOP-SIZE))
      (lambda (dx dy)
	dx
	(call-with-values
	    (lambda () (graphics-operation window 'WINDOW-FRAME-SIZE))
	  (lambda (fx fy)
	    fx
	    (graphics-operation window 'SET-WINDOW-POSITION
				x
				(- dy (+ y fy)))))))
    window))

(define (rename-window window name)
  (cond ((string? name) 'OK)
	((symbol? name) (set! name (symbol->string name)))
	((number? name) (set! name (number->string name)))
	(else (error "Window name must be string")))
  (graphics-operation window 'set-window-name name)
  (graphics-operation win 'set-icon-name name)
  name)


(define (resize-window window width height)
  (let ((name (graphics-type-name (graphics-type window))))
    (case name
      ((X WIN32) (graphics-operation window 'RESIZE-WINDOW width height))
      ((OS/2) (graphics-operation window 'SET-WINDOW-SIZE width height))
      (else (error "Unsupported graphics type:" name)))))

(define (show-window-size window)
  (call-with-values (lambda () (graphics-device-coordinate-limits window))
    (lambda (x1 y1 x2 y2)
      (newline)
      (display `("width:" ,(+ (- x2 x1) 1) "  height:" ,(+ (- y1 y2) 1))))))

;;; Mouse stuff

(define standard-mouse-shape 34)
(define requesting-input-mouse-shape 11)


;;; Needs to be generalized for OS/2

(define (get-pointer-coordinates window cont)
  ;;; cont = (lambda (x y button) ...)
  (x-graphics/discard-events window)
  (graphics-operation window 'set-mouse-shape requesting-input-mouse-shape)
  ((x-graphics/read-button window)
   (lambda (x y button)
     (graphics-operation window 'set-mouse-shape standard-mouse-shape)
     (cont x y button))))


;;; For gnuplot output

(define *gnuplotting* #f)

(define (start-gnuplot filename)
  (if *gnuplotting*
      (begin (close-port *gnuplotting*)
	     (set! *gnuplotting* #f)))
  (set! *gnuplotting*
	(open-output-file filename))
  'ok)

(define (stop-gnuplot)
  (if *gnuplotting*
      (begin (close-port *gnuplotting*)
	     (set! *gnuplotting* #f)))
  'ok)

;;; For JW

(define frame make-display-frame)
(define plot-frame make-display-frame)

(define (plot-point window x y)
  (if *gnuplotting*
      (begin (newline *gnuplotting*)
	     (write x *gnuplotting*)
	     (display " " *gnuplotting*)
	     (write y *gnuplotting*)))
  (plot-point-internal window x y))

(define (plot-point-internal window x y)
  (graphics-draw-point window 
		       (exact->inexact x)
		       (exact->inexact y)))

(define (plot-line window x0 y0 x1 y1)
  (if *gnuplotting*
      (begin (newline *gnuplotting*)
	     (write x0 *gnuplotting*)
	     (display " " *gnuplotting*)
	     (write y0 *gnuplotting*)
	     (write-line x1 *gnuplotting*)
	     (display " " *gnuplotting*)
	     (write y1 *gnuplotting*)
	     (newline *gnuplotting*)))
  (plot-line-internal window x0 y0 x1 y1))

(define (plot-circle win x y radius #!optional eps)
  (if (default-object? eps) (set! eps 0.01))
  (plot-parametric-fill 
   win
   (lambda (c) (cons (+ x (* radius (cos c))) 
		     (+ y (* radius (sin c)))))
   0. 2pi
   (plane-near? eps)))

(define (plot-line-internal window x0 y0 x1 y1)
  (graphics-draw-line window
		      (exact->inexact x0)
		      (exact->inexact y0)
		      (exact->inexact x1)
		      (exact->inexact y1)))

(define (plot-function window f x0 x1 dx)
  (if *gnuplotting* (newline *gnuplotting*))
  (let loop ((x x0) (fx (f x0)))
    (if *gnuplotting*
	(begin (newline *gnuplotting*)
	       (write x *gnuplotting*)
	       (display " " *gnuplotting*)
	       (write fx *gnuplotting*)))
    (let ((nx (+ x dx)))
      (let ((nfx (f nx)))
	(plot-line-internal window x fx nx nfx)
	(if (< (* (- nx x0) (- nx x1)) 0.)
	    (loop nx nfx))))))

(define (plot-inverse window f y0 y1 dy)
  (if *gnuplotting* (newline *gnuplotting*))
  (let loop ((y y0) (fy (f y0)))
    (if *gnuplotting*
        (begin (newline *gnuplotting*)
	       (write fy *gnuplotting*)
               (display " " *gnuplotting*)
               (write y *gnuplotting*)))
    (let ((ny (+ y dy)))
      (let ((nfy (f ny)))
        (plot-line-internal window fy y nfy ny)
        (if (< (* (- ny y0) (- ny y1)) 0.)
            (loop ny nfy))))))

(define (plot-parametric win f a b dx)
  (if *gnuplotting* (newline *gnuplotting*))
  (let loop ((x a))
    (let ((fx (f x)))
      (plot-point win (car fx) (cdr fx))
      (if (< x b) (loop (+ x dx))))))

#|
(define (plot-parametric-fill win f a b near?)
  (if *gnuplotting* (newline *gnuplotting*))
  (let loop ((a a) (xa (f a)) (b b) (xb (f b)))
    (let ((m (/ (+ a b) 2)))
      (let ((xm (f m)))
	(plot-point win (car xm) (cdr xm))
	(if (not (and (near? xa xm) (near? xb xm)))
	    (begin (loop a xa m xm)
		   (loop m xm b xb)))))))
|#

(define (plot-parametric-fill win f a b near?)
  (if *gnuplotting* (newline *gnuplotting*))
  (let loop ((a a) (xa (f a)) (b b) (xb (f b)))
    (if (not (close-enuf? a b
			  (* *allowable-roundoffs*
			     *machine-epsilon*)))
	(let ((m (/ (+ a b) 2)))
	  (let ((xm (f m)))
	    (plot-point win (car xm) (cdr xm))
	    (if (not (near? xa xm))
		(loop a xa m xm))
	    (if (not (near? xb xm))
		(loop m xm b xb)))))))


;;; Chap 4
(define make-point cons)
(define abscissa car)
(define ordinate cdr)


(define *allowable-roundoffs* 10)

(define (plot-fun win f a b eps)
  (if *gnuplotting* (newline *gnuplotting*))
  (plot-parametric-fill 
   win
   (lambda (x) (cons x (f x)))
   a b 
   (plane-near? eps)))

(define (plane-near? eps)
  (let ((eps^2 (square eps)))
    (lambda (x y)
      (< (+ (square (- (car x) (car y)))
	    (square (- (cdr x) (cdr y))))
	 eps^2))))

(define (cylinder-near? eps)
  (let ((eps^2 (square eps)))
    (lambda (x y)
      (< (+ (square ((principal-value pi)
		     (- (car x) (car y))))
	    (square (- (cdr x) (cdr y))))
	 eps^2))))

(define (torus-near? eps)
  (let ((eps^2 (square eps)))
    (lambda (x y)
      (< (+ (square ((principal-value pi)
		     (- (car x) (car y))))
	    (square ((principal-value pi) 
		     (- (cdr x) (cdr y)))))
	 eps^2))))

#|
;;; for example

(define foo (frame -1 1 -1 1))

(show-window-size foo)
(width: 400   height: 400)

(define pi (* 4 (atan 1 1)))

(plot-function foo (lambda (x) (sin (* pi x))) -1 1 .01)

(plot-line foo -.9 0 .9 0)

(plot-line foo 0 -.9 0 .9)

;;; I pressed left mouse button.
(get-pointer-coordinates foo list)
;Value 24: (.16791979949874686 .5037593984962406 0)

(graphics-close foo)

(start-gnuplot "plot2")
;;; Do above stuff
(stop-gnuplot)
|#

;;; For gjs

(define plotting-window #f)

(define (plot-xy window xs ys)
  (if *gnuplotting* (newline *gnuplotting*))
  (cond ((or (eq? window 'new) (eq? window #t))
	 (set! plotting-window
	       (make-display-frame 0.0 1.0 0.0 1.0)))
	((or (eq? window 'old) (eq? window 'clear) (eq? window #f))
	 'done)
	((eq? window plotting-window)
	 'done)
	(else
	 (if (graphics-device? plotting-window)
	     (graphics-close plotting-window))
	 (set! plotting-window window)))
  (if (not (graphics-device? plotting-window))
      (error "Plotting window is not initialized"))
  (if (eq? window 'clear)
      (graphics-clear plotting-window))

  (if (vector? xs) (set! xs (vector->list xs)))
  (if (vector? ys) (set! ys (vector->list ys)))
  (let ((minx (apply min xs))
	(maxx (apply max xs))
	(miny (apply min ys))
	(maxy (apply max ys)))
    (let ((dx (- maxx minx))
	  (dy (- maxy miny)))
      (if (zero? dx)
	  "Range of x is zero."
	  (if (zero? dy)
	      "Range of y is zero."
	      (map (lambda (x y)
		     (plot-point plotting-window
				 (/ (- x minx) dx)
				 (/ (- y miny) dy)))
		   xs
		   ys))))
    (list minx maxx miny maxy)))

#|
(define xs (iota 700 0.0 0.01))

(length xs)
;Value: 700

(define ys (map sin xs))

(plot-xy 'new xs ys)
|#

(define (plot-f window f)
  (if *gnuplotting* (newline *gnuplotting*))
  (cond ((or (eq? window 'new) (eq? window #t))
	 (set! plotting-window
	       (make-display-frame 0.0 1.0 0.0 1.0)))
	((or (eq? window 'old) (eq? window 'clear) (eq? window #f))
	 'done)
	((eq? window plotting-window)
	 'done)
	(else
	 (if (graphics-device? plotting-window)
	     (graphics-close plotting-window))
	 (set! plotting-window window)))
  (if (not (graphics-device? plotting-window))
      (error "Plotting window is not initialized"))
  (if (eq? window 'clear)
      (graphics-clear plotting-window))

  (call-with-values
      (lambda ()
	(graphics-device-coordinate-limits plotting-window))
    (lambda (left bottom right top)
      (let ((numx (- right left)))
	(call-with-values
	    (lambda ()
	      (graphics-coordinate-limits plotting-window))
	  (lambda (x0 y0 x1 y1)
	    (plot-function plotting-window
			   f
			   x0
			   x1
			   (/ (- x1 x0) numx))))))))
#|
(plot-f (frame 0 7 -1 1) cos)
|#

(define gnuplot
  (let ((count 0))
    (lambda (fs x0 x1 dx #!optional style save-data?)
      (let* ((fs (if (list? fs) fs (list fs)))
	     (style (if (default-object? style) "" style))
	     (dirname (->namestring (user-homedir-pathname)))
	     (file-name (string-append dirname
				       "temp-display"
				       (number->string count)))
	     (clean (if (default-object? save-data?)
			(string-append " ; /bin/rm " file-name ".*")
			""))
	     (data-file-name (string-append "\"" file-name ".data" "\""))
	     (gnuplot-invoke-string
	      (string-append "gnuplot -persist " file-name ".gnuplot"))
	     (gnuplot-control-string
	      (let flp ((fcol 1)
			(ss (string-append data-file-name
					   " using 1:2"
					    " " style " ")))
		(if (= fcol (length fs))
		    (string-append "plot" ss)
		    (flp (+ fcol 1)
			 (string-append ss ", "
					data-file-name
					" using 1:" (number->string (+ fcol 2))
					" " style " "))))))
	(with-output-to-file (string-append file-name ".data")
	  (lambda ()
	    (let loop ((x x0))
	      (begin
		(newline)
		(write x)
		(for-each (lambda (f)
			    (display " ")
			    (write (f x)))
			  fs))
	      (let ((nx (+ x dx)))
		(if (< (* (- nx x0) (- nx x1)) 0.)
		    (loop nx))))))
	(with-output-to-file (string-append file-name ".gnuplot")
	  (lambda () (display gnuplot-control-string)))

	(run-shell-command
	 (string-append "cd " dirname ";"
			gnuplot-invoke-string
			" > /dev/null 2>&1"
			clean)
	 'output #f
	 'shell-file-name "/bin/sh")
	(set! count (+ count 1))
	(if (default-object? save-data?)
	    "done" 
	    `(data-file-name-is ,data-file-name))
	))))

#|
;;; Gnuplot can be used to plot any number of functions, with optional style.
;;; May add further argument to save data files.

(gnuplot sin 0 10 .01)
#| "done" |#

(gnuplot (list sin cos) 0 10 .01)
#| "done" |#

(gnuplot (list sin cos sqrt) 0 10 .01 "with dots")
#| "done" |#

(gnuplot (list sin cos) 0 10 .01 "" #t)
(data-file-name-is "\"/home/gjs/temp-display3.data\"")
|#
