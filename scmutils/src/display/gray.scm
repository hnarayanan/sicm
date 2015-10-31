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

;;;; Graphics with grey scales, from 6.001 picture code by Newts/Hal Abelson

;;;; Miscellaneous Utilities

(define floating-vector-ref (make-primitive-procedure 'floating-vector-ref))
(define floating-vector-set! (make-primitive-procedure 'floating-vector-set!))
(define floating-vector-cons (make-primitive-procedure 'floating-vector-cons))
(define floating-vector-length (make-primitive-procedure 'floating-vector-length))

(define (make-floating-vector length init)
  (let ((result (floating-vector-cons length)))
    (if (not (= init 0.))
	(do ((i 0 (fix:+ i 1)))
	    ((fix:= i length))
	  (floating-vector-set! result i init)))
    result))

(define (floating-vector-copy vector)
  (let* ((length (floating-vector-length vector))
	 (result (floating-vector-cons length)))
    (do ((i 0 (fix:+ i 1)))
	((fix:= i length))
      (floating-vector-set! result i (floating-vector-ref vector i)))
    result))

(define (side-effecting-iter n proc)
  (define (reverse-order-iter count)
    (if (fix:= count n)
	'done
	(begin
	  (proc count)
	  (reverse-order-iter (fix:+ 1 count)))))
  (reverse-order-iter 0))

(define (lo-bound interval-length)
  (fix:- 1 (quotient (fix:+ 1 interval-length) 2)))

(define (up-bound interval-length)
  (floor->exact (1+ (/ interval-length 2))))

(define (floating-vector->list vector)
  (generate-list (floating-vector-length vector)
    (lambda (i)
      (floating-vector-ref vector i))))

#| If not defined elsewhere
(define (generate-list n proc) ; ==> ( (proc 0) (proc 1) ... (proc n-1) )
  (let loop ((i (fix:- n 1)) (list '()))
    (if (fix:< i 0)
        list
        (loop (fix:- i 1) (cons (proc i) list)))))
|#

;;; Colormaps

(define (n-gray-map window)
  (let ((name (graphics-type-name (graphics-type window))))
    (case name
      ((X) (n-gray-map/X11 window))
      ((WIN32) (n-gray-map/win32 window))
      ((OS/2) (n-gray-map/os2 window))
      (else (error "Unsupported graphics type:" name)))))

(define (n-gray-map/X11 window)
  (let ((properties (x-display/properties (x-graphics/display window))))
    (or (1d-table/get properties '6001-GRAY-MAP #f)
	(let ((gm (allocate-grays window)))
	  (1d-table/put! properties '6001-GRAY-MAP gm)
	  gm))))

(define (allocate-grays window)
  (let ((w-cm (graphics-operation window 'get-colormap))
	(visual-info (graphics-operation win 'visual-info)))
    (let ((find-info
	   (let ((length (vector-length visual-info)))
	     (if (= length 0)
		 (error "X-GET-VISUAL-INFO: no results"))
	     (lambda (class depth-min depth-max)
	       (let loop ((index 0))
		 (and (< index length)
		      (let ((info (vector-ref visual-info index)))
			(if (and (= class (vector-ref info 4))
				 ;; kludge, but X made us do it.
				 (<= depth-min (vector-ref info 8) depth-max))
			    info
			    (loop (+ index 1)))))))))
	  (make-gray-map
	   (lambda (n-levels)
	     (let ((gm (make-string n-levels))
		   (step (/ 65535 (- n-levels 1))))
	       (do ((index 0 (+ index 1)))
		   ((= index n-levels))
		 (vector-8b-set!
		  gm
		  index
		  (let ((intensity (round->exact (* step index))))
		    (x-colormap/allocate-color
		     w-cm
		     intensity intensity intensity))))
	       gm))))
      (cond ((find-info visual-class:static-gray 256 256)
	     (make-gray-map 256))
	    ((or (find-info visual-class:gray-scale 256 256)
		 (find-info visual-class:pseudo-color 250 256))
	     (make-gray-map 128))
	    ((find-info visual-class:static-gray 2 2)
	     (make-gray-map 2))
	    (else
	     (error "ALLOCATE-GRAYS: not known display type" window))))))

(define-integrable visual-class:static-gray 0)
(define-integrable visual-class:gray-scale 1)
(define-integrable visual-class:static-color 2)
(define-integrable visual-class:pseudo-color 3)
(define-integrable visual-class:true-color 4)
(define-integrable visual-class:direct-color 5)

(define n-gray-map/win32
  (let ((map (make-string 128)))
    (do ((i 0 (fix:+ i 1)))
	((fix:= i 128))
      (vector-8b-set! map i i))
    (lambda (window) window map)))

(define n-gray-map/os2
  (let ((map (make-string 256)))
    (do ((i 0 (fix:+ i 1)))
	((fix:= i 256))
      (vector-8b-set! map i i))
    (lambda (window) window map)))


(define os2-image-colormap:gray-256
  (make-initialized-vector 256
    (lambda (index)
      (+ (* index #x10000)
	 (* index #x100)
	 index))))

;;;; Pictures

(define (procedure->picture width height fn)
  (let ((new-pic (make-picture width height)))
    (picture-map! new-pic fn)
    new-pic))

(define (picture-map f . pic-list)
  (if (and (apply = (map (lambda (pic) (picture-width pic)) pic-list))
	   (apply = (map (lambda (pic) (picture-height pic)) pic-list)))
      (let* ((width (picture-width (car pic-list)))
	     (height (picture-height (car pic-list)))
	     (new-pic (make-picture width height))
	     (picdata (picture-data new-pic)))
	(cond ((null? pic-list)
	       (error "no pictures -- PICTURE-MAP"))
	      ((null? (cdr pic-list))
	       (let ((p1-data (picture-data (car pic-list))))
		 (let y-loop ((y 0))
		   (if (fix:< y height)
		       (let ((out-yth-row (vector-ref picdata y))
			     (in-yth-row (vector-ref p1-data y)))
			 (let x-loop ((x 0))
			   (if (fix:< x width)
			       (begin
				 (floating-vector-set!
				  out-yth-row x
				  (exact->inexact
				   (f (floating-vector-ref in-yth-row x))))
				 (x-loop (fix:+ 1 x)))
			       (y-loop (fix:+ 1 y)))))))))
	      ((null? (cddr pic-list))
	       (let ((p1-data (picture-data (car pic-list)))
		     (p2-data (picture-data (cadr pic-list))))
		 (let y-loop ((y 0))
		   (if (fix:< y height)
		       (let ((out-yth-row (vector-ref picdata y))
			     (in-yth-row1 (vector-ref p1-data y))
			     (in-yth-row2 (vector-ref p2-data y)))
			 (let x-loop ((x 0))
			   (if (fix:< x width)
			       (begin
				 (floating-vector-set!
				  out-yth-row x
				  (exact->inexact
				   (f (floating-vector-ref in-yth-row1 x)
				      (floating-vector-ref in-yth-row2 x))))
				 (x-loop (fix:+ 1 x)))
			       (y-loop (fix:+ 1 y)))))))))
	      (else
	       (let ((data-list
		      (map (lambda (pic) (picture-data pic)) pic-list)))
		 (let y-loop ((y 0))
		   (if (fix:< y height)
		       (let ((out-yth-row (vector-ref picdata y))
			     (in-yth-rows (map (lambda (data)
						 (vector-ref data y))
					       data-list)))
			 (let x-loop ((x 0))
			   (if (fix:< x width)
			       (begin
				 (floating-vector-set!
				  out-yth-row x
				  (exact->inexact
				   (apply f
					  (map (lambda (row)
						 (floating-vector-ref row x))
					       in-yth-rows))))
				 (x-loop (fix:+ 1 x)))
			       (y-loop (fix:+ 1 y))))))))))
	(picture-set-data! new-pic picdata)
	new-pic)
      (error "picture sizes do not match -- PICTURE-MAP")))

(define (picture-display window pic #!optional pic-min pic-max)
  (define (check-image pic window brick-wid brick-hgt)
    (if (image? (picture-image pic))
	(let ((image (picture-image pic)))
	  (and (1d-table/get (graphics-device/properties window) image #f)
	       (fix:= (fix:* (picture-width pic) brick-wid)
		      (image/width image))
	       (fix:= (fix:* (picture-height pic) brick-hgt)
		      (image/height image))))
	#f))

  (call-with-values
      (lambda ()
	(graphics-device-coordinate-limits window))
    (lambda (x1 y1 x2 y2)
      (set! *last-picture-displayed* pic)
      (graphics-set-coordinate-limits window 0 (- y2 y1) (- x2 x1) 0)
      (let* ((win-wid (+ 1 (abs (- x2 x1))))
	     (win-hgt (+ 1 (abs (- y1 y2))))
	     (len&margin (integer-divide win-wid (picture-width pic)))
	     (wid&margin (integer-divide win-hgt (picture-height pic)))
	     (h-margin (integer-divide-remainder len&margin))
	     (v-margin (integer-divide-remainder wid&margin))
	     (brick-wid (integer-divide-quotient len&margin))
	     (brick-hgt (integer-divide-quotient wid&margin))
	     (pic-min (if (default-object? pic-min)
			  (picture-min pic)
			  (exact->inexact pic-min)))
	     (pic-max (if (default-object? pic-max)
			  (picture-max pic)
			  (exact->inexact pic-max)))
	     (true-min-max? (and (= pic-min (picture-min pic))
				 (= pic-max (picture-max pic))))
	     (image-cached? (check-image pic window brick-wid brick-hgt)))
	(if (or (fix:< brick-wid 1) (fix:< brick-hgt 1))
	    (error "Window is too small to display" pic '--PICTURE-DISPLAY)
	    (let ((image (if (and image-cached? true-min-max?)
			     (picture-image pic)
			     (build-image pic window
					 brick-wid brick-hgt
					 pic-min pic-max))))
	      (graphics-clear window)
	      (image/draw window
			  (quotient h-margin 2)
			  (- (quotient v-margin 2))
			  image)
	      (if (and true-min-max? (not image-cached?))
		  (picture-set-image! pic image))))))))

(define *last-picture-displayed*
  false)

;;; Representation of pictures using records

(declare (usual-integrations))

(define picture-type (make-record-type 
		      'picture 
		      '(width
			height
			data
			min
			max 
			image)))

(define %make-picture (record-constructor picture-type '(width height)))

(define %picture-min (record-accessor picture-type 'min))
(define %picture-max (record-accessor picture-type 'max))
(define %picture-set-data! (record-updater picture-type 'data))
(define %picture-set-image! (record-updater picture-type 'image))
(define %picture-set-min! (record-updater picture-type 'min))
(define %picture-set-max! (record-updater picture-type 'max))

(define (make-picture width height #!optional initial-val)
  (let ((pic (%make-picture width height))
	(initial-val (if (default-object? initial-val)
			 0.
			 (exact->inexact initial-val))))
    (%picture-set-min! pic initial-val)
    (%picture-set-max! pic initial-val)
    (%picture-set-data! pic 
			(make-initialized-vector
			 height
			 (lambda (n)
			   n	; ignored
			   (make-floating-vector width initial-val))))
    (%picture-set-image! pic #f)
    pic))

(define picture? (record-predicate picture-type))

(define picture-width
  (record-accessor picture-type 'width))

(define picture-height
  (record-accessor picture-type 'height))

(define picture-data
  (record-accessor picture-type 'data))

(define picture-image
  (record-accessor picture-type 'image))

(define (picture-set-image! picture image)
  (let ((img (picture-image picture)))
    (if (image? img)
	(image/destroy img))
    (%picture-set-image! picture image)))

(define (picture-min picture)
  (let ((pic-min (%picture-min picture)))
    (if (not pic-min) 
	(begin (find-min-max picture)
	       (%picture-min picture))
	pic-min)))

(define (picture-max picture)
  (let ((pic-max (%picture-max picture)))
    (if (not pic-max) 
	(begin (find-min-max picture)
	       (%picture-max picture))
	pic-max)))

(define (make-picture-referencer bad-type-predicate bad-range-signal)
  (lambda (picture x y)
    (cond ((bad-type-predicate x)
	   (error:wrong-type-argument x "picture X coordinate" 'PICTURE-REF))
	  ((bad-type-predicate y)
	   (error:wrong-type-argument y "picture Y coordinate" 'PICTURE-REF))
	  ((not (and (fix:>= x 0)
		     (fix:< x (picture-width picture))))
	   (bad-range-signal x 'PICTURE-REF))
	  ((not (and (fix:>= y 0)
		     (fix:< y (picture-height picture))))
	   (bad-range-signal y 'PICTURE-REF))
	  (else
	   (floating-vector-ref
	    (vector-ref (picture-data picture) y) x)))))

(define (make-picture-setter bad-type-predicate bad-range-signal)
  (lambda (picture x y value)
    (cond ((bad-type-predicate x)
	   (error:wrong-type-argument x "picture X coordinate" 'PICTURE-SET!))
	  ((bad-type-predicate y)
	   (error:wrong-type-argument y "picture Y coordinate" 'PICTURE-SET!))
	  ((not (and (fix:>= x 0)
		     (fix:< x (picture-width picture))))
	   (bad-range-signal x 'PICTURE-SET!))
	  ((not (and (fix:>= y 0)
		     (fix:< y (picture-height picture))))
	   (bad-range-signal y 'PICTURE-SET!))
	  (else
	   (floating-vector-set! (vector-ref (picture-data picture) y)
			x (exact->inexact value))
	   (invalidate-cached-values picture)))))

(define picture-ref (make-picture-referencer
		     (lambda (var)
		       (declare (integrate var))
		       (not (fix:fixnum? var)))
		     error:bad-range-argument))

(define no-error-picture-ref (make-picture-referencer
			  (lambda (var)
			    (declare (integrate var))
			    var  ;ignored
			    false)
			  (lambda (var proc-name)
			    var proc-name   ;ignored
			    false)))

(define picture-set! (make-picture-setter
		      (lambda (var)
			(declare (integrate var))
			(not (fix:fixnum? var)))
		      error:bad-range-argument))

(define no-error-picture-set! (make-picture-setter
			   (lambda (var)
			     (declare (integrate var))
			     var  ;ignored
			     false)
			   (lambda (var proc-name)
			     var proc-name  ;ignored 
			     false)))

(define (picture-map! picture fn)
  (let ((picdata (picture-data picture))
	(width (picture-width picture))
	(height (picture-height picture)))
    (let y-loop ((y 0))
      (if (< y height)
	  (let ((yth-row (vector-ref picdata y)))
	    (let x-loop ((x 0))
	      (if (< x width)
		  (begin (floating-vector-set! yth-row x 
				      (exact->inexact 
				       (fn x y)))
			 (x-loop (1+ x)))
		  (y-loop (1+ y))))))
      (invalidate-cached-values picture))))

(define (picture-set-data! picture data)
  (%picture-set-data! picture data)
  (invalidate-cached-values picture))

;;; Note that picture-data and picture-set-data! are both unsafe operations
;;; in the sense that both of them do not ensure that only floating point 
;;; numbers are ever stored in the picture array.


(define (invalidate-cached-values picture)
  (%picture-set-min! picture #f)
  (%picture-set-max! picture #f)
  (let ((img (picture-image picture)))
    (if (image? img)
	(image/destroy img))
    (%picture-set-image! picture '())))

(define (find-min-max picture)
  (let* ((picdata (picture-data picture))
	 (width (picture-width picture))
	 (height (picture-height picture))
	 (current-min (floating-vector-ref (vector-ref picdata 0) 0))
	 (current-max current-min))
    (let y-loop ((y 0))
      (if (< y height)
	  (let ((yth-row (vector-ref picdata y)))
	    (let x-loop ((x 0))
	      (if (< x width)
		  (let ((v (floating-vector-ref yth-row x)))
		    (set! current-min (min current-min v))
		    (set! current-max (max current-max v))
		    (x-loop (1+ x)))
		  (y-loop (1+ y)))))))
    (%picture-set-min! picture current-min)
    (%picture-set-max! picture current-max)))

;;; Procedure to build an image given a picture and the magnification factors

(define (build-image pic window h-sf v-sf pic-min pic-max)
  (let* ((gray-map (n-gray-map window))
	 (pic-height (picture-height pic))	;py
	 (pic-width (picture-width pic))	;x
	 (pic-data (picture-data pic))
	 (image-width (fix:* h-sf pic-width)) ;x
	 (image-height (fix:* v-sf pic-height)) ;iy
	 (image (image/create window image-width image-height))
	 (byte-string (make-string (fix:* image-width image-height)))
	 (py-max (- pic-height 1))
	 (rect-index-height (fix:* v-sf image-width))
	 (range (flo:- pic-max pic-min))
	 (index-range (string-length gray-map))
	 (mul (if (flo:< range 1e-12)
		  0.
		  (/ index-range
		     (flo:* (flo:+ 1. 7.142e-8)	; 1+epsilon
			    range))))
	 (gray-pixel
	  (lambda (pixel-value)
	    (vector-8b-ref
	     gray-map
	     (let ((pixel
		    (flo:floor->exact
		     (flo:* mul (flo:- pixel-value pic-min)))))
	       (cond ((fix:< pixel 0) 0)
		     ((fix:< pixel index-range) pixel)
		     (else (fix:- index-range 1))))))))

    (cond ((and (fix:= 1 h-sf) (fix:= 1 v-sf))
	   (let y-loop ((py py-max) (iy-index 0))
	     (if (fix:<= 0 py)
		 (begin
		   (let ((pic-row (vector-ref pic-data py)))
		     (let x-loop ((px 0))
		       (if (fix:< px pic-width)
			   (begin
			     (vector-8b-set!
			      byte-string
			      (fix:+ px iy-index)
			      (gray-pixel (floating-vector-ref pic-row px)))
			     (x-loop (fix:+ px 1))))))
		   (y-loop (fix:- py 1) (fix:+ iy-index rect-index-height))))))

	  ((and (fix:= 2 h-sf) (fix:= 2 v-sf))
	   (let y-loop ((py py-max) (iy-index 0))
	     (if (fix:<= 0 py)
		 (let ((pic-row (vector-ref pic-data py)))
		   (let x-loop ((px 0) (ix 0))
		     (if (fix:< px pic-width)
			 (let* ((n-is-0 (fix:+ ix iy-index))
				(n-is-1 (fix:+ n-is-0 image-width))
				(v
				 (gray-pixel
				  (floating-vector-ref pic-row px))))
			   (vector-8b-set! byte-string n-is-0 v)
			   (vector-8b-set! byte-string (fix:+ n-is-0 1) v)
			   (vector-8b-set! byte-string n-is-1 v)
			   (vector-8b-set! byte-string (fix:+ n-is-1 1) v)
			   (x-loop (fix:+ px 1) (fix:+ ix h-sf)))
			 (y-loop (fix:- py 1) 
				 (fix:+ iy-index rect-index-height))))))))

	  ((and (fix:= 3 h-sf) (fix:= 3 v-sf))
	   (let y-loop ((py py-max) (iy-index 0))
	     (if (fix:<= 0 py)
		 (let ((pic-row (vector-ref pic-data py)))
		   (let x-loop ((px 0) (ix 0))
		     (if (fix:< px pic-width)
			 (let* ((row0 (fix:+ ix iy-index))
				(row1 (fix:+ row0 image-width))
				(row2 (fix:+ row1 image-width))
				(v
				 (gray-pixel
				  (floating-vector-ref pic-row px))))
			   (vector-8b-set! byte-string row0 v)
			   (vector-8b-set! byte-string (fix:+ row0 1) v)
			   (vector-8b-set! byte-string (fix:+ row0 2) v)
			   (vector-8b-set! byte-string row1 v)
			   (vector-8b-set! byte-string (fix:+ row1 1) v)
			   (vector-8b-set! byte-string (fix:+ row1 2) v)
			   (vector-8b-set! byte-string row2 v)
			   (vector-8b-set! byte-string (fix:+ row2 1) v)
			   (vector-8b-set! byte-string (fix:+ row2 2) v)
			   (x-loop (fix:+ px 1) (fix:+ ix h-sf)))
			 (y-loop (fix:- py 1) 
				 (fix:+ iy-index rect-index-height))))))))

	  ((and (fix:= 4 h-sf) (fix:= 4 v-sf))
	   (let y-loop ((py py-max) (iy-index 0))
	     (if (fix:<= 0 py)
		 (let ((pic-row (vector-ref pic-data py)))
		   (let x-loop ((px 0) (ix 0))
		     (if (fix:< px pic-width)
			 (let* ((row0 (fix:+ ix iy-index))
				(row1 (fix:+ row0 image-width))
				(row2 (fix:+ row1 image-width))
				(row3 (fix:+ row2 image-width))
				(v
				 (gray-pixel
				  (floating-vector-ref pic-row px))))
			   (vector-8b-set! byte-string row0 v)
			   (vector-8b-set! byte-string (fix:+ row0 1) v)
			   (vector-8b-set! byte-string (fix:+ row0 2) v)
			   (vector-8b-set! byte-string (fix:+ row0 3) v)
			   (vector-8b-set! byte-string row1 v)
			   (vector-8b-set! byte-string (fix:+ row1 1) v)
			   (vector-8b-set! byte-string (fix:+ row1 2) v)
			   (vector-8b-set! byte-string (fix:+ row1 3) v)
			   (vector-8b-set! byte-string row2 v)
			   (vector-8b-set! byte-string (fix:+ row2 1) v)
			   (vector-8b-set! byte-string (fix:+ row2 2) v)
			   (vector-8b-set! byte-string (fix:+ row2 3) v)
			   (vector-8b-set! byte-string row3 v)
			   (vector-8b-set! byte-string (fix:+ row3 1) v)
			   (vector-8b-set! byte-string (fix:+ row3 2) v)
			   (vector-8b-set! byte-string (fix:+ row3 3) v)
			   (x-loop (fix:+ px 1) (fix:+ ix h-sf)))
			 (y-loop (fix:- py 1) 
				 (fix:+ iy-index rect-index-height))))))))

	  (else 
	   (let y-loop ((py py-max) (iy-index 0))
	     (if (fix:<= 0 py)
		 (let ((pic-row (vector-ref pic-data py)))
		   (let x-loop ((px 0) (ix 0))
		     (if (fix:< px pic-width)
			 (let* ((v
				 (gray-pixel (floating-vector-ref pic-row px)))
				(n-start (fix:+ ix iy-index))
				(n-end (fix:+ n-start rect-index-height)))
			   (let n-loop ((n n-start))
			     (if (fix:< n n-end)
				 (let ((m-end (fix:+ n h-sf)))
				   (let m-loop ((m n))
				     (if (fix:< m m-end)
					 (begin
					   (vector-8b-set! byte-string m v)
					   (m-loop (fix:+ m 1)))
					 (n-loop (fix:+ n image-width)))))
				 (x-loop (fix:+ px 1) (fix:+ ix h-sf)))))
			 (y-loop (fix:- py 1) 
				 (fix:+ iy-index rect-index-height)))))))))
    ;; Kludge: IMAGE/FILL-FROM-BYTE-VECTOR should take an argument
    ;; that specifies what color a given byte in BYTE-STRING maps to.
    ;; OS/2 requires this information, so we supply it here.
    (if (eq? 'OS/2 microcode-id/operating-system)
	(os2-image/set-colormap image os2-image-colormap:gray-256))
    (image/fill-from-byte-vector image byte-string)
    (1d-table/put! (graphics-device/properties window) image #t)
    image))

#|
;;; For example

(define foo (make-window 1000 100 0 0))
(define bar
  (procedure->picture 500 10
		      (lambda (x y)
			(* (sin (/ x 5.)) y))))
(picture-display foo bar)
(graphics-close foo)
|#
