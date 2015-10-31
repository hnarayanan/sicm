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

;;; Suppose we are given a sequence of ellipsoids, each representing
;;; the error in a measurement of sequential iterations of a given
;;; map.  We run the map both directions on the sequence, and push the
;;; ellipsoids thru the map, making a new sequence that has
;;; contributions of information from all of the other measurements.

;;; "FORTRAN" style! (for a bit of fun)

(define (shadow phase-space f df f^-1 df^-1)
  (let ((jd (joint-distribution phase-space)))
    (lambda (data)
      (let* ((size (vector-length data))
	     (top (- size 1))
	     (top-1 (- size 2))
	     (forward (make-vector size))
	     (backward (make-vector size))
	     (answer (make-vector size)))

	(vector-set! forward 0
	  (push-ellipsoid-thru-map f df^-1 
				   (vector-ref data 0)))
	(for-indices 1 top 1+
	  (lambda (i)
	    (vector-set! forward i
	      (push-ellipsoid-thru-map f df^-1
		(jd (vector-ref forward (-1+ i))
		    (vector-ref data i))))))

	(vector-set! backward top
	  (push-ellipsoid-thru-map f^-1 df 
				   (vector-ref data top)))
	(for-indices top-1 0 -1+
	  (lambda (i)
	    (vector-set! backward i
	      (push-ellipsoid-thru-map f^-1 df
		(jd (vector-ref backward (1+ i))
			 (vector-ref data i))))))
	
	(vector-set! answer 0
		     (jd (vector-ref data 0) (vector-ref backward 1)))
	(for-indices 1 top-1 1+
          (lambda (i)
	    (vector-set! answer i
			 (jd (vector-ref forward (-1+ i))
			     (vector-ref data i)
			     (vector-ref backward (1+ i))))))
	(vector-set! answer top
		     (jd (vector-ref data top)
			 (vector-ref forward top-1)))
	
	(list answer forward backward)))))

(define (for-indices start end succ proc)
  (let lp ((i start))
    (if (= i end)
	(proc i)
	(begin (proc i)
	       (lp (succ i)))))
  'done)

;;; Suppose we are given a sequence of ellipsoids, each representing
;;; the error in a measurement of sequential iterations of a given
;;; map, we produce a new sequence, formed by iterating the map over
;;; the given sequence, in both directions, and merging the
;;; distributions with the original ellipsoids.

;;; operates on list of ellipsoids
(define (smooth phase-space f df f^-1 df^-1 data)
  (let ((forward
	 (map (lambda (ellipsoid)
		(push-ellipsoid-thru-map f df^-1 ellipsoid))
	      data))
	(back
	 (map (lambda (ellipsoid)
		(push-ellipsoid-thru-map f^-1 df ellipsoid))
	      data))
	(jd (joint-distribution phase-space)))
    (append (list (jd (cadr back) (car data)))
	    (map jd (cddr back) (cdr data) forward)
	    (list (jd (cadr (reverse forward))
		      (car (last-pair data)))))))

(define (n-smooth jd f df f^-1 df^-1 half-width)
  (lambda (data)			; a vector of length m
    (let ((forward
	   (accum-iter half-width
		       (lambda (d)
			 (map-vector (lambda (ellipsoid)
				       (push-ellipsoid-thru-map f df^-1 ellipsoid))
				     d))
		       data))
	  (backward
	   (accum-iter half-width
		       (lambda (d)
			 (map-vector (lambda (ellipsoid)
				       (push-ellipsoid-thru-map f^-1 df ellipsoid))
				     d))
		       data)))
      (generate-vector (vector-length data)
		       (lambda (i)
			 (apply jd
				(append (get-elements i forward)
					(list (vector-ref data i))
					(get-back-elements i backward))))))))


(define (map-vector f v)
  (generate-vector (vector-length v)
		   (lambda (i) 
		     (f (vector-ref v i)))))

(define (get-elements i lov)
  (if (or (= i 0) (null? lov))
      '()
      (cons (vector-ref (car lov) (- i 1))
	    (get-elements (- i 1) (cdr lov)))))

(define (get-back-elements i lov)
  (if (null? lov) 
      '()
      (let ((n (vector-length (car lov)))
	    (j (+ i 1)))
	(if (< j n)
	    (cons (vector-ref (car lov) j)
		  (get-back-elements j (cdr lov)))
	    '()))))
      
(define (accum-iter n f d)
  (if (= n 0)
      '()
      (let ((nd (f d)))
	(cons nd
	      (accum-iter (- n 1)
			  f
			  nd)))))

(define current-data)
(define stash-current-data true)

(define (smooth-it phase-space f df f^-1 df^-1 half-width plot-comparison)
  (let* ((jd (joint-distribution phase-space))
	 (s  (n-smooth jd f df f^-1 df^-1 half-width))
	 (si (n-smooth (lambda args
			 (let ((e (apply jd args)))
			   (make-ellipsoid (center-point e) 
					   (scalar*matrix (/ 1.0 (length args))
							  (covariance-matrix e)))))
		       f
		       df
		       f^-1
		       df^-1
		       half-width)))
    (define lp
      (lambda (data n-times)
	(if compare-to-true-orbit
	    (plot-comparison comparison-window data true-orbit phase-space))
	(if stash-current-data
	    (set! current-data data))
	(if (= n-times 0)
	    data
	    (lp (si data) (- n-times 1)))))
    (lambda (data n-times)
      (lp (s data) (- n-times 1)))))

;;; stuff to make data, orbits, add noise, compare orbits, ...

(define (make-data sigma orbit)
  (define (make-covariance-matrix sigma)
     (if (number? sigma)
	 (scalar*matrix (/ 1.0 (* 2.0 (square sigma)))
			(identity-matrix (vector-length (car orbit))))
	 (diagonal-matrix
		 (map-vector (lambda (s) (/ 1.0 (* 2.0 (square s))))
			     sigma))))
  (define (data-list orbit sigma cov)
    (let ((list-data (map (lambda (c)
			    (make-ellipsoid c cov))
			  (add-noise sigma orbit))))
      (list->vector list-data)))
  (if (pair? sigma)
      (data-list orbit (car sigma) (make-covariance-matrix (cdr sigma)))
      (data-list orbit sigma (make-covariance-matrix sigma))))


(define (make-orbit f ic n)
  (if (= n 0)
      '()
      (cons ic (make-orbit f (f ic) (- n 1)))))

(define (add-noise sigma orbit)
  (let ((gr
	 (gaussian-random-tuples (vector-length (car orbit))
				 (length orbit))))
    (let ((sgr (if (number? sigma)
		   (map (scale-vector sigma) gr)
		   (map (lambda (v)
			  (map-2-vectors * sigma v))
			gr))))
      (map add-vectors orbit sgr))))

(define (map-2-vectors f v1 v2)
  (generate-vector (vector-length v1)
		   (lambda (i) 
		     (f (vector-ref v1 i) (vector-ref v2 i)))))


(define (logify eps)
  (let ((log10 (log 10.0)))
    (let ((logeps (/ (log eps) log10)))
      (lambda (x)
	(if (< x eps)
	    logeps
	    (/ (log x) log10))))))

(define (square x) (* x x))


(define (compute-distances phase-space o1 o2)
  (let ((d (phase-space 'distance)))
    (generate-vector (vector-length o1)
		     (lambda (i) 
		       (d (vector-ref o1 i)
			  (vector-ref o2 i))))))


(define (plot-comparison comparison-window data true-orbit phase-space)
  (let ((true-orbit-vector (list->vector true-orbit)))
    (plot-vector-sequence comparison-window
			  (map-vector (logify eps)
				      (compute-distances phase-space
							 true-orbit-vector
							 (map-vector center-point
								     data))))))

(define (plot-comparison-2 comparison-window data true-orbit phase-space)
  (let ((true-orbit-vector (list->vector true-orbit)))
    (plot-vector-sequence comparison-window
			  (map-vector (logify eps)
				      (compute-distances torus
							 (map-vector (lambda (v) (vector-head v 2))
								     true-orbit-vector)
							 (map-vector (lambda (v) (vector-head v 2))
								     (map-vector center-point
										 data))))
			  "green")
    (plot-vector-sequence comparison-window
			  (map-vector (logify eps)
				      (compute-distances euclidean-space
							 (map-vector (lambda (v) (vector-tail v 2))
								     true-orbit-vector)
							 (map-vector (lambda (v) (vector-tail v 2))
								     (map-vector center-point
										 data))))
			  "red")))

;;; Test stuff below

(define (standard-maps K)
  (define (map v)
    (let ((x (vector-ref v 0))
	  (y (vector-ref v 1)))
      (let ((newy (+ y (* K (sin x)))))
	(vector (principal-value-0 (+ x newy))
		(principal-value-0 newy)))))
  (define (inverse v)
    (let ((x (vector-ref v 0))
	  (y (vector-ref v 1)))
      (let ((newx (- x y)))
	(vector (principal-value-0 newx)
		(principal-value-0 (- y (* k (sin newx))))))))
  (define (variations v)
    (let ((x (vector-ref v 0))
	  (y (vector-ref v 1)))
      (let ((kcosx (* K (cos x))))
	(vector (vector (+ kcosx 1.0) 1.0)
		(vector kcosx 1.0)))))
  (define (inverse-variations v)
    (let ((x (vector-ref v 0))
	  (y (vector-ref v 1)))
      (let ((kcosx-y (* -1.0 K (cos (- x y)))))
	(vector (vector 1.0 -1.0)
		(vector kcosx-y (- 1.0 kcosx-y))))))
  (list map inverse variations inverse-variations))


(define (smaps-with-K)
  (define (map v)
    (let ((x (vector-ref v 0))
	  (y (vector-ref v 1))
	  (K (vector-ref v 2)))
      (let ((newy (+ y (* K (sin x)))))
	(vector (principal-value-0 (+ x newy))
		(principal-value-0 newy)
		K))))
  (define (inverse v)
    (let ((x (vector-ref v 0))
	  (y (vector-ref v 1))
	  (K (vector-ref v 2)))
      (let ((newx (- x y)))
	(vector (principal-value-0 newx)
		(principal-value-0 (- y (* k (sin newx))))
		K))))
  (define (variations v)
    (let ((x (vector-ref v 0))
	  (y (vector-ref v 1))
	  (K (vector-ref v 2)))
      (let ((kcosx (* K (cos x)))
	    (sinx (sin x)))
	(vector (vector (+ kcosx 1.0) 1.0 sinx)
		(vector kcosx 1.0 sinx)
		#(0.0 0.0 1.0)))))
  (define (inverse-variations v)
    (let ((x (vector-ref v 0))
	  (y (vector-ref v 1))
	  (K (vector-ref v 2)))
      (let ((-kcosx-y (* -1.0 K (cos (- x y))))
	    (sinx-y (sin (- x y))))
	(vector #(1.0 -1.0 0.0)
		(vector -kcosx-y (- 1.0 -kcosx-y) sinx-y)
		#(0.0 0.0 1.0)))))
  (list map inverse variations inverse-variations))


(define true-orbit)
(define measured-data)
(define comparison-window)
(define eps 1e-18)
(define compare-to-true-orbit false)


(define (try-standard-map K sigma initial-point record-size iterations smoothing-window)
  (set! comparison-window (frame 600 0 500 500 0 record-size -18 0))
  (let ((s (standard-maps K)))
    (let ((f (car s)) (f^-1 (cadr s)) (df (caddr s)) (df^-1 (cadddr s)))
      (set! compare-to-true-orbit true)
      (set! true-orbit (make-orbit f initial-point record-size))
      (set! measured-data (make-data sigma true-orbit))
      (plot-comparison comparison-window measured-data true-orbit torus)
      ((smooth-it torus f df f^-1 df^-1 smoothing-window plot-comparison)
       measured-data iterations))))



(define improved-data)

(define (shadow-standard-map K sigma initial-point record-size)
  (set! comparison-window (frame 600 0 500 500 0 record-size -18 0))
  (let ((s (standard-maps K)))
    (let ((f (car s)) (f^-1 (cadr s)) (df (caddr s)) (df^-1 (cadddr s)))
      (set! true-orbit (make-orbit f initial-point record-size))
      (set! measured-data (make-data sigma true-orbit))
      (plot-comparison comparison-window measured-data true-orbit torus)
      (set! improved-data ((shadow torus f df f^-1 df^-1) measured-data))
      (plot-comparison comparison-window (car improved-data) true-orbit torus)
      'done
      )))


;;;(set! comparison-window (frame 600 0 500 500 0 record-size -18 0))

(define (try-smap-with-K sigma initial-point record-size iterations smoothing-window)
  (graphics-clear comparison-window)
  (let ((s (smaps-with-K)))
    (let ((f (car s)) (f^-1 (cadr s)) (df (caddr s)) (df^-1 (cadddr s)))
      (set! compare-to-true-orbit true)
      (set! true-orbit (make-orbit f initial-point record-size))
      (set! measured-data (make-data sigma true-orbit))
      (plot-comparison-2 comparison-window measured-data true-orbit torusXeuclidean)
      ((smooth-it torusXeuclidean f df f^-1 df^-1 smoothing-window plot-comparison-2)
       measured-data iterations))))


(define (eigenvalues m)
  (let ((t (+ (matrix-ref m 0 0) (matrix-ref m 1 1))) ; (trace m)
	(d (- (* (matrix-ref m 0 0) (matrix-ref m 1 1)) ;(determinant m)
	      (* (matrix-ref m 0 1) (matrix-ref m 1 0)))))
    (sort (list (/ (+ t (sqrt (- (* t t) (* 4 d)))) 2)
		(/ (- t (sqrt (- (* t t) (* 4 d)))) 2))
	  >)))


(define (plot-estimated-errors error-window data)	;a set of ellipsoids
  (let ((record-size (vector-length data)))
    (let ((ev (map-vector (lambda (e)
			    (let ((ev (eigenvalues (covariance-matrix e))))
			      (list (/ 1 (sqrt (* 2 (car ev)))) (/ 1 (sqrt (* 2 (cadr ev)))))))
			  data)))
      (plot-vector-sequence error-window
			    (map-vector (lambda (e) (log10 (cadr e))) ev)
			    "red")
      (plot-vector-sequence error-window
			    (map-vector (lambda (e) (log10 (car e))) ev)
			    "green")
      (plot-vector-sequence error-window
			    (map-vector (lambda (e) (* .5 (log10 (* (car e) (cadr e)))))
					ev)
			    "yellow"))))

(define log10
  (let ((l10 (log 10)))
    (lambda (x)
      (/ (log x) l10))))


(define (gritch m)			;m a covariance matrix
  (let ((a11 (matrix-ref m 0 0))
	(a22 (matrix-ref m 1 1))
	(a33 (matrix-ref m 2 2))
	(a12 (* 2.0 (matrix-ref m 0 1)))
	(a13 (* 2.0 (matrix-ref m 0 2)))
	(a23 (* 2.0 (matrix-ref m 1 2))))
    (/ 1
       (sqrt (* 2
		(- (- a33 (/ (square a13) 4 a11))
		   (/ (square (- a23 (/ (* a12 a13) 2 a11)))
		      (* 4 (- a22 (/ (square a12) 4 a11))))))))))












