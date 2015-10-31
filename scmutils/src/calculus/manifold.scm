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

;;;;  Manifolds are built here.

(define-record-type <manifold-point>
    (%make-manifold-point spec manifold)
    manifold-point?
  (spec manifold-point-representation)
  (manifold point->manifold)
  (coordinate-representations coordinate-reps set-coordinate-reps!))

(define (make-manifold-point spec manifold coordinate-system coordinate-rep)
  (let ((m (%make-manifold-point spec manifold)))
    (set-coordinate-reps! m (list (list coordinate-system coordinate-rep)))
    m))

(define (transfer-point embedded embedding)
  (lambda (point)
    (assert (eq? (embedded 'manifold) (point->manifold point)))
    (assert (= ((embedded 'manifold) 'embedding-dimension)
	       ((embedding 'manifold) 'embedding-dimension)))		 
    (let ((m (%make-manifold-point
	      (manifold-point-representation point)
	      (embedding 'manifold))))
      (set-coordinate-reps! m '())
      m)))

;;; A Kludge.
(define (get-coordinate-rep point)
  ;(assert (eq? ((point->manifold point) 'name) manifold-name))
  (let ((rep
	 (find (lambda (rep)
		 ;(eq? ((car rep) 'name) chart-name)
		 #t)
	       (coordinate-reps point))))
    (assert rep)
    (cadr rep)))

(define (get-coordinates point coordinate-system thunk)
  (let ((entry (assq coordinate-system (coordinate-reps point))))
    (if entry
	(cadr entry)
	(let ((val (s:map/r simplify-numerical-expression (thunk))))
	  (set-coordinate-reps! point
				(cons (list coordinate-system val)
				      (coordinate-reps point)))
	  val))))

(define (my-manifold-point? point manifold)
  (and (manifold-point? point)
       (eq? (point->manifold point) manifold)))


;;; There is a kludge in this system... A single coordinate is just
;;; the number:

(define (c:generate n type proc)
  (if (fix:= n 1)
      (proc 0)
      (s:generate n type proc)))

(define (c:lookup m struct)
  (if (structure? struct)
      (assq m
	    (vector->list
	     (if (up? struct)
		(up->vector struct)
		(down->vector struct))))
      struct))

(define (specify-manifold manifold-name #!optional type)
  (if (default-object? type) (set! type Real))
  (let ((patches '()) (counter 0) (distinguished-points '()))
    (define (generator dimension #!optional embedding-dimension)
      (assert (exact-integer? dimension))
      (if (default-object? embedding-dimension)
	  (set! embedding-dimension dimension)
	  (assert (and (exact-integer? embedding-dimension)
		       (fix:>= embedding-dimension dimension))))
      (set! counter (+ counter 1))      
      (let ((name
	     (symbol (let* ((namestring (symbol->string manifold-name))
			    (i (string-search-forward "^n" namestring)))
		       (if i
			   (string->symbol
			    (string-append (string-head namestring i)
					   "^"
					   (number->string dimension)))
			   manifold-name))
		     '-
		     counter)))
	(define (the-manifold m)
	  (case m
	    ((name manifold-name) name)
	    ((manifold) the-manifold)
	    ((type manifold-type) type)
	    ((dimension manifold-dimension) dimension)
	    ((embedding-dimension) embedding-dimension)
	    ((get-patch)
	     (lambda (patch-name)
	       (let ((patch-entry (assq patch-name patches)))
		 (if patch-entry
		     ((cadr patch-entry) the-manifold)
		     (error "Unknown patch" patch-name name)))))
	    ((distinguished-points) distinguished-points)
	    ((add-distinguished-point!)
	     (lambda (sspec m)
	       (set! distinguished-points
		     (cons (list sspec m) distinguished-points))))
	    ((patch-names)
	     (map car patches))
	    (else
	     (error "Unknown message: manifold generator" m))))
	the-manifold))
    (define (setup m)
      (case m
	((new-patch)
	 (lambda (new-patch-name patch-generator patch-setup)
	   (set! patches
		 (cons (list new-patch-name
			     patch-generator
			     patch-setup)
		       patches))))
	((patch-setup)
	 (lambda (patch-name)
	   (let ((patch-entry (assq patch-name patches)))
	     (if patch-entry
		 (caddr patch-entry)
		 (error "Unknown patch" patch-name)))))
	((generator) generator)	
	(else (error "Unknown message: manifold setup" m))))
    setup))

;;; Coordinate patches are added to manifolds.

(define (patch patch-name manifold)
  ((manifold 'get-patch) patch-name))

(define (attach-patch patch-name manifold-spec)
  (let ((coordinate-systems '()))
    (define (patch-generator manifold)
      (define (the-patch m)
	(case m
	  ((name patch-name) patch-name)
	  ((patch) the-patch)
	  ((get-coordinate-system)
	   (lambda (coordinate-system-name)
	     (let ((coordinate-system-entry
		    (assq coordinate-system-name
			  coordinate-systems)))
	       (if coordinate-system-entry
		   ((cdr coordinate-system-entry) the-patch)
		   (error "Unknown coordinate-system"
			  coordinate-system-name patch-name)))))
	  ((coordinate-system-names)
	   (map car coordinate-systems))
	  (else (manifold m)		; pass the buck.
		;; (error "unknown message: patch" m)
		)))
      the-patch)
    (define (patch-setup m)
      (case m
	((new-coordinate-system)
	 (lambda (new-coordinate-system-name new-coordinate-system)
	   (set! coordinate-systems
		 (cons (cons new-coordinate-system-name
			     new-coordinate-system)
		       coordinate-systems))))
	((generator) patch-generator)
	(else (error "Unknown message patch-setup" m))))
    ((manifold-spec 'new-patch)
     patch-name patch-generator patch-setup)
    patch-name))

(define (coordinate-system coordinate-system-name patch)
  ((patch 'get-coordinate-system) coordinate-system-name))

(define (coordinate-system-at coordinate-system-name patch-name manifold)
  (coordinate-system coordinate-system-name
		     (patch patch-name manifold)))


(define (make-manifold manifold-type . args)
  (apply (manifold-type 'generator) args))

;;; Coordinate systems are added to coordinate patches.
;;; A coordinate system is an invertible map from the space to R^n.

(define (attach-coordinate-system coordinate-system-name
				  patch-name
				  manifold-type
				  transformations
				  #!optional coordinate-prototype)
  (define (the-coordinate-system-generator the-patch)
    (let* ((manifold (the-patch 'manifold))
	   (transform-delivery (transformations manifold))
	   (coordinate-prototype
	    (if (default-object? coordinate-prototype)
		(c:generate (manifold 'dimension) 'up
			    (lambda (i) (symbol 'x i)))
		(begin
		  (assert
		   (fix:= (manifold 'dimension)
			  (s:dimension coordinate-prototype)))
		  coordinate-prototype)))
	   (access-chains
	    (s:map-chain (lambda (element chain) chain)
			 coordinate-prototype))
	   (dual-chains (flip-indices access-chains)))
      (let ((coordinate-function-specs #f)
	    (coordinate-basis-vector-field-specs #f)
	    (coordinate-basis-1form-field-specs #f)
	    (coordinate-basis #f))
	(define (the-coordinate-system m)
	  (case m
	    ((name coordinate-system-name)
	     coordinate-system-name)
	    ((patch) the-patch)
	    ((->point) (transform-delivery 'coords->point))
	    ((->coords) (transform-delivery 'point->coords))
	    ((check-point) (transform-delivery 'check-point))
	    ((check-coords) (transform-delivery 'check-coords))
	    ((typical-coords)
	     (typical-object coordinate-prototype))
	    ((coordinate-prototype) coordinate-prototype)
	    ((set-coordinate-prototype!)
	     (lambda (new)
	       (assert (fix:= (manifold 'dimension) (s:dimension new)))
	       (set! coordinate-prototype new)
	       (set! access-chains 
		     (s:map-chain (lambda (element chain) chain)
				  coordinate-prototype))
	       (set! coordinate-function-specs #f)
	       (set! coordinate-basis-vector-field-specs #f)
	       (set! coordinate-basis-1form-field-specs #f)
	       (set! coordinate-basis #f)))	       
	    ((access-chains) access-chains)
	    ((dual-chains) dual-chains)

	    ((coordinate-function-specs)
	     (if (not coordinate-function-specs)
		 (set! coordinate-function-specs
		       (s:map/r
			(lambda (coordinate-name access-chain)
			  (list coordinate-name
				(compose
				 (apply component access-chain)
				 (transform-delivery 'point->coords))))
			coordinate-prototype
			access-chains)))
	     coordinate-function-specs)
	    ((coordinate-basis-vector-field-specs)
	     (if (not coordinate-basis-vector-field-specs)
		 (set! coordinate-basis-vector-field-specs
		       (flip-indices
			(s:map/r
			 (lambda (coordinate-name access-chain)
			   (let* ((oname (symbol 'd/d coordinate-name))
				  (vf
				   (apply coordinate-basis-vector-field
					  the-coordinate-system
					  oname
					  access-chain)))
			     (set-operator-optionals! vf
			       (cons `(manifold ,manifold)
				     (operator-optionals vf)))
			     (list oname vf)))
			 coordinate-prototype
			 access-chains))))
	     coordinate-basis-vector-field-specs)
	    ((coordinate-basis-1form-field-specs)
	     (if (not coordinate-basis-1form-field-specs)
		 (set! coordinate-basis-1form-field-specs
		       (s:map/r
			(lambda (coordinate-name access-chain)
			  (let* ((oname (symbol 'd coordinate-name))
				 (ff
				  (apply coordinate-basis-1form-field
					 the-coordinate-system
					 oname
					 access-chain)))
			    (set-operator-optionals! ff
			        (cons `(manifold ,manifold)
				      (operator-optionals ff)))
			    (list oname ff)))
			coordinate-prototype
			access-chains)))
	     coordinate-basis-1form-field-specs)

	    ((coordinate-basis)
	     (if (not coordinate-basis)
		 (set! coordinate-basis
		       (coordinate-system->basis the-coordinate-system)))
	     coordinate-basis)
	    ((coordinate-functions)
	     (s:map/r cadr
		      (the-coordinate-system
		       'coordinate-function-specs)))
	    ((coordinate-basis-vector-fields)
	     (s:map/r cadr
		      (the-coordinate-system
		       'coordinate-basis-vector-field-specs)))
	    ((coordinate-basis-1form-fields)
	     (s:map/r cadr
		      (the-coordinate-system
		       'coordinate-basis-1form-field-specs)))
	    (else
	     (cond ((symbol? m) (manifold m))		;pass the buck.
		   ((c:lookup m
			      (the-coordinate-system
			       'coordinate-function-specs))
		    => cadr)
		   ((c:lookup m
			      (the-coordinate-system
			       'coordinate-basis-vector-field-specs))
		    => cadr)
		   ((c:lookup m
			      (the-coordinate-system
			       'coordinate-basis-1form-field-specs))
		    => cadr)
		   (else (error "bad message" m))))))
	the-coordinate-system)))
  ((((manifold-type 'patch-setup) patch-name)
    'new-coordinate-system)
   coordinate-system-name the-coordinate-system-generator)
  coordinate-system-name)

(define (coordinate-system-dimension coordinate-system)
  (coordinate-system 'dimension))

;;; So that the generic operation can get this.
(environment-define scmutils-base-environment
		    'coordinate-system-dimension
		    coordinate-system-dimension)

(define (frame? x)
  (and (procedure? x)
       ;; Kludge.  See frame-maker.scm
       (not (x 'manifold))))

(define (chart coordinate-system)
  (if (frame? coordinate-system)
      (event->coords coordinate-system)
      (coordinate-system '->coords)))

(define (point coordinate-system)
  (if (frame? coordinate-system)
      (coords->event coordinate-system)
      (coordinate-system '->point)))

(define (typical-point coordinate-system)
  ((point coordinate-system)
   (typical-coords coordinate-system)))

(define (typical-coords coordinate-system)
  (s:map/r generate-uninterned-symbol
	   (coordinate-system 'coordinate-prototype)))

(define (corresponding-velocities coords)
  (s:map/r (lambda (x)
	     (string->uninterned-symbol
	      (string-append "v:"
			     (symbol->string x))))
	   coords))

;;; This adds names to the generic environment for the coordinate
;;; functions, the coordinate-basis vector fields and 1form fields.
;;; The names are generated from the coordinate prototype.
;;;   E.g. if the coordinate prototype is (up 'r 'theta)
;;;      r and theta become coordinate functions
;;;      d/dr and d/dtheta are coordinate basis vector fields
;;;      dr and dtheta are coordinate 1form fields.

(define (install-coordinates coordinate-system
			     #!optional coordinate-prototype)
  (define (install-symbol name value)
    (if (environment-bound? user-generic-environment name)
	(begin
	  (write-line `(clobbering ,name))
	  (set! *saved-environment-values*
		(cons (cons name
			    (environment-lookup user-generic-environment
						name))
		      *saved-environment-values*))))
    (environment-define user-generic-environment name value))
  (define (install-symbols s)
    (s:foreach (lambda (symval)
		 (install-symbol (car symval) (cadr symval)))
	       s))
  (if (not (default-object? coordinate-prototype))
      ((coordinate-system 'set-coordinate-prototype!) coordinate-prototype))
  (install-symbols (coordinate-system 'coordinate-function-specs))
  (install-symbols (coordinate-system 'coordinate-basis-vector-field-specs))
  (install-symbols (coordinate-system 'coordinate-basis-1form-field-specs))
  (list (coordinate-system 'name) ((coordinate-system 'manifold) 'name)))


(define *saved-environment-values* '())

(define (uninstall-coordinates)
  (for-each (lambda (saved-values)
	      (environment-assign! user-generic-environment
				   (car saved-values)
				   (cdr saved-values)))
	    *saved-environment-values*)
  (set! *saved-environment-values* '())
  'done)

;;;; Common manifolds

(define R^n (specify-manifold 'R^n))
(define R^n-type R^n)

(attach-patch 'origin R^n)

(attach-coordinate-system 'rectangular 'origin R^n
  (lambda (manifold)
    (define (me m)
      (case m
	((check-coordinates)
	 (lambda (coords)
	   (fix:= (s:dimension coords) (manifold 'dimension))))
	((coords->point)
	 (lambda (coords)
	   (if ((me 'check-coordinates) coords)
	       (make-manifold-point coords manifold me coords)
	       (error "Bad coordinates: rectangular" coords me))))
	((check-point)
	 (lambda (point)
	   (my-manifold-point? point manifold)))
	((point->coords)
	 (lambda (point)
	   (if (not ((me 'check-point) point))
	       (error "Bad point: rectangular" point me))
	   (get-coordinates point me
	     (lambda ()
	       (let ((prep (manifold-point-representation point)))
		 (if (fix:= (s:dimension prep)
			    (manifold 'embedding-dimension))
		     prep
		     (error "Bad point: rectangular" point me)))))))
	((manifold) manifold)
	(else (error "Bad message: rectangular" m me))))
    me))

(attach-coordinate-system 'polar/cylindrical 'origin R^n
  (lambda (manifold)
    (define (me m)
      (case m
	((check-coordinates)
	 (lambda (coords)
	   (and (up? coords)
		(fix:= (s:dimension coords) (manifold 'dimension))
		(not (fix:< (s:dimension coords) 2))
		(or (not (number? (ref coords 0)))
		    (not (< (ref coords 0) 0))))))
	((coords->point)
	 (lambda (coords)
	   (if ((me 'check-coordinates) coords)
	       (let ((r (ref coords 0)) (theta (ref coords 1)))
		 (make-manifold-point
		  (s:generate (s:dimension coords) 'up
			      (lambda (i)
				(cond ((= i 0) (* r (cos theta)))
				      ((= i 1) (* r (sin theta)))
				      (else (ref coords i)))))
		  manifold
		  me
		  coords))
	       (error "Bad coordinates: polar/cylindrical"
		      coords me))))
	((check-point)
	 (lambda (point)
	   (my-manifold-point? point manifold)))
	((point->coords)
	 (lambda (point)
	   (if (not ((me 'check-point) point))
	       (error "Bad point: polar/cylindrial" point me))
	   (get-coordinates point me
	     (lambda ()
	       (let ((prep (manifold-point-representation point)))
		 (if (and (up? prep)
			  (fix:= (s:dimension prep)
				 (manifold 'embedding-dimension)))
		     (let ((x (ref prep 0)) (y (ref prep 1)))
		       (let ((rsq (+ (square x) (square y))))
			 (if (and (number? rsq) (= rsq 0))
			     (error "polar/cylindrical singular"
				    point me))
			 (s:generate (s:dimension prep) 'up
				     (lambda (i)
				       (cond ((= i 0) (sqrt rsq))
					     ((= i 1) (atan y x))
					     (else (ref prep i)))))))
		     (error "Bad point: polar/cylindrial"
			    point me)))))))
	((manifold) manifold)
	(else
	 (error "Bad message: polar/cylindrical" m me))))
    me))

(attach-coordinate-system 'spherical/cylindrical 'origin R^n
  (lambda (manifold)
    (define (me m)
      (case m
	((check-coordinates)
	 (lambda (coords)
	   (and (up? coords)
		(fix:= (s:dimension coords) (manifold 'dimension))
		(not (fix:< (s:dimension coords) 3))
		(or (not (number? (ref coords 0)))
		    (not (< (ref coords 0) 0))))))
	((coords->point)
	 (lambda (coords)
	   (if ((me 'check-coordinates) coords)
	       (let ((r (ref coords 0)) 
		     (theta (ref coords 1)) 
		     (phi (ref coords 2)))
		 (make-manifold-point
		  (s:generate (s:dimension coords) 'up
		     (lambda (i)
		       (cond ((= i 0) (* r (sin theta) (cos phi)))
			     ((= i 1) (* r (sin theta) (sin phi)))
			     ((= i 2) (* r (cos theta)))
			     (else (ref coords i)))))
		  manifold
		  me
		  coords))
	       (error "Bad coordinates: spherical/cylindrical"
		      coords me))))
	((check-point)
	 (lambda (point) (my-manifold-point? point manifold)))
	((point->coords)
	 (lambda (point)
	   (if (not ((me 'check-point) point))
	       (error "Bad point: spherical/cylindrial" point me))
	   (get-coordinates point me
	     (lambda ()
	       (let ((prep (manifold-point-representation point)))
		 (if (and (up? prep)
			  (fix:= (s:dimension prep)
				 (manifold 'embedding-dimension)))
		     (let ((x (ref prep 0)) 
			   (y (ref prep 1))
			   (z (ref prep 2)))
		       (let ((r (sqrt
				 (+ (square x) (square y) (square z)))))
			 (if (and (number? r) (= r 0))
			     (error "spherical/cylindrical singular"
				    point me))
			 (s:generate (s:dimension prep) 'up
				     (lambda (i)
				       (cond ((= i 0) r)
					     ((= i 1) (acos (/ z r)))
					     ((= i 2) (atan y x))
					     (else (ref prep i)))))))
		     (error "Bad point: spherical/cylindrial"
			    point me)))))))
	((manifold) manifold)
	(else
	 (error "Bad message: spherical/cylindrical" m me))))
    me))

;;; For R4 only
(attach-coordinate-system 'spacetime-spherical 'origin R^n
  (lambda (manifold)
    (define (me m)
      (case m
	((check-coordinates)
	 (lambda (coords)
	   (and (up? coords) (fix:= (s:dimension coords) 4))))
	((coords->point)
	 (lambda (coords)
	   (if ((me 'check-coordinates) coords)
	       (let ((t (ref coords 0))
		     (r (ref coords 1))
		     (theta (ref coords 2))
		     (phi   (ref coords 3)))
		 (make-manifold-point
		  (up t
		      (* r (sin theta) (cos phi)) 
		      (* r (sin theta) (sin phi)) 
		      (* r (cos theta)))
		  manifold
		  me
		  coords))
	       (error "Bad coordinates: spacetime-spherical"
		      coords me))))
	((check-point)
	 (lambda (point)
	   (my-manifold-point? point manifold)))
	((point->coords)
	 (lambda (point)
	   (if (not ((me 'check-point) point))
	       (error "Bad point: spacetime-spherical" point me))
	   (get-coordinates point me
	     (lambda ()
	       (let ((prep (manifold-point-representation point)))
		 (if (and (up? prep) (fix:= (s:dimension prep) 4))
		     (let ((t (ref prep 0))
			   (x (ref prep 1))
			   (y (ref prep 2))
			   (z (ref prep 3)))
		       (let ((r (sqrt (+ (square x) (square y) (square z)))))
			 (if (and (number? r) (= r 0))
			     (error "spacetime-spherical singular" point me))
			 (up t
			     r
			     (acos (/ z r))
			     (atan y x))))
		     (error "Bad point: spacetime-spherical" point me)))))))
	((manifold) manifold)
	(else (error "Bad message: spacetime-spherical" m me))))
    me))

(define R1 (make-manifold R^n 1))
(define R1-rect (coordinate-system-at 'rectangular 'origin R1))
(define the-real-line R1-rect)

(define R2 (make-manifold R^n 2))
(define R2-rect (coordinate-system-at 'rectangular 'origin R2))
(define R2-polar (coordinate-system-at 'polar/cylindrical 'origin R2))

(define R3 (make-manifold R^n 3))
(define R3-rect (coordinate-system-at 'rectangular 'origin R3))
(define R3-cyl (coordinate-system-at 'polar/cylindrical 'origin R3))
(define R3-spherical
  (coordinate-system-at 'spherical/cylindrical 'origin R3))

(define R4 (make-manifold R^n 4))
(define R4-rect (coordinate-system-at 'rectangular 'origin R4))
(define R4-cyl (coordinate-system-at 'polar/cylindrical 'origin R4))

(define spacetime (make-manifold R^n 4))
(define spacetime-rect
  (coordinate-system-at 'rectangular 'origin spacetime))
(define spacetime-sphere
  (coordinate-system-at 'spacetime-spherical 'origin spacetime))

#|
(define m ((R2-rect '->point) (up 3 4)))

((R2-polar '->coords) m)
;Value: #(5 .9272952180016122)

(install-coordinates R2-rect (up 'x 'y))

(install-coordinates R2-polar (up 'r 'theta))

(define mr ((R2-rect '->point) (up 'x0 'y0)))

(define mp ((R2-polar '->point) (up 'r0 'theta0)))

(define circular (- (* x d/dy) (* y d/dx)))

(pec ((circular (+ (* 2 x) (* 3 y))) mr))
#| Result:
(+ (* 3 x0) (* -2 y0))
|#

(pec ((circular theta) mr))
#| Result:
1
|#

(pec ((dr circular) mr))
#| Result:
0
|#


(pec (((d r) d/dr) mr))
#| Result:
1
|#

(pec ((dr d/dr) mr))
#| Result:
1
|#

(pec ((dr (literal-vector-field 'v R2-polar)) mr))
#| Result:
(v^0 (up (sqrt (+ (expt x0 2) (expt y0 2))) (atan y0 x0)))
|#

(pec (((d r) (literal-vector-field 'v R2-polar)) mr))
#| Result:
(v^0 (up (sqrt (+ (expt x0 2) (expt y0 2))) (atan y0 x0)))
|#

(pec ((dr (literal-vector-field 'v R2-rect)) mr))
#| Result:
(+ (/ (* x0 (v^0 (up x0 y0))) (sqrt (+ (expt x0 2) (expt y0 2))))
   (/ (* y0 (v^1 (up x0 y0))) (sqrt (+ (expt x0 2) (expt y0 2)))))
|#

(pec (((d r) (literal-vector-field 'v R2-rect)) mr))
#| Result:
(+ (/ (* x0 (v^0 (up x0 y0))) (sqrt (+ (expt x0 2) (expt y0 2))))
   (/ (* y0 (v^1 (up x0 y0))) (sqrt (+ (expt x0 2) (expt y0 2)))))
|#

;;; Consider the two following metrics on the space

(define (g-polar u v)
  (+ (* (dr u) (dr v))
     (* (* r (dtheta u)) (* r (dtheta v)))))

(define (g-rect u v)
  (+ (* (dx u) (dx v))
     (* (dy u) (dy v))))

(pec (((- g-polar g-rect)
      (literal-vector-field 'v R2-rect)
      (literal-vector-field 'v R2-rect))
     mr))
#| Result:
0
|#

(pec (((- g-polar g-rect)
      (literal-vector-field 'v R2-polar)
      (literal-vector-field 'v R2-polar))
     mr))
#| Result:
0
|#

(pec (((- g-polar g-rect)
      (literal-vector-field 'v R2-polar)
      (literal-vector-field 'v R2-polar))
     mp))
#| Result:
0
|#

(pec (((- g-polar g-rect)
      (literal-vector-field 'v R2-rect)
      (literal-vector-field 'v R2-rect))
     mp))
#| Result:
0
|#
|#

;;; The surface of the sphere, specialized to two dimensions.

(define S^2-type (specify-manifold 'S^2))

(define (S^2-coordinates orientation)
  (let ((orientation^-1 (invert orientation)))
    (lambda (manifold)
      (define (me m)
	(define (coordinates-ok? coords)
	  (and (up? coords)
	       (fix:= (s:dimension coords) 2)
	       (or (not (number? (ref coords 0)))
		   (not (< (ref coords 0) 0)))))
	(case m
	  ((check-coordinates) coordinates-ok?)
	  ((coords->point)
	   (lambda (coords)
	     (if (coordinates-ok? coords)
		 (let ((colatitude (ref coords 0))
		       (longitude (ref coords 1)))
		   (make-manifold-point
		    (* orientation
		       (up (* (sin colatitude) (cos longitude))
			   (* (sin colatitude) (sin longitude))
			   (cos colatitude)))
		    manifold
		    me
		    coords))
		 (error "Bad coordinates: S^2-spherical" coords me))))
	  ((check-point)
	   (lambda (point) (my-manifold-point? point manifold)))
	  ((point->coords)
	   (lambda (point)
	     (if (not ((me 'check-point) point))
		 (error "Bad point: S^2-spherical" point me))
	     (get-coordinates point me
	       (lambda ()
		 (let ((prep
			(* orientation^-1
			   (manifold-point-representation point))))
		   (if (and (up? prep)
			    (fix:= (s:dimension prep)
				   (manifold 'embedding-dimension)))
		       (let ((x (ref prep 0))
			     (y (ref prep 1))
			     (z (ref prep 2)))
			 ;; (if (and (number? x) (= x 0))
			 ;;     (error "S^2-spherical singular" point me))
			 (up (acos z) (atan y x)))
		       (error "Bad point: S^2-spherical" point me)))))))
	  ((manifold) manifold)
	  ((orientation) orientation)
	  (else (error "Bad message: S^2-spherical" m me))))
      me)))

(attach-patch 'north-pole S^2-type)

(attach-coordinate-system 'spherical 'north-pole S^2-type
  (S^2-coordinates (down (up 1 0 0) (up 0 1 0) (up 0 0 1))))

(attach-patch 'tilted S^2-type)

(attach-coordinate-system 'spherical 'tilted S^2-type
  (S^2-coordinates (down (up 1 0 0) (up 0 0 1) (up 0 -1 0))))


(attach-patch 'south-pole S^2-type)

(attach-coordinate-system 'spherical 'south-pole S^2-type
  (S^2-coordinates (down (up 1 0 0) (up 0 1 0) (up 0 0 -1))))


(define S2 (make-manifold S^2-type 2 3))

(define S2-spherical
  (coordinate-system-at 'spherical 'north-pole S2))

(define S2-tilted
  (coordinate-system-at 'spherical 'tilted S2))

#|
(define m
  ((S2-spherical '->point) (up 'theta 'phi)))

(pec ((S2-tilted '->coords) m))
#| Result:
(up (acos (* -1 (sin phi) (sin theta)))
    (atan (cos theta) (* (sin theta) (cos phi))))
|#
|#

(define S^n-type (specify-manifold 'S^n))

;; Manifold points are represented by
;;(up 
;;    (* (sin theta0) (cos theta1) )
;;    (* (sin theta0) (sin theta1) (cos theta2) )
;;    ...
;;    (* (sin theta0) (sin theta1) ... (sin theta_n-1) )
;;    (cos theta0)
;;    )

;;; Notice that the (cos theta0) has been rotated to the bottom
;;; to be compatible with the traditional S^2 convention.
;;
;; The first n-1 angles  must be nonzero to avoid the coordinate singularity.
;;
;; S^n-coordinates takes an orientation function that takes the dimension 
;; of the embedding space (1+the dimension of the manifold) and produces the
;; matrix that shifts the location of the "north pole".

(define (S^n-coordinates orientation-function)
  (define (list-top-to-bottom l)
    (append (cdr l) (list (car l))))
  (define (list-bottom-to-top l)
    (cons (car (last-pair l)) (butlast l)))  
  (lambda (manifold)
    (let* ((n (manifold 'dimension))
	   (orientation-matrix (orientation-function (+ n 1)))
	   (orientation-inverse-matrix (invert orientation-matrix)))
      (define (me m)
	(case m
	  ((check-coordinates)
	   (lambda (coords)
	     (or (and (fix:= n 1) (fix:= (s:dimension coords) 1))
		 (and (up? coords)
		      (fix:= (s:dimension coords) (manifold 'dimension))
		      (let ((remaining-coords (butlast (up-structure->list coords))))
			(every (lambda (coord)
				 (or (not (number? coord)) (not (< coord 0))))
			       remaining-coords))))))
	  ((coords->point)
	   (lambda (coords)
	     (if (not ((me 'check-coordinates) coords))
		 (error "Bad coordinates: S^n-spherical" coords me))
	     (if (fix:= n 1)
		 (let ((pt (up (cos coords) (sin coords))))
		   (make-manifold-point (* orientation-matrix pt)
					manifold me coords))
		 (let* ((coordl (up-structure->list coords))
			(sines (map sin coordl)) (cosines (map cos coordl))
			(pt
			 (list->up-structure
			  (list-top-to-bottom
			   (make-initialized-list (fix:+ n 1)
			     (lambda (i)
			       (if (fix:= i n)
				   (apply * sines)
				   (apply *
					  (cons (list-ref cosines i)
						(list-head sines i))))))))))
		   (make-manifold-point (* orientation-matrix pt)
					manifold me coords)))))
	  ((check-point)
	   (lambda (point) (my-manifold-point? point manifold)))
	  ((point->coords)
	   (lambda (point)
	     (if (not ((me 'check-point) point))
		 (error "Bad point: S^n-spherical" point me))
	     (define (safe-atan y x)
	       (if (and (number? y) (number? x) (= y 0) (= x 0))
		   (warn "S^n-spherical singular" point me))  
	       (atan y x))
	     (let* ((pt
		     (reverse
		      (list-bottom-to-top
		       (up-structure->list
			(* orientation-inverse-matrix
			   (manifold-point-representation point)))))) )
	       (if (fix:= n 1)
		   (safe-atan (ref pt 1) (ref pt 0))
	           (let lp ((r (car pt)) (rest (cdr pt))
			    (ans (list (safe-atan (car pt) (cadr pt)))))
		     (if (null? (cdr rest))
			 (list->up-structure ans)
			 (let ((r (sqrt (+ (square (car rest)) (square r)))))
			   (lp r
			       (cdr rest)
			       (cons (safe-atan r (cadr rest)) ans)))))))))
	  ((manifold) manifold)
	  ((orientation) orientation-function)
	  (else (error "S^n-spherical: Bad message" m me))))
      me)))
	 
(attach-patch 'north-pole S^n-type)
(attach-coordinate-system 'spherical 'north-pole S^n-type
  (S^n-coordinates m:make-identity))

(attach-patch 'tilted S^n-type)
(attach-coordinate-system 'spherical 'tilted S^n-type
  (S^n-coordinates
   (let ((c (cos :pi/2)) (s (sin :pi/2)))
     (lambda (n)
       (s:generate n 'down
         (lambda (col)
	   (s:generate n 'up
		       (lambda (row)
			 (cond ((and (= row (- n 2)) (= col (- n 1)) -1))
			       ((and (= row (- n 1)) (= col (- n 2)) +1))
			       ((and (= row col) (< row (- n 2))) +1)
			       (else 0))))))))))

(define S1 (make-manifold S^n-type 1))
(define S1-circular (coordinate-system-at 'spherical 'north-pole S1))
(define S1-tilted (coordinate-system-at 'spherical 'tilted S1))

#|
(define m ((S1-circular '->point) 'theta))

(pe (manifold-point-representation m))
; Result: (up (cos theta) (sin theta))

(pe ((compose (S1-circular '->coords) (S1-circular '->point)) 'theta))
; Result: theta

(pe ((compose (S1-circular '->coords) (S1-tilted '->point)) 'theta))
; Result: (atan (cos theta) (* -1 (sin theta)))

|#                      

(define S2p (make-manifold S^n-type 2))
(define S2p-spherical (coordinate-system-at 'spherical 'north-pole S2p))
(define S2p-tilted (coordinate-system-at 'spherical 'tilted S2p))

#|
(define m ((S2p-spherical '->point) (up 'theta 'phi)))

(pe (manifold-point-representation m))
; Result: (up (* (sin theta) (cos phi))
;             (* (sin phi) (sin theta))
;             (cos theta))

(pe ((compose (S2p-spherical '->coords) (S2p-spherical '->point))
     (up 'theta 'phi)))
; Result: (up theta phi)

(pe ((compose (S2p-spherical '->coords) (S2p-tilted '->point))
     (up 'theta 'phi)))
; Result: (up (atan (sqrt (+ (* (expt (cos theta) 2) (expt (cos phi) 2))
;			     (expt (sin phi) 2)))
;		    (* -1 (sin theta) (cos phi)))
;	      (atan (* (sin phi) (sin theta)) (cos theta)))

(pe ((compose (S2p-spherical '->coords) (S2p-spherical '->point))
     (up 1 0)))
;(up 1. 0.)

(pe ((compose (S2p-spherical '->coords) (S2p-spherical '->point))
     (up 0 1)))
;(up 0 0)
;Should be warned singular!
|#                      

(define S3 (make-manifold S^n-type 3))
(define S3-spherical (coordinate-system-at 'spherical 'north-pole S3))
(define S3-tilted (coordinate-system-at 'spherical 'tilted S3))
#|
(pe ((compose (S3-spherical '->coords)
	      (S3-spherical '->point))
     (up 'a 'b 'c)))
; Result: (up a b c)

(pe ((compose (S3-spherical '->coords)
	      (S3-tilted '->point))
     (up 'a 'b 'c)))
; Result:
;(up
; (atan
;  (sqrt
;   (+ (* (expt (sin c) 2) (expt (sin b) 2) (expt (cos a) 2))
;      (* (expt (sin c) 2) (expt (cos b) 2))
;      (expt (cos c) 2)))
;  (* (sin c) (sin b) (sin a)))
; (atan (sqrt (+ (* (expt (sin b) 2) (expt (sin a) 2) (expt (cos c) 2))
;                (expt (cos a) 2)))
;       (* (sin a) (cos b)))
; (atan (* -1 (cos a)) (* (sin b) (sin a) (cos c))))

(pe ((compose (S3-spherical '->coords)
	      (S3-spherical '->point))
     (up 0 0 0)))
;(up 0 0 0)
;Should be warned singular!
|#                      

;;; Stereographic Projection from the final coordinate.
;;
;;  The default pole is p = (0 0 ... 0 1)
;;  We fire a ray through m = (m_0 ... m_n)
;;  x(t) = p + t(m - p)
;;
;;  x(0) = p, x(1) = m
;;  x_n(t) = 1-t(1+m_n), 0 = x_n(1/(1+m_n))
;;  
;;  The orientation function should return an orthogonal (n+1)-by-(n+1)
;;  matrix.  It can be interpreted as moving the pole / plane of projection
;;  and possibly reflecting.
;;
(define (S^n-stereographic orientation-function)
  (lambda (manifold)
    (let* ((n (manifold 'dimension))
	   (orientation-matrix (orientation-function (+ n 1)))
	   (orientation-inverse-matrix (invert orientation-matrix)))
      (define (me m)
	(case m
	  ((check-coordinates)
	   (lambda (coords)
	     (or (and (fix:= n 1) (fix:= (s:dimension coords) 1))
		 (and (up? coords)
		      (fix:= (s:dimension coords) n)))))
	  ((coords->point)
	   (lambda (coords)
	     (if (not ((me 'check-coordinates) coords))
		 (error "Bad coordinates: S^n-stereographic"
			coords me))
	     (let* ((coords (if (fix:= n 1) (up coords) coords))
		    (delta (dot-product coords coords))
		    (xn (/ (- delta 1) (+ 1 delta)))
		    (pt (s:generate
			 (fix:+ n 1) 'up
			 (lambda (i)
			   (if (fix:= i n) xn
			       (/ (* 2 (ref coords i))
				  (+ 1 delta)))))))
	       (make-manifold-point (* orientation-matrix pt)
				    manifold me coords))))
	  ((check-point)
	   (lambda (point) (my-manifold-point? point manifold)))
	  ((point->coords)
	   (lambda (point)
	     (if (not ((me 'check-point) point))
		 (error "Bad point: S^n" point me))
	     (let* ((n (manifold 'dimension))
		    (pt (* orientation-inverse-matrix
			   (manifold-point-representation point))))
	       (if (and (number? (ref pt n)) (= (ref pt n) 1))
		   (error "S^n-stereographic singular" point me))
	       (let ((coords
		      (s:generate n 'up
				  (lambda (i)
				    (/ (ref pt i)
				       (- 1 (ref pt n)))))))
		 (if (fix:= n 1) (ref coords 0) coords)))))
	  ((manifold) manifold)
	  ((orientation) orientation-function)
	  (else (error "S^n-stereographic: Bad message" m me))))
      me)))

(attach-coordinate-system 'stereographic 'north-pole S^n-type
 (S^n-stereographic m:make-identity))

(attach-patch 'south-pole S^n-type)
(attach-coordinate-system 'stereographic 'south-pole S^n-type
 (S^n-stereographic
  (lambda (n)
    (m:generate n n
		(lambda (i j)
		  (if (= i j)
		      (if (= j n) -1 1)
		      0))))))


#|
(define S1 (make-manifold S^n-type 1))
|#

(define S1-slope
  (coordinate-system-at 'stereographic 'north-pole S1))

#|
(define m ((S1-slope '->point) 's))

(pe (manifold-point-representation m))
; Result:  
;(up (/ (* 2 s)
;       (+ 1 (expt s 2)))
;    (/ (+ -1 (expt s 2))
;       (+ 1 (expt s 2))))

(pe  (manifold-point-representation
      ((compose (S1-slope '->point)
		(S1-slope '->coords))
       m)))
; Result: 
;(up (/ (* 2 s)
;       (+ 1 (expt s 2)))
;    (/ (+ -1 (expt s 2))
;       (+ 1 (expt s 2))))


(pe ((compose (S1-slope '->coords)
	      (S1-slope '->point))
     's))
; Result: s
|#     

#|
(define S2p (make-manifold S^n-type 2))
|#     

(define S2p-stereographic (coordinate-system-at 'stereographic 'north-pole S2p))
(define S2p-Riemann S2p-stereographic)

#|
(define m ((S2p-Riemann '->point) (up 'x 'y)))

(pe (manifold-point-representation m))
; Result: 
;(up (/ (* 2 x) 
;       (+ 1 (expt x 2) (expt y 2)))
;    (/ (* 2 y)
;       (+ 1 (expt y 2) (expt x 2)))
;    (/ (+ -1 (expt x 2) (expt y 2))
;       (+ +1 (expt x 2) (expt y 2))))

(pe (manifold-point-representation
     ((compose (S2p-Riemann '->point) (S2p-Riemann '->coords))
      m)))
; Result: 
;(up (/ (* 2 x)
;       (+ 1 (expt x 2) (expt y 2)))
;    (/ (* 2 y)
;       (+ 1 (expt x 2) (expt y 2)))
;    (/ (+ -1 (expt x 2) (expt y 2))
;       (+ 1 (expt x 2) (expt y 2))))

(pe ((compose (S2p-Riemann '->coords) (S2p-Riemann '->point))
    (up 'x 'y)))
; Result:  (up x y)

(pe (manifold-point-representation
      ((S2p-Riemann '->point)
       (up (cos 'theta) (sin 'theta)))))
; Result: (up (cos theta) (sin theta) 0)
; The equator is invariant.
|#              

;;; Gnomic Projection of the sphere
;;
;;  We map the nothern hemisphere to the plane by firing a ray from the origin.
;;  The coordinates are given by the intersection with the z = 1 plane.
;;  x(t) = t*m
;;  x_n(t) = t*m_n, 1 = x_n(1/m_n)
;;
;;  orientation-function should should return an n+1-by-n+1 orthogonal matrix.
;;  It can be interpreted as moving the plane of projection, and point mapped to
;;  the origin, as well as possibly reflecting.
;;
;;  Given the coordinates x we have  <x,x> = (1-m_n^2)/m_n^2
;;  1 + <x,x> = (m_n^2 + 1 - m_n^2)/m_n^2
;;  m_n = sqrt(1/(1+<x,x>))
;;  where positive square root is sufficient for the nothern hemisphere.

(define (S^n-gnomic orientation-function)
 (lambda (manifold)
   (let* ((n (manifold 'dimension))
	  (orientation-matrix (orientation-function (+ n 1)))
	  (orientation-inverse-matrix (invert orientation-matrix)))
     (define (me m)
	(case m
	  ((check-coordinates)
	   (lambda (coords)
	     (or (and (fix:= n 1) (fix:= (s:dimension coords) 1))
		 (and (up? coords) (fix:= (s:dimension coords) n)))))
	  ((coords->point)
	   (lambda (coords)
	     (if (not ((me 'check-coordinates) coords))
		 (error "Bad coordinates: S^n" coords me))
	     (let* ((coords (if (fix:= n 1) (up coords) coords))
		    (delta (dot-product coords coords))
		    (d (sqrt (+ 1 delta)))
		    (xn (/ 1 d))
		    (pt (s:generate
			 (fix:+ n 1) 'up
			 (lambda (i)
			   (if (fix:= i n)
			       xn
			       (/ (ref coords i) d))))))
	       (make-manifold-point (* orientation-matrix pt)
				    manifold me coords))))
	  ((check-point)
	   (lambda (point) (my-manifold-point? point manifold)))
	  ((point->coords)
	   (lambda (point)
	     (if (not ((me 'check-point) point))
		 (error "Bad point: S^n-gnomic" point me))
	     (let* ((n (manifold 'dimension))
		    (pt (* orientation-inverse-matrix
			   (manifold-point-representation point))))
	       (if (and (number? (ref pt n)) (not (> 0 (ref pt n))))
		   (error "Point not covered by S^n-gnomic coordinate patch."
			  point me))
	       (let ((coords
		      (s:generate n 'up
				  (lambda (i)
				    (/ (ref pt i)
				       (ref pt n))))))
		 (if (fix:= n 1) (ref coords 0) coords)))))
	  ((manifold) manifold)
	  ((orientation) orientation-function)
	  (else (error "S^n: Bad message" m me))))
     me)))

(attach-coordinate-system 'gnomic 'north-pole S^n-type
 (S^n-gnomic m:make-identity))

#|
(define S1 (make-manifold S^n-type 1))
|#

(define S1-gnomic (coordinate-system-at 'gnomic 'north-pole S1))

#|
(define m ((S1-gnomic '->point) 's))

(pe (manifold-point-representation m))
; Result:  (up (/ s (sqrt (+ 1 (expt s 2))))
;              (/ 1 (sqrt (+ 1 (expt s 2)))))

(pe  (manifold-point-representation
      ((compose (S1-gnomic '->point) (S1-gnomic '->coords)) m)))
; Result: (up (/ s (sqrt (+ 1 (expt s 2))))
;             (/ 1 (sqrt (+ 1 (expt s 2)))))

(pe ((compose (S1-slope '->coords) (S1-slope '->point)) 's))
; Result: s

|#     


#|
(define S2p (make-manifold S^n-type 2))
|#

(define S2p-gnomic (coordinate-system-at 'gnomic 'north-pole S2p))

#|
(define S2p-stereographic (coordinate-system-at 'stereographic 'north-pole S2p))
(define m ((S2p-gnomic '->point) (up 'x 'y)))


(pe (manifold-point-representation m))
; Result: (up (/ x (sqrt (+ 1 (expt x 2) (expt y 2))))
;             (/ y (sqrt (+ 1 (expt x 2) (expt y 2))))
;             (/ 1 (sqrt (+ 1 (expt x 2) (expt y 2)))))

(pe (manifold-point-representation
     ((compose (S2p-gnomic '->point) (S2p-gnomic '->coords))
      m)))
; Result: (up (/ x (sqrt (+ 1 (expt x 2) (expt y 2))))
;             (/ y (sqrt (+ 1 (expt x 2) (expt y 2))))
;             (/ 1 (sqrt (+ 1 (expt x 2) (expt y 2)))))

(pe ((compose (S2p-gnomic '->coords) (S2p-gnomic '->point))
    (up 'x 'y)))
; Result:  (up x y)


(pe (manifold-point-representation
      ((S2p-gnomic '->point)
       (up (cos 'theta) (sin 'theta)))))
; Result: (up (/ (cos theta) (sqrt 2))
;             (/ (sin theta) (sqrt 2)) 
;             (/ 1 (sqrt 2)))
; The unit circle on the plane represents the intersection of S2 and
;      z = (/ 1 (sqrt 2))

; Straight lines in the gnomic coordinates are geodesics.
; We compute a straight line, then transform it back to stereographic coordinates.

(define q ((S2p-stereographic '->point) (up -1.5 1.5)))
(define p ((S2p-stereographic '->point) (up 1.5 0)))

(pe (simplify (simplify ((S2p-stereographic '->coords)
			 ((S2p-gnomic '->point)
			  (+ (* 't ((S2p-gnomic '->coords) p))
			     (* (- 1 't) ((S2p-gnomic '->coords) q))))))))
; Result: 
;(up
; (/ (+ (* 3.257142857142857 t) -.8571428571428571)
;    (+ -1
;       (sqrt (+ (* 11.343673469387754 (expt t 2))
;                (* -7.053061224489795 t)
;                2.4693877551020407))))
; (/ (+ (* -.8571428571428571 t) .8571428571428571)
;    (+ -1
;       (sqrt (+ (* 11.343673469387754 (expt t 2))
;                (* -7.053061224489795 t)
;                2.4693877551020407)))))
|#              

#|
;; Now a fun example synthesizing the to projective coordinates.

(define S3 (make-manifold S^n-type 3))
|#

(define S3-gnomic (coordinate-system-at 'gnomic 'north-pole S3))
(define S3-stereographic (coordinate-system-at 'stereographic 'south-pole S3))

#|
; S3 is one-to-one with the quaternions.
; We interpret the first three components of the embedding space as the
;    i,j,k imaginary party and the 4th component as the real part.
; The gnomic projection removes the double-cover of quaternions to rotations.
; The solid unit-sphere of the stereographic projection from the south pole likewise.

(pe ((S3-gnomic '->coords) ((S3-stereographic '->point) (up 'x 'y 'z))))
(up (/ (* 2 x) (+ -1 (expt x 2) (expt y 2) (expt z 2)))
    (/ (* 2 y) (+ -1 (expt y 2) (expt x 2) (expt z 2)))
    (/ (* 2 z) (+ -1 (expt z 2) (expt x 2) (expt y 2))))

(pe ((S3-stereographic '->coords) ((S3-gnomic '->point) (up 'x 'y 'z))))
(up (/ x (+ -1 (sqrt (+ 1 (expt x 2) (expt y 2) (expt z 2)))))
    (/ y (+ -1 (sqrt (+ 1 (expt y 2) (expt x 2) (expt z 2)))))
    (/ z (+ -1 (sqrt (+ 1 (expt z 2) (expt x 2) (expt y 2))))))

(pe (euclidean-norm ((S3-stereographic '->coords)
		     ((S3-gnomic '->point) (up 'x 'y 'z)))))

(/ (sqrt (+ (expt x 2) (expt y 2) (expt z 2)))
   (sqrt (+ 2
	    (expt x 2) (expt y 2) (expt z 2)
	    (* -2
	       (sqrt (+ 1 (expt x 2) (expt y 2) (expt z 2)))))))
|#

;;; SO(3).  Points are represented by 3x3 (down (up ...) ...)

;;; There is only one instance of an SOn manifold defined, SO3.
;;; As a consequence the name is not SOn but rather SO3-type.

(define SO3-type (specify-manifold 'SO3))

(define Euler-chart
  (lambda (manifold)
    (define (me m)
      (case m
	((check-coordinates)
	 (lambda (coords)
	     (and (up? coords)
		  (fix:= (s:dimension coords) 3)
		  (or (not (number? (ref coords 0)))
		      (not (= (ref coords 0) 0))))))
	((coords->point)
	 (lambda (coords)
	   (if (not ((me 'check-coordinates) coords))
	       (error "Bad coordinates: Euler-angles" coords me))
	   (let ((theta (ref coords 0))
		 (phi (ref coords 1))
		 (psi (ref coords 2)))
	     (let ((Mx-theta (rotate-x-tuple theta))
		   (Mz-phi (rotate-z-tuple phi))
		   (Mz-psi (rotate-z-tuple psi)))
	       (let ((the-point (* Mz-phi Mx-theta Mz-psi)))
		 (make-manifold-point 
		  the-point
		  manifold
		  me
		  coords))))))
	((check-point)
	 (lambda (point) (my-manifold-point? point manifold)))
	((point->coords)
	 (lambda (point)
	   (if (not ((me 'check-point) point))
	       (error "Bad manifold point: Euler-angles" point me))
	   (get-coordinates point me
	     (lambda ()
	       (let ((the-point
		      (manifold-point-representation point)))
		 (let ((theta (acos (ref the-point 2 2)))
		       (phi (atan (ref the-point 2 0)
				  (- (ref the-point 2 1))))
		       (psi (atan (ref the-point 0 2)
				  (ref the-point 1 2))))
		   (up theta phi psi)))))))
	((manifold) manifold)
	(else (error "Euler-chart: Bad message" m me)) ))
    me))
	   
(define alternate-chart
  (lambda (manifold)
    (define (me m)
      (case m
	((check-coordinates)
	 (lambda (coords)
	     (and (up? coords)
		  (fix:= (s:dimension coords) 3)
		  (or (not (number? (ref coords 0)))
		      (and (not (<= (ref coords 0) (- pi/2)))
			   (not (>= (ref coords pi/2))))))))
	((coords->point)
	 (lambda (coords)
	   (if (not ((me 'check-coordinates) coords))
	       (error "Bad coordinates: alternate-angles" coords me))
	   (let ((theta (ref coords 0))
		 (phi (ref coords 1))
		 (psi (ref coords 2)))
	     (let ((Mx-theta (rotate-x-tuple theta))
		   (Mz-phi (rotate-z-tuple phi))
		   (My-psi (rotate-y-tuple psi)))
	       (let ((the-point (* Mz-phi Mx-theta My-psi)))
		 (make-manifold-point the-point manifold me coords))))))
	((check-point)
	 (lambda (point) (my-manifold-point? point manifold)))
	((point->coords)
	 (lambda (point)
	   (if (not ((me 'check-point) point))
	       (error "Bad manifold point: alternate-angles" point me))
	   (get-coordinates point me
	     (lambda ()
	       (let ((the-point
		      (manifold-point-representation point)))
		 (let ((theta (asin (ref the-point 1 2)))
		       (phi
			(atan (- (ref the-point 1 0))
			      (ref the-point 1 1)))
		       (psi
			(atan (- (ref the-point 0 2))
			      (ref the-point 2 2))))
		   (up theta phi psi)))))))
	((manifold) manifold)
	(else (error "alternate-chart: Bad message" m me))))
    me))	   

(attach-patch 'Euler-patch SO3-type)

(attach-coordinate-system 'Euler 'Euler-patch SO3-type Euler-chart)

(attach-patch 'alternate-patch SO3-type)

(attach-coordinate-system 'alternate 'alternate-patch SO3-type alternate-chart)


(define SO3 (make-manifold SO3-type 3))

(define Euler-angles (coordinate-system-at 'Euler 'Euler-patch SO3))

(define alternate-angles (coordinate-system-at 'alternate 'alternate-patch SO3))

#|
(pec ((compose (alternate-angles '->coords)
	       (Euler-angles '->point))
      (up 'theta 'phi 'psi)))
#| Result:
(up
 (asin (* (cos psi) (sin theta)))
 (atan (+ (* (cos theta) (sin phi) (cos psi)) (* (cos phi) (sin psi)))
       (+ (* (cos phi) (cos theta) (cos psi)) (* -1 (sin psi) (sin phi))))
 (atan (* -1 (sin psi) (sin theta)) (cos theta)))
|#

(pec ((compose (Euler-angles '->coords)
	       (alternate-angles '->point)
	       (alternate-angles '->coords)
	       (Euler-angles '->point))
      (up 'theta 'phi 'psi)))
#| Result:
(up theta phi psi)
|#
|#

(define (literal-scalar-field name coordinate-system)
  (let ((n (coordinate-system 'dimension)))
    (let ((function-signature
	   (if (fix:= n 1) (-> Real Real) (-> (UP* Real n) Real))))
      (let ((function
	     (compose (literal-function name function-signature)
		      (coordinate-system '->coords))))
	(eq-put! function 'function-name name)
	(eq-put! function 'coordinate-system coordinate-system)
	function))))

(define (zero-coordinate-function c) 0)

;;; An alias.

(define literal-manifold-function literal-scalar-field)

(define (zero-manifold-function m)
  (assert (manifold-point? m))
  0)

(define (one-manifold-function m)
  (assert (manifold-point? m))
  1)

(define ((constant-manifold-function c) m)
  (assert (manifold-point? m))
  c)

#|
;;; A scalar field can be defined by combining coordinate functions:

(install-coordinates R3-rect (up 'x 'y 'z))

(install-coordinates R3-cyl (up 'r 'theta 'zeta))

(define h (+ 5 (square x) (* -1 x (cube y)) (/ 1 y)))

;;; The field, however defined, can be seen as independent of
;;; coordinate system:

(h ((R3-rect '->point) (up 3. 4. 'z)))
;Value: -177.75

(h ((R3-cyl '->point) (up 5. (atan 4 3) 'z)))
;Value: -177.74999999999997

;;; However this may be too clever, producing a traditional notation
;;; that is hard to understand deeply.  Perhaps it is better to be
;;; explicit about what is coordinate-system independent.  For
;;; example, we can define a coordinate-free function h by composing a
;;; definition in terms of coordinates with a coordinate function.

(define (h-concrete xy)
  (let ((x (ref xy 0))
	(y (ref xy 1)))
    (+ 5
       (square x)
       (* -1 x (cube y))
       (/ 1 y))))

(define h
  (compose h-concrete (R3-rect '->coords)))

(h ((R3-rect '->point) (up 3. 4 5)))
;Value: -177.75
|#

;;; Adding in stuff to S^2-type manifolds

(attach-coordinate-system 'stereographic 'north-pole S^2-type
 (S^n-stereographic m:make-identity))

(attach-coordinate-system 'stereographic 'south-pole S^2-type
 (S^n-stereographic
  (lambda (n)
    (m:generate n n
		(lambda (i j)
		  (if (= i j)
		      (if (= j n) -1 1)
		      0))))))

(attach-coordinate-system 'gnomic 'north-pole S^2-type
 (S^n-gnomic m:make-identity))

(attach-coordinate-system 'gnomic 'south-pole S^2-type
 (S^n-gnomic
  (lambda (n)
    (m:generate n n
		(lambda (i j)
		  (if (= i j)
		      (if (= j n) -1 1)
		      0))))))

(define S2-stereographic
  (coordinate-system-at 'stereographic 'north-pole S2))
(define S2-Riemann S2-stereographic)

(define S2-gnomic
  (coordinate-system-at 'gnomic 'north-pole S2))

