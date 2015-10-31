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

;;;;    Derived Generic Operators

(declare (usual-integrations))

(define ratnum?
  (access ratnum?
	  (->environment '(runtime number))))

(define (g:cube x)
  (g:* x x x))


(define (g:log10 x)
  (g:/ (g:log x) (g:log 10)))
(define (g:log2 x)
  (g:/ (g:log x) (g:log 2)))

(define (g:exp10 x)
  (g:expt 10 x))
(define (g:exp2 x)
  (g:expt 2 x))


;;; See numbers.scm

(define (g:tan x)
  (g:/ (g:sin x) (g:cos x)))
(define (g:cot x)
  (g:/ (g:cos x) (g:sin x)))

(define (g:sec x)
  (g:/ :one (g:cos x)))
(define (g:csc x)
  (g:/ :one (g:sin x)))


(define (g:tanh x)
  (g:/ (g:sinh x) (g:cosh x)))
(define (g:sech x)
  (g:/ :one (g:cosh x)))
(define (g:csch x)
  (g:/ :one (g:sinh x)))

(define (g:asinh z)
  (g:log (g:+ z (g:sqrt (g:+ :one (g:square z))))))

(define (g:acosh z)
  (g:* :two
       (g:log (g:+ (g:sqrt (g:/ (g:+ z :one) :two))
		   (g:sqrt (g:/ (g:- z :one) :two))))))

(define (g:atanh z)
  (g:/ (g:- (g:log (g:+ :one z))
	    (g:log (g:- :one z)))
       :two))

(define (g:arg-shift f . shifts)
  (define (g . xs)
    (g:apply f (map g:+ xs shifts)))
  g)

(define (g:arg-scale f . scales)
  (define (g . xs)
    (g:apply f (map g:* xs scales)))
  g)


(define (g:sigma f low high)
  (if (fix:> low high)
      0
      (let lp ((i (fix:+ low 1)) (sum (f low)))
	(if (fix:> i high)
	    sum
	    (lp (fix:+ i 1) (g:+ sum (f i)))))))

;;; The generalized selector:


(define (g:ref x . selectors)
  (ref-internal x selectors))

(define ((component . selectors) x)
  (ref-internal x selectors))

(define (ref-internal x selectors)
  (cond ((null? selectors) x)
	((procedure? x)
	 (if (operator? x)
	     (make-operator (compose (lambda (y)
				       (ref-internal y selectors))
				     x)
			    `(compose (component ,@selectors)
				      ,(operator-name x))
			    (operator-subtype x))
	     (compose (lambda (y)
			(ref-internal y selectors))
		      x)))
	(else
	 (let ((i (car selectors)) (js (cdr selectors)))
	   (cond ((exact-integer? i)
		  (cond ((vector? x)
			 (ref-internal
			  (vector-ref x (adjust-index i (vector-length x)))
			  js))
			((structure? x)
			 (ref-internal (s:ref x (adjust-index i (s:length x)))
				       js))
			((matrix? x)
			 (if (null? js)
			     (cond ((column-matrix? x)
				    (ref-internal
				     (matrix-ref x
						 (adjust-index i (m:num-rows x))
						 0)
				     js))
				   ((row-matrix? x)
				    (ref-internal
				     (matrix-ref x
						 0
						 (adjust-index i
							       (m:num-cols x)))
				     js))
				   (else
				    (error "Not enuf indices -- REF" x i js)))
			     (ref-internal
			      (matrix-ref x 
					  (adjust-index i (m:num-rows x))
					  (adjust-index (car js)
							(m:num-cols x)))
			      (cdr js))))
			((series? x)
			 (ref-internal (stream-ref (series->stream x) i) js))
			((stream-pair? x)
			 (ref-internal (stream-ref x i) js))
			((list? x)
			 (ref-internal
			  (list-ref x (adjust-index i (length x)))
			  js))
			((string? x)
			 (if (not (null? js))
			     (error "String has no substructure -- REF" x i js))
			 (string-ref x (adjust-index i (string-length x))))
			(else
			 (error "Unknown compound -- G:REF" x i))))
		 ((and (pair? i)
		       (exact-integer? (car i))
		       (pair? (cdr i))
		       (exact-integer? (cadr i)))
		  (cond ((vector? x)
			 (ref-internal
			  (subvector x
				     (adjust-index (car i) (vector-length x))
				     (adjust-end (cadr i) (vector-length x)))
			  js))
			((structure? x)
			 (ref-internal
			  (s:structure
			   (s:type x)
			   (subvector (s:->vector x)
				      (adjust-index (car i) (s:length x))
				      (adjust-end (cadr i) (s:length x))))
			  js))
			((matrix? x)
			 (if (null? js)
			     (cond ((column-matrix? x)
				    (ref-internal
				     (m:submatrix x
						  (adjust-index (car i)
								(m:num-rows x))
						  (adjust-end (cadr i)
							      (m:num-rows x))
						  0
						  1)
				     js))
				   ((row-matrix? x)
				    (ref-internal
				     (m:submatrix x
						  0
						  1
						  (adjust-index (car i)
								(m:num-cols x))
						  (adjust-end (cadr i)
							      (m:num-cols x)))
				     js))
				   (else
				    (error "Not enuf indices -- REF" x i js)))
			     (ref-internal
			      (m:submatrix x 
					   (adjust-index (car i) (m:num-rows x))
					   (adjust-end (cadr i) (m:num-rows x))
					   (adjust-index (caar js)
							 (m:num-cols x))
					   (adjust-end (cadar js)
						       (m:num-cols x)))
			      (cdr js))))
			((list? x)
			 (ref-internal
			  (sublist x
				   (adjust-index (car i) (length x))
				   (adjust-end (cadr i) (length x)))
			  js))
			((string? x)
			 (if (not (null? js))
			     (error "String has no substructure -- REF" x i js))
			 (substring x
				    (adjust-index (car i) (string-length x))
				    (adjust-end (cadr i) (string-length x))))
			(else
			 (error "Unknown compound -- G:REF" x i))))
		 (else
		  (error "Unknown selector type -- REF" x i js)))))))

(define (adjust-index i n)
  (if (fix:< i 0)
      (let ((j (fix:+ n i)))
	(if (fix:< j 0)
	    (error "Bad index -- REF" i))
	j)
      (begin
	(if (not (fix:< i n))
	    (error "Bad index -- REF" i))
	i)))

(define (adjust-end i n)
  (let ((n (fix:+ n 1)))
    (if (fix:< i 0)
	(let ((j (fix:+ n i)))
	  (if (fix:< j 0)
	      (error "Bad index -- REF" i))
	  j)
	(begin
	  (if (not (fix:< i n))
	      (error "Bad index -- REF" i))
	  i))))


(define (g:size x)
  (cond ((vector? x)      (vector-length x))
	((matrix? x)      (matrix-size x))
	((structure? x)   (s:length x))
	((series? x)      #f)
	((stream-pair? x) #f)
	((list? x)        (length x))
	((string? x)      (string-length x))
	(else
	 (error "Unknown compound -- G:size" x))))

;;; Generic composition duplicates composition in utils

(define (g:compose . fs)
  (define (lp fs)
    (cond ((null? (cdr fs)) (car fs))
	  (else (g:compose-2 (car fs) (lp (cdr fs))))))
  (cond ((null? fs) g:identity)
	((null? (cdr fs)) (car fs))
	(else
	 (g:compose-bin (lp (butlast fs))
			(car (last-pair fs))))))

(define (g:identity x) x)

(define (g:compose-2 f g)
  (cond ((pair? g)
	 (lambda x
	   (g:apply f
		    (map (lambda (gi)
			   (g:apply gi x))
			 g))))
	(else
	 (lambda x
	   (f (g:apply g x))))))

(define (g:compose-bin f g)
  (cond ((and (pair? g) (not (structure? g)))
	 (let ((a
		(a-reduce joint-arity
			  (map g:arity g))))
	   (cond ((equal? a *at-least-zero*)
		  (lambda x
		    (g:apply f
			   (map
			    (lambda (gi)
			      (g:apply gi x))
			    g))))
		 ((equal? a *exactly-zero*)
		  (lambda ()
		    (g:apply f
			   (map (lambda (gi)
				  (gi))
				g))))
		 ((equal? a *at-least-one*)
		  (lambda (x . y)
		    (g:apply f
			   (map (lambda (gi)
				  (g:apply gi x y))
				g))))
		 ((equal? a *exactly-one*)
		  (lambda (x)
		    (g:apply f
			   (map (lambda (gi)
				  (gi x))
				g))))

		 ((equal? a *at-least-two*)
		  (lambda (x y . z)
		    (g:apply f
			   (map (lambda (gi)
				  (g:apply gi x y z))
				g))))
		 ((equal? a *exactly-two*)
		  (lambda (x y)
		    (g:apply f
			   (map (lambda (gi)
				  (gi x y))
				g))))
		 ((equal? a *at-least-three*)
		  (lambda (u x y . z)
		    (g:apply f
			   (map (lambda (gi)
				  (g:apply gi u x y z))
				g))))
		 ((equal? a *exactly-three*)
		  (lambda (x y z)
		    (g:apply f
			   (map (lambda (gi)
				  (gi x y z))
				g))))
		 ((equal? a *one-or-two*)
		  (lambda (x #!optional y)
		    (if (default-object? y)
			(g:apply f
			       (map (lambda (gi)
				      (gi x))
				    g))
			(g:apply f
			       (map (lambda (gi)
				      (gi x y))
				    g)))))
		 (else
		  (lambda x
		    (g:apply f
			   (map
			    (lambda (gi)
			      (g:apply gi x))
			    g)))))))
	(else
	 (let ((a (g:arity g)))
	   (cond ((equal? a *at-least-zero*)
		  (lambda x
		    (g:apply f
			     (list (g:apply g x)))))
		 ((equal? a *exactly-zero*)
		  (lambda ()
		    (g:apply f
			     (list (g:apply g '())))))
		 ((equal? a *at-least-one*)
		  (lambda (x . y)
		    (g:apply f
			     (list (g:apply g x y)))))
		 ((equal? a *exactly-one*)
		  (lambda (x)
		    (g:apply f
			     (list (g:apply g (list x))))))
		 ((equal? a *at-least-two*)
		  (lambda (x y . z)
		    (g:apply f
			     (list (g:apply g x y z)))))
		 ((equal? a *exactly-two*)
		  (lambda (x y)
		    (g:apply f
			     (list (g:apply g (list x y))))))
		 ((equal? a *at-least-three*)
		  (lambda (u x y . z)
		    (g:apply f
			     (list (g:apply g u x y z)))))
		 ((equal? a *exactly-three*)
		  (lambda (x y z)
		    (g:apply f
			     (list (g:apply g (list x y z))))))
		 ((equal? a *one-or-two*)
		  (lambda (x #!optional y)
		    (if (default-object? y)
			(g:apply f
				 (list (g:apply g (list x))))
			(g:apply f
				 (list (g:apply g (list x y)))))))
		 (else
		  (lambda x
		    (g:apply f
			     (list (g:apply g x))))))))))


