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

;;;; Expression Canonicalizer, by weak discrimination net.

(declare (usual-integrations))

(define *expression-tables* '())


;;; This is used in numsymb

(define (make-expression-canonicalizer)
  (let ((expression-table (make-new-expression-table)))
    (define (canonicalized obj)
      (canonicalize-object obj expression-table))
    (set! *expression-tables*
	  (weak-cons expression-table *expression-tables*))
    canonicalized))
	   

;;; This composition may not be useful...

(define (expression-memoizer f)
  (compose (eq-hash-memoizer f)
	   (make-expression-canonicalizer)))

(define (eq-hash-memoizer f)
  (let ((table (make-eq-hash-table)))
    (define (memo-f x)
      (let ((seen (hash-table/get table x *not-seen*)))
	(if (eq? seen *not-seen*)
	    (let ((val (f x)))
	      (hash-table/put! table x val)
	      val)
	    seen)))
    memo-f))

(define (canonicalize-object object table)
  (canonicalize-lp object (fringe object) table))


(define canonicalizer-hits 0)
(define canonicalizer-misses 0)

(define (canonicalize-lp object features table)
  (if (not object)
      object
      (features
       (lambda (first-feature find-next-feature)
	 (cond ((null? first-feature)
		(let ((ans (object-in-table-enders object table)))
		  (cond (ans
			 (set! canonicalizer-hits (int:+ canonicalizer-hits 1))
			 ans)
			(else
			 (set! canonicalizer-misses (int:+ canonicalizer-misses 1))
			 (add-table-ender! table object)
			 object))))
	       ((terminal-table? table)
		(let ((ans (object-in-table-extenders object table)))
		  (cond (ans
			 (set! canonicalizer-hits (int:+ canonicalizer-hits 1))
			 ans)
			(else
			 (set! canonicalizer-misses (int:+ canonicalizer-misses 1))
			 (add-table-extender! table object features)
			 (if (worth-subindexing? table)
			     (extend-index! table))
			 object))))
	       (else
		(canonicalize-lp object
				 find-next-feature
				 (find-next-subtable! first-feature
						      table))))))))


;;; If this is too small Scheme dies with PDL overflow in GC.
(define *subindex-threshold* 40)

(define (worth-subindexing? table)
  (fix:> (length (table-extenders table)) *subindex-threshold*))

(define (extend-index! table)
  (for-each (lambda (ext)
	      (if ext
		  ((extender-stream ext)
		   (lambda (first find-next)
		     (canonicalize-lp (extender-object ext)
				      find-next
				      (find-next-subtable! first table))))))
	    (table-extenders table))
  (clear-table-extenders! table))

;;; The expression is represented by its fringe.

(define (fringe tree)
  (define (fringen node exhausted)
    ;; exhausted = (lambda (receiver) ...)
    (lambda (receiver1)
      ;; receiver1 = (lambda (first get-next) ...)
      (cond ((pair? node)
	     ((fringen (car node)
		       (lambda (receiver2)
			 ;; receiver2 = (lambda (first get-next) ...)
			 ((fringen (cdr node) exhausted)
			  receiver2)))
	      receiver1))
	    ((null? node) (exhausted receiver1))
	    ((vector? node)
	     (let ((n (vector-length node)))
	       (let lp ((i 0) (receiver receiver1))
		 (if (fix:= i n)
		     (exhausted receiver)
		     ((fringen (vector-ref node i)
			       (lambda (receiver2)
				 ;; receiver2 = (lambda (first get-next) ...)
				 (lp (fix:+ i 1) receiver2)))
		      receiver)))))
	    (else (receiver1 node exhausted)))))
  (fringen tree
	   (lambda (receiver)
	     (receiver '() '()))))

#|
(let lp ((es (fringe '(1 #(2 3 (4 5) 6) 7))))
  (es (lambda (first find-next)
	(if (eq? first '())
	    'done
	    (cons (square first)
		  (lp find-next))))))
;Value: (1 4 9 16 25 36 49 . done)
|#

;;; Implementation in terms of weak lists

(define (make-new-expression-table)
  (cons '() (cons '() '())))

(define (table-enders table) (car table))

(define (add-table-ender! table obj)
  (set-car! table (weak-cons obj (car table))))

(define (object-in-table-enders object table)
  (get-weak-member object (table-enders table)))


(define (table-extenders table) (cadr table))

(define (add-table-extender! table obj rest)
  (set-car! (cdr table) (cons (weak-cons obj rest) (cadr table))))

(define (clear-table-extenders! table)
  (set-car! (cdr table) '()))

(define (object-in-table-extenders object table)
  (weak-find object (table-extenders table)))

(define extender-object weak-car)

(define extender-stream weak-cdr)


(define (table-subindex table) (cddr table))

(define (add-table-subindex! table key value)
  (set-cdr! (cdr table) (cons (cons key value) (cddr table))))

(define (terminal-table? table)
  (null? (table-subindex table)))

(define (find-next-subtable! feature table)
  (let ((subindex (table-subindex table)))
    (let ((v (assv feature subindex)))
      (if v
	  (cdr v)
	  (let ((nt (make-new-expression-table)))
	    (add-table-subindex! table feature nt)
	    nt)))))

(define (expression-canonicalizer-gc-daemon)
  (if (weak-pair? *expression-tables*)
      (begin
	(let lp ((expression-tables *expression-tables*))
	  (if (weak-pair? expression-tables)
	      (begin
		(let ((table (weak-car expression-tables)))
		  (if table (clean-expression-table table)))
		(lp (weak-cdr expression-tables)))))
	(set! *expression-tables*
	      (clean-weak-list *expression-tables*))))
  'done)

(define (clean-expression-table table)
  (set-car! table (clean-weak-list (car table)))
  (set-car! (cdr table) (clean-weak-alist (cadr table)))
  (set-cdr! (cdr table) (clean-subtable-alist (cddr table)))
  (or (not (null? (car table)))
      (not (null? (cadr table)))
      (not (null? (cddr table)))))

;;; Install in system -- may only be done once!
(add-gc-daemon! expression-canonicalizer-gc-daemon)

#|
;;; Testing
#|
(begin 
  (set! numerical-expression-canonicalizer (make-expression-canonicalizer))
  'foo)
|#

;;; A simple test

(define ((L-central-polar m V) local)
  (let ((q (coordinate local))
        (qdot (velocity local)))
    (let ((r (ref q 0))
          (phi (ref q 1))
          (rdot (ref qdot 0))
          (phidot (ref qdot 1)))
      (- (* 1/2 m
           (+ (square rdot)
              (square (* r phidot))) )
         (V r)))))

(show-time
 (lambda ()
   (series:print
    (((Lie-transform
       (Lagrangian->Hamiltonian
	(L-central-polar 'm (lambda (r) (- (/ 'GM r)))))
       'dt)
      state->q)
     (->H-state 0
		 (coordinate-tuple 'r_0 'phi_0)
		 (momentum-tuple 'p_r_0 'p_phi_0)))
    4)))

(up r_0 phi_0)
(up (/ (* dt p_r_0) m) (/ (* dt p_phi_0) (* m (expt r_0 2))))
(up
 (+ (/ (* -1/2 GM (expt dt 2)) (* m (expt r_0 2)))
    (/ (* 1/2 (expt dt 2) (expt p_phi_0 2)) (* (expt m 2) (expt r_0 3))))
 (/ (* -1 (expt dt 2) p_phi_0 p_r_0) (* (expt m 2) (expt r_0 3))))
(up
 (+
  (/ (* 1/3 GM (expt dt 3) p_r_0) (* (expt m 2) (expt r_0 3)))
  (/ (* -1/2 (expt dt 3) (expt p_phi_0 2) p_r_0) (* (expt m 3) (expt r_0 4))))
 (+ (/ (* (expt dt 3) p_phi_0 (expt p_r_0 2)) (* (expt m 3) (expt r_0 4)))
    (/ (* 1/3 GM (expt dt 3) p_phi_0) (* (expt m 2) (expt r_0 5)))
    (/ (* -1/3 (expt dt 3) (expt p_phi_0 3)) (* (expt m 3) (expt r_0 6)))))
process time: 14740 (13780 RUN + 960 GC); real time: 14827
;Value: ...
|#

#|
;;; Debugging stuff:

(define (count-entries table)
  (let ((max-depth 0) (num-enders 0) (num-extenders 0)
	(max-num-branches 0) (max-num-enders 0) (max-num-extenders 0))
    (let lp ((depth 0) (table table))
      (set! max-depth (max depth max-depth))
      (set! num-enders
	    (fix:+ (weak-length (table-enders table)) num-enders))
      (set! max-num-enders
	    (max (weak-length (table-enders table)) max-num-enders))
      (set! num-extenders
	    (fix:+ (length (table-extenders table)) num-extenders))
      (set! max-num-extenders
	    (max (length (table-extenders table)) max-num-extenders))
      (set! max-num-branches
	    (max (length (table-subindex table)) max-num-branches))
      (for-each (lambda (s)
		  (lp (fix:+ depth 1) (cdr s)))
		(table-subindex table)))
    (write-line
     `(,max-depth ,num-enders ,num-extenders
		  ,max-num-enders ,max-num-extenders ,max-num-branches))))

(define (find-big-subindex table num-branches)
  (if (fix:= num-branches (length (table-subindex table)))
      (begin (write-line (map car (table-subindex table)))
	     table)
      (for-each (lambda (s)
		  (find-big-subindex (cdr s) num-branches))
		(table-subindex table))))
|#

#|
;;; Stuff to fix

(load "/usr/local/scmutils/relativity/calculus")
(load "/usr/local/scmutils/relativity/metric")

(define cartesian-plane-basis
  (coordinate-system->basis cartesian-plane))
(define d/dx (coordinate-basis-vector-field cartesian-plane 'd/dx 0))
(define d/dy (coordinate-basis-vector-field cartesian-plane 'd/dy 1))
(define dx (coordinate-basis-1form cartesian-plane 'dx 0))
(define dy (coordinate-basis-1form cartesian-plane 'dy 0))


(define polar-basis (coordinate-system->basis polar))
(define r (compose (component 0) (polar '->coords)))
(define theta (compose (component 1) (polar '->coords)))
(define d/dr (coordinate-basis-vector-field polar 'd/dr 0))
(define d/dtheta (coordinate-basis-vector-field polar 'd/dtheta 1))
(define dr (coordinate-basis-1form polar 'dr 0))
(define dtheta (coordinate-basis-1form polar 'dtheta 1))

(define X
  (components->vector-field 
   (up (literal-function 'X^0 (-> (UP Real Real) Real))
       (literal-function 'X^1 (-> (UP Real Real) Real)))
   cartesian-plane
   'X))

(define V
  (components->vector-field 
   (up (literal-function 'V^0 (-> (UP Real Real) Real))
       (literal-function 'V^1 (-> (UP Real Real) Real)))
   cartesian-plane
   'V))

(define (polar-metric v1 v2)
  (let ((1form-basis (basis->1form-basis polar-basis))
	(r (compose (component 0) (polar '->coords))))
    (+ (* ((ref 1form-basis 0) v1)
	  ((ref 1form-basis 0) v2))
       (* (square r)
	  ((ref 1form-basis 1) v1)
	  ((ref 1form-basis 1) v2)))))

(set! *divide-out-terms* #f)

(pe ((metric->first-Christoffel polar-basis polar-metric)
     ((polar '->point) (up 'r^0 'theta^0))))
(down (down (down 0 0) (down 0 r^0))
      (down (down 0 r^0) (down (* -1 r^0) 0)))

;;; This runs out of memory, without intermediate simp.
(pe ((metric->second-Christoffel polar-basis polar-metric)
     ((polar '->point) (up 'r^0 'theta^0))))
(down (down (up 0 0) (up 0 (/ 1 r^0)))
      (down (up 0 (/ 1 r^0)) (up (* -1 r^0) 0)))
|#


#|
(define (clean-weak-list weak-list)
  (if (weak-pair? weak-list)
      (if (weak-pair/car? weak-list)
	  (weak-cons (weak-car weak-list)
		     (clean-weak-list (weak-cdr weak-list)))
	  (clean-weak-list (weak-cdr weak-list)))
      weak-list))

(define (clean-weak-list weak-list)
  (if (weak-pair? weak-list)
      (let scanlp ((scan weak-list))
	(let ((rest (weak-cdr scan)))
	  (if (weak-pair? rest)
	      (if (weak-pair/car? rest)
		  (scanlp rest)
		  (begin
		    (weak-set-cdr! scan (weak-cdr rest))
		    (scanlp scan)))
	      (let frontlp ((front weak-list))
		(if (weak-pair/car? front)
		    front
		    (let ((rest (weak-cdr front)))
		      (if (weak-pair? rest)
			  (frontlp rest)
			  rest)))))))
      weak-list))

(define (clean-weak-alist weak-alist)
  (if (pair? weak-alist)
      (cond ((not (car weak-alist))
	     (clean-weak-alist (cdr weak-alist)))
	    ((weak-pair/car? (car weak-alist))
	     (cons (car weak-alist)
		   (clean-weak-alist (cdr weak-alist))))
	    (else (clean-weak-alist (cdr weak-alist))))
      weak-alist))


(define (clean-subtable-alist alist)
  (if (pair? alist)
      (if (clean-expression-table (cdar alist))
	  (cons (car alist)
		(clean-subtable-alist (cdr alist)))
	  (clean-subtable-alist (cdr alist)))
      alist))
|#
