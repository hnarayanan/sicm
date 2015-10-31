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

;;;; Cluster Analysis of a set of objects with a distance measure

(declare (usual-integrations))

(define (cluster objects cluster-separation distance)
  (if (null? objects)
      '()
      (let merge-lp ((clusters (map make-singleton-cluster objects)))
	(if (null? (cdr clusters))
	    clusters
	    (let ((candidates '()) (min-d 1e307)) ;+infinity
	      (let scan-lp-1 ((c1 clusters))
		(if (null? (cdr c1))
		    (merge-lp (cons (merge-2-clusters candidates distance)
				    (multiset-difference clusters candidates)))
		    (let scan-lp-2 ((c2 (cdr c1)))
		      (let ((d (cluster-separation (car c1) (car c2))))
			(if (< d min-d)
			    (begin (set! candidates (list (car c1) (car c2)))
				   (set! min-d d)))
			(if (null? (cdr c2))
			    (scan-lp-1 (cdr c1))
			    (scan-lp-2 (cdr c2))))))))))))

(define (multiset-difference s1 s2)
  (if (null? s2)
      s1
      (multiset-difference (remove-one (car s2) s1)
			   (cdr s2))))

(define (remove-one x s)
  (cond ((null? s) '())
	((eq? x (car s)) (cdr s))
	(else
	 (cons (car s)
	       (remove-one x (cdr s))))))


;;; A cluster has: elements, a diameter, the subclusters it was made from.

(define (merge-2-clusters clusters distance)
  (let ((c1s (cluster-elements (car clusters)))
	(c2s (cluster-elements (cadr clusters))))
    (make-a-cluster (append c1s c2s)
		    (max (apply max
				(apply append
				       (map (lambda (c1)
					      (map (lambda (c2)
						     (distance c1 c2))
						   c2s))
					    c1s)))
			(cluster-diameter (car clusters))
			(cluster-diameter (cadr clusters)))
		    clusters)))

(define (make-a-cluster elements diameter subclusters)
  (list elements diameter subclusters))

(define (cluster-elements cluster) (car cluster))
(define (cluster-diameter cluster) (cadr cluster))
(define (cluster-subclusters cluster) (caddr cluster))

(define (make-singleton-cluster el)
  (make-a-cluster (list el) 0 '()))


(define (set-separation element-distance)
  (lambda (cl1 cl2)
    (let ((c1s (cluster-elements cl1))
	  (c2s (cluster-elements cl2)))
      (apply min
	     (apply append
		    (map (lambda (c1)
			   (map (lambda (c2)
				  (element-distance c1 c2))
				c2s))
			 c1s))))))
