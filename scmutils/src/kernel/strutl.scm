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

;;;; STRUTL.SCM -- Stream utilities

(declare (usual-integrations))

(define stream:for-each
 (let ()
   (define (loop p s n)
     (cond ((empty-stream? s)
	    'done)
	   ((int:= n 1)
	    (p (head s))
	    '...)
	   (else
	    (p (head s))
	    (loop p (tail s) (int:- n 1)))))
   (lambda (p s . optionals)
     (loop p s
	   (if (not (null? optionals))
	       (car optionals)
	       -1)))))


(define print-stream
  (lambda (s . optionals)
     (apply stream:for-each write-line s optionals)))

(define (combiner-padded-streams f pad)
  (define (lp ss)
    (cons-stream (apply f
			(map (lambda (s)
			       (if (null? s)
				   pad
				   (head s)))
			     ss))
		 (lp (map (lambda (s)
			    (if (null? s)
				s
				(tail s)))
			  ss))))
  (lambda args (lp args)))


(define (stream-of-iterates next value)
  (cons-stream value
	       (stream-of-iterates next (next value))))


(define (infinite-stream-of x)
  (cons-stream x
	       (infinite-stream-of x)))


(define (stream-evaluate s x)
  (map-stream (lambda (f) (f x)) s))

(define (stream-apply s x)
  (map-stream (lambda (f) (apply f x)) s))


(define (map-stream f s)
  (if (empty-stream? s)
      the-empty-stream
      (cons-stream (f (head s))
		   (map-stream f (tail s)))))

(define map-streams stream-map)


(define (merge-streams s1 s2)
  (cons-stream (stream-car s1)
	       (cons-stream (stream-car s2)
			    (merge-streams (stream-cdr s1)
					   (stream-cdr s2)))))

(define (shorten-stream n s)
  (if (or (fix:= n 0) (empty-stream? s))
      the-empty-stream
      (cons-stream (head s)
		   (shorten-stream (fix:- n 1)
				   (tail s)))))

(define (stream:+ s1 s2)
  (map-streams g:+ s1 s2))

(define (stream:- s1 s2)
  (map-streams g:- s1 s2))

(define (stream:* s1 s2)
  (map-streams g:* s1 s2))

(define (stream:/ s1 s2)
  (map-streams g:/ s1 s2))



(define zero-stream 
  (cons-stream :zero zero-stream))

(define one-stream
  (cons-stream :one one-stream))

(define (integers-starting-from n)
  (cons-stream n (integers-starting-from (int:+ n 1))))


(define natural-number-stream
  (cons-stream :one
	       (stream:+ one-stream
			 natural-number-stream)))

(define factorial-stream
  (let ()
    (define (fact-helper n! n+1)
      (cons-stream n!
		   (fact-helper (g:* n+1 n!) (g:+ n+1 1))))
    (fact-helper 1 1)))

(define (stream-of-powers x unity)
  (stream-of-iterates (lambda (y) (g:* x y))
		      unity))


(define stream-for-each
  (access stream-for-each (->environment '(runtime stream))))

(define stream-append
  (access stream-append (->environment '(runtime stream))))

(define stream-filter
  (access stream-filter (->environment '(runtime stream))))

(define stream-accumulate
  (access stream-accumulate (->environment '(runtime stream))))

;;; MIT Scheme system provides 
;;;  PRIME-NUMBERS-STREAM

;;; expands a stream with zeros interpolated between given values.

(define (stream:inflate stream n-interpolated-zeros)
  (cons-stream (head stream)
	       (stream:list-append (make-list n-interpolated-zeros
					      (g:zero-like (head stream)))
				   (stream:inflate (tail stream)
						   n-interpolated-zeros))))

(define (stream:list-append list stream)
  (if (null? list)
      stream
      (cons-stream (car list)
		   (stream:list-append (cdr list)
				       stream))))