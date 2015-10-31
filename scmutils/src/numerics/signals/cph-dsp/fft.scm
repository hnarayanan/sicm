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
;;; $Id: fft.scm,v 1.4 1999/10/18 02:30:01 cph Exp $
;;;
;;; Copyright (c) 1993-98 Massachusetts Institute of Technology
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

;;;; Fast-Fourier Transform

(declare (usual-integrations))

(define (flo:real-fft reals #!optional n wn-vectors)
  (let ((n
	 (if (or (default-object? n) (not n))
	     (fix:ceiling-lg (flo:vector-length reals))
	     n)))
    (let ((reals (flo:vector-grow reals n 0.))
	  (wn-vectors
	   (if (or (default-object? wn-vectors) (not wn-vectors))
	       (compute-wn-vectors n)
	       wn-vectors)))
      (cons reals
	    (flo:real-fft! reals (car wn-vectors) (cdr wn-vectors))))))

(define (flo:real-fft! reals cosines sines)
  (let ((imags (flo:make-vector (flo:vector-length reals) 0.)))
    (flo:bit-reverse-vector! reals)
    (do-butterflies! reals imags cosines sines)
    imags))

(define (flo:real-inverse-fft reals #!optional n wn-vectors)
  (let ((n
	 (if (or (default-object? n) (not n))
	     (fix:ceiling-lg (flo:vector-length reals))
	     n)))
    (let ((reals (flo:vector-grow reals n 0.))
	  (wn-vectors
	   (if (or (default-object? wn-vectors) (not wn-vectors))
	       (compute-wn-vectors n)
	       wn-vectors)))
      (cons reals
	    (flo:real-inverse-fft! reals wn-vectors)))))

(define (flo:real-inverse-fft! reals #!optional wn-vectors)
  (let ((imags (flo:make-vector (flo:vector-length reals) 0.))
	(wn-vectors
	 (if (or (default-object? wn-vectors) (not wn-vectors))
	     (compute-wn-vectors (flo:vector-length reals))
	     wn-vectors)))
    (flo:inverse-fft-reverse! reals)
    (do-butterflies! reals imags (car wn-vectors) (cdr wn-vectors))
    imags))

(define (flo:complex-fft reals imags #!optional n wn-vectors)
  (let ((n
	 (if (or (default-object? n) (not n))
	     (fix:ceiling-lg (flo:vector-length reals))
	     n)))
    (let ((reals (flo:vector-grow reals n 0.))
	  (imags (flo:vector-grow imags n 0.))
	  (wn-vectors
	   (if (or (default-object? wn-vectors) (not wn-vectors))
	       (compute-wn-vectors n)
	       wn-vectors)))
      (flo:complex-fft! reals imags (car wn-vectors) (cdr wn-vectors))
      (cons reals imags))))

(define (flo:complex-fft! reals imags cosines sines)
  (flo:bit-reverse-vector! reals)
  (flo:bit-reverse-vector! imags)
  (do-butterflies! reals imags cosines sines))

(define (flo:complex-inverse-fft reals imags #!optional n wn-vectors)
  (let ((n
	 (if (or (default-object? n) (not n))
	     (fix:ceiling-lg (flo:vector-length reals))
	     n)))
    (let ((reals (flo:vector-grow reals n 0.))
	  (imags (flo:vector-grow imags n 0.))
	  (wn-vectors
	   (if (or (default-object? wn-vectors) (not wn-vectors))
	       (compute-wn-vectors n)
	       wn-vectors)))
      (flo:complex-inverse-fft! reals imags (car wn-vectors) (cdr wn-vectors))
      (cons reals imags))))

(define (flo:complex-inverse-fft! reals imags cosines sines)
  (flo:inverse-fft-reverse! reals)
  (flo:inverse-fft-reverse! imags)
  (do-butterflies! reals imags cosines sines))

(define (fix:ceiling-lg n)
  (do ((n* 1 (fix:lsh n* 1)))
      ((fix:>= n* n) n*)))

(define (flo:bit-reverse-vector! data)
  (let ((n (flo:vector-length data))
	(temp (flo:vector-cons 1)))
    (let ((n/2 (fix:lsh n -1))
	  (n-1 (fix:- n 1)))
      (do ((i 1 (fix:+ i 1))
	   (j n/2
	      (let loop ((j j) (k n/2))
		(if (fix:<= k j)
		    (loop (fix:- j k) (fix:lsh k -1))
		    (fix:+ j k)))))
	  ((fix:= i n-1))
	(if (fix:< i j)
	    (begin
	      (flo:vector-set! temp 0 (flo:vector-ref data j))
	      (flo:vector-set! data j (flo:vector-ref data i))
	      (flo:vector-set! data i (flo:vector-ref temp 0))))))))

(define (flo:inverse-fft-reverse! data)
  (let ((n (flo:vector-length data)))
    (let ((n/2 (fix:lsh n -1))
	  (n. (int:->flonum n))
	  (temp (flo:vector-cons 1)))
      (flo:vector-set! data 0 (flo:/ (flo:vector-ref data 0) n.))
      (flo:vector-set! data n/2 (flo:/ (flo:vector-ref data n/2) n.))
      (do ((i 1 (fix:+ i 1)))
	  ((fix:= i n/2))
	(flo:vector-set! temp 0 (flo:/ (flo:vector-ref data i) n.))
	(flo:vector-set! data i (flo:/ (flo:vector-ref data (fix:- n i)) n.))
	(flo:vector-set! data (fix:- n i) (flo:vector-ref temp 0)))))
  (flo:bit-reverse-vector! data))

(define compute-wn-vectors
  (let ((-2pi (flo:* -8. (flo:atan2 1. 1.))))
    (lambda (n)
      (let ((base-angle (flo:/ -2pi (int:->flonum n)))
	    (n/2 (fix:lsh n -1)))
	(let ((cosines (flo:vector-cons n/2))
	      (sines (flo:vector-cons n/2)))
	  (flo:vector-set! cosines 0 1.)
	  (flo:vector-set! sines 0 0.)
	  (do ((i 1 (fix:+ i 1))
	       (angle base-angle (flo:+ angle base-angle)))
	      ((fix:= i n/2))
	    (flo:vector-set! cosines i (flo:cos angle))
	    (flo:vector-set! sines i (flo:sin angle)))
	  (cons cosines sines))))))

(define (do-butterflies! reals imags cosines sines)
  (let ((n (flo:vector-length reals))
	(temps (flo:vector-cons 4)))
    (do ((le 2 (fix:lsh le 1))
	 (le1 1 le)
	 (wn-index-delta (fix:lsh n -1) (fix:lsh wn-index-delta -1)))
	((fix:= wn-index-delta 0))
      (do ((i1 0 (fix:+ i1 le)))
	  ((fix:= i1 n))
	(let ((i2 (fix:+ i1 le1)))
	  (flo:vector-set! temps 2 (flo:vector-ref reals i2))
	  (flo:vector-set! temps 3 (flo:vector-ref imags i2))
	  (flo:vector-set! reals i2
			   (flo:- (flo:vector-ref reals i1)
				  (flo:vector-ref temps 2)))
	  (flo:vector-set! imags i2
			   (flo:- (flo:vector-ref imags i1)
				  (flo:vector-ref temps 3)))
	  (flo:vector-set! reals i1
			   (flo:+ (flo:vector-ref reals i1)
				  (flo:vector-ref temps 2)))
	  (flo:vector-set! imags i1
			   (flo:+ (flo:vector-ref imags i1)
				  (flo:vector-ref temps 3)))))
      (do ((j 1 (fix:+ j 1))
	   (wn-index wn-index-delta (fix:+ wn-index wn-index-delta)))
	  ((fix:= j le1))
	(flo:vector-set! temps 0 (flo:vector-ref cosines wn-index))
	(flo:vector-set! temps 1 (flo:vector-ref sines wn-index))
	(do ((i1 j (fix:+ i1 le)))
	    ((fix:>= i1 n))
	  (let ((i2 (fix:+ i1 le1)))
	    (flo:vector-set! temps 2
			     (flo:+ (flo:* (flo:vector-ref reals i2)
					   (flo:vector-ref temps 0))
				    (flo:* (flo:vector-ref imags i2)
					   (flo:vector-ref temps 1))))
	    (flo:vector-set! temps 3
			     (flo:- (flo:* (flo:vector-ref imags i2)
					   (flo:vector-ref temps 0))
				    (flo:* (flo:vector-ref reals i2)
					   (flo:vector-ref temps 1))))
	    (flo:vector-set! reals i2
			     (flo:- (flo:vector-ref reals i1)
				    (flo:vector-ref temps 2)))
	    (flo:vector-set! imags i2
			     (flo:- (flo:vector-ref imags i1)
				    (flo:vector-ref temps 3)))
	    (flo:vector-set! reals i1
			     (flo:+ (flo:vector-ref reals i1)
				    (flo:vector-ref temps 2)))
	    (flo:vector-set! imags i1
			     (flo:+ (flo:vector-ref imags i1)
				    (flo:vector-ref temps 3)))))))))

(define (halve-fft-results! results)
  (flo:set-vector-length! (car results)
			  (fix:lsh (flo:vector-length (car results)) -1))
  (flo:set-vector-length! (cdr results)
			  (fix:lsh (flo:vector-length (cdr results)) -1))
  results)

(define (fft-results->magnitude-squared! results)
  (let ((reals (car results))
	(imags (cdr results)))
    (let ((n (flo:vector-length reals)))
      (do ((i 0 (fix:+ i 1)))
	  ((fix:= i n))
	(flo:vector-set!
	 reals i
	 (flo:+ (flo:* (flo:vector-ref reals i)
		       (flo:vector-ref reals i))
		(flo:* (flo:vector-ref imags i)
		       (flo:vector-ref imags i))))))
    reals))

(define magnitude-squared->log-magnitude!
  (let ((log-scale-factor (flo:/ 10. (flo:log 10.))))
    (lambda (reals)
      (let ((n (flo:vector-length reals)))
	(do ((i 0 (fix:+ i 1)))
	    ((fix:= i n))
	  (flo:vector-set!
	   reals
	   i
	   (flo:* (flo:log (if (flo:< (flo:vector-ref reals i) 1e-100)
			       1e-100
			       (flo:vector-ref reals i)))
		  log-scale-factor))))
      reals)))

(define (fft-results->angle! results)
  (let ((reals (car results))
	(imags (cdr results)))
    (let ((n (flo:vector-length reals)))
      (do ((i 0 (fix:+ i 1)))
	  ((fix:= i n))
	(flo:vector-set!
	 reals i
	 (if (and (flo:zero? (flo:vector-ref reals i))
		  (flo:zero? (flo:vector-ref imags i)))
	     0.
	     (flo:atan2 (flo:vector-ref imags i)
			(flo:vector-ref reals i))))))
    reals))

(define (fft-results->complex results)
  (let ((reals (car results))
	(imags (cdr results)))
    (let ((n (flo:vector-length reals)))
      (let ((result (make-vector n)))
	(do ((i 0 (fix:+ i 1)))
	    ((fix:= i n))
	  (vector-set! result
		       i
		       (if (flo:zero? (flo:vector-ref imags i))
			   (flo:vector-ref reals i)
			   (make-rectangular (flo:vector-ref reals i)
					     (flo:vector-ref imags i)))))
	result))))