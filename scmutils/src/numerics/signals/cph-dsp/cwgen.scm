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

(declare (usual-integrations))

(define (cw->sampled-waveform cw keying-speed beat-freq samples/sec)
  (let ((envelope (cw->envelope cw keying-speed samples/sec))
	(omega/sample (exact->inexact (/ (* 2pi beat-freq) samples/sec))))
    (let ((nsamples (flo:vector-length envelope)))
      (do ((i 0 (fix:+ i 1))
	   (omega 0. (flo:+ omega omega/sample)))
	  ((fix:= i nsamples))
	(if (not (flo:= 0. (flo:vector-ref envelope i)))
	    (flo:vector-set! envelope i
			     (flo:* (flo:vector-ref envelope i)
				    (flo:sin omega))))))
    envelope))

(define (cw->envelope cw keying-speed samples/sec)
  (let ((nbits (string-length cw))
	(samples/bit (round->exact (/ samples/sec (* keying-speed 5/6))))
	(samples/change (round->exact (* samples/sec 5e-3))))
    (let ((nsamples (fix:+ (fix:* samples/bit nbits) samples/change))
	  (rise
	   (let ((rise (flo:make-vector samples/bit 1.))
		 (delta-omega (/ (/ 2pi 4.) samples/change)))
	     (do ((i 0 (fix:+ i 1))
		  (omega 0. (flo:+ omega delta-omega)))
		 ((= i samples/change))
	       (flo:vector-set! rise i (flo:sin omega)))
	     rise)))
      (let ((fall (flo:vector-map rise (lambda (x) (flo:- 1. x))))
	    (result (flo:vector-cons nsamples)))
	(do ((i 0 (fix:+ i 1)))
	    ((fix:= i samples/bit))
	  (flo:vector-set! result i (flo:vector-ref rise i)))
	(do ((i 1 (+ i 1))
	     (j samples/bit (+ j samples/bit)))
	    ((= i nbits))
	  (let ((bit (string-ref cw i))
		(j* (fix:+ j samples/bit)))
	    (if (char=? (string-ref cw (- i 1)) bit)
		(let ((x (if (char=? #\0 bit) 0. 1.)))
		  (do ((k j (fix:+ k 1)))
		      ((fix:= k j*))
		    (flo:vector-set! result k x)))
		(let ((template (if (char=? #\0 bit) fall rise)))
		  (do ((k j (fix:+ k 1))
		       (l 0 (fix:+ l 1)))
		      ((fix:= k j*))
		    (flo:vector-set! result k (flo:vector-ref template l)))))))
	(do ((i 0 (fix:+ i 1))
	     (j (fix:- nsamples samples/change) (fix:+ j 1)))
	    ((fix:= j nsamples))
	  (flo:vector-set! result j (flo:vector-ref fall i)))
	result))))

(define (string->cw string)
  (if (string-null? string)
      ""
      (let loop ((chars (string->list string)))
	(string-append (char->cw (car chars))
		       (if (null? (cdr chars))
			   ""
			   (string-append "000"
					  (loop (cdr chars))))))))

(define (char->cw char)
  (vector-ref char->cw-table (char->integer char)))

(define (define-cw-char char . elements)
  (vector-set!
   char->cw-table
   (char->integer char)
   (let loop ((elements elements))
     (let ((prefix
	    (case (car elements)
	      ((dit) "1")
	      ((dah) "111")
	      (else (error "Ill-formed element:" (car elements))))))
       (if (null? (cdr elements))
	   prefix
	   (string-append prefix "0" (loop (cdr elements))))))))

(define char->cw-table
  (make-vector 256 #f))

(vector-set! char->cw-table (char->integer #\space) "0")

(define-cw-char #\a 'dit 'dah)
(define-cw-char #\b 'dah 'dit 'dit 'dit)
(define-cw-char #\c 'dah 'dit 'dah 'dit)
(define-cw-char #\d 'dah 'dit 'dit)
(define-cw-char #\e 'dit)
(define-cw-char #\f 'dit 'dit 'dah 'dit)
(define-cw-char #\g 'dah 'dah 'dit)
(define-cw-char #\h 'dit 'dit 'dit 'dit)
(define-cw-char #\i 'dit 'dit)
(define-cw-char #\j 'dit 'dah 'dah 'dah)
(define-cw-char #\k 'dah 'dit 'dah)
(define-cw-char #\l 'dit 'dah 'dit 'dit)
(define-cw-char #\m 'dah 'dah)
(define-cw-char #\n 'dah 'dit)
(define-cw-char #\o 'dah 'dah 'dah)
(define-cw-char #\p 'dit 'dah 'dah 'dit)
(define-cw-char #\q 'dah 'dah 'dit 'dah)
(define-cw-char #\r 'dit 'dah 'dit)
(define-cw-char #\s 'dit 'dit 'dit)
(define-cw-char #\t 'dah)
(define-cw-char #\u 'dit 'dit 'dah)
(define-cw-char #\v 'dit 'dit 'dit 'dah)
(define-cw-char #\w 'dit 'dah 'dah)
(define-cw-char #\x 'dah 'dit 'dit 'dah)
(define-cw-char #\y 'dah 'dit 'dah 'dah)
(define-cw-char #\z 'dah 'dah 'dit 'dit)
(define-cw-char #\1 'dit 'dah 'dah 'dah 'dah)
(define-cw-char #\2 'dit 'dit 'dah 'dah 'dah)
(define-cw-char #\3 'dit 'dit 'dit 'dah 'dah)
(define-cw-char #\4 'dit 'dit 'dit 'dit 'dah)
(define-cw-char #\5 'dit 'dit 'dit 'dit 'dit)
(define-cw-char #\6 'dah 'dit 'dit 'dit 'dit)
(define-cw-char #\7 'dah 'dah 'dit 'dit 'dit)
(define-cw-char #\8 'dah 'dah 'dah 'dit 'dit)
(define-cw-char #\9 'dah 'dah 'dah 'dah 'dit)
(define-cw-char #\0 'dah 'dah 'dah 'dah 'dah)
(define-cw-char #\. 'dit 'dah 'dit 'dah 'dit 'dah)
(define-cw-char #\, 'dah 'dah 'dit 'dit 'dah 'dah)
(define-cw-char #\? 'dit 'dit 'dah 'dah 'dit 'dit)
(define-cw-char #\= 'dah 'dit 'dit 'dit 'dah)
(define-cw-char #\/ 'dah 'dit 'dit 'dah 'dit)
(define-cw-char #\- 'dah 'dit 'dit 'dit 'dit 'dah)
(define-cw-char #\: 'dah 'dah 'dah 'dit 'dit 'dit)
(define-cw-char #\; 'dah 'dit 'dah 'dit 'dah 'dit)
(define-cw-char #\( 'dah 'dit 'dah 'dah 'dit)
(define-cw-char #\) 'dah 'dit 'dah 'dah 'dit 'dah)
(define-cw-char #\" 'dit 'dah 'dit 'dit 'dah 'dit)
(define-cw-char #\$ 'dit 'dit 'dit 'dah 'dit 'dit 'dah)
(define-cw-char #\' 'dit 'dah 'dah 'dah 'dah 'dit)
(define-cw-char #\_ 'dit 'dit 'dah 'dah 'dit 'dah)
