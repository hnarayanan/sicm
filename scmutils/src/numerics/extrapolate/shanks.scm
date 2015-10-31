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

;;;; Shank's sequence extrapolator:
;;;  { a0, a1, a2, ... }
;;;      ==>  {(a0*a2-a1*a1)/(a0-2a1+a2), ... }


(define (shanks-transform seq)
  (let* ((a0 (head seq)) (t0 (tail seq))
	 (a1 (head t0))  (t1 (tail t0))
	 (a2 (head t1)))
    (cons-stream (/ (- (* a2 a0) (* a1 a1))
		    (+ a2 a0 (* -2 a1)))
		 (shanks-transform t0))))
		 
(define (make-shanks-tableau seq)
  (cons-stream seq (make-shanks-tableau (shanks-transform seq))))

(define (first-terms-of-tableau tableau)
  (map-stream head tableau))

(define (shanks-sequence seq)
  (first-terms-of-tableau (make-shanks-tableau seq)))

(define (shanks-limit f start-h tolerance . opts)
  (apply stream-limit
	 (shanks-sequence (make-zeno-sequence f start-h))
	 tolerance
	 opts))
#|
;;; The following sequence is easily accelerated with shanks method
;;;  but not Richardson's method.

(define (exact z)
  (/ 1 (+ z 1) (+ z 2)))

(define (ck k)
  (let ((c (- 1 (expt 2 (- (+ k 1))))))
    (if (even? k) 
        c
        (- c))))

(define (as ck z)
  (define (addem-up sum k z^k)
    (cons-stream sum
                 (addem-up (+ (* (ck k) z^k) sum)
                           (+ k 1)
                           (* z^k z))))
  (addem-up 0 0 1))




;;; Another shanks winner:

(define (ln2)
  (define (l sum k)
    (cons-stream sum
                 (l (+ sum 
                       (if (even? k)
                           (/ -1 k)
                           (/ 1 k)))
                    (+ k 1))))
  (l 1 2))


(stream:for-each write-line
		 (ln2)
		 10)
1
.5
.8333333333333333
.5833333333333333
.7833333333333332
.6166666666666666
.7595238095238095
.6345238095238095
.7456349206349207
.6456349206349207
;Value: ...

(stream:for-each write-line
		 (shanks-sequence (ln2))
		 8)
1.
.7
.6932773109243697
.6931488693329254
.6931471960735491
.6931471806635636
.6931471805604039
.6931471805599444
;Value: ...

;Value: ln2

(log 2)
;Value: .6931471805599453




|#