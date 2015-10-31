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

(define (gcd-test d f g)
  (let ((pd (pcf:expression-> d (lambda (p v) p)))
	(pf (pcf:expression-> f (lambda (p v) p)))
	(pg (pcf:expression-> g (lambda (p v) p))))
    (poly:= (poly:gcd (poly:* pd pf) (poly:* pd pg))
	    pd)))


(define d1
  '(+ (expt x1 2) x1 3))

(define f1
  '(+ (* 2 (expt x1 2)) (* 2 x1) 1))

(define g1
  '(+ (expt x1 2) (* 2 x1) 2))

(gcd-test d1 f1 g1)
;Value: #t


(define d2
  '(+ (* 2 (expt x1 2) (expt x2 2))
      (* x1 x2)
      (* 2 x1)))

(define f2
  '(+ (expt x2 2)
      (* 2 (expt x1 2) x2)
      (expt x1 2)
      1))

(define g2
  '(+ (* (expt x1 2) (expt x2 2))
      (* (expt x1 2) x2)
      (* x1 x2)
      (expt x1 2)
      x1))

(gcd-test d2 f2 g2)
;Value: #t


(define d3
  '(+ (* x2 x2 x3 x3)
      (* x2 x2 x3)
      (* 2 x1 x1 x2 x3)
      (* x1 x3)))

(define f3
  '(+ (* x3 x3)
      (* x2 x2 x3)
      (* x1 x1 x2 x3)
      (* x1 x3)
      (* x1 x1 x2 x2)))

(define g3
  '(+ (* x2 x3)
      (* 2 x1 x3)
      x3
      x1))

(gcd-test d3 f3 g3)
;Value: #t


(define d4
  '(+ (* x1 x1 x4 x4)
      (* x2 x2 x3 x4)
      (* x1 x1 x2 x4)
      (* x2 x4)
      (* x1 x1 x2 x3)))

(define f4
  '(+ (* x1 x2 x3 x3 x4 x4)
      (* x1 x3 x3 x4 x4)
      (* x1 x4 x4)
      (* x4 x4)
      (* x1 x3 x4)))

(define g4
  '(+ (* x1 x3 x3 x4 x4)
      (* x3 x3 x4 x4)
      (* x4 x4)
      (* x1 x2 x2 x3 x4)
      (* x1 x2 x2)))

(show-time
 (lambda ()
   (gcd-test d4 f4 g4)))
process time: 740 (740 RUN + 0 GC); real time: 734
;Value: #t



(define d5
  '(+ (* x1 x1 x1 x2 x2 x3 x3 x4 x5 x5)
      (* x1 x2 x2 x5 x5)
      (* x1 x1 x1 x3 x4 x4 x5)
      (* x1 x1 x1 x2 x3 x3 x4 x5)
      (* x1 x1 x2 x3 x3 x4 x4)))

(define f5
  '(+ (* x1 x2 x2 x5 x5)
      (* x1 x2 x3 x3 x4 x5)
      (* x1 x2 x3 x3 x4 x4)
      (* x1 x2 x2 x4 x4)
      1))

(define g5
  '(+ (* x1 x3 x3 x4 x5 x5)
      (* x2 x5 x5)
      (* x1 x2 x4 x5)
      (* x2 x5)
      (* x1 x2 x3 x4 x4)))

(gcd-test d5 f5 g5)



(define d4a
  '(+ (* x1 x1 x1 x2 x2 x3 x3 x4 1 1)
      (* x1 x2 x2 1 1)
      (* x1 x1 x1 x3 x4 x4 1)
      (* x1 x1 x1 x2 x3 x3 x4 1)
      (* x1 x1 x2 x3 x3 x4 x4)))

(define f4a
  '(+ (* x1 x2 x2 1 1)
      (* x1 x2 x3 x3 x4 1)
      (* x1 x2 x3 x3 x4 x4)
      (* x1 x2 x2 x4 x4)
      1))

(define g4a
  '(+ (* x1 x3 x3 x4 1 1)
      (* x2 1 1)
      (* x1 x2 x4 1)
      (* x2 1)
      (* x1 x2 x3 x4 x4)))

;;; 11 Dec 2008 Zohar
(show-time
 (lambda ()
   (gcd-test d4a f4a g4a)))
;process time: 30 (30 RUN + 0 GC); real time: 35
;Value: #t


(define d10
  '(+ (* x1 x2 x2 x4 x4 x8 x9 x9 x10 x10)
      (* x2 x2 x4 x5 x5 x6 x7 x9 x10 x10)
      (* x1 x1 x2 x3 x5 x5 x7 x7 x9 x9)
      (* x1 x3 x3 x4 x4 x7 x7 x9 x9)
      (* x1 x1 x3 x4 x7 x7 x8 x8)))

(define f10
  '(+ (* x1 x2 x3 x3 x4 x6 x7 x8 x9 x9 x10 x10)
      (* x2 x2 x3 x3 x4 x4 x6 x6 x9 x10 x10)
      (* x1 x2 x2 x3 x3 x4 x5 x6 x7 x8 x8 x9 x9 x10)
      (* x1 x1 x2 x4 x4 x5 x5 x8 x8 x9 x9 x10)
      (* x3 x4 x4 x5 x6 x7 x7 x9 x10)))

(define g10
  '(+ (* x1 x2 x2 x3 x3 x5 x5 x6 x6 x7 x8 x9 x9 x10 x10)
      (* x3 x8 x9 x9 x10 x10)
      (* x1 x2 x2 x3 x4 x5 x5 x6 x6 x8 x8 x9 x10)
      (* x1 x3 x6 x7 x8 x10)
      (* x4 x4 x5 x5 x6 x6 x7 x9 x9)))

(show-time
 (lambda ()
   (gcd-test d10 f10 g10)))
;;; 11 Dec 2008 Zohar
;process time: 220 (220 RUN + 0 GC); real time: 218
;Value: #t


