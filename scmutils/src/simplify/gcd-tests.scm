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

;;;; Test polynomials
;;;   from Hsin-Chao Liao and Richard J. Fateman
;;;  "Evaluation of the Heuristic Polynomial GCD"

;;; In my tests 
;;;    Euclid  = PrimitivePRS GCD
;;;    Collins = SubresultantPRS GCD
;;;    Sparse  = Sparse Probabilistic Interpolation GCD (not modular)

;;; Collins is always dominated by either Euclid or Sparse, so it is
;;; not part of the new mix.  I am also being paranoid about the
;;; possible failure of sparse.

(define (try n)
  (let* ((f (pcf:expression-> (expression (F n)) (lambda (p v) p)))
	 (g (pcf:expression-> (expression (G n)) (lambda (p v) p)))
	 (ans))
    (display "\n\nEuclid:\n")
    (show-time
     (lambda ()
       (poly/gcd-euclid f g)))
    (display "\n\nSparse:\n")
    (show-time
     (lambda ()
       (set! ans (poly/gcd-sparse f g))))
    (if (or (poly/not-divisible? f ans)
	    (poly/not-divisible? g ans))
	(error "Bad gcd" f g ans)))
  'done)

#|
;;; Old stuff
(define (try n)
  (let* ((f (pcf:expression-> (expression (F n)) (lambda (p v) p)))
	 (g (pcf:expression-> (expression (G n)) (lambda (p v) p)))
	 (fs (pcf->sparse f))
	 (gs (pcf->sparse g))
	 (ans))
    (display "Collins:\n")
    (show-time
     (lambda ()
       (poly/gcd-collins f g)))
    (display "\n\nEuclid:\n")
    (show-time
     (lambda ()
       (poly/gcd-euclid f g)))
    (display "\n\nSparse:\n")
    (show-time
     (lambda ()
       (set! ans (sparse-gcd fs gs))))
    (assert (and (sparse-divisible? fs ans)
		 (sparse-divisible? gs ans)))
    )
  'done)


(define (try-nc n)
  (let* ((f (pcf:expression-> (expression (F n)) (lambda (p v) p)))
	 (g (pcf:expression-> (expression (G n)) (lambda (p v) p)))
	 (fs (pcf->sparse f))
	 (gs (pcf->sparse g))
	 (ans))
    (display "Collins:\n")
    (display ";failed to converge in reasonable time")
    (display "\n\nEuclid:\n")
    (show-time
     (lambda ()
       (poly/gcd-euclid f g)))
    (display "\n\nSparse:\n")
    (show-time
     (lambda ()
       (set! ans (sparse-gcd fs gs))))
    (assert (and (sparse-divisible? fs ans)
		 (sparse-divisible? gs ans)))
    )
  'done)
|#

#|
;;; Case 1: GCD=1

(define (F nu)
  (* (+ 'x (sigma (lambda (i) (symbol 'y i)) 1 nu) 1)
     (+ 'x (sigma (lambda (i) (symbol 'y i)) 1 nu) 2)))


(define (G nu)
  (* (+ (square 'x) (sigma (lambda (i) (symbol 'y i)) 1 nu) 1)
     (+ (* -3 'y1 (square 'x)) (square 'y1) 1)))

(try 4)
Collins: ;process time: 20 (20 RUN + 0 GC); real time: 18

Euclid: ;process time: 130 (130 RUN + 0 GC); real time: 130

Sparse: ;process time: 0 (0 RUN + 0 GC); real time: 1


(try 5)
Collins: ;process time: 60 (60 RUN + 0 GC); real time: 62

Euclid: ;process time: 910 (910 RUN + 0 GC); real time: 902

Sparse: ;process time: 0 (0 RUN + 0 GC); real time: 1


(try 6)
Collins: ;process time: 150 (150 RUN + 0 GC); real time: 145

Euclid: ;process time: 4660 (4660 RUN + 0 GC); real time: 4667

Sparse: ;process time: 0 (0 RUN + 0 GC); real time: 2

;;; Sparse time constant, Collins is power of 3, Euclid not obvious
|#

#|
;;; Case 2: Linearly dense quartic inputs with quadratic gcd

(define (H nu)
  (square (+ 'x (sigma (lambda (i) (symbol 'y i)) 1 nu) 1)))

(define (F nu)
  (* (H nu)
     (square (- 'x (sigma (lambda (i) (symbol 'y i)) 1 nu) 2))))

(define (G nu)
  (* (H nu)
     (square (+ 'x (sigma (lambda (i) (symbol 'y i)) 1 nu) 2))))

(try 5)
Collins: ;process time: 30 (30 RUN + 0 GC); real time: 30

Euclid: ;process time: 10 (10 RUN + 0 GC); real time: 3

Sparse: ;process time: 70 (70 RUN + 0 GC); real time: 71


(try 6)
Collins: ;process time: 60 (60 RUN + 0 GC); real time: 60

Euclid: ;process time: 10 (10 RUN + 0 GC); real time: 3

Sparse: ;process time: 210 (210 RUN + 0 GC); real time: 208


(try 7)
Collins: ;process time: 120 (120 RUN + 0 GC); real time: 125

Euclid: ;process time: 10 (10 RUN + 0 GC); real time: 5

Sparse: ;process time: 610 (610 RUN + 0 GC); real time: 617

;;; Euclid time constant, Collins power of 2, Sparse power of 3
|#

#|
;;; Case 3: Sparse GCD and inputs where degrees are proportional to
;;; the number of variables.

(define (H nu)
  (+ (expt 'x (+ nu 1))
     (sigma (lambda (i)
	      (expt (symbol 'y i) (+ nu 1)))
	    1 nu)
     1))

(define (F nu)
  (* (H nu)
     (- (expt 'x (+ nu 1))
	(sigma (lambda (i)
		 (expt (symbol 'y i) (+ nu 1)))
	       1 nu)
	2)))

(define (G nu)
  (* (H nu)
     (+ (expt 'x (+ nu 1))
	(sigma (lambda (i)
		 (expt (symbol 'y i) (+ nu 1)))
	       1 nu)
	2)))

(try 8)
Collins: ;process time: 0 (0 RUN + 0 GC); real time: 4

Euclid: ;process time: 0 (0 RUN + 0 GC); real time: 1

Sparse: ;process time: 430 (430 RUN + 0 GC); real time: 432


(try 9)
Collins: ;process time: 10 (10 RUN + 0 GC); real time: 7

Euclid: ;process time: 0 (0 RUN + 0 GC); real time: 1

Sparse: ;process time: 850 (820 RUN + 30 GC); real time: 860

(try 10)
Collins: ;process time: 10 (10 RUN + 0 GC); real time: 10

Euclid: ;process time: 0 (0 RUN + 0 GC); real time: 1

Sparse: ;process time: 1590 (1590 RUN + 0 GC); real time: 1591

;;; Euclid, Collins constant, Sparse is power of 2.
|#

#|
;;; Case 3': Alternatively

(define (G nu)
  (* (H nu)
     (+ (expt 'x nu)
	(sigma (lambda (i)
		 (expt (symbol 'y i) nu))
	       1 nu)
	2)))

(try 3)
Collins: ;process time: 40 (40 RUN + 0 GC); real time: 46

Euclid: ;process time: 10 (10 RUN + 0 GC); real time: 3

Sparse: ;process time: 0 (0 RUN + 0 GC); real time: 3


(try 4)
Collins: ;process time: 940 (940 RUN + 0 GC); real time: 945

Euclid: ;process time: 10 (10 RUN + 0 GC); real time: 7

Sparse: ;process time: 10 (10 RUN + 0 GC); real time: 10


(try 5)
Collins: ;process time: 18240 (17750 RUN + 490 GC); real time: 18246

Euclid: ;process time: 100 (100 RUN + 0 GC); real time: 103

Sparse: ;process time: 20 (20 RUN + 0 GC); real time: 26


(try 6)
Collins: ;failed to converge in reasonable time

Euclid: ;process time: 150 (150 RUN + 0 GC); real time: 146

Sparse: ;process time: 70 (70 RUN + 0 GC); real time: 70

;;; Collins factor of 20, Euclid unclear, Sparse unclear
|#

#|
;;; Case 4: Quadratic non-monic GCD.  F and G have other quadratic
;;; factors.

(define (H nu)
  (+ (* (expt 'y1 2) (expt 'x 2)
	(sigma (lambda (i) (expt (symbol 'y i) 2))
	       1 nu)
	1)))

(define (F nu)
  (* (H nu)
     (+ (expt 'x 2)
	(* -1 (expt 'y1 2))
	(sigma (lambda (i) (expt (symbol 'y i) 2))
	       1 nu)
	-1)))

(define (G nu)
  (* (H nu)
     (square (+ (* 'y1 'x)
		(sigma (lambda (i) (symbol 'y i))
		       1 nu)
		2))))

(try 5)
Collins: ;process time: 360 (360 RUN + 0 GC); real time: 357

Euclid: ;process time: 0 (0 RUN + 0 GC); real time: 3

Sparse: ;process time: 10 (10 RUN + 0 GC); real time: 5


(try 6)
Collins: ;process time: 1290 (1260 RUN + 30 GC); real time: 1292

Euclid: ;process time: 0 (0 RUN + 0 GC); real time: 6

Sparse: ;process time: 20 (20 RUN + 0 GC); real time: 9


(try 7)
Collins: ;process time: 3920 (3920 RUN + 0 GC); real time: 3926

Euclid: ;process time: 10 (10 RUN + 0 GC); real time: 9

Sparse: ;process time: 10 (10 RUN + 0 GC); real time: 16

;;; Euclid, Sparse constant, Collins factor of 3.
|#

#|
;;; Case 5: Completely dense non-monic quadratic inputs with dense
;;; non-monic linear GCD.

(define (Pi f lo hi)
  (if (> lo hi)
      1
      (* (f lo) (Pi f (+ lo 1) hi))))

(define (H nu)
  (- (* (+ 'x 1)
	(Pi (lambda (i) (+ (symbol 'y i) 1))
	    1 nu))
     3))

(define (F nu)
  (* (H nu)
     (+ (* (- 'x 2)
	   (Pi (lambda (i) (- (symbol 'y i) 2))
	    1 nu))
	3)))

(define (G nu)
  (* (H nu)
     (- (* (+ 'x 2)
	   (Pi (lambda (i) (+ (symbol 'y i) 2))
	    1 nu))
	3)))

(try 4)
Collins: ;process time: 5440 (5440 RUN + 0 GC); real time: 5433

Euclid: ;process time: 120 (120 RUN + 0 GC); real time: 122

Sparse: ;process time: 140 (140 RUN + 0 GC); real time: 145


(try 5)
Collins: ;failed to converge in reasonable time

Euclid: ;process time: 1120 (1120 RUN + 0 GC); real time: 1119

Sparse: ;process time: 870 (870 RUN + 0 GC); real time: 873

(try 6)
Collins: ;failed to converge in reasonable time

Euclid: ;process time: 11080 (11000 RUN + 80 GC); real time: 11089

Sparse: ;process time: 22560 (22440 RUN + 120 GC); real time: 22554
|#

#|
;;; Case 5': Sparse non-monic quadratic inputs with linear GCDs.

(define (H nu)
  (- (* 'x
	(Pi (lambda (i) (symbol 'y i))
	    1 nu))
     1))

(define (F nu)
  (* (H nu)
     (+ (* 'x
	   (Pi (lambda (i)
		 (symbol 'y i))
	    1 nu))
	3)))

(define (G nu)
  (* (H nu)
     (- (* 'x
	   (Pi (lambda (i)
		 (symbol 'y i))
	    1 nu))
	3)))

(try 5)
Collins: ;process time: 0 (0 RUN + 0 GC); real time: 0

Euclid: ;process time: 0 (0 RUN + 0 GC); real time: 1

Sparse: ;process time: 0 (0 RUN + 0 GC); real time: 0


(try 100)
Collins: ;process time: 250 (250 RUN + 0 GC); real time: 248

Euclid: ;process time: 250 (250 RUN + 0 GC); real time: 253

Sparse: ;process time: 90 (90 RUN + 0 GC); real time: 94
|#

#|
;;; Case 6: Trivariate inputs with increasing degrees.

(define (H j)
  (* (expt 'x j) 'y (- 'z 1)))

(define (F j)
  (* (H j)
     (+ (expt 'x j)
	(* (expt 'y (+ j 1))
	   (expt 'z j))
	1)))

(define (G j)
  (* (H j)
     (+ (expt 'x (+ j 1))
	(* (expt 'y j)
	   (expt 'z (+ j 1)))
	-7)))

(try 10)
Collins: ;process time: 10 (10 RUN + 0 GC); real time: 4

Euclid: ;process time: 0 (0 RUN + 0 GC); real time: 0

Sparse: ;process time: 0 (0 RUN + 0 GC); real time: 2


(try 30)
Collins: ;process time: 280 (280 RUN + 0 GC); real time: 282

Euclid: ;process time: 0 (0 RUN + 0 GC); real time: 4

Sparse: ;process time: 60 (60 RUN + 0 GC); real time: 57
|#

#|
;;; Case 7: Trivariate polynomials whose GCD has common factors with
;;; it cofactors

(define P (+ 'x (* -1 'y 'z) 1))

(define Q (+ 'x (* -1 'y) (* 3 'z)))

(define (H j)
  (* (expt P j) (expt Q j)))

(define (F j k)
  (* (expt P j) (expt Q k)))

(define (G j k)
  (* (expt P k) (expt Q j)))

(define (try j k)
  (let* ((f (pcf:expression-> (expression (F j k)) (lambda (p v) p)))
	 (g (pcf:expression-> (expression (G j k)) (lambda (p v) p)))
	 (ans))
#|
    (display "Collins:\n")
    (show-time
     (lambda ()
       (poly/gcd-collins f g)))
|#    (display "\n\nEuclid:\n")
    (show-time
     (lambda ()
       (poly/gcd-euclid f g)))
    (display "\n\nSparse:\n")
    (show-time
     (lambda ()
       (set! ans (poly/gcd-sparse f g))))
    (if (or (poly/not-divisible? f ans)
	    (poly/not-divisible? g ans))
	(error "Bad gcd" f g ans))
    )
  'done)

(try 1 4)
Collins: ;process time: 220 (220 RUN + 0 GC); real time: 223

Euclid: ;process time: 0 (0 RUN + 0 GC); real time: 4

Sparse: ;process time: 0 (0 RUN + 0 GC); real time: 3


(try 2 4)
Collins: ;process time: 20 (20 RUN + 0 GC); real time: 26

Euclid: ;process time: 10 (10 RUN + 0 GC); real time: 5

Sparse: ;process time: 30 (30 RUN + 0 GC); real time: 30


(try 3 4)
Collins: ;process time: 10 (10 RUN + 0 GC); real time: 4

Euclid: ;process time: 0 (0 RUN + 0 GC); real time: 3

Sparse: ;process time: 1010 (1010 RUN + 0 GC); real time: 1005


(try 4 5)
Collins: ;process time: 10 (10 RUN + 0 GC); real time: 9

Euclid: ;process time: 0 (0 RUN + 0 GC); real time: 5

Sparse: ;process time: 47510 (47300 RUN + 210 GC); real time: 47514
|#
