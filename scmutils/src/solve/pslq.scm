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

;;;; PSLQ algorithm 
;;;   for finding integer linear combinations among given numerical
;;;   constants,  discovered by mathematician and sculptor Helaman
;;;   Ferguson of Maryland's Center for Computing Sciences in 1992.

;;;   H. R. P. Ferguson and D. H. Bailey, ``A Polynomial Time,
;;;   Numerically Stable Integer Relation Algorithm,'' RNR Technical
;;;   Report RNR-91-032, NASA Ames Research Center, MS T045-1, Moffett
;;;   Field, CA 94035-1000.

;;; Algorithm obtained from "Recognizing Numerical Constants" by 
;;; David H. Bailey and Simon Plouffe.  Coded by GJS on 16 May 2006.  

(define (pslq xs #!optional integer-limit threshold gamma nint)
  (if (default-object? integer-limit) (set! integer-limit 1e10))
  (if (default-object? threshold) (set! threshold 1e-15))
  (if (default-object? gamma) (set! gamma (sqrt (/ 4 3))))
  (if (default-object? nint) (set! nint round->exact))
  (let* ((x (if (list? xs) (list->vector xs) xs))
	 (n (vector-length x))
	 (n-1 (fix:- n 1))
	 (A (generate-array n n (lambda (i j) (if (fix:= i j) 1 0))))
	 (B (generate-array n n (lambda (i j) (if (fix:= i j) 1 0))))
	 (s (v:generate n
			(lambda (k)
			  (sqrt (sigma (lambda (j)
					 (square (vector-ref x j)))
				       k
				       n-1)))))
	 (s_1 (vector-ref s 0))
	 (y (v:generate n (lambda (k) (/ (vector-ref x k) s_1))))
	 (s (v:generate n (lambda (k) (/ (vector-ref s k) s_1))))
	 (H (generate-array n n-1 (lambda (i j) 'foo)))
	 (block
	  (lambda (i)
	    (lambda (j)
	      (let ((t (nint (/ (array-ref H i j) (array-ref H j j)))))
		(vector-set! y j
			     (+ (vector-ref y j)
				(* t (vector-ref y i))))
		(for 0 (lambda (k) (fix:<= k j)) fix:1+
		     (lambda (k)
		       (array-set! H i k
				   (- (array-ref H i k)
				      (* t (array-ref H j k))))))
		(for 0 (less-than n) fix:1+
		     (lambda (k)
		       (array-set! A i k
				   (- (array-ref A i k)
				      (* t (array-ref A j k))))
		       (array-set! B k j
				   (+ (array-ref B k j)
				      (* t (array-ref B k i))))))))))
	 (cratio
	  (lambda (v)
	    (let ((lv (map magnitude (vector->list v))))
	      (/ (apply min lv) (apply max lv)))))
	 (out
	  (lambda (v)
	    (if (list? xs) (vector->list v) v))))
    (for 0 (less-than n) fix:1+
	 (lambda (i)
	   (for (fix:+ i 1) (less-than n-1) fix:1+
		(lambda (j) (array-set! H i j 0)))
	   (if (fix:< i n-1)
	       (array-set! H i i
			   (/ (vector-ref s (fix:+ i 1))
			      (vector-ref s i))))
	   (for 0 (less-than i) fix:1+
		(lambda (j)
		  (array-set! H i j
			      (- (/ (* (vector-ref y i)
				       (vector-ref y j))
				    (* (vector-ref s j)
				       (vector-ref s (fix:+ j 1))))))))))

    (for 1 (less-than n) fix:1+
	 (lambda (i)
	   (for (fix:- i 1)
		(lambda (j) (fix:>= j 0))
		(lambda (j) (fix:- j 1))
		(block i))))
    (let lp ()
      (let* ((m
	      (index-of-max 0 n-1
			    (lambda (i)
			      (* (expt gamma i)
				 (magnitude (array-ref H i i))))))
	     (m+1 (fix:+ m 1)))
	(interchange-rows! y m m+1)
	(interchange-rows! A m m+1)
	(interchange-rows! H m m+1)
	(interchange-cols! B m m+1)
	(if (fix:< m (fix:- n 2))
	    (let* ((t0
		    (sqrt (+ (square (array-ref H m m))
			     (square (array-ref H m m+1)))))
		   (t1 (/ (array-ref H m m) t0))
		   (t2 (/ (array-ref H m m+1) t0)))
	      (for m (less-than n) fix:1+
		   (lambda (i)
		     (let ((t3 (array-ref H i m))
			   (t4 (array-ref H i m+1)))
		       (array-set! H i m (+ (* t1 t3) (* t2 t4)))
		       (array-set! H i m+1 (- (* t1 t4) (* t2 t3))))))))
	(for m+1 (less-than n) fix:1+
	     (lambda (i)
	       (for (min (fix:- i 1) m+1) 
		    (lambda (j) (fix:>= j 0))
		    (lambda (j) (fix:- j 1))
		    (block i))))
	(let ((M
	       (/ 1
		  (apply max
			 (map euclidean-norm
			      (vector->list H)))))
	      (Amax
	       (apply max
		      (map (lambda (row)
			     (apply max
				    (map magnitude
					 (vector->list row))))
			   (vector->list A))))
	      (i-ymin
	       (index-of-min 0 n (lambda (i) (vector-ref y i)))))
	  (cond ((> (magnitude Amax) integer-limit)
		 `(exhausted ,M ,(cratio y) ,(out (nth-col B i-ymin))))
		((< (magnitude (vector-ref y i-ymin)) threshold)
		 `(relation ,M  ,(cratio y) ,(out (nth-col B i-ymin))))
		(else (lp))))))))

;;; The following two procedures are also in bulirsch-stoer.scm

(define (for initial test increment to-do)
  (let loop ((x initial))
    (if (test x)
	(begin (to-do x)
	       (loop (increment x))))))

(define (less-than n)
  (lambda (i)
    (fix:< i n)))


;;; The following should be in iterat.scm

(define (interchange-rows! v m n)
  (let ((t (vector-ref v m)))
    (vector-set! v m (vector-ref v n))
    (vector-set! v n t)))

(define (interchange-cols! a m n)
  (for 0 (less-than (num-rows a)) fix:1+
       (lambda (i)
	 (let ((t (array-ref a i m)))
	   (array-set! a i m (array-ref a i n))
	   (array-set! a i n t)))))

(define (index-of-max low high f)	; low <= i < high
  (let lp ((i (fix:+ low 1)) (imax low) (fmax (magnitude (f low))))
    (if (fix:= i high)
	imax
	(let ((i+1 (fix:+ i 1)) (fi (magnitude (f i))))
	  (if (> fi fmax)
	      (lp i+1 i fi)
	      (lp i+1 imax fmax))))))

(define (index-of-min low high f)	; low <= i < high
  (let lp ((i (fix:+ low 1)) (imin low) (fmin (magnitude (f low))))
    (if (fix:= i high)
	imin
	(let ((i+1 (fix:+ i 1)) (fi (magnitude (f i))))
	  (if (< fi fmin)
	      (lp i+1 i fi)
	      (lp i+1 imin fmin))))))

#|
(let ((v (vector 1 2 3 4 3 2 1)))
  (index-of-max 0 (vector-length v)
		(lambda (i)
		  (vector-ref v i))))
;Value: 3
|#

#|
;;; For example

;;; Additive
(pp (pslq (list 2 3 5 7)))
(relation 1.184578324321196 5.177735492072799e-16 (-2 -1 0 1))
;;; -2*2 -1*3 + 0*5 + 1*7 = 0

;;; Multiplicative
(pp (pslq (map log (list 14/20 2 3 5 7 11 13 17 19 23))))
(relation 1.020041872300852 2.4106956219418228e-14 (-1 -1 0 -1 1 0 0 0 0 0))
;;; 20/14 * 1/2 * 1/5 * 7 = 1

(pp (pslq (map log (list 16/20 2 3 5 7 11 13 17 19 23))))
(relation 1.493061061669929 2.07772215758565e-13 (-1 2 0 -1 0 0 0 0 0 0))
;;; 20/16 * 2^2 * 1/5 = 1

;;; More complicated, reconstructing a sum of "reals":
(define foo
  (+ (* 3 pi) (* 2 pi pi) (sqrt 5) (exp 1)))

(define X (list pi (expt pi 2) (sqrt 5) (exp 1) (- foo)))

(pp (pslq X))
(relation 3.275277109516825 2.510269140274375e-14 (3 2 1 1 1))


;;; Another example -- multiplicative.

(define foo (u:value (/ :e (* :k 300))))
(define constants (map u:value (list 2 3 5 7 :e :k)))

(define X (map log (cons foo constants)))

(pp (pslq X))
(relation 2.7524838065594364 1.241962711073869e-12 (-1 -2 -1 -2 0 1 -1))

;;; Note that 2^2*3*5^2 = 300


;;; Golden ratio is not a combination of powers of small primes!

(pp (pslq (map log (list :phi 2 3 5 7))))
(exhausted 203.86503449466494 .04036045258941379 (545 256 -275 33 -98))

(pp (pslq (map log (list :phi 2 3 5 7 11 13 17))))
(exhausted 16.21613670112226 .05280532161214609 (15 46 14 -7 40 5 -11 -37))

(pp (pslq (map log (list :phi 2 3 5 7 11 13 17 19 23))))
(exhausted 4.25285519002329
           1.5593067030632273e-2
           (-1 -21 10 5 14 -3 2 -10 -12 11))

;;; But... tee hee!
(apply *
       (map expt
	    (list :phi 2 3 5 7 11 13 17 19 23)
	    (list -1 -21 10 5 14 -3 2 -10 -12 11)))
;Value: .9999999999966921
|#

#|
;;; To find the minimal polynomial with the given root:

(define x (+ (sqrt 3) (sqrt 2)))

(pp (pslq (list 1 x (expt x 2))))
(exhausted 70279.8021842838 .26471112930545593 (-198997 138888 -24041))

(pp (pslq (list 1 x (expt x 2) (expt x 3))))
(exhausted 1336.1415192642594 .3174289756822062 (-1340 2946 -27 -246))

(pslq (list 1 x (expt x 2) (expt x 3) (expt x 4)))
(relation 4.812399236375176 1.5281630476267974e-12 (1 0 -10 0 1))

(pslq (list 1 x (expt x 2) (expt x 3) (expt x 4) (expt x 5)))
(relation 4.323732566781159 1.865972649320703e-12 (0 1 0 -10 0 1))

(+ 5 (* 2 (sqrt 6)))
;Value: 9.898979485566356

(square x)
;Value: 9.898979485566358
|#