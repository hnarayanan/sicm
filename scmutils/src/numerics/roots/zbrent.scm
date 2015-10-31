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

;;;; Finds roots of function f in domain from x1 to x2.
;;;   From Numerical Recipes, 2nd Edition, Press, et.al.

(declare (usual-integrations))

;;; Given a real continuous function f of one real variable, two
;;; distinct argument values x1 and x2 satisfying f(x1) X f(x2) <= 0,
;;; a positive tolerance tol, and a maximum number of iterations,
;;; itmax.

;;; To produce a list.  The first element is true if the algorithm
;;; converged to a root, the second is the root, and the third is the
;;; number of iterations required.  The number of function evaluations
;;; is always two more than the number of iterations.

(define (zbrent f x1 x2 #!optional tol itmax)
  (if (default-object? tol) (set! tol 0))
  (if (default-object? itmax) (set! itmax 100))
  (let ((fa (f x1)) (fb (f x2)))
    (if (or (and (> fa 0) (> fb 0)) (and (< fa 0) (< fb 0)))
	(error "Root must be bracketed in zbrent" x1 x2))
    (let lp ((iter 0) (a x1) (fa fa)
		      (b x2) (fb fb)
		      (c x1)  (fc fa)
		      (d (- x2 x1)) (e (- x2 x1)))
      (cond ((fix:= iter itmax)
	     (list #f b iter))
	    ((or (and (> fb 0) (> fc 0))
		 (and (< fb 0) (< fc 0)))
	     (let ((u (- b a)))
	       (lp iter a fa b fb a fa u u)))
	    ((< (abs fc) (abs fb))
	     (lp iter b fb c fc a fa d e))
	    (else
	     (let ((tol1
		    (+ (* *machine-epsilon* (abs b))
		       (/ tol 2.0)))
		   (xm (/ (- c b) 2.0)))
	       (define (next1 p q)
		 (let ((p (abs p))
		       (q (if (> p 0.0) (- q) q)))
		   (let ((min1 (- (* 3.0 xm q) (abs (* tol1 q))))
			 (min2 (abs (* e q))))
		     (if (< (* 2.0 p) (min min1 min2))
			 (next2 d (/ p q))
			 (next2 d xm)))))
	       (define (next2 e d)
		 (let ((nb
			(if (> (abs d) tol1)
			    (+ b d)
			    (+ b
			       (if (> xm 0.0)
				   (abs tol1)
				   (- (abs tol1)))))))
		   (lp (fix:+ iter 1) b fb nb (f nb) c fc d e)))
	       (cond ((or (<= (abs xm) tol1) (= fb 0.0))
		      (list #t b iter))
		     ((and (>= (abs e) tol1) (> (abs fa) (abs fb)))
		      (let ((s (/ fb fa)))
			(if (= a c)
			    (next1 (* 2.0 xm s) (- 1.0 s))
			    (let ((q (/ fa fc)) (r (/ fb fc)))
			      (next1 (* s
					(- (* 2.0 xm q (- q r))
					   (* (- b a) (- r 1.0))))
				     (* (- q 1.0)
					(- r 1.0)
					(- s 1.0)))))))
		     (else
		      (next2 d xm)))))))))

#|
(define *machine-epsilon*
  (let loop ((e 1.0))
     (if (= 1.0 (+ e 1.0))
         (* 2 e)
         (loop (/ e 2)))))
|#

#|
(begin
  (define numval 0)
  (let ((v
	 (zbrent (lambda (x)
		   (set! numval (+ numval 1))
		   (- (sin x) 0.5))
		 0. 1.5
		 1e-15
		 100)))
    (append v (list numval))))
;Value 10: (#t .5235987755982988 8 10)

(begin
  (define n 3)
  (define numval 0)
  (let ((v
	 (zbrent (lambda (x)
		   (set! numval (+ numval 1))
		   (+ (* 2 x (exp (- n))) 1 (* -2 (exp (* -1 n x)))))
		 0. 1.
		 1e-15
		 100)))
    (append v (list numval))))
;Value 11: (#t .22370545765466293 7 9)

(begin
  (define n 10)
  (define numval 0)
  (let ((v
	 (zbrent (lambda (x)
		   (set! numval (+ numval 1))
		   (- (* (+ 1 (expt (- 1 n) 2)) x) (expt (- 1 (* n x)) 2)))
		 0. 1.
		 1e-15
		 100)))
    (append v (list numval))))
;Value 12: (#t 9.900009998000501e-3 8 10)

(begin
  (define n 5)
  (define numval 0)
  (let ((v
	 (zbrent (lambda (x)
		   (set! numval (+ numval 1))
		   (- (* x x) (expt (- 1 x) n)))
		 0. 1.
		 1e-15
		 100)))
    (append v (list numval))))
;Value 14: (#t .34595481584824217 7 9)


(begin
  (define n 19)
  (define a 0)
  (define b 1e-4)
  (define numval 0)
  (let ((v
	 (zbrent (lambda (x)
		   (set! numval (+ numval 1))
		   (+ (expt x n) (* a x) b))
		 -3. 5.
		 1e-15
		 100)))
    (append v (list numval))))
;Value 15: (#t -.6158482110660264 23 25)


(begin
  (define n 3)
  (define numval 0)
  (let ((v
	 (zbrent (lambda (x)
		   (set! numval (+ numval 1))
		   (expt x n))
		 -1. 10.
		 1e-15
		 200)))
    (append v (list numval))))
;Value 17: (#t -1.4076287793739637e-16 158 160)


(begin
  (define n 9)
  (define numval 0)
  (let ((v
	 (zbrent (lambda (x)
		   (set! numval (+ numval 1))
		   (expt x n))
		 -1. 10.
		 1e-15
		 200)))
    (append v (list numval))))
;Value 18: (#t -1.1555192900497495e-17 147 149)

(begin
  (define n 19)
  (define numval 0)
  (let ((v
	 (zbrent (lambda (x)
		   (set! numval (+ numval 1))
		   (expt x n))
		 -1. 10.
		 1e-15
		 200)))
    (append v (list numval))))
;Value 19: (#t 1.4548841231758658e-16 152 154)


(define (kepler ecc m)
  (define 2pi (* 8 (atan 1 1)))
  (zbrent
   (lambda (e)
     (write-line e)
     (- e (* ecc (sin e)) m))
   0.0
   2pi
   1e-15
   100))
;Value: kepler

(kepler .99 .01)
6.283185307179586
0.
9.999999999999998e-3
.9967954452700871
.06907877394105355
.5329371096055704
.2160603559873964
.4415169261644992
.3111033353177872
.3466110470491019
.3419230916206179
.34226662531904334
.34227031654601936
.34227031649177464
.3422703164917755
;Value 20: (#t .3422703164917755 13)
|#