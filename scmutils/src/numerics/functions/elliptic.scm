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

;;; Elliptic functions from Press

(declare (usual-integrations))


(define Carlson-elliptic-1
  (let ((eps (expt *machine-epsilon* 1/6))
	(C1 (/ 1. 24.))
	(C2 0.1)
	(C3 (/ 3. 44.))
	(C4 (/ 1. 14.)))
    (define (rf x y z)
      (let loop ((x x) (y y) (z z))
	(let ((sqrtx (sqrt x))
	      (sqrty (sqrt y))
	      (sqrtz (sqrt z)))
	  (let ((alamb (+ (* sqrtx (+ sqrty sqrtz)) (* sqrty sqrtz))))
	    (let ((xp (* 0.25 (+ x alamb)))
		  (yp (* 0.25 (+ y alamb)))
		  (zp (* 0.25 (+ z alamb))))
	      (let ((ave (/ (+ xp yp zp) 3.)))
		(let ((delx (/ (- ave xp) ave))
		      (dely (/ (- ave yp) ave))
		      (delz (/ (- ave zp) ave)))
		  (let ((error (max (abs delx) (abs dely) (abs delz))))
		    (if (> error eps)
			(loop xp yp zp)
			(let ((e2 (- (* delx dely) (* delz delz)))
			      (e3 (* delx dely delz)))
			  (/ (+ 1.
				(* (- (* C1 e2)
				      (+ C2 (* C3 e3))) e2)
				(* C4 e3))
			     (sqrt ave))))))))))))
    rf))
		      

	
(define Carlson-elliptic-1-simple
  (let ((eps (sqrt *machine-epsilon*)))
    (define (rf1 x y z)
      (let ((av (/ (+ x y z) 3.0)))
	(if (< (max (abs (/ (- x av) av))
		    (abs (/ (- y av) av))
		    (abs (/ (- z av) av)))
	       eps)		
	    (/ 1.0 (sqrt av))
	    (let ((lamb
		   (+ (sqrt (* x y))
		      (sqrt (* x z))
		      (sqrt (* y z)))))
	      (rf1 (/ (+ x lamb) 4.0)
		   (/ (+ y lamb) 4.0)
		   (/ (+ z lamb) 4.0))))))
    rf1))



(define Carlson-elliptic-2
  (let* ((eps (sqrt *machine-epsilon*))
	 (C1 (/ -3. 14.)) ; opposite Press
	 (C2 (/ 1. 6.))
	 (C3 (/ -9. 22.)) ; opposite Press
	 (C4 (/ 3. 26.))
	 (C5 (* 0.25 C3))
	 (C6 (* -1.5 C4)) ; opposite Press
	 )
    (define (rd x y z)
      (let loop ((x x) (y y) (z z) (sum 0.) (fac 1.))
	(let ((sqrtx (sqrt x))
	      (sqrty (sqrt y))
	      (sqrtz (sqrt z)))
	  (let ((alamb (+ (* sqrtx (+ sqrty sqrtz)) (* sqrty sqrtz))))
	    (let ((sump (+ sum (/ fac (* sqrtz (+ z alamb)))))
		  (facp (* 0.25 fac)))
	      (let ((xp (* 0.25 (+ x alamb)))
		    (yp (* 0.25 (+ y alamb)))
		    (zp (* 0.25 (+ z alamb))))
		(let ((ave (* 0.2 (+ xp yp (* 3. zp)))))
		  (let ((delx (/ (- ave xp) ave))
			(dely (/ (- ave yp) ave))
			(delz (/ (- ave zp) ave)))
		    (if (> (max (abs delx) (abs dely) (abs delz)) eps)
			(loop xp yp zp sump facp)
			(let* ((ea (* delx dely))
			       (eb (* delz delz))
			       (ec (- ea eb))
			       (ed (- ea (* 6. eb)))
			       (ee (+ ed ec ec)))
			  (+ (* 3. sump)
			     (/ (* facp 
				   (+ (+ 1. (* ed (+ C1 (* C5 ed) (* C6 delz ee))))
				      (* delz (+ (* C2 ee) (* delz (+ (* C3 ec) (* delz C4 ea)))))))
				(* ave (sqrt ave))))))))))))))
    rd))


(define (elliptic-integral-F phi k)
  (let ((s (sin phi)))
    (* s (Carlson-elliptic-1 (square (cos phi))
			     (* (- 1. (* s k)) (+ 1. (* s k)))
			     1.))))

(define (complete-elliptic-integral-K k)
  (elliptic-integral-F pi/2 k))

(define (elliptic-integral-E phi k)
  (let ((s (sin phi))
	(cc (square (cos phi))))
    (let ((q (* (- 1. (* s k)) (+ 1. (* s k)))))
      (* s (- (Carlson-elliptic-1 cc q 1.)
	      (* (square (* s k))
		 (/ (Carlson-elliptic-2 cc q 1.) 3.)))))))

(define (complete-elliptic-integral-E k)
  (elliptic-integral-E pi/2 k))

;;; older definition of the complete elliptic integrals
;;; probably from A&Stegun

(define (elliptic-integrals k continue) ; (continue K E)
  (if (= k 1) 
      (continue :+infinity 1.)
      (let loop ((a 1.0) 
		 (b (sqrt (- 1.0 (square k))))
		 (c k)
		 (d 0.0)
		 (powers-2 1.0))
	(if (< (abs c) *machine-epsilon*)
	    (let ((first-elliptic-integral (/ pi/2 a)))
	      (continue first-elliptic-integral
			(* first-elliptic-integral 
			   (- 1.0 (/ d 2.0)))))
	    (loop (/ (+ a b) 2.0)
		  (sqrt (* a b))
		  (/ (- a b) 2.0) 
		  (+ d (* (square c) powers-2))
		  (* powers-2 2.0))))))

;;; K
(define (first-elliptic-integral k)
  (elliptic-integrals k (lambda (K E) K)))

;;; E
(define (second-elliptic-integral k)
  (elliptic-integrals k (lambda (K E) E)))

(define (first-elliptic-integral&derivative k cont) ; (cont K dK/dk)
  (if (= k 0.0)
      (cons pi/2 0.0)
      (elliptic-integrals k
         (lambda (Kk Ek)
	   (cont Kk
		 (/ (- (/ Ek (- 1 (* k k))) Kk) k))))))

#|

(define (elliptic-integral-F-check phi k)
  (definite-integral 
    (lambda (theta) (/ 1. (sqrt (- 1. (square (* k (sin theta)))))))
    0. phi 1.e-13))

;(elliptic-integral-F-check 1. .9)
;Value: 1.159661070732225


; (elliptic-integral-F 1. .9)
;Value: 1.159661070732199
; (elliptic-integral-F pi/2 .9)
;Value: 2.2805491384227703
;(first-elliptic-integral .9)
;Value: 2.2805491384227703

(define (elliptic-integral-E-check phi k)
  (definite-integral 
    (lambda (theta) (sqrt (- 1. (square (* k (sin theta))))))
    0. phi 1.e-13))

(elliptic-integral-E 1. .9)
;Value: .8762622199915486
(elliptic-integral-E-check 1. .9)
;Value: .876262219991453

(complete-elliptic-integral-E .9)
;Value: 1.1716970527816144
(second-elliptic-integral .9)
;Value: 1.171697052781614

|#

;;; sn cn dn
;;; lisptran of Press
(define Jacobi-elliptic-functions
  (let ((eps (sqrt *machine-epsilon*)))
    (lambda (uu k cont)
      ;; (cont sn cn dn)
      (let ((emc (- 1. (square k)))
	    (u uu)
	    (d 1.0))
	(if (= emc 0.) 
	    (let ((cn (/ 1. (cosh u))))
	      (cont (tanh u) cn cn))
	    (let ((bo (< emc 0.)))
	      (if bo 
		  (begin
		    (set! d (- 1. emc))
		    (set! emc (- (/ emc d)))
		    (set! d (sqrt d))
		    (set! u (* u d))))
	      (let ((dn 1.))
		(let loop ((a 1.) (emc emc) (i 1) (em '()) (en '()))
		  (let ((emc (sqrt emc)))
		    (let ((c (* 0.5 (+ a emc))))
		      (if (and (> (abs (- a emc)) (* eps a))
			       (fix:< i 13))
			  (loop c (* a emc) (fix:+ i 1) (cons a em) (cons emc en))
			  ;; label 1
			  (let* ((u (* c u))
				 (sn (sin u))
				 (cn (cos u)))
			    (if (not (= sn 0.)) 
				(let* ((a (/ cn sn))
				       (c (* a c)))
				  (let loop2 ((em em) (en en))
				    (if (and (not (null? em)) (not (null? en)))
					(let ((b (car em)))
					  (set! a (* c a))
					  (set! c (* dn c))
					  (set! dn (/ (+ (car en) a) (+ a b)))
					  (set! a (/ c b))
					  (loop2 (cdr em) (cdr en)))
					(let ((a (/ 1. (sqrt (+ 1. (square c))))))
					  (if (< sn 0.)
					      (set! sn (- a))
					      (set! sn a))
					  (set! cn (* c sn)))))))
			    (if bo 
				(cont (/ sn d) a cn)
				(cont sn cn dn))))))))))))))
				      

#|

(Jacobi-elliptic-functions 1.3369113189159216 0. list)
;Value 12: (.9727733548546672 .231758495172875 1.)
(sin 1.3369113189159216)
;Value: .9727733548546673

(elliptic-integral-F 1.1 .92)
;Value: 1.3369113189159216
(Jacobi-elliptic-functions 1.3369113189159216 0.92 list)
;Value 13: (.8912073600614343 .45359612142557926 .5724913337139167)
(sin 1.1)
;Value: .8912073600614354

(+ (square .8912073600614343) (square .45359612142557926))
;Value: .9999999999999999 ok cn

(sqrt (- 1 (* (square 0.92) (square .8912073600614343))))
;Value: .5724913337139168 ok dn

(define (check-elliptic-1 phi k)
  (let ((u (elliptic-integral-F phi k)))
    (Jacobi-elliptic-functions 
     u k 
     (lambda (sn cn dc)
       (- phi (asin sn))))))

;;; seems to work for -pi/2 < phi < pi/2 and k<1 
;;; why does the Jacobi program have a branch for k>1?

;;; (elliptic-integral-F (* 40 (/ pi 180)) (sin (* 50 (/ pi 180))))
;;; (elliptic-integral-E (* 40 (/ pi 180)) (sin (* 50 (/ pi 180))))


|#