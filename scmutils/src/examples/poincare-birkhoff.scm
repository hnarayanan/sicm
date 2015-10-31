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

;;;; Poincare-Birkhoff construction of fixed points

;;; use driven pendulum as system

(define (explore-dp-aa win ml^2 mlg mlAw^2 omega Imax)
  (plot-line win -pi/2 (pendulum-separatrix-action ml^2 mlg) -pi/2 Imax)
  (plot-line win pi/2 (pendulum-separatrix-action ml^2 mlg)  pi/2 Imax)
  (explore-map win
	       (dp-aa-map ml^2 mlg mlAw^2 omega)
	       100))

#|
;;; figure 3.11
(define win (frame -pi pi -20. 20.))
(graphics-close win)
(graphics-clear win)
(let ((m 1) (l 1) (g 9.8) 
      (omega/omega0 4.2)
      (A 0.05))
  (let* ((ml^2 (* m l l))
	 (mlg (* m l g))
	 (omega0 (sqrt (/ g l)))
	 (omega (* omega/omega0 omega0))
	 (mlAw^2 (* m l A omega omega)))
    (graphics-clear win)
    (explore-map win 
		 (dp-map ml^2 mlg mlAw^2 omega) 
		 100)))

;;; figure 3.11 in global pendulum action-angle
(define win (frame -pi pi 0. 40.))
(graphics-close win)
(graphics-clear win)
(let ((m 1) (l 1) (g 9.8) 
      (omega/omega0 4.2)
      (A 0.05))
  (let* ((ml^2 (* m l l))
	 (mlg (* m l g))
	 (omega0 (sqrt (/ g l)))
	 (omega (* omega/omega0 omega0))
	 (mlAw^2 (* m l A omega omega)))
    (graphics-clear win)
    (explore-dp-aa win ml^2 mlg mlAw^2 omega 40.)))

;;; figure 3.12 in global pendulum action-angle
(define win (frame -pi pi 0. 20.))
(graphics-close win)
(graphics-clear win)
(let ((m 1) (l 1) (g 9.8) 
      (omega/omega0 2.0)
      (A 0.1))
  (let* ((ml^2 (* m l l))
	 (mlg (* m l g))
	 (omega0 (sqrt (/ g l)))
	 (omega (* omega/omega0 omega0))
	 (mlAw^2 (* m l A omega omega)))
    (graphics-clear win)
    (explore-dp-aa win ml^2 mlg mlAw^2 omega 20.)))

;;; small A case
(define win (frame -pi pi 0. 20.))
(graphics-clear win)
(graphics-close win)
(let ((m 1) (l 1) (g 9.8) 
      (omega/omega0 4.2)
      (A 0.01))
  (let* ((ml^2 (* m l l))
	 (mlg (* m l g))
	 (omega0 (sqrt (/ g l)))
	 (omega (* omega/omega0 omega0))
	 (mlAw^2 (* m l A omega omega)))
    (graphics-clear win)
    (explore-dp-aa win ml^2 mlg mlAw^2 omega 20.)))
|#

;;;----------------------------------------------------------------
;;; define the six-fold iterated map
#|
(define ((iterated-map map n) x y continue failure)
  (if (fix:< n 0) (error "iterated-map: cannot invert map"))
  (let loop ((x x) (y y) (i n))
    (if (fix:= i 0) 
	(continue x y)
	(map x y
	     (lambda (nx ny)
	       (loop nx ny (fix:- i 1)))
	     (lambda () (error "iterated map failure"))))))
|#

#|
;;; plot the twist

(plot-parametric win2 
		 (lambda (J) 
		   (cons -1. J))
		 0.01 7.9 .05)
(plot-parametric win2 
		 (lambda (J) 
		   ((dp-aa-map ml^2 mlg Aw^2/g omega)
		    -1. J cons list))
		 0.01 (pendulum-separatrix-action 1. 9.8)
		 .001)

(let ((T^6 (iterated-map (dp-aa-map ml^2 mlg Aw^2/g omega) 6)))
  (plot-parametric win2 
		   (lambda (J) 
		     (T^6 -1. J cons list))
		 0.01 (pendulum-separatrix-action 1. 9.8) .001))
|#

;;;----------------------------------------------------------------

(define (radially-mapping-points map pv Jmin Jmax phi eps)
  (bisect (lambda (J)
	    (pv
	     (- phi
		(map phi J (lambda (phip Jp) phip) list))))
	  Jmin Jmax eps))

#|
(radially-mapping-points 
 (iterated-map (dp-aa-map ml^2 mlg Aw^2/g omega) 6)
 (principal-value pi)
 5. 6. -1. 1e-8)
;Value: 5.64281507928981

(define win2 (frame -pi pi 4.5 7.0))
(graphics-clear win2)
(graphics-close win2)
(do-it win2 4.2 .01 20.)

;;; pb-5.ps
(let ((m 1) (l 1) (g 9.8) 
      (omega/omega0 4.2)
      (A 0.01))
  (let* ((ml^2 (* m l l))
	 (mlg (* m l g))
	 (omega0 (sqrt (/ g l)))
	 (omega (* omega/omega0 omega0))
	 (mlAw^2 (* m l A omega omega)))
    (let ((T^6 (iterated-map (dp-aa-map ml^2 mlg mlAw^2 omega) 6)))
      (plot-function 
       win2 
       (lambda (phi)
	 (let ((J0 (radially-mapping-points 
		    T^6
		    (principal-value pi)
		    5. 6.5 phi 1e-8)))
	   (let ((J1 (T^6 phi J0 (lambda (phi J) J) list)))
	     (plot-point win2 phi J0)
	     J1)))
       -pi pi .02))))

(define win (frame -pi pi -20 20))
(graphics-clear win)
(graphics-close win)
(let ((m 1) (l 1) (g 9.8) 
      (omega/omega0 4.2)
      (A 0.01))
  (let* ((ml^2 (* m l l))
	 (mlg (* m l g))
	 (omega0 (sqrt (/ g l)))
	 (omega (* omega/omega0 omega0))
	 (mlAw^2 (* m l A omega omega)))
    (explore-map win (dp-map ml^2 mlg mlAw^2 omega) 100)))

(let ((T^2 (iterated-map (dp-aa-map ml^2 mlg Aw^2/g omega) 2)))
  (plot-parametric 
   win2 
   (lambda (J) (T^2 -1. J cons list))
   12. 15. .01))
(plot-parametric win2 
		 (lambda (J) 
		   (cons -1. J))
		 12. 15. .01)
(let ((T^2 (iterated-map (dp-aa-map ml^2 mlg Aw^2/g omega) 2)))
  (plot-parametric 
   win2 
   (lambda (J) (T^2 -pi/2 J cons list))
   12. 15. .01))
(plot-parametric win2 
		 (lambda (J) 
		   (cons -pi/2 J))
		 12. 15. .01)


(let ((T^2 (iterated-map (dp-aa-map ml^2 mlg Aw^2/g omega) 2)))
  (plot-function 
   win2 
   (lambda (phi)
     (let ((J0 (radially-mapping-points 
		T^2 
		(principal-range pi/2)
		13. 14. phi 1e-8)))
       (let ((J1 (T^2 phi J0 (lambda (phi J) J) list)))
	 (plot-point win2 phi J0)
	 J1)))
   -pi/2 pi/2 .05))

(let ((T^3 (iterated-map (dp-aa-map ml^2 mlg Aw^2/g omega) 3)))
  (plot-function 
   win2 
   (lambda (phi)
     (let ((J0 (radially-mapping-points 
		T^3 
		(principal-range pi/2)
		9. 11. phi 1e-8)))
       (let ((J1 (T^3 phi J0 (lambda (phi J) J) list)))
	 (plot-point win2 phi J0)
	 J1)))
   -pi/2 pi/2 .05))

|#


#|
driven rotor examples

;;; first a surface of section
(define win (frame -pi pi -20. 20.))
(graphics-close win)
(graphics-clear win)
(define (explore-it win)
  (let ((m 1) 
	(l 1)
	(g 0.0) 
	(omega (* 4.2 (sqrt 9.8)))
	(A 0.1))
    (let ((ml^2 (* m l l))
	  (mlg (* m l g))
	  (mlAw^2 (* m l A omega omega)))
      (explore-map win 
		   (dp-map ml^2 mlg mlAw^2 omega) 
		   100))))

(define (do-it win upperJ lowerJ n)
  (let ((m 1) 
	(l 1)
	(g 0.0) 
	(omega (* 4.2 (sqrt 9.8)))
	(A 0.1)
	(dphi 0.05)
	(dJ 1.0))
    (let ((ml^2 (* m l l))
	  (mlg (* m l g))
	  (mlAw^2 (* m l A omega omega)))
      (let* ((T (dp-map ml^2 mlg mlAw^2 omega))
	     (T^n (iterated-map T n))
	     (J0 (radially-mapping-points 
		  T^n 
		  (principal-value pi)
		  lowerJ upperJ -pi 1e-10)))
	(let loop ((phi -pi) (lowerJ lowerJ) (upperJ upperJ) (lastphi -pi) (lastJ0 J0))
	  (let ((J0 (radially-mapping-points 
		     T^n 
		     (principal-value pi)
		     lowerJ upperJ phi 1e-10)))
	    (let ((J1 (T^n phi J0 (lambda (phi J) J) list)))
	      (plot-line win lastphi lastJ0 phi J0)
	      (plot-point win phi J1)
	      (if (< phi pi) (loop (+ phi dphi) (- J0 dJ) (+ J0 dJ) phi J0)))))))))

;;; pb-2.ps for 1:1 islands
(define win (frame -pi pi -20. 20.))
(graphics-close win)
(graphics-clear win)
(do-it win 10. 20. 1)
(do-it win -10. -20. 1)
(explore-it win)

;;; pb-3.ps for 0:1 stationary resonances
(define win2 (frame -pi pi -2 2))
(do-it win2 -10 10. 1)
(explore-it win2)
(graphics-close win2)
(graphics-clear win2)

;;; pb-4.ps for 1:3 resonance
;;; examination shows point moves right to adjacent island
;;; after three maps returns to same island
(define win2 (frame -pi pi 3.5 5.5))
(do-it win2 3 5 3)
(explore-it win2)

(let ((m 1) 
      (l 1)
      (g 0.0) 
      (omega (* 4.2 (sqrt 9.8)))
      (A 0.1))
  (let ((ml^2 (* m l l))
	(mlg (* m l g))
	(mlAw^2 (* m l A omega omega)))
    (explore-map win2
		 (dp-map ml^2 mlg mlAw^2 omega) 
		 2)))

|#

#|
;;;----------------------------------------------------------------
;;; standard map without pv for derivatives

(define ((sm K) v)
  (let ((theta (vector-ref v 0))
	(I (vector-ref v 1)))
    (let ((Ip (+ I (* K (sin theta)))))
      (vector (+ theta Ip) Ip))))

(print-expression 
 (determinant 
  ((D (sm 'K)) (vector 'theta 'I))))
1
;No value

(print-expression 
 (trace
  ((D (sm 'K)) (vector 'theta 'I))))
(+ 2 (* K (cos theta)))
;No value

(define win (frame 0. 2pi 0. 2pi))
(graphics-clear win)
(define golden-mean (/ (+ 1 (sqrt 5)) 2))

(define (do-it win upperJ lowerJ n)
  (let ((K 1.0)
	(dphi 0.05)
	(dJ 0.1))
    (let* ((T (standard-map K))
	   (T^n (iterated-map T n))
	   (J0 (radially-mapping-points 
		T^n 
		(principal-value pi)
		lowerJ upperJ 0. 1e-10)))
      (let loop ((phi 0.) (lowerJ lowerJ) (upperJ upperJ) (lastphi 0.) (lastJ0 J0))
	(let ((J0 (radially-mapping-points 
		   T^n 
		   (principal-value pi)
		   lowerJ upperJ phi 1e-10)))
	  (let ((J1 (T^n phi J0 (lambda (phi J) J) list)))
	    (plot-line win lastphi lastJ0 phi J0)
	    (plot-point win phi J1)
	    (if (< phi 2pi) (loop (+ phi dphi) (- J0 dJ) (+ J0 dJ) phi J0))))))))


(print-stream (convergents (continued-fraction (/ 1 golden-mean))) 10)
0
1
1/2
2/3
3/5
5/8
8/13
13/21
21/34
34/55
;Value: ...

(explore-map win (standard-map 1.) 1000)
(graphics-clear win)
(do-it win 1. 2. 3)
(do-it win 2. 3. 5)
(do-it win 1.8 2.2 8)

(do-it win 1.8 2.2 13)
|#
;;; a curious idea: for each theta find the radially mapping point 
;;; for T^q where q is the denominator of the continued fraction convergent
;;; doesn't this have to approach the invariant curve? 
;;; the assumption is that the image of the radial point is closer and closer.
;;; this can be verified.

;;; assume golden winding for now
(define ((radial-mapping-sequence K Jmin Jmax) theta)
  (let loop ((fib1 2) (fib2 3) (Jmin Jmin) (Jmax Jmax))
    (let ((q1 (+ fib1 fib2)))
      (let ((J1 (find-it K theta Jmin Jmax q1 1.e-14)))
	(write-line (list q1 J1))
	(let ((q2 (+ fib2 q1)))
	  (let ((J2 (find-it K theta J1 Jmax q2 1.e-14)))
	    (write-line (list q2 J2))
	    (loop q1 q2 J1 J2)))))))

#|
((radial-mapping-sequence .9 1. 1.8) 0.)

(5 1.6035445999819784)
(8 1.7191758369104382)
(13 1.6759896796735894)
(21 1.6875796969307162)
(34 1.680096679278964)
(55 1.682584220600586)
(89 1.682332856021873)
(144 1.6825571884913444)
(233 1.6825376516138038)
(377 1.6825376532161402)
(610 1.6825376516889134)
|#

(define (find-it K phi Jmin Jmax q eps)
  (let ((pmap (iterated-map (standard-map K) q)))
    (find-a-root
     (lambda (J)
       ((principal-value pi)
	(- phi (pmap phi J (lambda (phip Jp) phip) list))))
     Jmin 
     Jmax
     .01
     eps
     (lambda (x) x)
     'fail)))
#|
(find-it .9 0. 1. 2. 3 1.e-10)
;Value 40: (1.8024056274152827)
(find-it .9 0. 1. 2. 5 1e-10)
;Value 42: (1.603544600046007)
(find-it .9 0. 1.6 1.8 8 1e-10)
;Value 43: (1.719175836862996)
(find-it .9 0. 1.6 1.8 13 1e-10)
;Value 44: (1.6759896796662364)
(find-it .9 0. 1.6 1.8 21 1e-10)
;Value 45: (1.687579696951434)
(find-it .9 0. 1.6 1.8 34 1e-10)
;Value: 1.6800966793205598
(find-it .9 0. 1.6 1.7 55 1e-10)
;Value: 1.6825842205900698
(find-it .9 0. 1.6 1.7 89 1e-10)
;Value: 1.6823328560683872
(find-it .9 0. 1.65 1.7 144 1e-10)
;Value: 1.6825571884866801
(find-it .9 0. 1.68 1.69 233 1e-10)
;Value: 1.6825833981111646
|#

;;; assume golden winding for now
(define ((radial-mapping-sequence K Jmin Jmax) theta)
  (let loop ((fib1 2) (fib2 3) (Jmin Jmin) (Jmax Jmax))
    (let ((q1 (+ fib1 fib2)))
      (let ((J1 (find-it K theta Jmin Jmax q1 1.e-10)))
	(write-line (list q1 J1))
	(let ((q2 (+ fib2 q1)))
	  (let ((J2 (find-it K theta J1 Jmax q2 1.e-10)))
	    (write-line (list q2 J2))
	    (loop q1 q2 J1 J2)))))))

#|
((radial-mapping-sequence .9 1.6 1.81) 0.)
|#