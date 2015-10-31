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

(define (Lagrangian-action L q t1 t2)
  (definite-integral (compose L (Gamma q))
                     t1 t2))

#|
(define ((L-free-particle mass) local)
  (let ((v (velocity local)))
    (* 1/2 mass (square v))))

(define (test-path t)
  (coordinate-tuple (+ (* 4 t) 7)
		    (+ (* 3 t) 5)
		    (+ (* 2 t) 1)))

(Lagrangian-action (L-free-particle 3) test-path 0 10)
;Value: 435.

(define ((variation nu t1 t2 h) t)
  (* h (- t t1) (- t t2) (nu t)))

(define ((varied-free-particle-action mass path nu t1 t2) h)
  (let ((dpath (variation nu t1 t2 h)))
    (Lagrangian-action (L-free-particle mass)
                       (+ path dpath)
                       t1
                       t2)))

((varied-free-particle-action 3.0 test-path 
                              (coordinate-tuple sin cos square)
                              0.0 10.0)
 0.001)
;Value: 436.29121428571443

(minimize
 (varied-free-particle-action 3.0 test-path 
                              (coordinate-tuple sin cos square) 
                              0.0 10.0)
 -2.0 1.0)
;Value: (-5.828670879282072e-16 435.00000000000085 5)
|#

;;; Equal-spaced times.

(define (make-path t0 q0 t1 q1 qs)
  (let ((n (length qs)))
    (let ((ts (linear-interpolants t0 t1 n)))
      (Lagrange-interpolation-function
       (append (list q0) qs (list q1))
       (append (list t0) ts (list t1))))))

(define ((parametric-path-action Lagrangian t0 q0 t1 q1) qs)
  (let ((path (make-path t0 q0 t1 q1 qs)))
    (Lagrangian-action Lagrangian path t0 t1)))

(define (find-path Lagrangian t0 q0 t1 q1 n)
  (let ((initial-qs (linear-interpolants q0 q1 n)))
    (let ((minimizing-qs
	   (multidimensional-minimize
	    (parametric-path-action Lagrangian t0 q0 t1 q1)
	    initial-qs)))
      (make-path t0 q0 t1 q1 minimizing-qs))))

(define (linear-interpolants x0 x1 n)
  (let ((dx (- x1 x0)) (n+1 (fix:+ n 1)))
    (let lp ((i 1) (xs '()))
      (if (fix:> i n)
	  (reverse xs)
	  (lp (fix:+ i 1)
	      (cons (+ x0 (/ (* i dx) n+1)) xs))))))

#|
;;; For example, consider the Harmonic oscillator with
;;;  spring constant, k, and mass, m.

(define ((L-harmonic m k) local)
  (let ((q (coordinate local)) 
        (v (velocity local)))
    (- (* 1/2 m (square v))
       (* 1/2 k (square q)))))

(define q
  (find-path (L-harmonic 1.0 1.0) 0. 1. pi/2 0. 3))


(define p (frame 0.0 pi/2 -3e-4 3e-4))

(plot-function p 
 (lambda (t)
   (- (q t) (cos t)))
 0.0
 pi/2
 .01)

(graphics-clear p)
(graphics-close p)

;;; Error curve is in actionerror3.xwd

;;; ********************** too slow...

(define q1
  (show-time
   (lambda ()
     (find-path (L-harmonic 1.0 1.0) 0. 1. pi/2 0. 4))))
;;;maharal process time: 12330 (10750 RUN + 1580 GC); real time: 12330
;;;PPA process time: 160700 (153800 RUN + 6900 GC); real time: 161913


(define p (frame 0.0 pi/2 -1e-4 1e-4))

(plot-function p 
 (lambda (t)
   (- (q1 t) (cos t)))
 0.0
 pi/2
 .01)

(graphics-clear p)
(graphics-close p)

;;; Error curve is in actionerror4.xwd
|#

#|
(define win2 (frame 0. pi/2 0. 1.2))

(define ((parametric-path-action Lagrangian t0 q0 t1 q1) 
         intermediate-qs)
    (let ((path (make-path t0 q0 t1 q1 intermediate-qs)))
      ;; display path
      (graphics-clear win2)
      (plot-function win2 path t0 t1 (/ (- t1 t0) 100))
      ;; compute action
      (Lagrangian-action Lagrangian path t0 t1)))

(define ((parametric-path-action Lagrangian t0 q0 t1 q1) 
         intermediate-qs)
    (let ((path (make-path t0 q0 t1 q1 intermediate-qs)))
      ;; display path
      ;(graphics-clear win2)
      (plot-function win2 path t0 t1 (/ (- t1 t0) 100))
      ;; compute action
      (Lagrangian-action Lagrangian path t0 t1)))

(find-path (L-harmonic 1. 1.) 0. 1. pi/2 0. 3)

(graphics-clear win2)
(graphics-close win2)
|#

#|
(define ((L-Kepler-polar GM m) local)
  (let ((q (coordinate local))
        (qdot (velocity local)))
    (let ((r (ref q 0))
          (phi (ref q 1))
          (rdot (ref qdot 0))
          (phidot (ref qdot 1)))
      (+ (* 1/2 m
           (+ (square rdot)
              (square (* r phidot))) )
         (/ (* GM m) r)))))


(define win2 (frame -1.5 +1.5 -1.5 +1.5))

(define ((parametric-path-action Lagrangian t0 q0 t1 q1) 
         intermediate-qs)
    (let ((path (make-path t0 q0 t1 q1 intermediate-qs)))
      ;; display path
      (graphics-clear win2)
      (plot-parametric-fill win2
			    (lambda (t)
			      (let ((rt (path t)))
				(cons (* (vector-ref rt 0)
					 (cos (vector-ref rt 1)))
				      (* (vector-ref rt 0)
					 (sin (vector-ref rt 1))))))
			    0.0
			    1.0
			    (plane-near? .001))
      ;; compute action
      (Lagrangian-action Lagrangian path t0 t1)))

(graphics-clear win2)

(set! *definite-integral-allowable-error* 1e-8)

(set! *quadrature-neighborhood-width* #f)

(define q
  (find-path (L-Kepler-polar 1.0 1.0) 0. #(1. 0.) 1 #(1. 2.) 3))
|#