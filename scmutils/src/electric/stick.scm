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

;;;
;;; assemblies of current sticks (Haus and Melcher)
;;;
;;; JW

#|

the vector potential at r 
of a current stick with origin at r0 and end at r1

using GR p 81

int (a + bx + cx^2)^(-1/2) dx = 

1/(c^1/2) ln ( 2 (c R)^1/2 + 2 c x + b)  [c>0]

(define ((the-general-integral a b c) x)
  (let ((R (+ a (* b x) (* c x x))))
    (* (/ 1 (sqrt c))
       (log (+ (* 2 (sqrt (* c R))) (* 2 c x) b)))))

;;; a is vector along current stick
;;; b is vector from point to beginning of stick
;;; c is vector from point to end of stick

|#

(define ((Astick i/4pi r0 r1) r)
  (let ((a (- r1 r0))
	(b (- r0 r))
	(c (- r1 r)))
    (let ((ahat (/ a (sqrt (square a)))))
      (* i/4pi
	 (* ahat 
	    (- (log (* 2 (+ (sqrt (square c)) (dot-product ahat c))))
	       (log (* 2 (+ (sqrt (square b)) (dot-product ahat b))))))))))
	
(define ((curl-rectangular A) x)
  (up (- (((partial 1) (ref A 2)) x) (((partial 2) (ref A 1)) x))
      (- (((partial 2) (ref A 0)) x) (((partial 0) (ref A 2)) x))
      (- (((partial 0) (ref A 1)) x) (((partial 1) (ref A 0)) x))))

(define ((div-rectangular A) x)
  (+ (((partial 0) (ref A 0)) x)
     (((partial 1) (ref A 1)) x)
     (((partial 2) (ref A 2)) x)))

(define ((grad-rectangular phi) x)
  (up (((partial 0) phi) x)
      (((partial 1) phi) x)
      (((partial 2) phi) x)))


;;; from Haus p. 322

(define ((Hstick i/4pi r0 r1) r)
  (let ((a (- r1 r0))
	(b (- r0 r))
	(c (- r1 r)))
    (let ((cxa (cross-product c a)))
      (* i/4pi
	 (/ cxa (square cxa))
	 (- (/ (dot-product a c) (sqrt (square c)))
	    (/ (dot-product a b) (sqrt (square b))))))))

(define (with-simplifier thunk simplifier)
  (fluid-let ((simplify simplifier))
    (thunk)))

(define ((Hstick-parametric p) r)
  (let ((i/4pi (ref p 0))
	(r0 (up (ref p 1) (ref p 2) (ref p 3)))
	(r1 (up (ref p 4) (ref p 5) (ref p 6))))
    ((Hstick i/4pi r0 r1) r)))

(define Hstick-parametric-compiled
  (with-simplifier
      (lambda () (compile-parametric 7 3 Hstick-parametric))
    expression))

(define ((Hstick-compiled i/4pi r0 r1) r)
  ((Hstick-parametric-compiled 
    (vector->list (vector-append (vector i/4pi) r0 r1)))
   r))
  
#|

from Jackson appendix

SI units of H are amp/meter = I/m

B = mu_0 H
units of B are tesla 

mu_0 has units m l / (I^2 t^2)

mu_0 -> Newtons/amp^2
(m l / t^2) / I^2 ok

epsilon_0 has units I^2 t^4 / (m l^3)

mu_0 epsilon_0 => t^2 / l^2 = 1/c^2

B, tesla is then 
(I/m) m l / (I^2 t^2)
= l / (I t^2)

|#

#|

(pe ((curl-rectangular (Astick 1 (up 0 0 0) (up 0 0 2))) (up 3 5 7)))
(up -.01726548015054591 1.0359288090327545e-2 0)
;Unspecified return value

(pe ((Hstick 1. (up 0 0 0) (up 0 0 2)) (up 3 5 7)))
(up -1.7265480150546023e-2 1.0359288090327615e-2 0.)

(pe ((curl-rectangular (Astick 1 (up 3 5 7) (up 97 51 13))) (up 17 19 23)))
(up .04140863618372075 -.09018445303816483 .04267883974763857)

(pe ((Hstick 1. (up 3 5 7) (up 97 51 13)) (up 17 19 23)))
(up 4.1408636183720746e-2 -.09018445303816482 .04267883974763856)

|#

;;; need derivative of mu.Hstick:

(define ((mu-dot-Hstick i/4pi r0 r1) mu r)
  (dot-product mu ((Hstick i/4pi r0 r1) r)))

;;; derivative wrt r
(define ((D-mu-dot-Hstick i/4pi r0 r1) mu r)
  (let ((a (- r1 r0))
	(b (- r0 r))
	(c (- r1 r)))
    (let ((cxa (cross-product c a)))
      (* i/4pi
	 (+ (* (+ (/ (cross-product mu a)
		     (square cxa))
		  (/ (* 2
		      (dot-product mu cxa)
		      (cross-product a
				     (cross-product c a)))
		     (square (square cxa))))	  
	       (- (/ (dot-product a c) (sqrt (square c)))
		  (/ (dot-product a b) (sqrt (square b)))))
	    (* (/ (dot-product mu cxa) (square cxa))
	       (- (+ (/ (* -1 a) (sqrt (square c)))
		     (/ (* 2 (dot-product a c) c)
			(* 2 (expt (square c) 3/2))))
		  (+ (/ (* -1 a) (sqrt (square b)))
		     (/ (* 2 (dot-product a b) b)
			(* 2 (expt (square b) 3/2)))))))))))

(define (D-mu-dot-Hstick-parametric p)
  (let ((i/4pi (ref p 0))
	(r0 (up (ref p 1) (ref p 2) (ref p 3)))
	(r1 (up (ref p 4) (ref p 5) (ref p 6))))
    (lambda (mu-r)
      (let ((mu (up (ref mu-r 0) (ref mu-r 1) (ref mu-r 2)))
	    (r (up (ref mu-r 3) (ref mu-r 4) (ref mu-r 5))))
	((D-mu-dot-Hstick i/4pi r0 r1) mu r)))))

(define D-mu-dot-Hstick-parametric-compiled
  (with-simplifier
      (lambda () (compile-parametric 7 6 D-mu-dot-Hstick-parametric))
    expression))

(define ((D-mu-dot-Hstick-compiled i/4pi r0 r1) mu r)
  ((D-mu-dot-Hstick-parametric-compiled 
    (vector->list (vector-append (vector i/4pi) r0 r1)))
   (vector-append mu r)))
  

;;;----------------------------------------------------------------
;;; make assemblies of current sticks

(define make-current-stick list)
(define current-stick->i/4pi car)
(define current-stick->r0 cadr)
(define current-stick->r1 caddr)

;;; from an assembly of current sticks we want the fields A, H, B

(define ((current-sticks->A current-sticks) r)
  (apply + (map (lambda (cs) ((apply Astick cs) r))
		current-sticks)))

(define ((current-sticks->H current-sticks) r)
  (apply + (map (lambda (cs) ((apply Hstick cs) r))
		current-sticks)))

(define ((current-sticks->H-compiled current-sticks) r)
  (apply + (map (lambda (cs) ((apply Hstick-compiled cs) r))
		current-sticks)))

(define (current-sticks->B current-sticks)
  (let ((mu_0 (* 4 pi 1.e-7)))
    (* mu_0
       (current-sticks->H current-sticks))))

(define (current-sticks->B-compiled current-sticks)
  (let ((mu_0 (* 4 pi 1.e-7)))
    (* mu_0
       (current-sticks->H-compiled current-sticks))))

;;; we also need the derivatives

(define ((current-sticks->D-mu-dot-H current-sticks) mu r)
  (let ((mu-r (vector-append mu r)))
    (apply + (map (lambda (cs) 
		    ((apply D-mu-dot-Hstick cs) mu r))
		  current-sticks))))

(define ((current-sticks->D-mu-dot-H-compiled current-sticks) mu r)
  (apply + (map (lambda (cs) 
		  ((apply D-mu-dot-Hstick-compiled cs) mu r))
		current-sticks)))

(define (current-sticks->D-mu-dot-B current-sticks)
  (let ((mu_0 (* 4 pi 1.e-7)))
    (* mu_0
       (current-sticks->D-mu-dot-H current-sticks))))

(define (current-sticks->D-mu-dot-B-compiled current-sticks)
  (let ((mu_0 (* 4 pi 1.e-7)))
    (* mu_0
       (current-sticks->D-mu-dot-H-compiled current-sticks))))

#|

;;; sample assembly

(define (rectangular-current-loop wx/2 wy/2 z i/4pi)
  (list (make-current-stick i/4pi (up wx/2 wy/2 z) (up wx/2 (- wy/2) z))
	(make-current-stick i/4pi (up wx/2 (- wy/2) z) (up (- wx/2) (- wy/2) z))
	(make-current-stick i/4pi (up (- wx/2) (- wy/2) z) (up (- wx/2) wy/2 z))
	(make-current-stick i/4pi (up (- wx/2) wy/2 z) (up wx/2 wy/2 z))))

(pe ((grad-rectangular 
      (current-sticks->H 
       (rectangular-current-loop 4. 1. 0. 1.)))
     (up 0. 0. 2.)))
(up (up -.05154098740812011 0 0.) (up 0 -.6251422988855859 0) (up 0 0 .676683286293706))

(pe ((current-sticks->D-mu-dot-H
       (rectangular-current-loop 4. 1. 0. 1.))
     (up 0 0 1)
     (up 0. 0. 2.)))
(up 0. 0. .676683286293706)

(pe ((current-sticks->D-mu-dot-H-compiled
       (rectangular-current-loop 4. 1. 0. 1.))
     (up 0 0 1)
     (up 0. 0. 2.)))
(up 0. 0. .676683286293706)

|#

(define ((draw-stick win projection) current-stick)
  (let ((r0 (current-stick->r0 current-stick))
	(r1 (current-stick->r1 current-stick)))
    (let ((xy0 (projection r0))
	  (xy1 (projection r1)))
      (plot-line win (ref xy0 0) (ref xy0 1) (ref xy1 0) (ref xy1 1)))))

(define (projection-xy v)
  (up (ref v 0) (ref v 1)))

(define (projection-xz v)
  (up (ref v 0) (ref v 2)))

(define (projection-yz v)
  (up (ref v 1) (ref v 2)))

(define (draw-assembly win projection assembly)
  (for-each (draw-stick win projection)
	    assembly))

#|

(define win (frame -5 5 -5 5))
(graphics-clear win)
(draw-assembly win projection-xy (rectangular-current-loop 4. 1. 0. 1.))
(draw-assembly win projection-yz (rectangular-current-loop 4. 1. 0. 1.))
(draw-assembly win projection-xz (rectangular-current-loop 4. 1. 0. 1.))

;;;----------------------------------------------------------------
;;; visualize H field
;;; define a system derivative to follow H field lines

|#

(define (((H-sysder H)) s)
  (let ((x (ref s 1)) (y (ref s 2)) (z (ref s 3)))
    (vector-append (vector 1) 
		   (H (up x y z)))))

(define ((monitor-xy win) s)
  (plot-point win (ref s 1) (ref s 2)))

(define ((monitor-xz win) s)
  (plot-point win (ref s 1) (ref s 3)))

(define ((monitor-yz win) s)
  (plot-point win (ref s 2) (ref s 3)))

(define (do-it-once win assembly monitor ic dt tfinal)
  (let ((sysder (H-sysder (current-sticks->H assembly))))
    ((evolve sysder)
     ic monitor dt tfinal 1.e-10)))

(define (do-it-many win assembly monitor v0 v1 n dt tfinal)
  (let ((dv (/ (- v1 v0) (- n 1))))
    (let loop ((ic v0) (i (- n 1)))
      (do-it-once win assembly monitor ic dt tfinal)
      (if (> i 0) (loop (+ ic dv) (- i 1))))))
	  

#|

(set! *compiling-sysder? #f)

(define win (frame -5 5 -5 5))
(graphics-clear win)

(draw-assembly win 
	       projection-xz
	       (rectangular-current-loop 4. 1. 0. 1.))

(do-it-once win 
	    (rectangular-current-loop 4. 1. 0. 1.)
	    (monitor-xz win)
	    (up 0. 1. 0. 0.)
	    .1
	    10)

(do-it-many win 
	    (rectangular-current-loop 4. 1. 0. 1.)
	    (monitor-xz win)
	    (up 0. 1. 0. 0.)
	    (up 0. 3. 0. 0.)
	    5
	    .1
	    10)	  

(define (stern-gerlach-pole wx/2 wy/2 big i/4pi)
  (list (make-current-stick i/4pi (up (- wx/2) wy/2 0) (up wx/2 wy/2 0))
	(make-current-stick i/4pi (up wx/2 wy/2 0) (up wx/2 big 0))
	(make-current-stick i/4pi (up wx/2 big 0) (up (- wx/2) big 0))
	(make-current-stick i/4pi (up (- wx/2) big 0) (up (- wx/2) wy/2 0))

	(make-current-stick i/4pi (up wx/2 (- wy/2) 0) (up (- wx/2) (- wy/2) 0))
	(make-current-stick i/4pi (up (- wx/2) (- wy/2) 0) (up (- wx/2) (- big) 0))
	(make-current-stick i/4pi (up (- wx/2) (- big) 0) (up wx/2 (- big) 0))
	(make-current-stick i/4pi (up wx/2 (- big) 0) (up wx/2 (- wy/2) 0))))

(define (two-sticks wx/2 wy/2 i/4pi)
  (list (make-current-stick i/4pi (up (- wx/2) wy/2 0) (up wx/2 wy/2 0))
	(make-current-stick i/4pi (up wx/2 (- wy/2) 0) (up (- wx/2) (- wy/2) 0))))

(define win (frame -15 15 -15 15))
(graphics-clear win)

(draw-assembly win 
	       projection-xy
	       (stern-gerlach-pole 10. 1. 50. 1.))

(do-it-many win 
	    (stern-gerlach-pole 10. 1. 50. 1.)
	    (monitor-xz win)
	    (up 0. 3. 0. 0.)
	    (up 0. 5. 0. 0.)
	    5
	    .1
	    10)

(graphics-clear win)
(draw-assembly win 
	       projection-xz
	       (stern-gerlach-pole 10. 1. 50. 1.))
(do-it-many win 
	    (stern-gerlach-pole 10. 1. 50. 1.)
	    (monitor-xz win)
	    (up 0. -15. 0. 0.)
	    (up 0. 15. 0. 0.)
	    20
	    .1
	    10)
(draw-assembly win 
	       projection-xz
	       (two-sticks 10. 1. 1.))
(do-it-many win 
	    (two-sticks 10. 1. 1.)
	    (monitor-xz win)
	    (up 0. -15. 0.5 0.)
	    (up 0. 15. 0.5 0.)
	    20
	    .1
	    20)

(define win (frame -15 15 -15 15))
(draw-assembly win 
	       projection-yz
	       (stern-gerlach-pole 10. 1. 50. 1.))
(do-it-many win 
	    (stern-gerlach-pole 10. 1. 50. 1.)
	    (monitor-yz win)
	    (up 0. -10. -.9 0.)
	    (up 0. -10. .9 0.)
	    5
	    .001
	    1)


(do-it-many win 
	    (stern-gerlach-pole 10. 1. 50. 1.)
	    (monitor-yz win)
	    (up 0. 0. -.9 0.)
	    (up 0. 0. .9 0.)
	    20
	    .01
	    2)

|#

