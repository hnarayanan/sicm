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

;;; If I have a 2nd-order system x'' = f(x, x') and I want 
;;;  to make a first-order system to integrate we proceed 
;;;  as follows:
;;;    Let y = [x, x'].  Then y' = [x', x''].  
;;;    Let g(y) = [y_1, f(y_0, y_1)]
;;;      then  y' = g(y).

(define (2nd-order->1st-order f)
  (lambda (y)				; y is 2n-long vector
    (let ((2n (vector-length y)))
      (let ((n (quotient 2n 2)))
	(let ((x (subvector y 0 n))
	      (xp (subvector y n 2n)))
	  (let ((xpp (f x xp)))
	    (vector-append xp xpp)))))))
  	    
(define (vector-append v1 v2)
  (list->vector (append (vector->list v1)
			(vector->list v2))))

#| 
;;; Damped harmonic oscillator:

(define ((harmonic b osq) x xp)
  (- (+ (* 2 b xp) (* osq x))))

(define g
  (2nd-order->1st-order
   (harmonic 0.0 (square 2pi))))


((advance-generator
  ((quality-control rk4 4)		;integration method
   g					;x' = (g x x' t)
   .0001))				;error tolerated
 #(0.0 1.0)				;initial state (at t = t0)
 2.0					;proceed to t = t0 + 1
 1.0					;first step no larger than .1
 0.5					;no step larger than .5
 (lambda (ns dt h cont)
   (pp ns)
   (cont))
 (lambda (ns dt sdt)
   (list ns dt sdt)))

#(0. 1.)
#(.07453223281524525 .8835729477710929)
#(.1371965389656881 .5068729446563314)
#(.15878562732939655 .06824985468942453)
#(.15097921035650724 -.31642455218443827)
#(.11968481154058178 -.659180581526617)
#(.06611091937948106 -.9096603042654966)
#(-4.8130448756624495e-3 -.9995588712859846)
#(-.08477792560763237 -.8463445937000282)
#(-.14291386501454892 -.44015083496347557)
#(-.1591561477550655 -.00657076288652179)
#(-.1480141082776238 .3676405822874569)
#(-.11344553035124767 .7014119733078732)
#(-.05704964091048201 .9335820189808471)
#(.01569688516221585 .9951590735626281)
#(.09623332936500754 .7965406154598041)
#(.14065155150677477 .4680681775501326)
#(.1590065948056912 4.4164041381693955e-2)
#(.149904255597975 -.33609199356697694)
#(.11735514997477059 -.6755623887720456)
#(.06268935441184409 -.9192086928392766)
#(-8.953316317775885e-3 -.9984651290862181)
#(-.08916804267338409 -.828382353827908)
#(-.14519891567668475 -.40964342222641487)
#(-.15912836005510742 2.1395187758375296e-2)
#(-.14631820537743567 .3936127856525461)
#(-.11008201110322616 .7223057817280027)
#(-.0522803572087781 .9445771533799789)
;Value 9: (#(-3.434179710966323e-6 1.0000652254450102)
            2.
            .08726396349185829)

|#
