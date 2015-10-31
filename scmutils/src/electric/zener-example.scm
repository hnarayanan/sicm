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

;;; Combining impedances

(define (series z1 z2) (+ z1 z2))

(define (parallel z1 z2) (/ (* z1 z2) (+ z1 z2)))

(define (divider z1 z2) (/ z2 (+ z1 z2)))

;;; The Zener diode model
(define ((Zener-diode-current Vz Gz) v)
  (cond ((< v (- Vz)) (* Gz (+ v Vz)))
	((> v 0.6) (* Gz (- v 0.6)))
	(else 0)))

(define ((load-line-current VOC RTH) v)
  (/ (- VOC v) RTH))

(define (operating-point RIN RL VI Vz Gz)
  (let ((RTH (parallel RIN RL))
	(VOC (* (divider RIN RL) VI)))
    (let ((iZ (Zener-diode-current Vz Gz))
	  (ll (load-line-current VOC RTH)))
      (bisect
       (lambda (v)
	 (+ (iZ (- v)) (ll v)))
       (- VI)
       (+ VI)
       1e-15))))

#|
;;; Without zener (VZ=20Volts) we get divider values

(operating-point 1000.0 2000.0 10.0 20.0 1.0)
#| 6.666666666666668 |#

(* .05					;additive noise of .05 V
   ((richardson-derivative
     (lambda (vi)
       (operating-point 1000.0 2000.0 vi 20.0 1.0))
     1e-10)
    10))
#| .03333333333333332 |#

(operating-point 1000.0 4000.0 10.0 20.0 1.0)
#| 8.000000000000004 |#

(* .05
   ((richardson-derivative
     (lambda (vi)
       (operating-point 1000.0 4000.0 vi 20.0 1.0))
     1e-10)
    10))
#| .04000000000000001 |#

;;; With Zener (VZ=5Volts) we get nice results
(* .05
   ((richardson-derivative
     (lambda (vi)
       (operating-point 1000.0 4000.0 vi 5.0 1.0))
     1e-10)
    10))
#| 4.9937578027083486e-5 |#

(* .05
   ((richardson-derivative
     (lambda (vi)
       (operating-point 1000.0 2000.0 vi 5.0 1.0))
     1e-10)
    10))
#| 4.9925112334852446e-5 |#

;;; If load is too big, zener is not active

(operating-point 1000.0 500.0 10.0 5.0 1.0)
#| 3.333333333333334 |#

;;; So it doesn't help
(* .05
   ((richardson-derivative
     (lambda (vi)
       (operating-point 1000.0 500.0 vi 5.0 1.0))
     1e-10)
    10))
#| 1.6666666666666666e-2 |#
|#
#|
(define (VD RIN RL VI Vz Gz)
  (let ((RTH (parallel RIN RL))
	(VOC (* (divider RIN RL) VI)))
    (/ (+ (* RTH GZ VZ) VOC)
       (+ (* RTH GZ) 1))))

(- (operating-point 1000.0 4000.0 10 5.0 1.0)
   (VD 1000.0 4000.0 10 5.0 1.0))
#| 1.7763568394002505e-15 |#

(((partial 2) VD) 'R_I 'R_L 'V_I 'V_z 'G_z)
#|
(/ R_L (+ (* R_I R_L) R_I R_L))
|#
;;; Mystery: units?, but correct with numerical stuff!

(define (dVD RIN RL)
  (/ RL (+ (* RIN RL) RIN RL)))

(* .05 (dVD 1000 2000))
#| 4.992511233150275e-5 |#
|#

;;; The Zener becomes inactive when the divider
;;; voltage goes below the Zener breakdown 
;;; voltage.

(define (RL-min VI VZ RIN)
  (/ (* VZ RIN) (- VI VZ)))
