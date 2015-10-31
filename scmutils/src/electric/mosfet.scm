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

;;; Simple MOSFET models, lamb is channel-length modulation parameter

(define (n-channel_enhancement-mode_mosfet-current K VT #!optional lamb)
  (if (default-object? lamb) (set! lamb 0))
  (define (the-nfet vGS vDS)
    (let* ((vov (- vGS VT))
	   (zero-volts (zero-like vov))
	   (zero-amperes
	    (if (with-units? vov) (& 0 &ampere) 0)))
      (cond ((<= vov zero-volts)	;Cutoff
	     zero-amperes)
	    ((<= zero-volts vov vDS)	;Saturation (Active)
             (let ((iD (* (/ K 2) (expt vov 2))))
               (if (= lamb 0)
                   iD
                   (* iD (+ 1 (* lamb vDS))))))
	    (else			;Ohmic (Switched On)
	     ;;(<= 0 vDS vov)
             (let ((iD (* K (- vov (/ vDS 2)) vDS)))
               (if (= lamb 0)
                   iD
                   (* iD (+ 1 (* lamb vDS)))))))))
  the-nfet)

(define (p-channel_enhancement-mode_mosfet-current K VT #!optional lamb)
  (if (default-object? lamb) (set! lamb 0))
  (define (the-pfet vSG vSD)
    (let* ((vov (- vSG VT))
	   (zero-volts (zero-like vov))
	   (zero-amperes
	    (if (with-units? vov) (& 0 &ampere) 0)))
      (cond ((<= vov zero-volts)	;Cutoff
	     zero-amperes)
	    ((<= zero-volts vov vSD)	;Saturation (Active)
             (let ((iD (* -1 (/ K 2) (expt vov 2))))
               (if (= lamb 0)
                   iD
                   (* iD (+ 1 (* lamb vSD))))))
	    (else			;Ohmic (Switched On)
	     ;;(<= 0 vSD vov)
             (let ((iD (* -1 K (- vov (/ vSD 2)) vSD)))
               (if (= lamb 0)
                   iD
                   (* iD (+ 1 (* lamb vSD)))))))))
  the-pfet)

	   
;;; Cox is capacitance/area; mu is carrier mobility

(define (mosfet-K mu Cox W/L)
  (* mu Cox W/L))


;;; k is relative permittivity of insulator, 
;;; tox is thickness of insulator

(define (Cox k tox)
  (/ (* k :epsilon_0) tox))

#|


(define ((phase-inverter K VT VDD) vin)
   (define Q1
     (n-channel_enhancement-mode_mosfet-current K VT))
  (define Q2
    (n-channel_enhancement-mode_mosfet-current K VT))
  (define (foo vout)
    (let ((i1 (Q1 vin vout))
	  (i2 (Q2 (- VDD vout) (- VDD vout))))
      (- i2 i1)))
  (let ((VT 1.0)
	(vout (bisect foo 0 VDD 1e-15)))
    (if (<= vin VT)
	(pp `(Q1 cutoff)))
    (if (< vout (- vin VT))
	(pp `(Q1 triode)))
    vout))


;;; Here Q1 is cutoff
((phase-inverter 0.02 1.0 5) 1.0)
(Q1 cutoff)
#| 5 |#

;;; But it suddenly gets quite happy.
((phase-inverter 0.02 1.0 5) 1.01)
#| 3.989999999999998 |#

((phase-inverter 0.02 1.0 5) 2)
#| 2.999999999999999 |#

((phase-inverter 0.02 1.0 5) 3)
#| 2.000000000000001 |#

;;; Note here Q1 goes out of saturation
((phase-inverter 0.02 1.0 5) 3.1)
(Q1 triode)
#| 1.9087287789486662 |#
|#
