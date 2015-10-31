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

  (define (safe-exp x)
    (if (differential? x)
	(exp x)
	(cond ((< x -500) (exp -501))
	      ((> x +500) (exp 501))
	      (else (exp x)))))

(define ((diode-current IS q/kT #!optional n) v)
  (if (default-object? n) (set! n 1.0))	 ; 1<n<2 depending on junction.
  (* IS (- (safe-exp (* (/ q/kT n) v)) 1)))

#|
;;; Incremental conductance

((D (diode-current 'I_S (/ 1 'V_T)))
 'v_D)
#|
(/ (* I_S (exp (/ v_D V_T))) V_T)
|#

|#

(define ((diode-incremental-resistance IS q/kT #!optional n) VD)
  (if (default-object? n) (set! n 1.0))	 ; 1<n<2 depending on junction.
  (/ 1
     (* (/ q/kT n)
	(* IS (safe-exp (* (/ q/kT n) VD))))))


#|
;;; What is the incremental resistance of the diode at 0.6V?
;;;     Assume IS = 10fA, VT = 26mV.

((diode-incremental-resistance (& 10 &femto &ampere)
			       (/ 1 (& 26 &milli &volt)))
 (& 0.6 &volt))
#|
(& 247.0546294493757 &ohm)
|#

((diode-incremental-resistance (& 10 &femto &ampere)
			       (/ 1 (& 26 &milli &volt)))
 (& 0.0 &volt))
#|
(& 2600000000000. &ohm)			;2.6e12 Ohm
|#

;;; At what voltage is the resistance 10^15 Ohm?

(let ((VT (& 26 &milli &volt)) (rd (& 1e15 &ohm)) (IS (& 10 &femto &ampere))) 
  (* VT (log (/ VT (* rd IS)))))
#|
(& -.1547583396828222 &volt)
|#

|#


