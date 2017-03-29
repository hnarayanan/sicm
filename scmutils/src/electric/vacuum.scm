#| -*-Scheme-*-

Copyright (C) 1986, 1987, 1988, 1989, 1990, 1991, 1992, 1993, 1994,
    1995, 1996, 1997, 1998, 1999, 2000, 2001, 2002, 2003, 2004, 2005,
    2006, 2007, 2008, 2009, 2010, 2011, 2012, 2013, 2014, 2015, 2016
    Massachusetts Institute of Technology

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

;;; A Vacuum diode's v-i characteristic is approximated by the Childs-Langmuir Law.
;;;  The parameter P is called the perveance.

(define ((vacuum-diode-plate-current P) vPK)
  (if (> vPK 0)
      (let ((iP (* P (expt vPK 3/2))))
	iP)
      0))

#|
((D (vacuum-diode-plate-current 'P)) 'v_\{PK\})
#|
(* 3/2 P (expt |v_{PK}| 1/2))
|#


;;; With units.

((vacuum-diode-plate-current (& 'P (/ &ampere (expt &volt 3/2))))
 (& 'v_\{PK\} &volt))
#|
(& (* P (expt |v_{PK}| 3/2)) &ampere)
|#


((D (vacuum-diode-plate-current (& 'P (/ &ampere (expt &volt 3/2)))))
 (& 'v_\{PK\} &volt))
#|
(& (* 3/2 P (expt |v_{PK}| 1/2)) &siemens)
|#


(define 6AL5-data			;from GE datasheet ET-T882
  '((.0065 2.0) (.0175 4.0) (.030 6.0) (.0445 8.0) (.0605 10.0)))

(define (perveance iP vPK)
  (/ iP (expt vPK 3/2)))


(let ((ps (map (lambda (datum) (apply perveance datum)) 6AL5-data)))
  (list (/ (apply + ps) (length ps)) ps))
#|
(.00208133144215051
 (2.298097038856279e-3
  .0021875
  2.041241452319315e-3
  1.9666407351750853e-3
  1.9131779844018695e-3))
|#
					 
;;; So, for a 6AL5, P=2mA/V^(3/2)

(define 6AL5-current
  (vacuum-diode-plate-current (& .002 (/ &ampere (expt &volt 3/2)))))

(6AL5-current (& 2 &volt))
#|
(& 5.656854249492381e-3 &ampere)
|#

(6AL5-current (& 4 &volt))
#|
(& .016 &ampere)
|#

((D 6AL5-current) (& 4 &volt))
#|
(& .006 &siemens)
|#

(/ 1 ((D 6AL5-current) (& 4 &volt)) )
#|
(& 166.66666666666666 &ohm)
|#


(6AL5-current (& 10 &volt))
#|
(& .06324555320336758 &ampere)
|#

((D 6AL5-current) (& 10 &volt))
#|
(& 9.486832980505138e-3 &siemens)
|#

(/ 1 ((D 6AL5-current) (& 10 &volt)) )
#|
(& 105.40925533894598 &ohm)
|#
|#

;;; A Vacuum triode's characteristic is also approximated
;;;  by a modified Childs-Langmuir Law.  

(define ((vacuum-triode-plate-current P mu) vGK vPK)
  (let ((veff (+ vGK (/ vPK mu))))
    (if (> veff 0)
        (let ((iP (* P (expt veff 3/2))))
          iP)
        0)))

;;; But this is not really very accurate, and it does not
;;;  account for grid current.  So we need a better model.
;;; One suggestion, by Kristjan Dempwolf and Udo Zolzer,
;;; "A Physically-motivated Triode Model for Circuit 
;;;  Simulations" takes eight parameters:

(define ((vacuum-triode-currents GK CK muK gammaK
                                 GG CG xiK IG0)
         vGK vPK continue)
  ;; continue = (lambda (iK iG iP) ...)
  (let ((veff (+ vGK (/ vPK muK))))
    (let* ((iK (* -1
                  GK
                  (expt (/ (log (+ 1 (exp (* CK veff))))
                           CK)
                        gammaK)))
           (iG (+ IG0
                  (* GG
                     (expt (/ (log (+ 1 (exp (* CG vGK))))
                              CG)
                           xiK))))
           (iP (- (+ iG iK))))
      (continue iK iG iP))))

#|
;;; Emperical fitting of 12AX7 characteristics

(define 12AX7-currents
  (vacuum-triode-currents 2.2075e-3 3.3 101.7 1.27
                          6.044e-4 10.83 1.336 6e-8))


;;; Seem to fit emperical curves OK.

(12AX7-currents -1 200 list)
#|
(-2.1482322265765274e-3 6.001304068339637e-8 2.1481722135358442e-3)
|#

(12AX7-currents .1 200 list)
#|
(-5.550807803694697e-3 3.840138769226011e-5 5.5124064160024374e-3)
|#

(12AX7-currents 1 100 list)
#|
(-5.268665619701756e-3 6.044614760091604e-4 4.664204143692595e-3)
|#
|#