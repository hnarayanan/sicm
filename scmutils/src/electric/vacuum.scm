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