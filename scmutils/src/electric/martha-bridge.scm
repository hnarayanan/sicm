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


(set! *divide-out-terms* #f)
#| #t |#

(define (bridge Z1 Z2 Z3 Z4)
  (parallel-series (l-network Z1 Z2)
		   (cascade (l-network Z3 Z4)
			    (polarity-reverse))))
#| bridge |#

((impedance
  (terminate 
   (bridge (resistor 'R_1)
	   (resistor 'R_2)
	   (resistor 'R_3)
	   (resistor 'R_4))
   (capacitor 'C)))
 's)

((impedance
  (terminate 
   (bridge (resistor 'R_1)
	   (resistor 'R_2)
	   (resistor 'R_3)
	   (resistor 'R_4))
   (resistor 'R_5)))
 's)
#|
(/
 (+ (* R_1 R_2 R_3)
    (* R_1 R_2 R_4)
    (* R_1 R_3 R_4)
    (* R_1 R_3 R_5)
    (* R_1 R_4 R_5)
    (* R_2 R_3 R_4)
    (* R_2 R_3 R_5)
    (* R_2 R_4 R_5))
 (+ (* R_1 R_2)
    (* R_1 R_4)
    (* R_1 R_5)
    (* R_2 R_3)
    (* R_2 R_5)
    (* R_3 R_4)
    (* R_3 R_5)
    (* R_4 R_5)))
|#
;;; Correct


((impedance
  (terminate-open
   (bridge (resistor 'R_1)
	   (resistor 'R_2)
	   (resistor 'R_3)
	   (resistor 'R_4))))
 's)
#|
(/ (+ (* R_1 R_3) (* R_1 R_4) (* R_2 R_3) (* R_2 R_4)) (+ R_1 R_2 R_3 R_4))
|#
;;;Correct

((impedance
  (terminate-short
   (bridge (resistor 'R_1)
	   (resistor 'R_2)
	   (resistor 'R_3)
	   (resistor 'R_4))))
 's)
#|
(/ (+ (* R_1 R_2 R_3) (* R_1 R_2 R_4) (* R_1 R_3 R_4) (* R_2 R_3 R_4))
   (+ (* R_1 R_2) (* R_1 R_4) (* R_2 R_3) (* R_3 R_4)))
|#
;;;Correct


