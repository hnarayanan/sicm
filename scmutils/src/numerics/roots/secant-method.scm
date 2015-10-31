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

;;; Secant method

(define (secant-method f guess1 guess2 argepsilon valepsilon zeroepsilon maxiter)
  (define (secant-iteration pk-1 fpk-1 pk fpk count)
    ;;(pp (list pk-1 fpk-1 pk fpk))
    (cond ((close-enuf? pk-1 pk argepsilon) pk)
	  ((< (magnitude fpk) valepsilon) pk)
	  ((> count maxiter)
	   `(maxiter: ,pk-1 ,fpk-1 ,pk fpk count))

	  ((< (magnitude (- fpk-1 fpk)) zeroepsilon)
	   (let* ((pk+1 (/ (+ pk pk-1) 2))
		  (fpk+1 (f pk+1)))
	     (secant-iteration pk fpk pk+1 fpk+1 (+ count 1))))
	  (else
	   (let* ((pk+1
		   (- pk
		      (* (- pk pk-1)
			 (/ fpk (- fpk fpk-1)))))
		  (fpk+1 (f pk+1)))
	     (secant-iteration pk fpk pk+1 fpk+1 (+ count 1))))))	   
  (secant-iteration guess1 (f guess1) guess2 (f guess2) 0))

#|
(secant-method cos -1. 2. 1e-15 1e-30 1e-30 100)
#| 1.5707963267948966 |#

(secant-method cube -1. 2. 1e-15 1e-30 1e-30 100)
#| -9.344250202828578e-11 |#

(secant-method cube 1. 2. 1e-15 1e-30 1e-30 100)
#| 9.273573199032126e-11 |#

(secant-method square -1. 3. 1e-17 1e-30 1e-30 100)
#| -6.35401500343362e-16 |#

(secant-method square -1. 2. 1e-17 1e-30 1e-30 100)
#| 0. |#

(secant-method square 1. 2. 1e-17 1e-30 1e-30 100)
#| 6.854008160388252e-16 |#

;;; It even works for complex roots.  
;;;   But must start with a complex guess.
(secant-method (+ cube 1) 1+i 2. 1e-17 1e-30 1e-30 100)
#| .5+.8660254037844387i |#
|#
