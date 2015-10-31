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


(define (delroot r l)
  (cond ((null? l) '())
	((= r (car l)) (cdr l))
	(else (cons (car l) (delroot r (cdr l))))))


(define (test test-roots)
  (let lp ((r1 test-roots)
	   (r2 (poly->roots (roots->poly test-roots)))
	   (maxd 0))
    (if (null? r1)
	(if (null? r2)
	    maxd
	    (error "wrong number of roots"))
	(if (null? r2)
	    (error "wrong number of roots")
	    (let ((rt1 (car r1)))
	      (let slp ((rs (cdr r2))
			(best (car r2))
			(d (magnitude (- rt1 (car r2)))))
		(if (null? rs)
		    (lp (cdr r1)
			(delroot best r2)
			(max maxd d))
		    (let ((dd (magnitude (- rt1 (car rs)))))
		      (if (< dd d)
			  (slp (cdr rs) (car rs) dd)
			  (slp (cdr rs) best d))))))))))
  

;(define roots->poly poly:roots->)
;(define poly->roots poly:->roots)

;(test '(0 -1 -2 -3 -4 -5 -6 -7 -8 -9 -10))
;Value: 6.160867371818313e-10
#| 7.534683987842072e-11 |#

;(test '(0 -1 1 -2 2))
;Value: 0.
#| 0. |#

;(test '(1 1 2))
;Value: 2.31818306528373e-16
#| 0. |#

;(test '(0 -1 1 -2 2 2 -2))
;Value: 1.3885371471200463e-16
#| 0. |#

;(test '(1+1i 1-1i 1+1i 1-1i 0))
;Value: 0.
#| 0. |#

;(pp (poly->roots (roots->poly '(1+1i 1-1i 1+1i 1-1i 0))))
(0. 1.+1.i 1.+1.i 1.-1.i 1.-1.i)

(test '(1+1i 1-1i 1+1i 1-1i 0 1+1i 1-1i 1+1i 1-1i 0))
;Value: 3.1086244689504383e-15
#| 0. |#

(pp (poly->roots (roots->poly '(1+1i 1-1i 1+1i 1-1i 0 1+1i 1-1i 1+1i 1-1i 0))))
#;
(0. 0. 1.-1.i 1.-1.i 1.-1.i 1.-1.i 1.+1.i 1.+1.i 1.+1.i 1.+1.i)


(poly->roots '(*dense* 1 1 -70 900 27000 -540000))
;Value (-20. 29.99999603453493+6.3846721169876244e-6i 29.999996463041075-6.633756128672598e-6i 30.000007502423998+2.490839936134864e-7i)
#| (-20. 30. 30. 30.) |#



(pp (poly->roots '(*sparse* 1 (12 . 1) (0 . 1))))
#;
(-.7071067811865476+.7071067811865472i
 -.7071067811865476-.7071067811865472i
 .7071067811865474-.7071067811865474i
 .7071067811865474+.7071067811865474i
 -.25881904510252063+.9659258262890684i
 -.25881904510252063-.9659258262890684i
 .2588190451025208-.9659258262890684i
 .2588190451025208+.9659258262890684i
 .9659258262890684+.2588190451025208i
 .9659258262890684-.2588190451025208i
 -.9659258262890684+.25881904510252096i
 -.9659258262890684-.25881904510252096i)
;No value

;(pp (poly->roots '(*sparse* 1 (13 . 1) (0 . 1))))
(-.8854560256532096+.4647231720437684i
 -.8854560256532096-.4647231720437684i
 -.5680647467311558+.8229838658936562i
 -.5680647467311558-.8229838658936562i
 .7485107481711011+.6631226582407951i
 .7485107481711011-.6631226582407951i
 -1.
 .9709418174260519+.23931566428755768i
 .9709418174260519-.23931566428755768i
 -.12053668025532321+.9927088740980542i
 -.12053668025532321-.9927088740980542i
 .35460488704253573+.9350162426854149i
 .35460488704253573-.9350162426854149i)
;No value

;Wilkinson's Polynomial
;(test '(1 2 3 4 5 6 7 8 9 10 11 12 13 14 15 16 17 18 19 20))
;Value: .01446231788755453
#| 8.232881514148367e-3 |#

;Jenkens-Traub #2
(test '(.025+.035i .025-.035i
        -.04+.03i  -.04-.03i
	.27+.37i .27-.37i
	-.4+.3i  -.4-.3i
	2.9+3.9i 2.9-3.9i
	-4+3i    -4-3i
	10+2i    10-2i
	-20 20 30 30 30))
;Value: 3.232969447708456e-13
#| 1.1013412404281553e-13 |#

(pp
 (poly->roots
  (roots->poly
   '(.025+.035i .025-.035i
     -.04+.03i  -.04-.03i
      .27+.37i .27-.37i
      -.4+.3i  -.4-.3i
      2.9+3.9i 2.9-3.9i
      -4+3i    -4-3i
      10+2i    10-2i
      -20 20 30 30 30))))

;Henrici's root-separation test.
; Setting cluster-tolerance to 2000 makes e=2^-46 separable 
; but e=2^-47 appear multiple.
(define (P e) `(*sparse* 1 (2 . 1) (1 . -1) (0 . ,(+ 1/4 e))))

(poly->roots (P (expt 2 -46)))
#|
(.4999999999778032+1.1916012606664697e-7i .500000000022219-1.1916014026578431e-7i)
|#

(poly->roots (P (expt 2 -47)))
#| (.5 .5) |#


