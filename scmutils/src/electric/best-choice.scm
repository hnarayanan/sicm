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

(define (vd-choices goal-ratio minR maxR set tolerance)
  (define (parallel r1 r2)
    (/ (* r1 r2) (+ r1 r2)))
  (define (vd-ratio r1 r2)
    (/ r2 (+ r1 r2)))
  (define (acceptable-relative-error candidate)
    (let ((achieved (car candidate)))
      (<= (/ (abs (- goal-ratio achieved)) goal-ratio)
	  tolerance)))
  (define (acceptable-resistance candidate)
    (<= minR (parallel (cadr candidate) (caddr candidate)) maxR))
  (let ((all-candidates
	 (apply append
		(map (lambda (r1)
		       (map (lambda (r2)
			      (list (vd-ratio r1 r2) r1 r2))
			    set))
		     set))))
    #;
    (pp (list (length all-candidates)
	      (length (filter acceptable-relative-error
			      all-candidates))
	      (length (filter acceptable-resistance
			      all-candidates))
	      (length (filter acceptable-resistance
			      (filter acceptable-relative-error
				      all-candidates)))))
    (filter acceptable-resistance
	    (filter acceptable-relative-error
		    all-candidates))))

#|
(vd-choices (/ 17.5 50) 10 30 E12 0.05)
(1296 30 224 5)
#|
((.35294117647058826 33. 18.) (.36065573770491804 39. 22.)
                              (.36486486486486486 47. 27.)
                              (.3644859813084112 68. 39.)
                              (.3643410852713178 82. 47.))
|#

(vd-choices (/ 17.5 50) 10 30 E12 0.01)
(1296 8 224 1)
#|
((.35294117647058826 33. 18.))
|#
|#

(define (best-choice:voltage-divider goal-ratio min-impedance max-impedance resistor-choices tolerance)
  (define (ratio R1 R2)
    (/ R2 (+ R1 R2)))
  (define (parallel R1 R2)
    (/ (* R1 R2) (+ R1 R2)))
  (define (relative-error goal achieved)
    (abs (/ (- goal achieved) goal)))
  (car
   (filter (lambda (suggestion)
	     (let ((R1 (cadr suggestion)) (R2 (caddr suggestion)))
	       (< min-impedance (parallel R1 R2) max-impedance)))
	   (sort 
	    (append-map
	     (lambda (R1)
	       (map (lambda (R2)
		      (let ((e1 (list (+ R1 (* tolerance R1))
				      (- R1 (* tolerance R1))))
			    (e2 (list (+ R2 (* tolerance R2))
				      (- R2 (* tolerance R2)))))
			(let ((nominal (ratio R1 R2))
			      (deviants (list (ratio (car e1) (car e2))
					      (ratio (car e1) (cadr e2))
					      (ratio (cadr e1) (car e2))
					      (ratio (cadr e1) (cadr e2)))))
			  
			  (list (relative-error goal-ratio nominal)
				R1 R2
				nominal
				(apply max
				       (map (lambda (r) (relative-error goal-ratio r))
					    deviants))
				(apply max deviants)
				(apply min deviants)
				deviants))))							      
		    resistor-choices))
	     resistor-choices)
	    (lambda (x y)
	      (cond ((< (car x) (car y)) #t)
		    ((= (car x) (car y))
		     (< (cadr x) (cadr y)))
		    (else #f)))))))

#|
;;; The following is from parts.scm
(define :E12				;10% resistors
  '(1.0 1.2 1.5
    1.8 2.2 2.7
    3.3 3.9 4.7
    5.6 6.8 8.2))

(define :E12x10 (map (lambda (x) (* 10 x)) :E12))
#| :E12x10 |#

(define :E12x100 (map (lambda (x) (* 100 x)) :E12))

(define :E12x1000 (map (lambda (x) (* 1000 x)) :E12))

(define E12 (append :E12 :E12x10 :E12x100 :E12x1000))
#| E12 |#

(best-choice:voltage-divider 1/5 10 30 E12 .1)
#|
(4.6511627906976744e-2 68. 18. .20930232558139536 .22222222222222227)
|#

(best-choice:voltage-divider 1/4 10 30 E12 .1)
#|
(7.518796992481258e-3 100. 33. .24812030075187969 .14964370546318273)
|#

(best-choice:voltage-divider 1/3 10 30 E12 .1)
#|
(.01980198019801971 68. 33. .32673267326732675 .14736842105263154)
|#

(best-choice:voltage-divider 1/2 10 30 E12 .1)
#|
(0. 22. 22. .5 .09999999999999998)
|#

(best-choice:voltage-divider 3/4 10 30 E12 .1)
#|
(2.5062656641603454e-3 33. 100. .7518796992481203 .04988123515439424)
|#




(best-choice:voltage-divider 0.2 10 30 E12 .1)
#|
(4.6511627906976744e-2 68. 18. .20930232558139536 .22222222222222227)
|#

(best-choice:voltage-divider 0.25 10 30 E12 .1)
#|
(7.518796992481258e-3 100. 33. .24812030075187969 .14964370546318273)
|#

(best-choice:voltage-divider 0.3 10 30 E12 .1)
#|
(4.1666666666666706e-2 33. 15. .3125 .19047619047619035)
|#

(best-choice:voltage-divider 0.35 10 30 E12 .1)
#|
(8.403361344537945e-3 33. 18. .35294117647058826 .142857142857143)
|#

(best-choice:voltage-divider 0.4 10 30 E12 .1)
#|
(0. 27. 18. .4 .12244897959183668)
|#

|#

