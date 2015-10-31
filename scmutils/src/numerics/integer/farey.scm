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

;;;;             Farey trees aka Stern-Brocot trees

;;; In a Farey tree each level is constructed by adjoining the
;;; mediants of the elements of the previous level into the previous
;;; level.  Because the mediants are always between the given
;;; fractions the levels are ordered if the first one is ordered.

(define (mediant m1/n1 m2/n2)
  (/ (+ (numerator m1/n1) (numerator m2/n2))
     (+ (denominator m1/n1) (denominator m2/n2))))

(define ((Farey lo hi) n)
  (let nlp ((n n))
    (if (int:= n 1)
	(list lo hi)
	(let llp ((in (nlp (int:- n 1)))
		  (out '()))
	  (if (null? (cdr in))
	      (reverse (cons (car in) out))
	      (llp (cdr in)
		   (cons (mediant (car in) (cadr in))
			 (cons (car in) out))))))))

#|
((Farey 0 1) 1)
;Value: (0 1)

((Farey 0 1) 2)
;Value: (0 1/2 1)

((Farey 0 1) 3)
;Value: (0 1/3 1/2 2/3 1)

((Farey 0 1) 4)
;Value: (0 1/4 1/3 2/5 1/2 3/5 2/3 3/4 1)

((Farey 0 1) 5)
;Value: (0 1/5 1/4 2/7 1/3 3/8 2/5 3/7 1/2 4/7 3/5 5/8 2/3 5/7 3/4 4/5 1)

...


;;; Note 1/0 is not a legit rational, but:

((Farey 0 (make-rational 1 0)) 4)
;Value: (0 1/3 1/2 2/3 1 3/2 2 3 1/0)

|#

;;; Each Farey tree level satisfies the following by construction.

(define (Farey-check sequence)
  (let lp ((n (length sequence)) (s sequence))
    (cond ((< n 3) 'ok)
	  ((= (mediant (car s) (caddr s)) (cadr s))
	   (lp (- n 1) (cdr s)))
	  (else 'lose))))

#|
(Farey-check ((Farey 0 1) 15))
;Value: ok
|#

;;; The Farey sequence of level n is the ordered list of fractions
;;; between 0 and 1 that have no denominators greater than n.

(define (Farey-sequence n)
  (filter (lambda (p/q) (not (> (denominator p/q) n)))
	  ((Farey 0 1) n)))

#|
(Farey-sequence 5)
;Value: (0 1/5 1/4 1/3 2/5 1/2 3/5 2/3 3/4 4/5 1)

(Farey-sequence 6)
;Value: (0 1/6 1/5 1/4 1/3 2/5 1/2 3/5 2/3 3/4 4/5 5/6 1)

(Farey-sequence 7)
;Value: (0 1/7 1/6 1/5 1/4 2/7 1/3 2/5 3/7 1/2 4/7 3/5 2/3 5/7 3/4 4/5 5/6 6/7 1)
|#	    


;;; The full Farey sequence is useful for understanding the
;;; convergents of continued fractions.  For any irrational the
;;; convergents are the closest numbers on the full Farey sequences.

(define (full-Farey-sequence n)
  (filter (lambda (p/q) (not (> (denominator p/q) n)))
	  ((Farey (make-rational 0 1) (make-rational 1 0)) n)))


;;; Even though the Farey sequence is derived by deleting members from
;;; a Farey tree level the Farey sequence satisfies the following:

#|
(Farey-check (Farey-sequence 7))
;Value: ok

(Farey-check (Farey-sequence 15))
;Value: ok

;;; That this is true is not obvious.
|#

;;; In the limit as n->infinity the length of the nth Farey sequence is

(define (Farey-length n)
  (* (/ 3 (square pi)) (square n)))

#|
(length (Farey-sequence 20))
;Value: 129  

(Farey-length 20)
;Value: 121.58542037080534

;;; Not bad.
|#

;;; Of course, the computation of the nth Farey sequence by filtering
;;; the nth level of the Farey tree is horribly inefficient.  There
;;; are better ways of computing the nth Farey sequence:

(define (Farey-sequence n)
  (let ((m (+ n 1)))
    (sort (a-reduce list-union
		    (map (lambda (q)
			   (a-reduce list-union
				     (map (lambda (p)
					    (if (> p q) '() (list (/ p q))))
					  (iota m))))
			 (cdr (iota m))))
	  <)))

(define (Lamothe-simplicity m/n)
  (/ 1 (* (numerator m/n) (denominator m/n))))

#|

;;; Theorem: The sums of the Lamothe-simplicities of the interior
;;; members of the nth Farey tree level from 0 to infinity is n-1.

(apply +
       (map Lamothe-simplicity
	    (cdr (butlast ((Farey 0 (make-rational 1 0)) 5)))))
;Value: 4

(apply +
       (map Lamothe-simplicity
	    (cdr (butlast ((Farey 0 (make-rational 1 0)) 6)))))
;Value: 5

(apply +
       (map Lamothe-simplicity
	    (cdr (butlast ((Farey 0 (make-rational 1 0)) 10)))))
;Value: 9
|#

;;; Given m/n to constuct its binary representation in the full Farey
;;; tree from 0 to infinity: 
;;;  ((Farey (make-rational 0 1) (make-rational 1 0)) n).


(define (Farey-encode m/n)
  (let lp ((m (numerator m/n))
	   (n (denominator m/n)))
    (cond ((int:= m n) '())
	  ((int:< m n)
	   (cons 'L (lp m (int:- n m))))
	  (else
	   (cons 'R (lp (int:- m n) n))))))

#|
(Farey-encode 2/7)
;Value: (L L L R)

(Farey-encode 5/8)
;Value: (L R L R)
|#

;;; For looking at the places of reals in the tree.

(define (Farey-encode-real x maxlevel)
  (cond ((or (= x 1) (int:= maxlevel 0)) '())
	((< x 1)
	 (cons 'L
	       (Farey-encode-real (/ x (- 1 x))
				  (int:- maxlevel 1))))
	(else
	 (cons 'R
	       (Farey-encode-real (- x 1)
				  (int:- maxlevel 1))))))

#|
(Farey-encode-real 5/8 10)
;Value: (L R L R)

(Farey-encode-real pi 20)
;Value: (R R R L L L L L L L R R R R R R R R R R)

(Farey-encode-real :phi 20)
;Value: (R L R L R L R L R L R L R L R L R L R L)
|#

(define (Farey-decode L/R-list)
  (let lp ((lst L/R-list)
	   (low (make-rational 0 1))
	   (hi  (make-rational 1 0)))
    (let ((x (mediant low hi)))
      (cond ((null? lst)
	     x)
	    ((eq? (car lst) 'L)
	     (lp (cdr lst) low x))
	    (else 
	     (lp (cdr lst) x hi)))))

#|
(Farey-decode (Farey-encode 3/7))
;Value: 3/7

(Farey-decode (Farey-encode 3/8))
;Value: 3/8


;;; Farey-encoding is not very efficient.

(Farey-decode (Farey-encode-real pi 25))
;Value: 355/113
|#

;;; The Farey encodings are closely related to the continued
;;; fractions.  For example, the continued fraction for pi is:
;;;  <3 7 15 1 292 ...>.          
;;; Its Farey encoding is:
;;;  (R^3 L^7 R^15 L^1 R^292 ... )