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

;;; Sparse divide of a numerator with multiple denominators.
;;;   Assumes that the terms are monomial ordered.

(define (fpf:divide-multiple dividend divisors)
  (let ((divisors-terms (map fpf:terms divisors)))
    (let num-lp
	((dividend-terms (fpf:terms dividend))
	 (quotients-terms (make-list (length divisors-terms) '()))
	 (remainder-terms '()))
      (if (null? dividend-terms)
	  (list (map fpf:make quotients-terms)
		(fpf:make remainder-terms))
	  (let div-lp
	      ((i 0)
	       (divisors-terms divisors-terms)
	       (qs-terms quotients-terms))
	    (cond ((null? divisors-terms)
		   (num-lp (cdr dividend-terms)
			   quotients-terms
			   (fpf:add-terms (list (car dividend-terms))
					  remainder-terms)))
		  ((fpf:monomial-divides? (car (car divisors-terms))
					  (car dividend-terms))
		   (let ((mq
			  (list
			   (fpf:monomial-quotient (car dividend-terms)
						  (car (car divisors-terms))))))
		     (num-lp (fpf:add-terms (fpf:mul-terms (fpf:negate-terms mq)
							   (car divisors-terms))
					    dividend-terms)
			     (list-with-substituted-coord quotients-terms i 
			       (fpf:add-terms mq (car qs-terms)))
			     remainder-terms)))
		  (else
		   (div-lp (+ i 1)
			   (cdr divisors-terms)
			   (cdr qs-terms)))))))))

(define (fpf:monomial-divides? m1 m2)
  (&and (map <= (fpf:exponents m1) (fpf:exponents m2))))

(define (fpf:monomial-quotient m1 m2)
  (fpf:make-term (map int:- (fpf:exponents m1) (fpf:exponents m2))
		 (/ (fpf:coefficient m1) (fpf:coefficient m2))))


#|
(fpf:divide-multiple
 (fpf:expression-> '(+ (* x x y) (* x y y) (* y y)) (lambda (p v) p))
 (list
   (fpf:expression-> '(- (* x y) 1) (lambda (p v) p))
   (fpf:expression-> '(- (* y y) 1 (* 0 x)) (lambda (p v) p))))
;Value: (((*fpf* ((1 0) . 1) ((0 1) . 1))
          1)
         (*fpf* ((1 0) . 1) ((0 1) . 1) ((0 0) . 1)))

(fpf:divide-multiple
 (fpf:expression-> '(+ (* x x y) (* x y y) (* y y)) (lambda (p v) p))
 (list
   (fpf:expression-> '(- (* y y) 1 (* 0 x)) (lambda (p v) p))
   (fpf:expression-> '(- (* x y) 1) (lambda (p v) p))))
;Value: (((*fpf* ((1 0) . 1) ((0 0) . 1))
          (*fpf* ((1 0) . 1)))
         (*fpf* ((1 0) . 2) ((0 0) . 1)))

|#

(define (fpf:s-poly g1 g2)
  (assert (and (not (zero? g1)) (not (zero? g2))))
  (let ((g1t (fpf:terms g1)) (g2t (fpf:terms g2)))
    (let ((m1 (car g1t)) (m2 (car g2t)))
      (let ((e1 (fpf:exponents m1)) (e2 (fpf:exponents m2)))
	(let ((d (map min e1 e2)))	;exponents of gcd(t1,t2)
	  (let ((es1 (map - e2 d)) (es2 (map - e1 d)))
	    (fpf:make
	     (fpf:add-terms
	      (fpf:mul-terms (list (fpf:make-term es1
						  (fpf:coefficient m2)))
			     g1t)
	      (fpf:mul-terms (list (fpf:make-term es2
						  (- (fpf:coefficient m1))))
			     g2t)))))))))

#|
(define f
  (fpf:expression-> '(+ (* (expt x 3) (expt y 2))
			(* -1 (expt x 2) (expt y 3))
			x)
		    (lambda (p v) p)))

(define g
  (fpf:expression-> '(+ (* 3 (expt x 4) y) (expt y 2))
		    (lambda (p v) p)))

(pp (fpf:->expression (fpf:s-poly f g) '(x y)))
(+ (* -3 (expt x 3) (expt y 3)) (* -1 (expt y 3)) (* 3 (expt x 2)))
|#

;;; Given a basis for an ideal makes a monic reduced basis that
;;; generates the same ideal.

;;; This algorithm is from Becker & Weispfenning, p203.

(define (fpf:reduction P)
  (let lp ((Q P))
    (let ((NQ
	   (filter (lambda (q) (not (zero? q)))
		   (map (lambda (p)
			  (cadr (fpf:divide-multiple p (delete p Q))))
			Q))))
      (if (equal? Q NQ)
	  (map fpf:monicize Q)
	  (lp NQ)))))

(define (fpf:monicize p)
  (if (zero? p)
      (error "Zero cannot be made monic -- fpf:monicize")
      (fpf:scale (/ 1 (fpf:coefficient (car (fpf:terms p)))) p)))


;;; Tests a set of polynomials.  Returns #t if it is a Grobner Basis, #f if not.

;;; This algorithm is from Becker & Weispfenning, p214.

(define (fpf:grobnertest G)
  (let lp ((B (distinct-pairs G)))
    (if (null? B)
	#t
	(let* ((g1g2 (car B))
	       (g1 (car g1g2))
	       (g2 (cadr g1g2))
	       (h (fpf:s-poly g1 g2))
	       (qsr (fpf:divide-multiple h G))
	       (h0 (cadr qsr)))
	  (if (zero? h0)
	      (lp (cdr B))
	      #f)))))

;;; F is a list of polynomials.  Returns a Grobner basis, such that
;;; the ideal generated by F is the same as the ideal generated by G.

;;; This algorithm is from Becker & Weispfenning, p214.

(define (fpf:grobner F)
  (let lp ((G F) (B (distinct-pairs F)))
    (if (null? B)
	G
	(let* ((g1g2 (car B))
	       (g1 (car g1g2))
	       (g2 (cadr g1g2))
	       (h (fpf:s-poly g1 g2))
	       (qsr (fpf:divide-multiple h G))
	       (h0 (cadr qsr)))
	  (if (zero? h0)
	      (lp G (cdr B))
	      (lp (list-union G (list h0))
		  (list-union (cdr B)
			      (map (lambda (x)
				     (list x h0))
				   G))))))))

#|
(define f1 
  (fpf:expression-> '(+ (expt x 3) (* -2 x y))
		    (lambda (p v) p)))

(define f2
  (fpf:expression-> '(+ (* (expt x 2) y) (* -2 (expt y 2)) x)
		    (lambda (p v) p)))

(pp (map (lambda (p) (fpf:->expression p '(x y)))
	 (fpf:grobner (list f1 f2))))
((+ (expt x 3) (* -2 x y))
 (+ (* (expt x 2) y) (* -2 (expt y 2)) x)
 (* -1 (expt x 2))
 (* 2 x y)
 (+ (* 2 (expt y 2)) (* -1 x)))


(pp (map (lambda (p) (fpf:->expression p '(x y)))
	 (fpf:reduction (fpf:grobner (list f1 f2)))))
((expt x 2)
 (* x y)
 (+ (expt y 2) (* -1/2 x)))
|#

#|
;;; The following algorithm also passes the test.

(define (fpf:grobner F)
  (let lp ((G F))
    (let ((Gp
	   (list-union G
	    (reduce list-union '()
		    (map-distinct-pairs
		     (lambda (p q)
		       (let* ((h (fpf:s-poly p q))
			      (qsr (fpf:divide-multiple h G))
			      (h0 (cadr qsr)))
			 (if (zero? h0)
			     '()
			     (list h0))))
		     G)))))
      (if (= (length G) (length Gp))
	  G
	  (lp Gp)))))
	  

(pp (map (lambda (p) (fpf:->expression p '(x y)))
	 (fpf:grobner (list f1 f2))))
((+ (expt x 3) (* -2 x y))
 (+ (* (expt x 2) y) (* -2 (expt y 2)) x)
 (* -1 (expt x 2))
 (* 2 x y)
 (+ (* 2 (expt y 2)) (* -1 x)))
|#

;;; Starting from expressions...

(define (grobner expressions #!optional order)
  (define (several . exps) `(several ,@exps))
  (fluid-let ((fpf:>exponents?
	       (if (default-object? order)
		   fpf:graded>
		   order))
	      (fpf:operator-table
	       (cons `(several ,several)
		     fpf:operator-table))
	      (fpf:operators-known
	       (cons 'several fpf:operators-known)))
    (fpf:expression-> (cons 'several expressions)
		      (lambda (ps vs)
			(map (lambda (p) (fpf:->expression p vs))
			     (fpf:grobner (cdr ps)))))))

#|
(pp (grobner (list '(+ (expt x 3) (* -2 x y))
		   '(+ (* (expt x 2) y) (* -2 (expt y 2)) x))))
((+ (expt x 3) (* -2 x y))
 (+ (* (expt x 2) y) (* -2 (expt y 2)) x)
 (* -1 (expt x 2))
 (* 2 x y)
 (+ (* 2 (expt y 2)) (* -1 x)))

(pp (grobner (list '(+ (expt x 3) (* -2 x y))
		   '(+ (* (expt x 2) y) (* -2 (expt y 2)) x))
	     fpf:lexicographical>))
((+ (expt x 3) (* -2 x y))
 (+ (* (expt x 2) y) x (* -2 (expt y 2)))
 (* -1 (expt x 2))
 (* 2 x y)
 (+ (* -1 x) (* 2 (expt y 2)))
 (* -4 (expt y 3)))

(pp (grobner (list '(+ (expt x 2) (expt y 2) (expt z 2) -1)
		   '(+ (expt x 2) (expt z 2) (- y))
		   '(- x z))))
((+ -1 (expt x 2) (expt y 2) (expt z 2))
 (+ (expt x 2) (expt z 2) (* -1 y))
 (+ x (* -1 z))
 (+ -1 (expt y 2) y)
 (+ (* 2 (expt z 2)) (* -1 y)))

(pp (grobner (list '(+ (expt x 2) (expt y 2) (expt z 2) -1)
		   '(+ (expt x 2) (expt z 2) (- y))
		   '(- x z))
	     fpf:lexicographical>))
((+ -1 (expt x 2) (expt y 2) (expt z 2))
 (+ (expt x 2) (* -1 y) (expt z 2))
 (+ x (* -1 z))
 (+ -1 (expt y 2) y)
 (+ (* -1 y) (* 2 (expt z 2)))
 (+ 1 (* -4 (expt z 4)) (* -2 (expt z 2))))
|#
	  
(define (reduced-grobner expressions #!optional order)
  (define (several . exps) `(several ,@exps))
  (fluid-let ((fpf:>exponents?
	       (if (default-object? order)
		   fpf:graded>
		   order))
	      (fpf:operator-table
	       (cons `(several ,several)
		     fpf:operator-table))
	      (fpf:operators-known
	       (cons 'several fpf:operators-known)))
    (fpf:expression-> (cons 'several expressions)
		      (lambda (ps vs)
			(map (lambda (p) (fpf:->expression p vs))
			     (fpf:reduction (fpf:grobner (cdr ps))))))))

#|
(pp (reduced-grobner
     (list '(+ (expt x 2) (expt y 2) (expt z 2) -1)
	   '(+ (expt x 2) (expt z 2) (- y))
	   '(- x z))
     fpf:lexicographical>))
((+ x (* -1 z))
 (+ y (* -2 (expt z 2)))
 (+ -1/4 (expt z 4) (* 1/2 (expt z 2))))


(pp (reduced-grobner
     (list '(+ (* 3 (expt x 2)) (* 2 y z) (* -2 x lam))
	   '(+ (* 2 x z) (* -2 y lam))
	   '(+ (* 2 x y) (* -2 z) (* -2 z lam))
	   '(+ (expt x 2) (expt y 2) (expt z 2) -1))
     fpf:lexicographical>))
((+ -1
    (expt x 2)
    (expt y 2)
    (expt z 2))
 (+ (expt y 3)
    (* y (expt z 2))
    (* -1 y)
    (* -9216/3835 (expt z 5))
    (* 906/295 (expt z 3))
    (* -2562/3835 z))
 (+ (* x z)
    (* y (expt z 2))
    (* -1152/3835 (expt z 5))
    (* -108/295 (expt z 3))
    (* 2556/3835 z))
 (+ lam
    (* -3/2 x)
    (* -3/2 y z)
    (* -167616/3835 (expt z 6))
    (* 36717/590 (expt z 4))
    (* -134419/7670 (expt z 2)))
 (+ (* x y)
    (* -19584/3835 (expt z 5))
    (* 1999/295 (expt z 3))
    (* -6403/3835 z))
 (+ (* (expt y 2) z)
    (* -6912/3835 (expt z 5))
    (* 827/295 (expt z 3))
    (* -3839/3835 z))
 (+ (* y (expt z 3))
    (* -1 y z)
    (* -576/59 (expt z 6))
    (* 1605/118 (expt z 4))
    (* -453/118 (expt z 2)))
 (+ (expt z 7)
    (* -1763/1152 (expt z 5))
    (* 655/1152 (expt z 3))
    (* -11/288 z)))
|#

#|
(pp (reduced-grobner
     (list '(- (* R1 R2) (* 10000 (+ R1 R2)))
	   '(- R1 (* 1/3 (+ R1 R2))))
     fpf:lexicographical>))
((+ R1 (* -1/2 R2))
 (+ (expt R2 2) (* -30000 R2)))

(pp (reduced-grobner
     (list '(+ (* 3 x) (* 2 y) z -4)
	   '(+ (* 2 x) (* 2 y) (* -1 z) 5))
     fpf:lexicographical>))
((+ 23/2 y (* -5/2 z)))

(pp (reduced-grobner
     '( (+ (* (+ a b) (- a c)) c)
	(- 3 (+ a b)) )
     fpf:lexicographical>))
((+ a (* -2/3 c))
 (+ -3 b (* 2/3 c)))


(pp (reduced-grobner
     (list '(- (+ x y) 3)
	   '(- (- x y) 4) )
     fpf:lexicographical>))
((+ 1/2 y))
;;; Telling us that y = -1/2

(pp (grobner
     (list '(- (+ x y) 3)
	   '(- (- x y) 4) )
     fpf:lexicographical>))
((+ -3 x y)
 (+ -4 x (* -1 y))
 (+ 1 (* 2 y)))
;;; Preserving the original stuff.

(pp (reduced-grobner
     '( (- (expt t 4) x)
	(- (expt t 3) y)
	(- (expt t 2) z) )
     fpf:lexicographical>))
((+ (expt t 2) (* -1 z))
 (+ (* t y) (* -1 (expt z 2)))
 (+ x (* -1 (expt z 2)))
 (+ (* t z) (* -1 y))
 (+ (expt y 2) (* -1 (expt z 3))))
|#
