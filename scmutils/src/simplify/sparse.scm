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

;;;; Sparse flat form support for interpolation and gcd.

(declare (usual-integrations))


;;; These algorithms work on the termlists of the flat multivariate
;;; polynomial form that is defined in the file fpf.  A flat sparse
;;; polynomial is a list of terms, each of which is the cons of an
;;; exponent list and a coefficient.  This package uses this format,
;;; but attempts to stand on its own: it does not use the fpf:xxx
;;; definitions.  This produces redundant code, but it is easier to
;;; work with the independent mechanism this way.


(define (sparse-exponents term) (car term))
(define (sparse-coefficient term) (cdr term))
(define (sparse-term exponents coefficient)
  (cons exponents coefficient))

(define (sparse-constant-term? term)
  (for-all? (sparse-exponents term) zero?))

(define (sparse-univariate? p)
  (and (pair? p)
       (fix:= (length (sparse-exponents (car p)))
	      1)))

(define (sparse-constant? p)
  (and (fix:= (length p) 1)
       (sparse-constant-term? (car p))))

(define (sparse-one-term? t)
  (and (sparse-constant-term? t)
       (= (sparse-coefficient t) 1)))

(define (sparse-one? p)
  (and (sparse-constant? p)
       (= (sparse-coefficient (car p)) 1)))

(define (sparse-zero? p)
  (null? p))

(define (sparse-zero-term? t)
  (and (sparse-constant-term? t)
       (= (sparse-coefficient t) 0)))

(define (sparse-constant-term arity-n constant)
  (sparse-term (make-list arity-n 0) constant))

(define (sparse-one arity-n)
  (list (sparse-constant-term arity-n 1)))


(define (sparse-identity-term arity-n varnum)
  (sparse-term (generate-list arity-n
			      (lambda (i)
				(if (fix:= i varnum) 1 0)))
	       1))

(define (sparse-linear arity-n varnum root)
  (if (zero? root)
      (list (sparse-identity-term arity-n varnum))
      (list (sparse-identity-term arity-n varnum)
	    (sparse-constant-term arity-n (- root)))))


(define (sparse-term-> t1 t2)
  (sparse:>exponents? (sparse-exponents t1)
		      (sparse-exponents t2)))


;;; Graded Lexicographical Order

(define (sparse:>exponents? fs1 fs2)
  (let ((o1 (reduce fix:+ 0 fs1))
	(o2 (reduce fix:+ 0 fs2)))
    (cond ((fix:> o1 o2) #t)
	  ((fix:< o1 o2) #f)
	  (else
	   (let lp ((l1 fs1) (l2 fs2))
	     (cond ((null? l1) #f)
		   ((null? l2) #t)
		   ((fix:> (car l1) (car l2)) #t)
		   ((fix:< (car l1) (car l2)) #f)
		   (else (lp (cdr l1) (cdr l2)))))))))

#|
;;; Lexicographical Order

(define (sparse:>exponents? fs1 fs2)
  (let lp ((l1 fs1) (l2 fs2))
    (cond ((null? l1) #f)
	  ((null? l2) #t)
	  ((fix:> (car l1) (car l2)) #t)
	  ((fix:< (car l1) (car l2)) #f)
	  (else (lp (cdr l1) (cdr l2))))))
|#

(define (sparse-normalize poly term)
  (if (or (and (number? term) (= term 1))
	  (sparse-one-term? term))
      poly
      (map (lambda (pterm)
	     (sparse-term
	      (map - (sparse-exponents pterm) (sparse-exponents term))
	      (/ (sparse-coefficient pterm) (sparse-coefficient term))))
	   poly)))

(define (sparse-scale poly term)
  (if (or (and (number? term) (= term 1))
	  (sparse-one-term? term))
      poly
      (map (lambda (pterm)
	     (sparse-term
	      (map + (sparse-exponents pterm) (sparse-exponents term))
	      (* (sparse-coefficient pterm) (sparse-coefficient term))))
	   poly)))

(define (sparse-negate-term t)
  (sparse-term (sparse-exponents t) (- (sparse-coefficient t))))

(define (sparse-add xlist ylist)
  (let tloop ((xlist xlist) (ylist ylist))
    (cond ((null? xlist) ylist)
	  ((null? ylist) xlist)
	  (else
	   (let ((e1 (sparse-exponents (car xlist)))
		 (e2 (sparse-exponents (car ylist))))
	     (cond ((equal? e1 e2)
		    (let ((ncoeff (+ (sparse-coefficient (car xlist))
				     (sparse-coefficient (car ylist)))))
		      (if (= ncoeff 0)
			  (tloop (cdr xlist) (cdr ylist))
			  (cons (sparse-term e1 ncoeff)
				(tloop (cdr xlist) (cdr ylist))))))
		   ((sparse:>exponents? e1 e2)
		    (cons (car xlist) (tloop (cdr xlist) ylist)))
		   (else
		    (cons (car ylist) (tloop xlist (cdr ylist))))))))))

(define (sparse-multiply xlist ylist)
  (let lp ((xlist xlist))
    (if (null? xlist)
	'()
	(sparse-add (sparse-multiply-term (car xlist) ylist)
		    (lp (cdr xlist))))))

(define (sparse-multiply-term t x)
  (let ((exponents (sparse-exponents t))
	(coeff (sparse-coefficient t)))
    (map (lambda (term)
	   (sparse-term (map + exponents (sparse-exponents term))
			(* coeff (sparse-coefficient term))))
	 x)))

(define (sparse-abs p)
  (if (null? p)
      '()
      (if (let ((c (sparse-coefficient (car p))))
	    (and (real? c) (< c 0)))
	  (map (lambda (term)
		 (sparse-term (sparse-exponents term)
			      (- (sparse-coefficient term))))
	       p)
	  p)))

(define (sparse-divide numerator-terms denominator-terms cont)
  (let ((dexps (sparse-exponents (car denominator-terms)))
	(dcoef (sparse-coefficient (car denominator-terms))))
    (define (dloop nterms cont)
      (if (null? nterms)
	  (cont '() '())
	  (let ((nexps (sparse-exponents (car nterms)))
		(ncoef (sparse-coefficient (car nterms))))
	    (cond ((&and (map >= nexps dexps)) ;monomial-divisible?
		   (let ((qt (sparse-term (map - nexps dexps)
					  (/ ncoef dcoef))))
		     (dloop
		      (sparse-add (cdr nterms)
		        (sparse-multiply-term (sparse-negate-term qt)
			  (cdr denominator-terms)))
		      (lambda (q r)
			(cont (sparse-add (list qt) q) r)))))
		  (else
		   (dloop (cdr nterms)
			  (lambda (q r)
			    (cont q
				  (sparse-add (list (car nterms))
						    r)))))))))
    (dloop numerator-terms cont)))


(define (sparse-divisible? n d)
  (null? (sparse-divide n d (lambda (q r) r))))

#|
;;; This was a bad idea.  It actually gives wrong answers:
;;; (x+1)/3 = 1/3 x + 1/3 but for x=6 7/3 is not integer divisible.

(define *heuristic-sparse-divisible-enabled* #t)

;;; Effectiveness Statistics
(define *heuristic-sparse-divisible-win* 0)
(define *heuristic-sparse-divisible-lose* 0)
(define *heuristic-sparse-divisible-bad-decision* 0)
(define *heuristic-sparse-divisible-testing* #t)

(define (sparse-divisible? n d)
  (if *heuristic-sparse-divisible-enabled*
      (let ((m (length (sparse-exponents (car n)))))
	(let ((na (sparse-evaluate n (generate-list m interpolate-random)))
	      (da (sparse-evaluate d (generate-list m interpolate-random))))
	  (if (and (integer? na) (integer? da) (zero? (remainder na da)))
	      (let ((val (null? (sparse-divide n d (lambda (q r) r)))))
		(set! *heuristic-sparse-divisible-lose*
		      (+ *heuristic-sparse-divisible-lose* 1))
		(if (not val)
		    (set! *heuristic-sparse-divisible-bad-decision*
			  (+ *heuristic-sparse-divisible-bad-decision* 1)))
		val)
	      (begin
		(set! *heuristic-sparse-divisible-win*
		      (+ *heuristic-sparse-divisible-win* 1))
		(if *heuristic-sparse-divisible-testing*
		    (let ((val (null? (sparse-divide n d (lambda (q r) r)))))
		      (if val
			  (begin (bkpt "Wrong answer! sparse-divisible")
				 val)
			  val))
		    #f)))))
      (null? (sparse-divide n d (lambda (q r) r)))))
|#

(define (fpf:->sparse p)
  (fpf:terms p))

#|
(define (divide-test q d r)
  (let ((pq (fpf:expression-> q (lambda (p v) p)))
	(pd (fpf:expression-> d (lambda (p v) p)))
	(pr (fpf:expression-> r (lambda (p v) p))))
    (let ((pn (fpf:+ (fpf:* pq pd) pr))
	  (sq (fpf:->sparse pq))
	  (sr (fpf:->sparse pr)))
      (let ((sn (fpf:->sparse pn))
	    (sd (fpf:->sparse pd)))
	;; n = q*d + r
	(sparse-divide sn sd
		       (lambda (q r)
			 (pp `((sn ,sn) = (q ,q) * (d ,sd) + (r ,r)))
			 (if (and (equal? q sq) (equal? r sr))
			     (pp #t)
			     (pp `((sq ,sq) (sr ,sr))))))))))
|#

;;; Evaluation of polynomials at argument lists.

(define (sparse-evaluate p x)
  (if (null? p)
      0
      (begin
	(assert (fix:= (length x)
		       (length (sparse-exponents (car p)))))
	(apply +
	       (map (lambda (term)
		      (* (sparse-coefficient term)
			 (apply *
				(map expt
				     x
				     (sparse-exponents term)))))
		    p)))))


;;; If x is smaller than the arity of p then the last vars are filled
;;; in by components of x, making a polynomial of lesser arity.

(define (sparse-evaluate> p x)
  (if (or (null? x) (null? p))
      p
      (let* ((n (length x))
	     (arity (length (sparse-exponents (car p))))
	     (narity (- arity n)))
	(sparse-combine-like-terms
	 (map (lambda (term)
		(sparse-term (list-head (sparse-exponents term) narity)
			     (* (sparse-coefficient term)
				(apply *
				       (map expt
					    x
					    (list-tail (sparse-exponents term)
						       narity))))))
	      p)))))

#|
(print-expression
 (sparse-evaluate>
  '(((2 3 0) . 3) ((1 1 1) . 1) ((0 0 1) . 4) ((0 0 0) . 1))
  '(y z)))
(((2) . (* 3 (expt y 3))) ((1) . (* y z)) ((0) . (* 4 z)) ((0) . 1))
|#

(define (sparse-evaluate< p x)
  (if (or (null? x) (null? p))
      p
      (let ((n (length x)))
	(sparse-combine-like-terms
	 (map (lambda (term)
		(sparse-term (list-tail (sparse-exponents term) n)
		  (* (sparse-coefficient term)
		     (apply *
			    (map expt
				 x
				 (list-head (sparse-exponents term)
					    n))))))
	      p)))))

#|
(print-expression
 (sparse-evaluate<
  '(((2 3 0) . 3) ((1 1 1) . 1) ((0 0 1) . 4) ((0 0 0) . 1))
  '(x y)))
(((1) . (+ 4 (* x y))) ((0) . (+ 1 (* 3 (expt x 2) (expt y 3)))))
|#

(define (sparse-combine-like-terms terms)
  (sparse-merge-adjacent-terms (sort terms sparse-term->)))

(define (sparse-merge-adjacent-terms terms)
  (cond ((null? terms)
	 '())
	((null? (cdr terms))
	 (if (= (sparse-coefficient (car terms)) 0)
	     '()
	     terms))
	((equal? (sparse-exponents (car terms))
		 (sparse-exponents (cadr terms)))
	 (let ((coeff (+ (sparse-coefficient (car terms))
			 (sparse-coefficient (cadr terms)))))
	   (if (= coeff 0)
	       (sparse-merge-adjacent-terms (cddr terms))
	       (sparse-merge-adjacent-terms
		(cons (sparse-term (sparse-exponents (car terms))
				   coeff)
		      (cddr terms))))))
	(else
	 (cons (car terms)
	       (sparse-merge-adjacent-terms (cdr terms))))))
