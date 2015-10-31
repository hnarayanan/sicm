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

;;;;         Multivariate Polynomial Functions -- GJS

;;; Polynomial functions are used to implement rational functions.
;;; These are used for simplification to Rational Canonical Form.

;;; Multivariate polynomial functions are not expressions in named
;;; indeterminates.  In fact, they are true functions -- they take
;;; arguments by position, without any presumed name for each
;;; argument. 

(declare (usual-integrations))

;;; The following procedures are mapped for coefficient arithmetic.
;;; Other implementation of coefficient arithmetic may be constructed
;;; by modifying this map.

(define-integrable base? number?)

(define-integrable base/zero :zero)
(define-integrable base/one :one)

(define-integrable base/zero? zero?)
(define-integrable base/one? one?)
(define-integrable base/negative? negative?)

(define-integrable base/negate -)
(define-integrable base/abs magnitude)

(define-integrable base/equal? =)

(define-integrable base/add +)
(define-integrable base/mul *)
(define-integrable base/sub -)

(define-integrable base/div scheme-number-divide)

(define-integrable base/gcd scheme-number-gcd)

(define-integrable base/expt expt)

(define-integrable poly/zero base/zero)

(define-integrable poly/one base/one)

(define-integrable (poly/zero? p)
  (and (base? p) (base/zero? p)))

(define-integrable (poly/one? p)
  (and (base? p) (base/one? p)))

(define (poly/make-identity arity)
  (poly/make-from-dense arity (list base/one base/zero)))

(define (poly/make-constant arity c)
  (if (not (fix:< (poly/arity c) arity))
      (error "Bad constant -- POLY/MAKE-CONSTANT" arity c))
  (poly/make-from-dense arity (list c)))

(define (poly/make-c*x^n arity c n)
  (poly/make-from-sparse arity (list (cons n c))))


(define (poly/identity? p)
  (if (base? p)
      #f
      (poly/equal? p poly/identity)))


(define (poly/monic? p)
  (if (base? p)
      (base/one? p)
      (poly/monic? (poly/leading-coefficient p))))

(define (poly/negative? p)
  (if (base? p)
      (and (real? p) (negative? p))
      (poly/negative? (poly/leading-coefficient p))))

(define (poly/equal? p1 p2)
  (cond ((and (base? p1) (base? p2)) (base/equal? p1 p2))
	((or (base? p1) (base? p2)) #f)
	(else
	 (let ((arity (poly/check-same-arity p1 p2)))
	   (and (fix:= (poly/degree p1) (poly/degree p2))
		(poly/equal? (poly/leading-coefficient p1)
			     (poly/leading-coefficient p2))
		(poly/equal? (poly/except-leading-term arity p1)
			     (poly/except-leading-term arity p2)))))))

(define (poly/extend n p)
  ;; Interpolates a variable at position n in polynomial p.
  (let* ((arity (poly/arity p))
	 (narity (if (fix:> n arity) (fix:+ n 1) (fix:+ arity 1))))
    (if (fix:= n 0)
	(poly/adjoin (fix:+ arity 1) 0 p poly/zero)
	(let lp ((p p))
	  (cond ((poly/zero? p) p)
		(else
		 (poly/adjoin narity
			      (poly/degree p)
			      (poly/extend (fix:- n 1)
					   (poly/leading-coefficient p))
			      (lp (poly/except-leading-term arity p)))))))))

(define (poly/contract n p)
  (if (poly/contractable? n p)
      (let ((arity (poly/arity p)))
	(if (fix:= n 0)
	    (poly/trailing-coefficient p)
	    (let lp ((p p))
	      (cond ((poly/zero? p) p)
		    (else
		     (poly/adjoin (fix:- arity 1)
				  (poly/degree p)
				  (poly/contract (fix:- n 1)
				    (poly/leading-coefficient p))
				  (lp
				   (poly/except-leading-term arity p))))))))
      (error "Poly not contractable" n p)))

(define (poly/contractable? n p)
  (or (base? p)
      (if (fix:= n 0)
	  (fix:= (poly/degree p) 0)
	  (and (fix:< n (poly/arity p))
	       (let lp ((p p))
		 (cond ((poly/zero? p) #t)
		       ((poly/contractable? (fix:- n 1)
					    (poly/leading-coefficient p))
			(poly/contractable? n
			  (poly/except-leading-term (poly/arity p) p)))
		       (else #f)))))))

(define (poly/make-vars arity)
  (if (fix:= arity 0)
      '()
      (let lp1 ((n 1) (l (list poly/identity)))
	(if (fix:= n arity)
	    l
	    (lp1 (fix:+ n 1)
		 (cons (poly/make-identity (fix:+ n 1))
		       (map (lambda (c)
			      (poly/extend 0 c))
			    l)))))))

;;; Functions can only be combined if they have the same arity.

(define (poly/check-same-arity p1 p2)
  (cond ((base? p1) (poly/arity p2))
	((base? p2) (poly/arity p1))
	((fix:= (poly/arity p1) (poly/arity p2))
	 (poly/arity p1))
	(else (error "Unequal arities -- POLY" p1 p2))))


(define (poly/add p1 p2)
  (cond ((and (base? p1) (base? p2)) (base/add p1 p2))
	((poly/zero? p1) p2)
	((poly/zero? p2) p1)
	(else
	 (let ((degree1 (poly/degree p1))
	       (degree2 (poly/degree p2))
	       (arity (poly/check-same-arity p1 p2)))
	   (cond ((fix:= degree1 degree2)
		  (let ((c (poly/add (poly/leading-coefficient p1)
				     (poly/leading-coefficient p2)))
			(s (poly/add (poly/except-leading-term arity p1)
				     (poly/except-leading-term arity p2))))
		    (if (poly/zero? c)
			s
			(poly/adjoin arity degree1 c s))))
		 ((fix:> degree1 degree2)
		  (poly/adjoin arity degree1
			       (poly/leading-coefficient p1)
			       (poly/add (poly/except-leading-term arity p1)
					 p2)))
		 (else			;(fix:< degree1 degree2)
		  (poly/adjoin arity degree2
			       (poly/leading-coefficient p2)
			       (poly/add p1
					 (poly/except-leading-term arity p2)))))))))

(define (poly/sub p1 p2)
  (cond ((and (base? p1) (base? p2)) (base/sub p1 p2))
	((poly/zero? p1) (poly/negate p2))
	((poly/zero? p2) p1)
	(else
	 (let ((degree1 (poly/degree p1))
	       (degree2 (poly/degree p2))
	       (arity (poly/check-same-arity p1 p2)))
	   (cond ((fix:= degree1 degree2)
		  (let ((c (poly/sub (poly/leading-coefficient p1)
				     (poly/leading-coefficient p2)))
			(s (poly/sub (poly/except-leading-term arity p1)
				     (poly/except-leading-term arity p2))))
		    (if (poly/zero? c)
			s
			(poly/adjoin arity degree1 c s))))
		 ((fix:> degree1 degree2)
		  (poly/adjoin arity degree1
			       (poly/leading-coefficient p1)
			       (poly/sub (poly/except-leading-term arity p1)
					 p2)))
		 (else			;(fix:< degree1 degree2)
		  (poly/adjoin arity degree2
			       (poly/negate (poly/leading-coefficient p2))
			       (poly/sub p1
					 (poly/except-leading-term arity p2)))))))))

(define (poly/negate p)
  (let ((arity (poly/arity p)))
    (cond ((base? p) (base/negate p))
	  (else (poly/adjoin arity (poly/degree p)
			     (poly/negate (poly/leading-coefficient p))
			     (poly/negate (poly/except-leading-term arity p)))))))

(define (poly/mul p1 p2)
  (cond ((and (base? p1) (base? p2)) (base/mul p1 p2))
	((poly/zero? p1) p1)
	((poly/zero? p2) p2)
	((poly/one? p1) p2)
	((poly/one? p2) p1) 
	(else
	 (let ((arity (poly/check-same-arity p1 p2))
	       (d (poly/degree p1))
	       (c (poly/leading-coefficient p1)))
	   (poly/add (let loop ((p p2))
		       (if (poly/zero? p)
			   p
			   (poly/adjoin arity
					(fix:+ d (poly/degree p))
					(poly/mul c (poly/leading-coefficient p))
					(loop (poly/except-leading-term arity p)))))
		     (poly/mul (poly/except-leading-term arity p1)
			       p2))))))

(define (poly/scale-1 arity p coeff)
  (let loop ((p p))
    (if (poly/zero? p)
	p
	(poly/adjoin arity (poly/degree p)
		     (poly/mul (poly/leading-coefficient p) coeff)
		     (loop (poly/except-leading-term arity p))))))

(define (poly/scale p coeff)
  (poly/scale-1 (poly/arity p) p coeff))


(define (poly/square p)
  (poly/mul p p))

(define (poly/expt base exponent)
  (define (expt-iter x count answer)
    (if (int:zero? count)
	answer
	(if (even? count)
	    (expt-iter (poly/square x) (int:quotient count 2) answer)
	    (expt-iter x (int:- count 1) (poly/mul x answer)))))
  (cond ((base? base) (base/expt base exponent))
	((not (exact-integer? exponent))
	 (error "Can only raise a PCF to an exact integer power" base exponent))
	((negative? exponent)
	 (error "No inverse (POLY/EXPT):" base exponent))
	((poly/one? base) base)
	((poly/zero? base)
	 (if (int:zero? exponent)
	     (error "0^0 -- POLY/EXPT"))
	 base)
	((int:zero? exponent) poly/one)
	(else
	 (expt-iter base exponent poly/one))))

;;; Division over a field.  
;;;  The following procedure takes two polynomials and calls its
;;;  continuation with the quotient and the remainder polynomials.
;;;
;;;     s^3 + 3s^2 + 4s + 5                      3
;;;    --------------------- = s^2 + 2s + 2 + -------
;;;           s + 1                            s + 1
;;;
;;;   For multivariate polynomials this only makes sense as
;;;   poly/quotient defined below, because the coefficient ring is not
;;;   a field, but a unique factorization domain (UFD).

(define (poly/div u v cont)
  ;; cont = (lambda (q r)
  ;;          (assert (poly/equal? u (poly/add (poly/mul q v) r))))
  (if (poly/zero? v)
      (error "Divide by zero (POLY/DIV):" u v))
  (cond ((and (base? u) (base? v)) (base/div u v cont))
	((or (poly/zero? u) (poly/one? v))
	 (cont u poly/zero))
	(else
	 (let ((arity (poly/check-same-arity u v)))
	   (let lp ((u u) (cont cont))
	     (if (fix:< (poly/degree u) (poly/degree v))
		 (cont poly/zero u)
		 (let ((lead-q-degree
			(fix:- (poly/degree u) (poly/degree v)))
		       (lead-q-coeff
			(poly/quotient (poly/leading-coefficient u)
				       (poly/leading-coefficient v))))
		   (let ((qt (poly/make-c*x^n arity
					      lead-q-coeff
					      lead-q-degree)))
		     (lp (poly/sub u (poly/mul qt v))
			 (lambda (sq sr)
			   (cont (poly/add qt sq) sr)))))))))))


;;; This exact quotient is only defined when the remainder is zero.  
;;; Thus for polynomials over the integers or for polynomials with
;;;    polynomial coefficients we must have only exact quotients in
;;;    the coeff/div.  So must check that the remainder is zero.
;;;  For polynomials over the rationals, we may use rat:/.

(define (poly/quotient u v)
  (poly/div u v
	    (lambda (q r)
	      (if (poly/zero? r)
		  q
		  (error "Inexact division (POLY/QUOTIENT):" u v)))))

(define (poly/not-divisible? n d)
  (poly/div n d (lambda (p r) (not (poly/zero? r)))))

;;; Sometimes we want to exactly divide by a coefficient object.

(define (map-poly-terms proc p)
  (let ((arity (poly/arity p)))
    (let lp ((p p))
      (if (poly/zero? p)
	  p
	  (poly/adjoin arity
		       (poly/degree p)
		       (proc (poly/leading-coefficient p))
		       (lp (poly/except-leading-term arity p)))))))

(define (poly/normalize p c)
  (cond ((poly/zero? c)
	 (error "Divide by zero (POLY/NORMALIZE):" p c))
	((poly/one? c) p)
	((base? c)
	 (base/div base/one c
		   (lambda (q r)
		     (poly/scale p q))))
	(else
	 (map-poly-terms (lambda (pc) (poly/quotient pc c))
			 p))))

;;; Pseudo division produces only a remainder--no quotient.
;;;  This can be used to generalize Euclid's algorithm for polynomials
;;;  over a unique factorization domain (UFD).

;;; This implementation differs from Knuth's Algorithm R in that
;;; Knuth's contributes to the integerizing factor, making it
;;; l(v)^(m-n+1), even though no factor of l(v) is needed if a u_j is
;;; zero for some n<j<m.  This matters a great deal in the
;;; multivariate case.

;;; Sussman's code -- good for Euclid Algorithm

(define (poly/pseudo-remainder u v cont)
  ;; cont = (lambda (r d) #| l(v)^d*u = v*q + r|# )
  (if (poly/zero? v)
      (error "Divide by zero (POLY/PSEUDO-REMAINDER):" u v))
  (let* ((arity (poly/check-same-arity u v))
	 (cvn (poly/make-constant arity
				  (poly/leading-coefficient v)))
	 (n (poly/degree v)))
    (let lp ((u u) (d -1))
      ;;(write-line u)
      (let ((m (poly/degree u))
	    (cum (poly/leading-coefficient u)))
	(if (fix:< m n)
	    (cont u d)
	    (lp (poly/sub (poly/mul u cvn)
			  (poly/mul (poly/make-c*x^n arity
						     cum
						     (fix:- m n))
				    v))
		(fix:+ d 1)))))))
		
;;; The following helpers are used in GCD routines

;;; The content of a polynomial is the GCD of its coefficients.
;;;  The content of a polynomial has the arity of its coefficients. 


(define (poly/content-maker poly/gcd)
  (define (poly/content u win lose)
    (let ((coeffs (poly/coefficients u)))
      (if (null? coeffs)
	  (win poly/zero)
	  (let lp ((c0 (car coeffs)) (cs (cdr coeffs)))
	    (if (null? cs)
		(win c0)
		(poly/gcd c0 (car cs)
		  (lambda (g1)
		    (if (poly/one? g1)
			(win g1)
			(lp g1 (cdr cs))))
		  lose))))))
  poly/content)


  
;;; The primitive-part of a polynomial is the polynomial with the
;;; content removed.

(define (poly/primitive-part-maker poly/gcd)
  (let ((poly/content (poly/content-maker poly/gcd)))
    (define (poly/primitive-part p win lose)
      (if (or (poly/zero? p) (poly/one? p))
	  (win p)
	  (poly/content p
	    (lambda (c)
	      (win
	       (cond ((poly/negative? p)
		      (if (poly/one? c)
			  (poly/negate p)
			  (poly/normalize p (poly/negate c))))
		     ((poly/one? c) p)
		     (else
		      (poly/normalize p c)))))
	    lose)))
    poly/primitive-part))

#|
;;; Distillation method (Zippel Section 8.2) is often very bad.

(define (poly/content-maker poly/gcd)
  (define (poly/content u)
    (let ((coeffs (poly/coefficients u)))
      (if (null? coeffs)
	  poly/zero
	  (let lp ((coeffs coeffs))
	    (let ((n (length coeffs)))
	      (if (fix:= n 1)
		  (car coeffs)
		  (let ((n/2 (quotient n 2)))
		    (let ((ps (poly/random-linear-combination
			       (list-head coeffs n/2)))
			  (qs (poly/random-linear-combination
			       (list-tail coeffs n/2))))
		      (let ((g (poly/gcd ps qs)))
			(if (poly/one? g)
			    g
			    (lp
			     (cons g
				   (filter (lambda (c)
					     (poly/not-divisible? c g))
					   coeffs)))))))))))))
  poly/content)

(define poly/random-linear-combination
  (let* ((number-of-primes 100)
	 (prime-table
	  (make-initialized-vector number-of-primes
	    (lambda (i) (stream-ref prime-numbers-stream i)))))
    (lambda (polys)
      (fold-left poly/add (car polys)
		 (map (lambda (p)
			(poly/mul
			 (vector-ref prime-table
				     (random number-of-primes))
			 p))
		      (cdr polys))))))
|#

;;; Euclidean GCD
;;;    This implementation differs from Knuth's Algorithm E in that it
;;;    is rotated to look more like Euclidean integer gcd.

(define (poly/gcd/euclid u v win lose)
  (let ((poly/content
	 (poly/content-maker poly/gcd/euclid))
	(poly/primitive-part
	 (poly/primitive-part-maker poly/gcd/euclid))) 
    (define (pgcd ppu ppv win lose)
      (if euclid-wallp? (pp (list ppu ppv)))
      (cond ((poly/zero? ppv) (win ppu))
	    ((fix:zero? (poly/degree ppv)) (win poly/one))
	    ((allocated-time-expired?) (lose))
	    (else
	     (poly/pseudo-remainder ppu ppv
	       (lambda (r delta)
		 (poly/primitive-part r
		   (lambda (ppr)
		     (pgcd ppv ppr win lose))
		   lose))))))
    (cond ((poly/zero? u) (win v)) ((poly/zero? v) (win u))
	  ((poly/one? u) (win u))  ((poly/one? v) (win v))
	  ((base? u)
	   (if (base? v)
	       (win (base/gcd u v))
	       (poly/content v
		 (lambda (vc)
		   (poly/gcd/euclid u vc win lose))
		 lose)))
	  ((base? v)
	   (poly/content u
	     (lambda (uc)
	       (poly/gcd/euclid uc v win lose))
	     lose))
	  (else
	   (let ((arity (poly/check-same-arity u v))
		 (win (lambda (g) (win (poly/abs g)))))
	     (poly/content u
	       (lambda (uc)
		 (poly/content v
		   (lambda (vc)
		     (if (poly/one? uc)
			 (if (poly/one? vc)
			     (pgcd u v win lose)
			     (pgcd u (poly/normalize v vc) win lose))
			 (if (poly/one? vc)
			     (pgcd (poly/normalize u uc) v win lose)
			     (poly/gcd/euclid uc vc
			       (lambda (c)
				 (if (poly/one? c)
				     (pgcd (poly/normalize u uc)
					   (poly/normalize v vc)
					   win lose)
				     (pgcd (poly/normalize u uc)
					   (poly/normalize v vc)
					   (lambda (g)
					     (win (poly/scale-1 arity g c)))
					   lose)))
			       lose))))
		   lose))
	       lose))))))

(define euclid-wallp? false)

(define (poly/gcd-euclid u v)
  (poly/gcd/euclid u v (lambda (g) g) (lambda () #f)))

;;; Algorithm C... Collins's GCD algorithm.  
;;;  I have not found this worthwhile--GJS.
;;;  Thus, I have not updated it to use the 
;;;  success and failure continuations needed.

(define (poly/gcd-collins u v)
  (let ((poly/content
	 (poly/content-maker poly/gcd-collins))
	(poly/primitive-part
	 (poly/primitive-part-maker poly/gcd-collins)))

    (define (pgcd u v oc)
      (if collins-wallp? (pp (list u v)))
      (cond ((poly/zero? v) u)
	    ((fix:zero? (poly/degree v)) poly/one)
	    (else
	     (poly/pseudo-remainder u v
	       (lambda (r delta)
		 (pgcd v
		       (poly/normalize r
			 (poly/expt (poly/leading-coefficient u)
				    oc))
		       delta))))))
    (define (ppgcd u v)
      (cond ((poly/one? u) u)
	    ((poly/one? v) v)
	    ((and (base? u) (base? v)) (base/gcd u v))
	    ((fix:>= (poly/degree u) (poly/degree v))
	     (poly/primitive-part (pgcd u v 0)))
	    (else
	     (poly/primitive-part (pgcd v u 0)))))
    (cond ((poly/zero? u) v)
	  ((poly/zero? v) u)
	  ((poly/one? u) u)
	  ((poly/one? v) v)
	  ((base? u)
	   (if (base? v)
	       (base/gcd u v)
	       (poly/gcd-collins u (poly/content v))))
	  ((base? v)
	   (poly/gcd-collins (poly/content u) v))
	  (else
	   (let ((arity (poly/check-same-arity u v))
		 (uc (poly/content u))
		 (vc (poly/content v)))
	     (let ((ans 
		    (if (poly/one? uc)
			(if (poly/one? vc)
			    (ppgcd u v)
			    (ppgcd u (poly/normalize v vc)))
			(if (poly/one? vc)
			    (ppgcd (poly/normalize u uc) v)
			    (let ((c (poly/gcd-collins uc vc)))
			      (if (poly/one? c)
				  (ppgcd (poly/normalize u uc)
					 (poly/normalize v vc))
				  (poly/scale-1 arity
				    (ppgcd (poly/normalize u uc)
					   (poly/normalize v vc))
				    c)))))))
	       (poly/abs ans)))))))

(define collins-wallp? false)

#|
;;; Test examples

(define x-1 (poly/make-from-dense 1 '(1 -1)))
(define x+1 (poly/make-from-dense 1 '(1 1)))

(poly/gcd-euclid (poly/mul (poly/mul x-1 x-1) x+1)
		 (poly/mul (poly/mul x+1 x+1) x-1))
;Value: (*dense* 1 1 0 -1)

(poly/gcd-euclid (poly/mul (poly/mul x+1 x+1) x-1)
		 (poly/mul (poly/mul x-1 x-1) x+1))
;Value: (*dense* 1 1 0 -1)

(define k1 (poly/make-from-dense 1 '(1 0 1 0 -3 -3 8 2 -5)))
(define k2 (poly/make-from-dense 1 '(3 0 5 0 -4 -9 21)))

(poly/gcd-euclid k1 k2)
;Value: 1

(poly/gcd-collins (poly/mul (poly/mul x-1 x-1) x+1)
		  (poly/mul (poly/mul x+1 x+1) x-1))
;Value: (*dense* 1 1 0 -1)

(poly/gcd-euclid k1 k2)
;;; With euclid-wallp? = #t
((*dense* 1 0 1 0 -3 -3 8 2 -5) (*dense* 3 0 5 0 -4 -9 21))
((*dense* 3 0 5 0 -4 -9 21) (*dense* 5 0 -1 0 3))
((*dense* 5 0 -1 0 3) (*dense* 13 25 -49))
((*dense* 13 25 -49) (*dense* 4663 -6150))
((*dense* 4663 -6150) (*dense* 1))
;Value: 1

(poly/gcd-collins k1 k2)
;;; With collins-wallp? = #t
((*dense* 1 0 1 0 -3 -3 8 2 -5) (*dense* 3 0 5 0 -4 -9 21) 1)
((*dense* 3 0 5 0 -4 -9 21) (*dense* -5 0 1 0 -3) 9)
((*dense* -5 0 1 0 -3) (*dense* -13 -25 49) 25)
((*dense* -13 -25 49) (*dense* -9326 12300) -2197)
((*dense* -9326 12300) (*dense* 260708) 86974276)
;Value: 1

(define p1 (poly/make-from-dense 1 '(1 1 1)))
(define p2 (poly/make-from-dense 1 '(3 2)))
(define p3 (poly/make-from-dense 1 '(5 3 2)))
(define p4 (poly/mul (poly/expt p1 3) (poly/mul p2 p3)))
(define p5 (poly/mul p1 (poly/expt p3 2)))

(poly/gcd-euclid p4 p5)
((*dense* 1 15 64 159 259 307 267 172 79 24 4) (*dense* 1 25 55 84 71 45 16 4))
((*dense* 1 25 55 84 71 45 16 4) (*dense* 1 3015 5234 6686 3835 1616 164))
((*dense* 1 3015 5234 6686 3835 1616 164) (*dense* 1 5 8 10 5 2))
((*dense* 1 5 8 10 5 2) (*dense* 1))
;Value: (*dense* 1 5 8 10 5 2)

(poly/gcd-collins p4 p5)
((*dense* 1 15 64 159 259 307 267 172 79 24 4) (*dense* 1 25 55 84 71 45 16 4) 1)
((*dense* 1 25 55 84 71 45 16 4)
 (*dense* 1 1884375 3271250 4178750 2396875 1010000 102500)
 390625)
((*dense* 1 1884375 3271250 4178750 2396875 1010000 102500)
 (*dense* 1 76562500 122500000 153125000 76562500 30625000)
 3550869140625)
((*dense* 1 76562500 122500000 153125000 76562500 30625000)
 (*dense* 1)
 5861816406250000)
;Value: (*dense* 1 5 8 10 5 2)

(poly/mul p1 p3)
;Value: (*dense* 1 5 8 10 5 2)

(define p6 (poly/mul (poly/expt p1 2) (poly/expt p3 2)))

(poly/gcd-collins p4 p6)
;Value: (*dense* 1 5 13 23 23 17 7 2)

(poly/gcd-euclid p4 p6)
;Value: (*dense* 1 5 13 23 23 17 7 2)
|#

;;; GCD memoizer.  A hairy attempt to speed up the gcd.  Ultimately
;;; this failed and we went to a sparse interpolation gcd.

(define (gcd-memoizer poly/gcd)
  (let ((table
	 ((weak-hash-table/constructor unordered-poly-hash
				       unordered-pair-equal?))))
    (define (the-memoized-gcd p1 p2)
      (if *gcd-memoizer-enabled*
	  (let ((x (cons p1 p2)))
	    (let ((seen (hash-table/get table x *not-seen*)))
	      (if (not (eq? seen *not-seen*))
		  (begin (set! *gcd-hit* (fix:+ *gcd-hit* 1))
			 seen)
		  (let ((ans (poly/gcd p1 p2)))
		    (set! *gcd-miss* (fix:+ *gcd-miss* 1))
		    (hash-table/put! table x ans)
		    ans))))
	  (poly/gcd p1 p2)))
    the-memoized-gcd))

(define *gcd-memoizer-enabled* #f)
(define *gcd-hit* 0)
(define *gcd-miss* 0)

(define (unordered-pair-equal? a1 a2)
  (and (pair? a1)
       (pair? a2)
       (or (and (equal? (car a1) (car a2))
		(equal? (cdr a1) (cdr a2)))
	   (and (equal? (car a1) (cdr a2))
		(equal? (cdr a1) (car a2))))))

(define (unordered-poly-hash a modulus)
  (let ((arity (poly/check-same-arity (car a) (cdr a))))
    (let ((args (vector-ref hash-args-vector arity))) 
      (let ((v (* (poly/horner (car a) args)
		  (poly/horner (cdr a) args))))
	(modulo (* (numerator v) (denominator v))
		modulus)))))

(define n-random-primes 100)
(define skip-initial-primes 100)

(define prime-numbers-vector
  (make-initialized-vector n-random-primes
			   (lambda (i)
			     (stream-ref prime-numbers-stream
					 (fix:+ i skip-initial-primes)))))

(define hash-args-vector
  (make-initialized-vector n-random-primes
    (lambda (i)
      (make-initialized-list i
	(lambda (j)
	  (vector-ref prime-numbers-vector
		      (random (min (* 2 i) n-random-primes))))))))

;;; (define poly/gcd-memoized (gcd-memoizer poly/gcd-euclid))
;;; (define poly/gcd-memoized (gcd-memoizer poly/gcd-collins))

;;; The following returns the derivative of a polynomial with respect
;;; to a given variable index.  Variable 1 is the principal variable.
;;; For example:

;;;(->expression
;;; (poly/derivative-partial
;;;  (->poly '(+ (* x y z) (* x x z) (* y y))
;;;	     '(x y z))
;;;  2)
;;; '(x y z))
;;;Value: (+ (* z x) (* 2 y))

(define (poly/derivative-partial p varnum)
  (cond ((base? p) poly/zero)
	((fix:< varnum 1) poly/zero)
	((fix:= varnum 1) (poly/derivative-principal p))
	(else
	 (let ((arity (poly/arity p)))
	   (if (fix:> varnum arity)
	       (error "Bad varnum -- POLY/DERIVATIVE-PARTIAL"
		      p varnum))
	   (let lp ((p p))
	     (if (poly/zero? p)
		 poly/zero
		 (let ((c (poly/leading-coefficient p)))
		   (let ((d (poly/derivative-partial c
			      (fix:- varnum
				     (fix:- arity
					    (poly/arity c))))))
		     (if (poly/zero? d)
			 (lp (poly/except-leading-term arity p))
			 (poly/adjoin arity
			   (poly/degree p)
			   d
			   (lp (poly/except-leading-term arity p))))))))))))


(define (poly/partial-derivative p varnums) ;compatibility
  (assert (fix:= (length varnums) 1))
  (poly/derivative-partial p (fix:+ (car varnums) 1)))


(define (poly/derivative-principal p)
  (if (base? p)
      poly/zero
      (let ((arity (poly/arity p)))
	(let lp ((p p))
	  (if (base? p)
	      poly/zero
	      (let ((deg (poly/degree p)))
		(if (fix:zero? deg)
		    poly/zero
		    (poly/adjoin arity
		      (fix:- deg 1)
		      (poly/mul deg (poly/leading-coefficient p))
		      (lp (poly/except-leading-term arity p))))))))))

;;; Evaluation of polynomials by Horner's rule is one key to their
;;; effective use.  The following is useful for univariate
;;; polynomials.

(define (poly/horner-univariate p x)
  (poly/hh p x (lambda (x) x)))

(define (poly/horner p args)
  (if (base? p)
      p
      (let ((arity (poly/arity p)))
	(if (not (fix:= arity (length args)))
	    (error "Wrong number of args -- POLY/HORNER" p args))
	(let lp ((p p) (args args))
	  (if (base? p)
	      p
	      (let ((arity (poly/arity p))
		    (nargs (length args)))
		(let ((args (list-tail args (fix:- nargs arity))))
		  (poly/hh p
			   (car args)
			   (lambda (c) (lp c (cdr args)))))))))))


;;; POLY/HORNER-HELPER is used to evaluate a polynomial for a
;;; particular value of the indeterminate.  In general, the
;;; coefficients of the polynomial will themselves be polynomials,
;;; which must be evaluated with values for their indeterminates.  The
;;; EVAL-COEFF argument will be used in the process of lifting this
;;; system to multivariate polynomials.  It will encapsulate the
;;; process of evaluating the coefficients of the current polynomial
;;; on the rest of the polynomial arguments.

(define (poly/horner-helper add mul raise)
  (lambda (p x eval-coeff)
    (if (base? p)
	p
	(let ((arity (poly/arity p)))
	  (let lp ((restp (poly/except-leading-term arity p))
		   (coeff-value (eval-coeff (poly/leading-coefficient p)))
		   (degree (poly/degree p)))
	    (if (poly/zero? restp)
		(mul coeff-value (raise x degree))
		(let ((next-degree (poly/degree restp))
		      (next-coeff (poly/leading-coefficient restp)))
		  (lp (poly/except-leading-term arity restp)
		      (add (mul coeff-value
				(raise x
				      (fix:- degree
					     next-degree)))
			   (eval-coeff next-coeff))
		      next-degree))))))))

(define poly/hh (poly/horner-helper poly/add poly/mul poly/expt))

;;; Given polynomial P(x), substitute x = r*y and compute the resulting
;;;  polynomial Q(y) = P(y*r).  When a multivariate polynomial is
;;;  scaled, each factor must have the same arity as the given
;;;  polynomial... or a base constant.

(define (poly/arg-scale p factors)
  (poly/horner p
	       (map poly/mul
		    (list-head factors (poly/arity p))
		    (poly/make-vars (poly/arity p)))))


;;; Given polynomial P(x), substitute x = y+h and compute the resulting
;;;  polynomial Q(y) = P(y+h).  When a multivariate polynomial is
;;;  shifted, each shift must have the same arity as the given
;;;  polynomial... or a base constant.

(define (poly/arg-shift p shifts)
  (poly/horner p
	       (map poly/add
		    (list-head shifts (poly/arity p))
		    (poly/make-vars (poly/arity p)))))




(define (poly/abs ans)
  (if (base? ans)
      (base/abs ans)
      (if (poly/negative? ans)
	  (poly/negate ans)
	  ans)))

(define (poly/leading-base-coefficient p)
  (if (base? p)
      p
      (poly/leading-base-coefficient (poly/leading-coefficient p))))

;;; This Horner's rule evaluator is restricted to 
;;;  numerical coefficients and univariate polynomials.
;;; It returns, by calling a continuation procedure,
;;;  a value, two derivatives, and an estimate of the
;;;  roundoff error incurred in computing that value.
#| 
;;; The recurrences used are from Kahan's 18 Nov 1986 paper
;;;  "Roundoff in Polynomial Evaluation", generalized for 
;;;  sparse representations and another derivative  by GJS.
;;; For p = A(z), q = A'(z), r = A''(z), and e = error in A(x),

p_{j+n} = z^n p_j + a_{j+n}

e_{j+n} = |z|^n ( e_j + (n-1) p_j ) + |p_{j+n}|

q_{j+n} = z^n q_j + n z^{n-1} p_j

r_{j+n} = z^n r_j + n z^{n-1} q_j + 1/2 n (n-1) z^{n-2} p_j
|#

(define (poly/horner-with-error a z cont)
  ;; cont = (lambda (p q r e) ...)
  (if (base? a)
      a
      (let ((arity (poly/arity a))
	    (az (magnitude z)))
	(if (not (fix:= arity 1))
	    (error "Wrong arity poly -- POLY/HORNER-WITH-ERROR" a z))
	(let lp ((degree (poly/degree a))
		 (p (poly/leading-coefficient a))
		 (q 0)
		 (r 0)
		 (e (* 1/2 (magnitude (poly/leading-coefficient a))))
		 (a (poly/except-leading-term arity a)) )
	  (let* ((next-degree (poly/degree a))
		 (n (if (base? a) degree (fix:- degree next-degree))))
	    (define (finish np nq nr ne)
	      (if (base? a)
		  (cont np nq (* 2 nr)
			(* *machine-epsilon* (+ (- ne (magnitude np)) ne)))
		  (lp next-degree np nq nr ne
		      (poly/except-leading-term arity a))))
	    (cond ((fix:= n 1)
		   (let* ((np (+ (* z p) (poly/leading-coefficient a)))
			  (nq (+ (* z q) p))
			  (nr (+ (* z r) q))
			  (ne (+ (* az e) (magnitude np))))
		     (finish np nq nr ne)))
		  ((fix:= n 2)
		   (let* ((z^n (* z z))
			  (az^n (magnitude z^n))		 
			  (np (+ (* z^n p) (poly/leading-coefficient a)))
			  (nq (+ (* z^n q) (* 2 (* z p))))
			  (nr (+ (* z^n r) (* 2 z q) p))
			  (ne (+ (* az^n (+ e p)) (magnitude np))))
		     (finish np nq nr ne)))
		  (else
		   (let* ((z^n-2 (expt z (fix:- n 2)))
			  (z^n-1 (* z^n-2 z))
			  (z^n (* z^n-1 z))
			  (az^n (magnitude z^n))		 
			  (np (+ (* z^n p)
				 (poly/leading-coefficient a)))
			  (nq (+ (* z^n q)
				 (* n (* z^n-1 p))))
			  (nr (+ (* z^n r)
				 (* n z^n-1 q)
				 (* 1/2 n (fix:- n 1) z^n-2 p)))
			  (ne (+ (* az^n (+ e (* (fix:- n 1) p)))
				 (magnitude np))))
		     (finish np nq nr ne)))))))))

;;; The representations of polynomials may be either sparse or dense.

(define (pcf? p)
  (or (base? p)
      (explicit-pcf? p)))

(define (explicit-pcf? p)
  (and (pair? p)
       (or (eq? (car p) '*sparse*)
	   (eq? (car p) '*dense*))))


(define (poly/type p)
  (if (base? p)
      '*dense*
      (car p)))

(define (poly/arity p)
  (if (base? p)
      0
      (cadr p)))

(define (poly/termlist p)
  (cond ((poly/zero? p) '())
	((base? p) (list p))		;a dense object
	(else (cddr p))))


(define (poly/sparse? p)
  (and (pair? p) (eq? (car p) '*sparse*)))

(define (poly/make-from-sparse arity termlist)
  (cond ((null? termlist) poly/zero)
	((and (null? (cdr termlist))
	      (fix:= (caar termlist) 0)	;but degree of a zero is -1!
	      (base? (cdar termlist)))
	 (cdar termlist))
	(else	 
	 (cons '*sparse*
	       (cons arity termlist)))))

(define (poly/dense? p)
  (or (base? p)
      (and (pair? p) (eq? (car p) '*dense*))))

(define (poly/make-from-dense arity termlist)
  (cond ((null? termlist) poly/zero)
	((and (null? (cdr termlist))
	      (base? (car termlist)))
	 (car termlist))
	(else
	 (cons '*dense*
	       (cons arity termlist)))))


(define poly/make poly/make-from-dense)
	 
(define (poly/degree p)
  (case (poly/type p)
    ((*sparse*)
     (poly/sparse/degree (poly/termlist p)))
    ((*dense*)
     (poly/dense/degree (poly/termlist p)))
    (else
     (error "Bad type -- POLY/DEGREE" p))))

(define (poly/leading-coefficient p)
  (case (poly/type p)
    ((*sparse*)
     (poly/sparse/leading-coefficient (poly/termlist p)))
    ((*dense*)
     (poly/dense/leading-coefficient (poly/termlist p)))
    (else
     (error "Bad type -- POLY/LEADING-COEFFICIENT" p))))

(define (poly/except-leading-term arity p)
  (case (poly/type p)
    ((*sparse*)
     (poly/make-from-sparse arity
      (poly/sparse/except-leading-term (poly/termlist p))))
    ((*dense*)
     (poly/make-from-dense arity
      (poly/dense/except-leading-term (poly/termlist p))))
    (else
     (error "Bad type -- POLY/EXCEPT-LEADING-TERM" p))))


(define *dense-break-even* 2)

(define (poly/adjoin arity n c p)
  ;; n=exponent, c=coefficient, p=polynomial
  (case (poly/type p)
    ((*sparse*)
     (let ((terms (poly/termlist p)))
       (if (fix:> (fix:- n (fix:- (length terms) 1))
		  *dense-break-even*)
	   (poly/make-from-sparse arity
	     (poly/sparse/adjoin n c terms))
	   (poly/make-from-dense arity
	     (poly/dense/adjoin n c
	       (sparse->dense terms))))))
    ((*dense*)
     (let ((terms (poly/termlist p)))
       (if (fix:> (fix:- n (fix:- (length terms) 1))
		  *dense-break-even*)
	   (poly/make-from-sparse arity
	     (poly/sparse/adjoin n c
	       (dense->sparse terms)))
	   (poly/make-from-dense arity
	     (poly/dense/adjoin n c terms)))))
    (else
     (error "Bad type -- POLY/ADJOIN" n c p))))

(define (poly/coefficient p n)
  (case (poly/type p)
    ((*sparse*)
     (poly/sparse/coefficient (poly/termlist p) n))
    ((*dense*)
     (poly/dense/coefficient (poly/termlist p) n))
    (else
     (error "Bad type -- POLY/COEFFICIENT" p n))))

(define (poly/coefficients p)
  (case (poly/type p)
    ((*sparse*)
     (poly/sparse/coefficients (poly/termlist p)))
    ((*dense*)
     (poly/dense/coefficients (poly/termlist p)))
    (else
     (error "Bad type -- POLY/COEFFICIENTS" p))))

;;; Returns a sorted numerical set of numbers (see sets.scm)
(define (poly/base-coefficients p)
  (let lp ((cs (poly/coefficients p)) (ans (empty-set numbers)))
    (cond ((null? cs) ans)
	  ((base? (car cs))
	   (lp (cdr cs)
	       ((adjoin-set numbers) (car cs) ans)))
	  (else
	   (lp (cdr cs)
	       ((union-sets numbers)
		(poly/base-coefficients (car cs))
		ans))))))

(define (poly/principal-reverse p)
  (let ((arity (poly/arity p)))
    (case (poly/type p)
      ((*sparse*)
       (poly/make-from-sparse arity
	(poly/sparse/principal-reverse (poly/termlist p))))
      ((*dense*)
       (poly/make-from-dense arity
	(poly/dense/principal-reverse (poly/termlist p))))
      (else
       (error "Bad type -- POLY/PRINCIPAL-REVERSE" p)))))

(define (poly/->dense p)
  (let ((arity (poly/arity p)))
    (case (poly/type p)
      ((*sparse*)
       (poly/make-from-dense arity
	(sparse->dense (poly/termlist p))))
      ((*dense*) p)
      (else
       (error "Bad type -- POLY/->DENSE" p)))))

(define (poly/->sparse p)
  (let ((arity (poly/arity p)))
    (case (poly/type p)
      ((*sparse*) p)
      ((*dense*)
       (poly/make-from-sparse arity
	(dense->sparse (poly/termlist p))))
      (else
       (error "Bad type -- POLY/->DENSE" p)))))

(define (poly/lowest-order p)
  (cond ((base? p) 0)
	(else
	 (case (poly/type p)
	   ((*sparse*)
	    (poly/sparse/lowest-order (poly/termlist p)))
	   ((*dense*)
	    (poly/dense/lowest-order (poly/termlist p)))
	   (else
	    (error "Bad type -- POLY/LOWEST-ORDER" p))))))

(define (poly/trailing-coefficient p)
  (cond ((base? p) p)
	(else
	 (case (poly/type p)
	   ((*sparse*)
	    (poly/sparse/trailing-coefficient (poly/termlist p)))
	   ((*dense*)
	    (poly/dense/trailing-coefficient (poly/termlist p)))
	   (else
	    (error "Bad type -- POLY/TRAILING-COEFFICIENT" p))))))

;;; Poly raw representations operate on termlists.

(define poly/sparse/zero '())

(define poly/sparse/zero? null?)

(define poly/sparse/one
  (list (cons 0 base/one)))

(define poly/sparse/identity
  (list (cons 1 base/one)))


(define (poly/sparse/degree p)
  (if (null? p) -1 (caar p)))

(define (poly/sparse/leading-coefficient p)
  (if (null? p) base/zero (cdar p)))

(define (poly/sparse/except-leading-term p)
  (cdr p))

(define (poly/sparse/adjoin d c p)
  (cons (cons d c) p))

(define (poly/sparse/coefficients p)
  (map cdr p))

(define (poly/sparse/coefficient p d)
  (if (not (and (fix:fixnum? d) (fix:>= d 0)))
      (error:wrong-type-argument d "nonnegative fixnum"
				 'POLY/SPARSE/COEFFICIENT))
  (let lp ((terms p))
    (cond ((null? terms) base/zero)
	  ((fix:= (caar terms) d) (cdar terms))
	  ((fix:< (caar terms) d) base/zero)
	  (else (lp (cdr terms))))))

(define (poly/sparse/principal-reverse p)
  (if (null? p)
      '()
      (let ((degree (caar p)))
	(let loop ((p p) (result '()))
	  (if (null? p)
	      result
	      (loop (cdr p)
		    (cons (cons (fix:- degree (caar p)) (cdar p))
			  result)))))))

(define (poly/sparse/lowest-order p)
  (caar (last-pair p)))

(define (poly/sparse/trailing-coefficient p)
  (cdar (last-pair p)))

(define poly/dense/zero '())

(define poly/dense/zero? null?)

(define poly/dense/one
  (list base/one))


(define poly/dense/identity
  (list base/one base/zero))

(define (poly/dense/degree p)
  (fix:- (length p) 1))

(define (poly/dense/leading-coefficient p)
  (if (null? p) base/zero (car p)))

(define (poly/dense/except-leading-term p)
  (cond ((null? (cdr p))
	 (cdr p))
	((poly/zero? (cadr p))
	 (poly/dense/except-leading-term (cdr p)))
	(else
	 (cdr p))))

(define (poly/dense/adjoin d c p)
  (let ((d* (length p)))
    (cond ((fix:= d d*)
	   (cons c p))
	  ((fix:> d d*)
	   (let loop ((d* (fix:+ d* 1)) (p (cons base/zero p)))
	     (if (fix:= d d*)
		 (cons c p)
		 (loop (fix:+ d* 1) (cons base/zero p)))))
	  (else
	   (error "Term not in order (POLY/ADJOIN):" d c p)))))

(define (poly/dense/coefficients p)
  (let loop ((p p))
    (cond ((null? p) '())
	  ((poly/zero? (car p)) (loop (cdr p)))
	  (else (cons (car p) (loop (cdr p)))))))

(define (poly/dense/coefficient p d)
  (if (not (and (fix:fixnum? d) (fix:>= d 0)))
      (error:wrong-type-argument d "nonnegative fixnum"
				 'POLY/DENSE/COEFFICIENT))
  (list-ref p (fix:- (fix:- (length p) 1) d)))

(define (poly/dense/principal-reverse p)
  (let loop ((p (reverse p)))
    (if (and (not (null? p))
	     (base? (car p))
	     (base/zero? (car p)))
	(loop (cdr p))
	p)))

(define (poly/dense/lowest-order p)
  (let lp ((i 0) (l (reverse p)))
    (cond ((null? l)
	   (error "Should not get here -- POLY/DENSE/LOWEST-ORDER" p))
	  ((poly/zero? (car l))
	   (lp (fix:+ i 1) (cdr l)))
	  (else i))))

(define (poly/dense/trailing-coefficient p)
  (let lp ((i 0) (l (reverse p)))
    (cond ((null? l)
	   (error "Should not get here -- POLY/DENSE/TRAILING-COEFFICIENT" p))
	  ((poly/zero? (car l))
	   (lp (fix:+ i 1) (cdr l)))
	  (else (car l)))))






(define (dense->sparse lst)
  (let lp ((lst lst) (degree (poly/dense/degree lst)))
    (if (null? lst)
	poly/sparse/zero
	(let ((coeff (car lst))
	      (terms (cdr lst)))
	  (if (and (base? coeff) (base/zero? coeff))
	      (lp terms (fix:- degree 1))
	      (poly/sparse/adjoin degree
				  coeff
				  (lp terms (fix:- degree 1))))))))

(define (sparse->dense lst)
  (if (poly/sparse/zero? lst)
      poly/dense/zero
      (let lp ((lst lst))
	(let ((degree (poly/sparse/degree lst))
	      (coeff (poly/sparse/leading-coefficient lst))
	      (rest (poly/sparse/except-leading-term lst)))
	  (cons coeff
		(if (poly/sparse/zero? rest)
		    (make-list degree base/zero)
		    (let make-zeros ((degree (fix:- degree 1)))
		      (if (fix:= degree (poly/sparse/degree rest))
			  (lp rest)
			  (cons base/zero
				(make-zeros (fix:- degree 1)))))))))))

(define poly/identity (poly/make-identity 1))

;;; Export stuff happens here:

(define poly:arity poly/arity)
(define poly:degree poly/degree)

(define poly:zero? poly/zero?)
(define poly:one? poly/one?)

(define poly:negate poly/negate)
(define poly:square poly/square)

(define poly:derivative poly/derivative-principal)

(define poly:=    poly/equal?)
(define poly:+    poly/add)
(define poly:-    poly/sub)
(define poly:*    poly/mul)

(define poly:expt poly/expt)

(define poly:divide poly/div)
(define poly:pseudo-remainder poly/pseudo-remainder)
(define poly:quotient poly/quotient)

(define poly:partial-derivative poly/partial-derivative)

(define poly:arg-shift poly/arg-shift)
(define poly:arg-scale poly/arg-scale)

(define poly:apply poly/horner)

(define poly:zero poly/zero)
(define poly:one poly/one)
(define poly:identity poly/identity)

(define poly:horners-rule-with-error poly/horner-with-error)
(define (poly:value p . args) (poly/horner p args))
(define (poly:principal-value p x) (poly/horner p (list x)))
(define poly:principal-reverse poly/principal-reverse)
(define poly:scale poly/scale)
(define poly:normalize-by poly/normalize)

(define poly:lowest-order poly/lowest-order)
(define poly:trailing-coefficient poly/trailing-coefficient)
(define poly:leading-coefficient poly/leading-coefficient)
(define poly:except-leading-term poly/except-leading-term)
(define poly:leading-base-coefficient poly/leading-base-coefficient)

(define poly:extend poly/extend)
(define poly:contract poly/contract)
(define poly:contractable? poly/contractable?)
(define poly:new-variables poly/make-vars)


#|
;;; Now set in pcf-fpf.scm to work with sparse stuff
(define (poly:gcd x y) (poly/gcd-memoized x y))
|#

;;; Old style stuff, such as POLY/HERMITE needs polys in reversed
;;;  dense form.

(define (poly:dense-> a_0-a_n)
  (poly/make-from-dense 1 (reverse a_0-a_n)))

(define (poly:->dense poly)
  (let ((p (poly/->dense poly)))
    (if (base? p)
	(list p)
	(reverse (poly/termlist p)))))

;;; For simplifier

(define (pcf:->expression p vars)
  (if (base? p)
      p
      (let ((arity (poly/arity p)))
	(if (not (fix:= arity (length vars)))
	    (error "Poly arity not = vars supplied -- PCF:->EXPRESSION"
		   p vars))
	(let ((hh (poly/horner-helper symb:+ symb:* symb:expt)))
	  (let lp ((p p) (args vars))
	    (if (base? p)
		p
		(let ((arity (poly/arity p))
		      (nargs (length args)))
		  (let ((args (list-tail args (fix:- nargs arity))))
		    (hh p (car args)
			(lambda (c)
			  (lp c (cdr args))))))))))))

(define (pcf:expression-> expr cont #!optional less?)
  ;; cont = (lambda (poly vars) ... )
  (let ((evars
	 (sort (list-difference (variables-in expr)
				pcf:operators-known)
		(if (default-object? less?) variable<? less?))))
    (cont ((expression-walker
	    (pair-up evars
		     (poly:new-variables (length evars))
		     pcf:operator-table))
	   expr)
	  evars)))

(define (poly:->lambda p)
  (let* ((n (poly/arity p))
	 (vars (generate-list-of-symbols 'x n))
	 (exp (pcf:->expression p vars)))	  
    `(lambda ,vars ,exp)))

(define +$poly
  (accumulation poly/add poly/zero))
(define -$poly
  (inverse-accumulation poly/sub poly/add poly/negate poly/zero))
(define *$poly
  (accumulation poly/mul poly/one))

(define pcf:operator-table
  `((+        ,+$poly)
    (-        ,-$poly)
    (*        ,*$poly)
    (negate   ,poly:negate)
    (expt     ,poly:expt)
    (square   ,poly:square)
    (gcd      ,(lambda (x y) (poly:gcd x y)))))

(define pcf:operators-known
  (map car pcf:operator-table))
