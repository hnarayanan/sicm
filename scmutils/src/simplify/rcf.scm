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

;;;; Rational Forms constructed over polynomials

(declare (usual-integrations))

(define rcf-tag '*RCF*)

(define (ratform? object)
  (and (pair? object)
       (eq? (car object) rcf-tag)))

(define (make-ratform n d)
  (list rcf-tag n d))

(define ratform-numerator cadr)
(define ratform-denominator caddr)


(define rcf:zero poly:zero)
(define rcf:one poly:one)

(define (rcf:zero? q)
  (and (not (ratform? q)) (poly:zero? q)))

(define (rcf:one? q)
  (and (not (ratform? q)) (poly:one? q)))

(define (rcf:arity q)	 
  (define (check-same-arity p1 p2)
    (let ((a1 (poly:arity p1)) (a2 (poly:arity p2)))
      (cond ((fix:= a1 0) a2)
	    ((fix:= a2 0) a1)
	    ((fix:= a1 a2) a1)
	    (else
	     (error "Unequal arities in RCF" q)))))
  (cond ((ratform? q)
	 (check-same-arity (ratform-numerator q)
			   (ratform-denominator q)))
	((pcf? q) (poly:arity q))
	(else (error "Wrong type -- RCF:ARITY" q))))

#|
(define (make-rcf n d)
  (cond ((poly:zero? d)
	 (error "Zero denominator -- MAKE-RCF" n d))
	((or (poly:zero? n) (poly:one? d)) n)
	((number? d) (poly:* (/ 1 d) n))
	(else
	 (let ((dn (poly:leading-base-coefficient d)))
	   (if (poly:one? dn)
	       (make-ratform n d)
	       (make-ratform (poly:normalize-by n dn)
			     (poly:normalize-by d dn)))))))
|#

(define (make-rcf n d)
  (cond ((poly:zero? d)
	 (error "Zero denominator -- MAKE-RCF" n d))
	((or (poly:zero? n) (poly:one? d)) n)
	(else
	 (let ((b ((set->list numbers)
		   ((union-sets numbers)
		    (poly/base-coefficients n)
		    (poly/base-coefficients d)))))
	   (let ((c (* (reduce-left lcm base/one
				    (map denominator
					 (filter ratnum? b)))
		       (sgn (poly:leading-base-coefficient d)))))
	     (if (base/one? c)
		 (make-ratform n d)
		 (make-ratform (poly:* n c)
			       (poly:* d c))))))))


(define (rcf:rcf? object)
  (or (ratform? object) (pcf? object)))

(define (rcf:pcf? object)
  (and (not (ratform? object)) (pcf? object)))

(define (rcf:= q r)
  (if (ratform? q)
      (if (ratform? r)
	  (and (poly:= (ratform-numerator q) (ratform-numerator r))
	       (poly:= (ratform-denominator q) (ratform-denominator r)))
	  (if (pcf? r)
	      #f
	      (error "Wrong type -- RCF:=" r)))
      (if (ratform? r)
	  (if (pcf? q)
	      #f
	      (error "Wrong type -- RCF:=" q))
	  (poly:= q r))))


;;; The notation here is from Knuth (p. 291).
;;; In various places we take the gcd of two polynomials
;;; and then use quotient to reduce those polynomials.

(define (rcf:+ u/u* v/v*)
  (rcf:binary-operator u/u* v/v*
     poly:+
     (lambda (u v v*)
       (if (poly:zero? u)
	   v/v*
	   (make-rcf (poly:+ (poly:* u v*) v) v*)))
     (lambda (u u* v)
       (if (poly:zero? v)
	   u/u*
	   (make-rcf (poly:+ u (poly:* u* v)) u*)))
     (lambda (u u* v v*)
       (if (poly:= u* v*)
	   (let* ((n (poly:+ u v)) (g (poly:gcd u* n)))
	     (if (poly:one? g)
		 (make-rcf n u*)
		 (make-rcf (poly:quotient n g) (poly:quotient u* g))))
	   (let ((d1 (poly:gcd u* v*)))
	     (if (poly:one? d1)
		 (make-rcf (poly:+ (poly:* u v*) (poly:* u* v))
			   (poly:* u* v*))
		 (let* ((u*/d1 (poly:quotient u* d1))
			(t (poly:+ (poly:* u (poly:quotient v* d1))
				   (poly:* u*/d1 v))))
		   (if (poly:zero? t)
		       rcf:zero
		       (let ((d2 (poly:gcd t d1)))
			 (if (poly:one? d2)
			     (make-rcf t (poly:* u*/d1 v*))
			     (make-rcf
			      (poly:quotient t d2)
			      (poly:* u*/d1
				      (poly:quotient v* d2)))))))))))))

(define (rcf:- u/u* v/v*)
  (rcf:binary-operator u/u* v/v*
     poly:-
     (lambda (u v v*)
       (if (poly:zero? u)
	   (make-ratform (poly:negate v) v*)
	   (make-rcf (poly:- (poly:* u v*) v) v*)))
     (lambda (u u* v)
       (if (poly:zero? v)
	   u/u*
	   (make-rcf (poly:- u (poly:* u* v)) u*)))
     (lambda (u u* v v*)
       (if (poly:= u* v*)
	   (let* ((n (poly:- u v)) (g (poly:gcd u* n)))
	     (if (poly:one? g)
		 (make-rcf n u*)
		 (make-rcf (poly:quotient n g) (poly:quotient u* g))))
	   (let ((d1 (poly:gcd u* v*)))
	     (if (poly:one? d1)
		 (make-rcf (poly:- (poly:* u v*) (poly:* u* v))
			   (poly:* u* v*))
		 (let* ((u*/d1 (poly:quotient u* d1))
			(t (poly:- (poly:* u (poly:quotient v* d1))
				   (poly:* u*/d1 v))))
		   (if (poly:zero? t)
		       rcf:zero
		       (let ((d2 (poly:gcd t d1)))
			 (if (poly:one? d2)
			     (make-rcf t (poly:* u*/d1 v*))
			     (make-rcf
			      (poly:quotient t d2)
			      (poly:* u*/d1
				      (poly:quotient v* d2)))))))))))))

(define (rcf:negate v/v*)
  (if (ratform? v/v*)
      (make-ratform (poly:negate (ratform-numerator v/v*))
		    (ratform-denominator v/v*))
      (poly:negate v/v*)))

(define (rcf:* u/u* v/v*)
  (rcf:binary-operator u/u* v/v*
    poly:*
    (lambda (u v v*)
      (cond ((poly:zero? u) rcf:zero)
	    ((poly:one? u) v/v*)
	    (else
	     (let ((d (poly:gcd u v*)))
	       (if (poly:one? d)
		   (make-rcf (poly:* u v) v*)
		   (make-rcf (poly:* (poly:quotient u d) v)
			     (poly:quotient v* d)))))))
    (lambda (u u* v)
      (cond ((poly:zero? v) rcf:zero)
	    ((poly:one? v) u/u*)
	    (else
	     (let ((d (poly:gcd u* v)))
	       (if (poly:one? d)
		   (make-rcf (poly:* u v) u*)
		   (make-rcf (poly:* u (poly:quotient v d))
			     (poly:quotient u* d)))))))
    (lambda (u u* v v*)
      (let ((d1 (poly:gcd u v*))
	    (d2 (poly:gcd u* v)))
	(if (poly:one? d1)
	    (if (poly:one? d2)
		(make-rcf (poly:* u v) (poly:* u* v*))
		(make-rcf (poly:* u (poly:quotient v d2))
			  (poly:* (poly:quotient u* d2) v*)))
	    (if (poly:one? d2)
		(make-rcf (poly:* (poly:quotient u d1) v)
			  (poly:* u* (poly:quotient v* d1)))
		(make-rcf (poly:* (poly:quotient u d1)
				  (poly:quotient v d2))
			  (poly:* (poly:quotient u* d2)
				  (poly:quotient v* d1)))))))))

(define (rcf:square q)
  (if (ratform? q)
      (make-ratform (let ((n (ratform-numerator q))) (poly:* n n))
		    (let ((d (ratform-denominator q))) (poly:* d d)))
      (poly:* q q)))

(define (rcf:/ u/u* v/v*)
  (rcf:* u/u* (rcf:invert v/v*)))

(define (rcf:invert v/v*)
  (make-rcf (rcf:denominator v/v*)
	    (rcf:numerator v/v*)))

(define (rcf:gcd u/u* v/v*)
  (rcf:binary-operator u/u* v/v*
     poly:gcd
     (lambda (u v v*)
       (cond ((poly:zero? u) v/v*)
	     ((poly:one? u) poly:one)
	     (else (poly:gcd u v))))
     (lambda (u u* v)
       (cond ((poly:zero? v) u/u*)
	     ((poly:one? v) poly:one)
	     (else (poly:gcd u v))))
     (lambda (u u* v v*)
       (let ((d1 (poly:gcd u v))
	     (d2 (poly:gcd u* v*)))
	 (make-rcf d1 d2)))))

(define (rcf:binary-operator u/u* v/v* int*int int*rat rat*int rat*rat)
  (if (ratform? u/u*)
      (if (ratform? v/v*)
	  (rat*rat (ratform-numerator u/u*)
		   (ratform-denominator u/u*)
		   (ratform-numerator v/v*)
		   (ratform-denominator v/v*))
	  (rat*int (ratform-numerator u/u*)
		   (ratform-denominator u/u*)
		   v/v*))
      (if (ratform? v/v*)
	  (int*rat u/u*
		   (ratform-numerator v/v*)
		   (ratform-denominator v/v*))
	  (int*int u/u* v/v*))))

(define (rcf:numerator q)
  (cond ((ratform? q) (ratform-numerator q))
	((pcf? q) q)
	(else (error "Wrong type -- NUMERATOR" q))))

(define (rcf:denominator q)
  (cond ((ratform? q) (ratform-denominator q))
	((pcf? q) poly:one)
	(else (error "Wrong type -- DENOMINATOR" q))))


(define (rcf:expt base exponent)
  (define (expt-iter x count answer)
    (if (fix:zero? count)
	answer
	(if (even? count)
	    (expt-iter (rcf:square x) (fix:quotient count 2) answer)
	    (expt-iter x (fix:-1+ count) (rcf:* x answer)))))
  (cond ((not (exact-integer? exponent))
	 (error "Can only raise a RCF to an exact integer power" base exponent))
	((fix:negative? exponent)
	 (rcf:invert (expt-iter base (int:negate exponent) rcf:one)))
	(else (expt-iter base exponent rcf:one))))

(define (rcf:arg-scale r points)
  (if (ratform? r)
      (rcf:/ (apply poly:arg-scale (ratform-numerator r) points)
	     (apply poly:arg-scale (ratform-denominator r) points))
      (apply poly:arg-scale r points)))

(define (rcf:arg-shift r points)
  (if (ratform? r)
      (rcf:/ (apply poly:arg-shift (ratform-numerator r) points)
	     (apply poly:arg-shift (ratform-denominator r) points))
      (apply poly:arg-shift r points)))

(define (rcf:value r points)
  (if (ratform? r)
      (rcf:/ (apply poly:value (ratform-numerator r) points)
	     (apply poly:value (ratform-denominator r) points))
      (apply poly:value r points)))

;;; The following only plugs r2 in for the principal indeterminate.

(define (rcf:compose r1 r2)
  (if (ratform? r2)
      (let ((nr1 (ratform-numerator r1))    (nr2 (ratform-numerator r2))
	    (dr1 (ratform-denominator r1))  (dr2 (ratform-denominator r2)))
	(let ((dn (poly:degree nr1))
	      (dd (poly:degree dr1))
	      (narity (fix:+ (poly:arity dr1) 1)))
	  (let ((nnr1 (poly:extend 1 (poly:principal-reverse nr1)))
		(ndr1 (poly:extend 1 (poly:principal-reverse dr1))))
	    (let ((scales (list (cadr (poly:new-variables narity)) 1)))
	      (let ((pn (poly:value (poly:principal-reverse
				     (poly:arg-scale nnr1 scales))
				    nr2
				    dr2))
		    (pd (poly:value (poly:principal-reverse
				     (poly:arg-scale ndr1 scales))
				    nr2
				    dr2)))
		(cond ((fix:> dn dd)
		       (rcf:/ pn (poly:* (poly:expt dr2 (fix:- dn dd)) pd)))
		      ((fix:< dn dd)
		       (rcf:/ (poly:* (poly:expt dr2 (fix:- dd dn)) pn) pd))
		      (else (rcf:/ pn pd))))))))
      (rcf:/ (poly:value (ratform-numerator r1) r2)
	     (poly:value (ratform-denominator r1) r2))))

(define (rcf:derivative r varnum)
  (if (ratform? r)
      (let ((u (ratform-numerator r)) (v (ratform-denominator r)))
	(rcf:/ (poly:- (poly:* (poly:derivative u varnum) v)
		       (poly:* u (poly:derivative v varnum)))
	       (poly:* v v)))
      (poly:derivative r varnum)))

;;; I don't know if this stuff is ever important...GJS

(define (assoc-accumulation rat:op poly:op rat:identity)
  (define (operate rats)
    (cond ((null? rats) rat:identity)
	  ((null? (cdr rats)) (car rats))
	  ((ratform? (car rats))
	   (cond ((ratform? (cadr rats))
		  (operate (cons (rat:op (car rats) (cadr rats))
				 (cddr rats))))
		 ((null? (cddr rats)) (rat:op (car rats) (cadr rats)))
		 ((not (ratform? (caddr rats)))
		  (operate (cons (car rats)
				 (cons (poly:op (cadr rats) (caddr rats))
				       (cdddr rats)))))
		 (else
		  (operate (cons (rat:op (car rats) (cadr rats))
				 (cddr rats))))))
	  ((ratform? (cadr rats))
	   (operate (cons (rat:op (car rats) (cadr rats))
			  (cddr rats))))
	  (else
	   (operate (cons (poly:op (car rats) (cadr rats))
			  (cddr rats))))))
  (lambda rats (operate rats)))

(define +$rcf (assoc-accumulation rcf:+ poly:+ rcf:zero))
(define *$rcf (assoc-accumulation rcf:* poly:* rcf:one))


(define (assoc-inverse-accumulation rat:inv-op rat:op rat:invert poly:op rat:identity)
  (let ((direct-op (assoc-accumulation rat:op poly:op rat:identity)))
    (define (operate . rats)
      (cond ((null? rats) rat:identity)
	    ((null? (cdr rats)) (rat:invert (car rats)))
	    (else (rat:inv-op (car rats) (apply direct-op (cdr rats))))))
    operate))

(define -$rcf
  (assoc-inverse-accumulation rcf:- rcf:+ rcf:negate poly:+ rcf:zero))
(define /$rcf
  (assoc-inverse-accumulation rcf:/ rcf:* rcf:invert poly:* rcf:one))

;;; For simplifier

(define (rcf:->expression p vars)
  (if (pcf? p)
      (pcf:->expression p vars)
      (symb:/ (pcf:->expression (ratform-numerator p) vars)
	      (pcf:->expression (ratform-denominator p) vars))))

(define (rcf:expression-> expr cont #!optional less?)
  ;; cont = (lambda (ratp vars) ... )
  (let ((evars
	 (sort (list-difference (variables-in expr)
				rcf:operators-known)
	       (if (default-object? less?) variable<? less?))))
    (cont ((expression-walker
	    (pair-up evars
		     (poly:new-variables (length evars))
		     rcf:operator-table))
	   expr)
	  evars)))

(define (rcf:->lambda p)
  (if (pcf? p)
      (poly->function p)
      (let* ((n (rcf:arity p))
	     (vars (generate-list-of-symbols 'x n))
	     (exp (rcf:->expression p vars)))	  
	`(lambda ,vars ,exp))))


(define rcf:operator-table
  `((+        ,+$rcf)
    (-        ,-$rcf)
    (*        ,*$rcf)
    (/        ,/$rcf)
    (negate   ,rcf:negate)
    (invert   ,rcf:invert)
    (expt     ,rcf:expt)
    (square   ,rcf:square)
    (gcd      ,rcf:gcd)))

(define rcf:operators-known
  (map car rcf:operator-table))
