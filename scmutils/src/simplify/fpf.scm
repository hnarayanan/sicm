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

;;;;      Flat Polynomial Form, for Commutative Rings

(declare (usual-integrations))

(define fpf:coeff? number?)
(define fpf:coeff-zero? zero?)

(define fpf:coeff-add +)
(define fpf:coeff-sub -)
(define fpf:coeff-mul *)
(define fpf:coeff-div /)

(define fpf:coeff-negate -)

(define fpf:coeff-expt expt)

(define fpf:coeff-divide scheme-number-divide)

;;; An fpf is a sorted list of terms.  
;;;  Each term has exponents and a coefficient. 

(define (fpf? x)
  (or (fpf:coeff? x) (explicit-fpf? x)))

(define (explicit-fpf? x)
  (and (pair? x)
       (eq? (car x) '*fpf*)))

(define (fpf:arity fpf)
  (if (fpf:coeff? fpf)
      0
      (fpf:number-of-vars (fpf:terms fpf))))

(define (fpf:number-of-vars termlist)
  (length (fpf:exponents (car termlist))))


(define (fpf:make terms)
  (cond ((null? terms) :zero)
	((and (null? (cdr terms))
	      (fpf:constant-term? (car terms)))
	 (fpf:coefficient (car terms)))
	(else
	 (cons '*fpf* terms))))

(define (fpf:terms fpf)
  (if (and (fpf:coeff? fpf) (fpf:coeff-zero? fpf))
      '()
      (cdr fpf)))


(define (fpf:make-term exponents coeff)
  (cons exponents coeff))

(define (fpf:exponents term)
  (car term))

(define (fpf:coefficient term)
  (cdr term))


(define (fpf:constant-term? term)
  (all-zeros? (fpf:exponents term)))

(define (all-zeros? exponents)
  (or (null? exponents)
      (and (fix:= 0 (car exponents))
	   (all-zeros? (cdr exponents)))))

(define (fpf:make-constant c arity)
  (list '*fpf*
	(fpf:make-term (make-list arity 0)
		       c)))
       
(define fpf:zero :zero)
(define fpf:one  :one)
(define fpf:-one :-one)

(define fpf:identity
  (fpf:make (list (fpf:make-term (list 1) :one))))

(define (fpf:new-variables n)
  (make-initialized-list n
    (lambda (i)
      (fpf:make (list (fpf:make-term
		       (make-initialized-list n
			 (lambda (j) (if (fix:= i j) 1 0)))
		       :one))))))


(define (fpf:same-exponents? fs1 fs2)
  (equal? fs1 fs2))

(define (fpf:>exponents? fs1 fs2)
  (fpf:graded> fs1 fs2))

(define (fpf:graded> fs1 fs2)		;Graded lexicographical order
  (let ((o1 (reduce fix:+ 0 fs1))
	(o2 (reduce fix:+ 0 fs2)))
    (cond ((fix:> o1 o2) #t)
	  ((fix:< o1 o2) #f)
	  (else
	   (fpf:lexicographical> fs1 fs2)))))

(define (fpf:lexicographical> fs1 fs2)	;Lexicographical order
  (let lp ((l1 fs1) (l2 fs2))
    (cond ((null? l1) #f)
	  ((null? l2) #t)
	  ((fix:> (car l1) (car l2)) #t)
	  ((fix:< (car l1) (car l2)) #f)
	  (else (lp (cdr l1) (cdr l2))))))


(define (fpf:map-coefficients proc terms)
  (if (null? terms)
      '()
      (let ((ncoeff (proc (fpf:coefficient (car terms)))))
	(if (fpf:coeff-zero? ncoeff)
	    (fpf:map-coefficients proc (cdr terms))
	    (cons (fpf:make-term (fpf:exponents (car terms))
				 ncoeff)
		  (fpf:map-coefficients proc (cdr terms)))))))

(define (fpf:binary-combine a1 a2 coeff-op terms-op opname)
  (define (wta)
    (error "Wrong type argument -- FPF" opname a1 a2))
  (if (fpf:coeff? a1)
      (if (fpf:coeff? a2)
	  (coeff-op a1 a2)
	  (if (explicit-fpf? a2)
	      (fpf:make
	       (terms-op (fpf:terms (fpf:make-constant a1 (fpf:arity a2)))
			 (fpf:terms a2)))
	      (wta)))
      (if (fpf:coeff? a2)
	  (if (explicit-fpf? a1)
	      (fpf:make
	       (terms-op (fpf:terms a1)
			 (fpf:terms (fpf:make-constant a2 (fpf:arity a1)))))
	      (wta))
	  (if (and (explicit-fpf? a1)
		   (explicit-fpf? a2)
		   (fix:= (fpf:arity a1) (fpf:arity a2)))
	      (fpf:make (terms-op (fpf:terms a1) (fpf:terms a2)))
	      (wta)))))

(define (fpf:+ a1 a2)
  (fpf:binary-combine a1 a2 fpf:coeff-add fpf:add-terms 'add))

(define (fpf:add-terms xlist ylist)
  (fpf:add-terms-general xlist ylist fpf:coeff-add))

(define (fpf:add-terms-general xlist ylist coeff-add)
  (let tloop ((xlist xlist) (ylist ylist))
    (cond ((null? xlist) ylist)
	  ((null? ylist) xlist)
	  (else
	   (let ((f1 (fpf:exponents (car xlist)))
		 (f2 (fpf:exponents (car ylist))))
	     (cond ((fpf:same-exponents? f1 f2)
		    (let ((ncoeff
			   (coeff-add (fpf:coefficient (car xlist))
				      (fpf:coefficient (car ylist)))))
		      (if (fpf:coeff-zero? ncoeff)
			  (tloop (cdr xlist) (cdr ylist))
			  (cons (fpf:make-term f1 ncoeff)
				(tloop (cdr xlist) (cdr ylist))))))
		   ((fpf:>exponents? f1 f2)
		    (cons (car xlist) (tloop (cdr xlist) ylist)))
		   (else
		    (cons (car ylist)
			  (tloop xlist (cdr ylist))))))))))

(define (fpf:- minuend subtrahend)
  (fpf:+ minuend (fpf:negate subtrahend)))

(define (fpf:scale scale-factor p)
  (if (fpf:coeff? p)
      (fpf:coeff-mul scale-factor p)
      (fpf:make (fpf:scale-terms scale-factor (fpf:terms p)))))

(define (fpf:scale-terms scale-factor terms)
  (fpf:scale-terms-general scale-factor terms fpf:coeff-mul))

(define (fpf:scale-terms-general scale-factor terms coeff-mul)
  (fpf:map-coefficients
   (lambda (coefficient)
     (coeff-mul scale-factor coefficient))
   terms))

(define (fpf:negate p)
  (if (fpf:coeff? p)
      (fpf:coeff-negate p)
      (fpf:make (fpf:negate-terms (fpf:terms p)))))

(define (fpf:negate-terms terms)
  (fpf:negate-terms-general terms fpf:coeff-negate))

(define (fpf:negate-terms-general terms neg)
  (fpf:map-coefficients neg terms))

(define (fpf:* m1 m2)
  (fpf:binary-combine m1 m2 fpf:coeff-mul fpf:mul-terms 'mul))

(define (fpf:mul-terms xlist ylist)
  (fpf:mul-terms-general xlist ylist fpf:coeff-add fpf:coeff-mul))

(define (fpf:mul-terms-general xlist ylist add mul)
  (let xloop ((xlist xlist))
    (if (null? xlist)
	'()
	(fpf:add-terms-general (fpf:term*terms-general (car xlist) ylist mul)
			       (xloop (cdr xlist))
			       add))))

(define (fpf:term*terms-general term terms coeff-mul)
  (let ((exponents (fpf:exponents term))
	(coeff (fpf:coefficient term)))
    (let lp ((terms terms))
      (if (null? terms)
	  '()
	  (cons (fpf:make-term
		 (fpf:combine-exponents exponents
					(fpf:exponents (car terms)))
		 (coeff-mul coeff (fpf:coefficient (car terms))))
		(lp (cdr terms)))))))

(define (fpf:combine-exponents exponents1 exponents2)
  (cond ((null? exponents1) exponents2)
	((null? exponents2) exponents1)
	(else
	 (map fix:+ exponents1 exponents2))))

(define (fpf:square p)
  (fpf:* p p))

(define (fpf:expt base exponent)
  (define (expt-iter x count answer)
    (if (int:zero? count)
	answer
	(if (even? count)
	    (expt-iter (fpf:square x) (int:quotient count 2) answer)
	    (expt-iter x (int:- count 1) (fpf:* x answer)))))
  (cond ((fpf:coeff? base) (fpf:coeff-expt base exponent))
	((not (explicit-fpf? base))
	 (error "Wrong type -- FPF:EXPT:" base exponent))
	((not (exact-integer? exponent))
	 (error "Can only raise an FPF to an exact integer power" base exponent))
	((negative? exponent)
	 (error "No inverse -- FPF:EXPT:" base exponent))
	(else
	 (expt-iter base exponent :one))))

(define (fpf:divide x y #!optional continue)
  (let ((cont
	 (if (default-object? continue)
	     (lambda (q r) (list (fpf:make q) (fpf:make r)))
	     (lambda (q r) (continue (fpf:make q) (fpf:make r))))))
    (if (and (fpf:coeff? x) (fpf:coeff y))
	(fpf:coeff-divide x y cont)
	(cond ((and (fpf:coeff? x) (explicit-fpf? y))
	       (fpf:divide-terms (fpf:terms (fpf:make-constant x (fpf:arity y)))
				 (fpf:terms y)
				 cont))
	      ((and (fpf:coeff? y) (explicit-fpf? x))
	       (fpf:divide-terms (fpf:terms x)
				 (fpf:terms (fpf:make-constant y (fpf:arity x)))
				 cont))
	      ((and (explicit-fpf? x)
		    (explicit-fpf? y)
		    (fix:= (fpf:arity x) (fpf:arity y)))
	       (fpf:divide-terms (fpf:terms x) (fpf:terms y) cont))
	      (else (error "Bad arguments -- FPF:DIVIDE" x y))))))

(define (fpf:divide-terms termlist1 termlist2 #!optional continue)
  (if (default-object? continue) (set! continue list))
  (fpf:divide-terms-general termlist1 termlist2
    fpf:coeff-add fpf:coeff-mul fpf:coeff-div fpf:coeff-negate continue))

(define (fpf:divide-terms-general numerator-terms denominator-terms add mul div neg cont)
  (let ((dexps (fpf:exponents (car denominator-terms)))
	(dcoeff (fpf:coefficient (car denominator-terms))))
    (define (dloop nterms cont)
      (if (null? nterms)
	  (cont '() '())
	  (let ((nexps (fpf:exponents (car nterms))))
	    (cond ((*and (map >= nexps dexps))
		   (let ((qt
			  (fpf:make-term (map int:- nexps dexps)
			    (div (fpf:coefficient (car nterms)) dcoeff))))
		     (dloop (fpf:add-terms-general nterms
			      (fpf:negate-terms-general
			       (fpf:term*terms-general
				qt denominator-terms mul)
			       neg)
			      add)
			    (lambda (q r)
			      (cont (fpf:add-terms-general
				     (list qt) q add) r)))))
		  (else
		   (dloop (cdr nterms)
			  (lambda (q r)
			    (cont q
				  (fpf:add-terms-general
				   (list (car nterms)) r add)))))))))
    (dloop numerator-terms cont)))

(define (fpf:horner-eval poly args)
  (if (fpf:coeff? poly) poly (fpf:horner-eval-terms (fpf:terms poly) args)))

(define (fpf:horner-eval-terms terms args)
  (fpf:horner-eval-general terms args
    fpf:coeff-add fpf:coeff-sub fpf:coeff-mul fpf:coeff-expt))

(define (fpf:horner-eval-general terms args add sub mul expt)
  (if (null? terms)
      :zero
      (let hloop ((terms (cdr terms))
		  (exponents (fpf:exponents (car terms)))
		  (sum (fpf:coefficient (car terms))))
	(if (null? terms)
	    (mul sum (a-reduce mul (map expt args exponents)))
	    (let ((new-exponents (fpf:exponents (car terms))))
	      (hloop (cdr terms)
		     new-exponents
		     (add (fpf:coefficient (car terms))
			  (mul sum
			       (a-reduce mul
					 (map expt
					      args
					      (map int:-
						   exponents
						   new-exponents)))))))))))

;;; Converting between flat polynomials and other kinds of expressions

(define (fpf:->expression p vars)
  (cond ((fpf:coeff? p) p)
	((explicit-fpf? p)
	 (a-reduce symb:+
		   (map (lambda (term)
			  (symb:* (fpf:coefficient term)
				  (a-reduce symb:*
					    (map (lambda (exponent var)
						   (symb:expt var exponent))
						 (fpf:exponents term)
						 vars))))
			(fpf:terms p))))
	(else
	 (error "Bad fpf -- ->EXPRESSION" p vars))))


(define (fpf:expression-> expr cont #!optional less?)
  ;; cont = (lambda (poly vars) ... )
  (let ((evars
	 (sort (list-difference (variables-in expr)
				fpf:operators-known)
	       (if (default-object? less?) variable<? less?))))
    (cont ((expression-walker
	    (pair-up evars
		     (fpf:new-variables (length evars))
		     fpf:operator-table))
	   expr)
	  evars)))


(define +$fpf (accumulation fpf:+ fpf:zero))
(define -$fpf (inverse-accumulation fpf:- fpf:+ fpf:negate fpf:zero))
(define *$fpf (accumulation fpf:* fpf:one))

(define fpf:operator-table
  `((+        ,+$fpf)
    (-        ,-$fpf)
    (*        ,*$fpf)
    (negate   ,fpf:negate)
    (square   ,fpf:square)
    (expt     ,fpf:expt)))

(define fpf:operators-known (map car fpf:operator-table))
