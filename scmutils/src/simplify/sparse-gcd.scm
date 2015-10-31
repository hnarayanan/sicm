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

;;;;             Sparse Multivariate Polynomial GCD 
;;;    a probabilistic method inspired by Richard Zippel's thesis
;;;      coded and debugged by Gerald Jay Sussman and Dan Zuras  
;;;              June 1998, refactored by GJS Feb 2011.

;;; This code differs from Zippel's in that it does not use modular
;;; arithmetic or do anything special with the Vandermonde matrices
;;; that arise in the problem.  This makes the idea of using sparse
;;; interpolation stand out in stark contrast without the confusing
;;; complications introduced by those optimizations.

(declare (usual-integrations))

;;; Given pcfs or fpfs
(define (poly/gcd-sparse u v)  
  (gcd-check-same-arity u v)
  (poly/gcd/sparse (poly->sparse u) (poly->sparse v)
    (lambda (g)
      (sparse->poly g (gcd-target-type u)))
    (lambda () #f)))

(define (poly/gcd/sparse su sv win lose)
  (let ((n (length (sparse-exponents (car su)))))
    (if (not (fix:= n (length (sparse-exponents (car sv)))))
	(error "Unequal arities--poly/gcd/sparse" su sv))		    
    (if (or (there-exists? su
			   (lambda (term)
			     (let ((c (sparse-coefficient term)))
			       (or (not (number? c)) (inexact? c)))))
	    (there-exists? sv
			   (lambda (term)
			     (let ((c (sparse-coefficient term)))
			       (or (not (number? c)) (inexact? c))))))
	(win (sparse-one n))
	(sparse-gcd su sv win lose))))

(define (sparse-gcd u v win lose)
  (sparse-gcd-wrapper u v win lose
    (lambda (P Q win lose)
      (let ((n (length (sparse-exponents (car P))))
	    (dPs (map (lambda (l) (apply max l))
		      (list-transpose (map car P))))
	    (dQs (map (lambda (l) (apply max l))
		      (list-transpose (map car Q)))))
	(let ((ds (map min dPs dQs)))
	  ;; ds is the list of the maximum possible degree for each
	  ;; variable in any possible gcd of P and Q.
	  (if (all-zeros? ds)		;No variables in common.
	      (win (sparse-one n))
	      (if *heuristic-sparse-gcd-enabled*
		  (sparse-heuristic-gcd P Q n ds
                     win
		     (lambda ()
		       (sparse-multivariate-gcd P Q n ds win lose)))
		  (sparse-multivariate-gcd P Q n ds win lose))))))))

(define *heuristic-sparse-gcd-enabled* #t)

;;; The purpose of sparse-gcd-wrapper is to remove and replace the
;;; content, so as to present sparse-heuristic-gcd and
;;; sparse-multivariate-gcd programs with primitive polynomials.  The
;;; sparse-content is defined to include the largest monomial factor,
;;; thus lowering the degree as much as possible.

(define (sparse-gcd-wrapper u v win lose further-work)
  (cond ((null? u) (win v))
	((null? v) (win u))
	((sparse-univariate? u)
	 (win (sparse-univariate-gcd u v)))
	((equal? u v) (win u))
	((sparse-one? u) (win u))
	((sparse-one? v) (win v))
	(else
	 (let ((uc (sparse-content u)) (vc (sparse-content v))
	       (win (lambda (g) (win (sparse-abs g)))))
	   (if (sparse-one-term? uc)
	       (if (sparse-one-term? vc)
		   (further-work u v win lose)
		   (further-work u (sparse-normalize v vc) win lose))
	       (if (sparse-one-term? vc)
		   (further-work (sparse-normalize u uc) v win lose)
		   (let ((c (sparse-monomial-gcd uc vc)))
		     (if (sparse-one-term? c)
			 (further-work (sparse-normalize u uc)
				       (sparse-normalize v vc)
				       win lose)
			 (further-work (sparse-normalize u uc)
				       (sparse-normalize v vc)
				       (lambda (g)
					 (win (sparse-scale g c)))
				       lose)))))))))

(define (sparse-monomial-gcd m1 m2)
  (sparse-term (map min (sparse-exponents m1) (sparse-exponents m2))
	       (base/gcd (sparse-coefficient m1) (sparse-coefficient m2))))

(define (sparse-content poly)
  (let lp ((p (cdr poly)) (ans (car poly)))
    (cond ((null? p) ans)
	  ((sparse-one-term? ans) ans)
	  (else (lp (cdr p) (sparse-monomial-gcd (car p) ans))))))

;;; sparse-heuristic-gcd is a quick-and-dirty test for GCD=1 before
;;; trying something more general and more expensive.

;;; Idea: let P,Q be polynomials, and let x0 be random numerical
;;; arguments.

;;;                 gcd
;;;  P(x),  Q(x) |------------> G(x)
;;;   -      -                   -
;;;   |      |                   |
;;;   |      |                   |
;;;   |      |                   |
;;;   V      V      gcd          V  
;;;  P(x0), Q(x0) |-----> G'= K*G(x0)

;;; If x0 is big enough, then G'=1 is good evidence that G(x)=1.

;;; It is assumed that the arities are the same and at least one.  It
;;; is also assumed that these polys have no content.

;;; It calls the win continuation with the sparse one polynomial if it
;;; succeeds and the lose continuation with no arguments if it fails.

;;; The maximum possible degree of the gcd in each variable is the
;;; minimum of the max in each input polynomial.

;;; If this many experiments yield 1 we conclude that gcd=1
(define *heuristic-sparse-gcd-trials* 4)

;;; Effectiveness Statistics
(define *heuristic-sparse-gcd-win* 0)
(define *heuristic-sparse-gcd-lose* 0)
(define *heuristic-sparse-gcd-bad-decision* 0)
;;; See end of file for experiments that show this is valuable.

(define (sparse-heuristic-gcd p1 p2 n ds win lose)
  (let lp ((trials 0) (args (generate-list n interpolate-random)))
    (if (fix:= trials *heuristic-sparse-gcd-trials*)
	(begin (set! *heuristic-sparse-gcd-win*
		     (+ *heuristic-sparse-gcd-win* 1))
	       (win (sparse-one n)))
	(let ((v1 (sparse-evaluate p1 args))
	      (v2 (sparse-evaluate p2 args)))
	  (cond ((= (base/gcd v1 v2) 1)
		 (lp (fix:+ trials 1)
		     (generate-list n interpolate-random)))
		(else
		 (set! *heuristic-sparse-gcd-lose*
		       (+ *heuristic-sparse-gcd-lose* 1))
		 (lose)))))))


;;; sparse-multivariate-gcd is the actual sparse gcd program.
;;; It calls the win continuation with the gcd sparse polynomial if it
;;; succeeds and the lose continuation with no arguments if it fails.

;;; It is assumed that the arities are the same and at least one.  It
;;; is also assumed that these polys have no content.

;;; The maximum possible degree of the gcd in each variable is the
;;; minimum of the max in each input polynomial.

;;; These are used to put bounds on the skeletons to be interpolated.
;;; We also sort the indeterminates to minimize computation.

(define (sparse-multivariate-gcd P Q n ds win lose)
  ;; The continuations win = (lambda (g) ...); lose = (lambda () ...)
  (define (adjust P permutation)
    (map (lambda (term)
	   (sparse-term (permutation (sparse-exponents term))
			(sparse-coefficient term)))
	 P))
  (sort-and-permute ds <
    (lambda (ods nds perm iperm)
      (if (equal? ods nds)
	  (sparse-multivariate-gcd-helper P Q n ods
            (lambda (g)
              (if (null? g)
	          (win (sparse-one n))
		  (win
	            (sparse-normalize g
		      (sparse-constant-term n
               	        (sparse-base-content g))))))
	    lose)
	  (sparse-multivariate-gcd-helper (adjust P perm) (adjust Q perm) n nds
            (lambda (g)
              (if (null? g)
	          (win (sparse-one n))
		  (win
		    (adjust (sparse-normalize g
		              (sparse-constant-term n
               	                (sparse-base-content g)))
			    iperm))))
	    lose)))))

;;; How hard are we willing to try for this gcd?

(define *sgcd-restart-limit* 200)
(define *sgcd-stage-limit* 2)


;;; Debugging stuff

(define *sgcd-wallp* #f)
(define *sgcd-tuning* #f)

(define (sparse-multivariate-gcd-helper P Q n ds win lose)
  ;; P, Q of arity n (They have n indeterminates.)  
  ;; ds is the n-long list of maximum degrees for each of the indeterminates.
  ;; The continuations win = (lambda (g) ...); lose = (lambda () ...)
  (if *sgcd-wallp* (pp `(sparse-gcd: (P ,P) (Q ,Q) (n ,n) (ds ,ds))))
  ;; The following is a piece of shit and a bad idea...
  (reset-interpolation-args! ds
			     (apply max
				    (map magnitude (map sparse-coefficient P)))
			     (apply max
				    (map magnitude (map sparse-coefficient Q))))

  ;; Now for the real stuff
  (let restart ((restart-count 0))
    (if *sgcd-wallp* (pp `(restart ,restart-count)))
    (if (or (fix:> restart-count *sgcd-restart-limit*)
	    (allocated-time-expired?))
	(lose) 				;failed!
	(let* ((rargs0 (make-interpolation-args (- n 1)))
	       (P0 (sparse-evaluate> P rargs0))
	       (Q0 (sparse-evaluate> Q rargs0))
	       (g0 (sparse-univariate-gcd P0 Q0)))
	  ;; P0&Q0 are univariate polys obtained from P&Q by replacing the
	  ;; rightmost arguments with n-1 random numbers.  Thus the argument
	  ;; with index 0 is an indeterminate.  g0 is their univariate GCD.
	  ;; These will be used to start the process.  In the kth stage we
	  ;; determine g_k, the GCD of Pk and Qk, where the arguments [0,k] are
	  ;; indeterminates.  When k=n-1 Pk and Qk are P and Q.  The g resulting
	  ;; from that will then be the GCD of P&Q.
	  (if *sgcd-wallp*
	      (pp `(restarted (rargs0 ,rargs0) (P0 ,P0) (Q0 ,Q0) (g0 ,g0))))

	  (let stagelp ((k 1) (g g0) (rargs rargs0) (stage-fail-count 0))	
	    (cond ((sparse-zero? g)	;if P/=0 and Q/=0 then g/=0
		   (if *sgcd-wallp* (pp `(g=zero! ,k ,rargs)))
		   (restart (fix:+ restart-count 1)))
		  ((fix:= k n)
		   (if *sgcd-tuning*
		       (pp
			`(restarts= ,restart-count P= ,P Q= ,Q G= ,g n= ,n ds= ,ds)))
		   (win g))
		  ((fix:> stage-fail-count *sgcd-stage-limit*)
		   (restart (fix:+ restart-count 1)))
		  ((and (not (fix:= k (fix:- n 1))) (= (list-ref ds k) 0))
		   (stagelp (fix:+ k 1)
			    (map (lambda (term)
				   (sparse-term (append (sparse-exponents term) '(0))
						(sparse-coefficient term)))
				 g)
			    (cdr rargs)
			    0))

		  ;;  g=g_k-1 has k indeterminates and nterms terms.
		  ;; We can solve for the coefficients of those terms
		  ;; if we have values for gk, at a suitable number of
		  ;; sample points, by solving the linear equations
		  ;; resulting from substituting each of n sets of k
		  ;; numbers for the k indeterminates with the
		  ;; corresponding RHS being the value of g_k for that
		  ;; set of k arguments.
		  (else
		   (let* (;; To avoid interpolating all possible terms
			  ;; we use the skeleton of g_k-1 to guide
			  ;; which terms may appear in g_k that we
			  ;; need to determine.
			  (skeleton (map sparse-exponents g))
			  (nterms (length skeleton))
			  (trial-arglists
			   (generate-list nterms
			     (lambda (i)
			       (make-interpolation-args k))))
			  ;; But the undetermined coefficients of the nterms of
			  ;; the skeleton of g are polynomials of arity n-k.  We
			  ;; can solve only for univariate stuff, so we make
			  ;; Pk,Qk have arity k+1, by evaluating for the
			  ;; rightmost n-(k+1) arguments.  Then, by evaluating
			  ;; these for the first k arguments with the arglist
			  ;; arguments, and getting the GCDs of those, we get
			  ;; univariate Gks.  All of these must be values of the
			  ;; same Gk, so the skeletons must be the same to
			  ;; proceed.
			  (Pk (sparse-evaluate> P (cdr rargs)))
			  (Qk (sparse-evaluate> Q (cdr rargs)))
			  (Gks (map
				(lambda (arglist)
				  (sparse-univariate-gcd
				   (sparse-evaluate< Pk arglist)
				   (sparse-evaluate< Qk arglist)))
				trial-arglists))
			  (GkSkels (map (lambda (Gk)
					  (map sparse-exponents Gk))
					Gks))
			  (nGkTerms (length (car GkSkels)))
			  (maxGkTerms (fix:+ (list-ref ds k) 1)))
		     (if *sgcd-wallp*
			 (pp `(stage (k ,k) (g ,g) (rargs ,rargs)
			       (skeleton ,skeleton)
			       (Pk ,Pk) (Qk ,Qk)
			       (trial-arglists ,trial-arglists)
			       (Gks ,Gks) (GkSkels ,GkSkels))))

		     ;; If the interpolation skeletons are not all equal, the
		     ;; random numbers were probably not good enough.  Once we
		     ;; have good enough Gks, we can be sure that the order of
		     ;; Gk is not greater than the min of the max orders of the
		     ;; kth variable in the initial P and Q.  This tells us m,
		     ;; the number of terms we need to get for Gk, and thus for
		     ;; each of the coefficients of the skeleton of g=g_k-1.  So
		     ;; we have a two layer interpolation.  We invert M and get
		     ;; solutions for the coefficients of the g skeleton for
		     ;; each of m random values for the kth argument of gk.  We
		     ;; then univariate interpolate for each of the coefficients
		     ;; of gk.  This gk is a candidate for the gcd of Pk&Qk.  If
		     ;; it works, we go to the next stage.

		     (cond ((not (all-equal? GkSkels))
			    (if *sgcd-wallp* (pp '(GkSkels not all the same)))
			    (stagelp k g rargs (fix:+ stage-fail-count 1)))
			   ((not (fix:<= nGkTerms maxGkTerms))
			    (if *sgcd-wallp* (pp '(Too many GkTerms)))
			    (restart (fix:+ restart-count 1)))
			   (else
			    (lu-decompose
			     (matrix-by-row-list
			      (map (lambda (arguments)
				     (map (lambda (exponents)
					    (apply *
						   (map expt arguments exponents)))
					  skeleton))
				   trial-arglists))
			     (lambda (matrix permutation sign)
			       (let* ((xk+1s
				       (make-interpolation-args maxGkTerms))
				      (coeffs
				       (map (lambda (xk+1)
					      (let ((values
						     (map (lambda (Gk)
							    (sparse-evaluate
							     Gk (list xk+1)))
							  Gks)))
						(vector->list
						 (lu-backsubstitute matrix permutation
							    (list->vector values)))))
					    xk+1s)))
				 (if *sgcd-wallp*
				     (pp `(after-lu (xk+1s ,xk+1s) (coeffs ,coeffs))))
				 (let clp ((css (list-transpose coeffs)) (cps '()))
				   (if *sgcd-wallp* (pp `(clp (css ,css) (cps ,cps))))
				   (if (null? css)
				       (let ((gk (expand-poly g (reverse! cps))))
					 (if *sgcd-wallp* (pp `(gk ,gk)))
					 (if (and (sparse-divisible? Pk gk)
						  (sparse-divisible? Qk gk))
					     (begin (if *sgcd-wallp* (pp '(divide won)))
						    (stagelp (fix:+ k 1) gk (cdr rargs) 0))
					     (begin (if *sgcd-wallp* (pp '(divide lost)))
						    (stagelp k g rargs
							     (fix:+ stage-fail-count 1))
						    ;;(restart (fix:+ restart-count 1))
						    )))
				       (univariate-interpolate-values xk+1s (car css)
				         (lambda (cp) (clp (cdr css) (cons cp cps)))
					 (lambda ()
					   (if *sgcd-wallp* (pp '(interpolation failed)))
					   (stagelp k g rargs (fix:+ stage-fail-count 1))
					   ;;(restart (fix:+ restart-count 1))
					   ))))))
			     (lambda (x)
			       (if *sgcd-wallp* (pp `(singular)))
			       (stagelp k g rargs (fix:+ stage-fail-count 1))
			       ;;(restart (fix:+ restart-count 1))
			       ))))))))))))

;;; This starts out with small primes and works its way by requiring a
;;; restart if necessary.

(define *interpolate-primes-stream* '())

(define (reset-interpolation-args! ds max-c-p max-c-q)
  (set! *interpolate-primes-stream*
	(stream-tail prime-numbers-stream (apply max ds)))
  'done)

(define (make-interpolation-args k)
  (let lp ((i 0) (s *interpolate-primes-stream*) (args '()))
    (if (fix:= i k)
	(begin (set! *interpolate-primes-stream* s)
	       args)
	(lp (fix:+ i 1) (tail s) (cons (head s) args)))))

#|
;;; This is trying to be a good boy, using the formula from Zippel for
;;; the mod prime, but I think it is not really necessary.  Timings at
;;; the end are using this choice...  The numbers here are usually too
;;; big for comfort.

(define (reset-interpolation-args! ds max-c-p max-c-q)
  (first-prime-stream-exceeding!
   (max max-c-p max-c-q (apply max ds)))
  'done)

(define (first-prime-stream-exceeding! n)
  (let lp ((s prime-numbers-stream))
    (if (> (head s) n)
	(set! *interpolate-primes-stream* s)
	(lp (tail s)))))
|#
#|
;;; This randomly works pretty well, but... it doesn't work for high
;;; degree polys, because the numbers that we start out with are
;;; pretty big.

(define (reset-interpolation-args! ds max-c-p max-c-q)
  'done)

(define *number-of-primes* 5000)

(define *prime-table*
  (make-initialized-vector *number-of-primes*
			   (lambda (i)
			     (stream-ref prime-numbers-stream i))))

(define (make-interpolation-args k)
  (generate-list k interpolate-prime))

(define (interpolate-prime i)
  (vector-ref *prime-table* (random *number-of-primes*)))
|#

#|
;;; I tried relatively-prime stuff, and it doesn't really do the job!
;;; This was a loser partly because it spent forever augmenting the list.
;;; That was dumb.

(define *interpolation-args* '())

(define (reset-interpolation-args! ds max-c-p max-c-q)
  (set! *interpolation-args* '()))

(define (make-interpolation-args k)
  (let next ((i 0) (args '()))
    (if (fix:= i k)
	args
	(let try-again ((trial (random *interpolate-size*)))
	  (if (for-all? *interpolation-args*
		(lambda (a) (= (base/gcd a trial) 1)))
	      (begin (set! *interpolation-args*
			   (cons trial *interpolation-args*))
		     (next (fix:+ i 1) (cons trial args)))
	      (try-again (random *interpolate-size*)))))))
|#

#|
;;; But perhaps this isn't so stupid?

(define (reset-interpolation-args! ds max-c-p max-c-q)
  ;; I have no idea what to do here.
  'done)

(define (make-interpolation-args k)
  (let next ((i 0) (args '()))
    (if (fix:= i k)
	args
	(let try-again ((trial (+ (random *interpolate-size*) 1)))
	  (if (for-all? args
		(lambda (a) (= (base/gcd a trial) 1)))
	      (next (fix:+ i 1) (cons trial args))
	      (try-again (random *interpolate-size*)))))))
|#

(define (sparse-univariate-gcd u v)		;Euclid's Algorithm is OK here.
  (define (pgcd ppu ppv)
    (if *ugcd-wallp* (pp `((ppu: ,ppu) (ppv: ,ppv))))
    (cond ((null? ppv) ppu)		;v=0      => u
	  ((sparse-constant? ppv)	;deg(v)=0 => 1
	   sparse-univariate-one)
	  (else
	   (pgcd ppv
		 (sparse-univariate-primitive-part
		  (sparse-univariate-pseudo-remainder ppu ppv))))))
  (cond ((null? u) v)
	((null? v) u)
	((sparse-constant? u)
	 (sparse-univariate-constant
	  (base/gcd (sparse-coefficient (car u))
		    (sparse-base-content v))))
	((sparse-constant? v)
	 (sparse-univariate-constant
	  (base/gcd (sparse-base-content u)
		    (sparse-coefficient (car v)))))
	(else
	 (let ((uc (sparse-base-content u))
	       (vc (sparse-base-content v)))
	   (let ((ans
		  (if (= uc 1)
		      (if (= vc 1)
			  (pgcd u v)
			  (pgcd u (sparse-univariate-normalize v vc)))
		      (if (= vc 1)
			  (pgcd (sparse-univariate-normalize u uc) v)
			  (let ((c (base/gcd uc vc)))
			    (if (= c 1)
				(pgcd (sparse-univariate-normalize u uc)
				      (sparse-univariate-normalize v vc))
				(sparse-univariate-scale
				 (pgcd (sparse-univariate-normalize u uc)
				       (sparse-univariate-normalize v vc))
				 c)))))))

	     (let ((ans (sparse-abs ans)))
	       (if *ugcd-testing*
		   (assert (and (sparse-divisible? u ans)
				(sparse-divisible? v ans))))
	       ans))))))

(define *ugcd-wallp* #f)
(define *ugcd-testing* #f)

(define (sparse-base-content poly)
  (let lp ((p (cdr poly)) (ans (sparse-coefficient (car poly))))
    (cond ((null? p) ans)
	  ((= ans 1) 1)
	  (else
	   (lp (cdr p)
	       (base/gcd (sparse-coefficient (car p)) ans))))))

(define (sparse-univariate-primitive-part poly)
  (if (null? poly)
      '()
      (sparse-univariate-normalize poly (sparse-base-content poly))))

(define (sparse-univariate-pseudo-remainder u v)
  (let ((cvn	                               ;leading coefficient of v
	 (sparse-coefficient (car v)))
	(n                                     ;degree v
	 (car (sparse-exponents (car v)))))
    (let lp ((u u) )
      (if (null? u)
	  '()
	  (let ((cum                           ;leading coefficient of u
		 (sparse-coefficient (car u)))
		(m			       ;degree u
		 (car (sparse-exponents (car u)))))
	    (if (< m n)
		u
		(lp (sparse-add (sparse-univariate-scale u cvn)
				(sparse-multiply-term
				 (sparse-term (list (- m n)) (- cum))
				 v)))))))))

(define (sparse-univariate-constant coeff)
  (list (sparse-term '(0) coeff)))

(define sparse-univariate-one
  (sparse-univariate-constant 1))

(define (sparse-univariate-scale p c)
  (map (lambda (term)
	 (sparse-term (sparse-exponents term)
		      (* c (sparse-coefficient term))))
       p))

(define (sparse-univariate-normalize p c)
  (map (lambda (term)
	 (sparse-term (sparse-exponents term)
		      (/ (sparse-coefficient term) c)))
       p))

#|
;;; Knuth's test
(sparse-univariate-gcd
 '(((8) . 1) ((6) . 1) ((4) . -3) ((3) . -3) ((2) . 8) ((1) . 2) ((0) . -5))
 '(((6) . 3) ((4) . 5) ((2) . -4) ((1) . -9) ((0) . 21)))

((ppu: (((8) . 1) ((6) . 1) ((4) . -3) ((3) . -3) ((2) . 8) ((1) . 2) ((0) . -5)))
 (ppv: (((6) . 3) ((4) . 5) ((2) . -4) ((1) . -9) ((0) . 21))))
((ppu: (((6) . 3) ((4) . 5) ((2) . -4) ((1) . -9) ((0) . 21)))
 (ppv: (((4) . -5) ((2) . 1) ((0) . -3))))
((ppu: (((4) . -5) ((2) . 1) ((0) . -3)))
 (ppv: (((2) . -13) ((1) . -25) ((0) . 49))))
((ppu: (((2) . -13) ((1) . -25) ((0) . 49)))
 (ppv: (((1) . -4663) ((0) . 6150))))
((ppu: (((1) . -4663) ((0) . 6150)))
 (ppv: (((0) . 1))))
;Value: (((0) . 1))
|#

(define (sparse-gcd-test n-trials arity max-order max-coeff max-terms)
  (define (random-sign)
    (if (= (random 2) 0) 1 -1))
  (define (random-monomial)
    (sparse-term (generate-list arity
				(lambda (_)
				  (random (+ max-order 1))))
		 (* (random-sign)
		    (+ (random max-coeff) 1))))
  (define (random-polynomial)
    (reduce sparse-add '()
	    (map list 
		 (generate-list max-terms
				(lambda (_) (random-monomial))))))
  (fluid-let ((*heuristic-sparse-gcd-enabled* #f))
    (let loop ((i 0))
      (if (> i n-trials)
	  #t
	  (let ((A (random-polynomial))
		(B (random-polynomial))
		(C (random-polynomial)))
	    (let ((A (sparse-abs (sparse-normalize A (sparse-content A)))))
	      (let ((AB (sparse-multiply A B))
		    (AC (sparse-multiply A C))
		    (gBC (sparse-gcd B C)))
		(let* ((AgBC (sparse-multiply A gBC))
		       (gABAC (sparse-gcd AB AC)))
		  (cond ((or (not (sparse-divisible? B gBC))
			     (not (sparse-divisible? C gBC)))
			 (pp `(gcd-failed1 ,i ,B ,C ,gBC))
			 (error "bad")
			 #f)
			((or (not (sparse-divisible? AB gABAC))
			     (not (sparse-divisible? AC gABAC)))
			 (pp `(gcd-failed2 ,i ,AB ,AC ,gABAC))
			 (error "bad")
			 #f)
			((not (equal? AgBC gABAC))
			 (pp (list 'not-gcd i A B C AB AC gBC AgBC gABAC))
			 #f)
			(else (loop (fix:+ i 1))))))))))))


#|
(define (gcd-test d f g)
  (let ((pd (fpf:expression-> d (lambda (p v) p)))
	(pf (fpf:expression-> f (lambda (p v) p)))
	(pg (fpf:expression-> g (lambda (p v) p))))
    (let ((pdf (fpf:* pd pf)) (pdg (fpf:* pd pg)))
      (let ((gcd?
	     (sparse-gcd
	      (fpf:->sparse pdf)
	      (fpf:->sparse pdg)))
	    (ans
	     (fpf:->sparse pd)))
	(if (equal? gcd? ans)
	    #t
	    (pp (list (list 'gcd? gcd?)
		      (list 'ans ans))))))))


(define d1
  '(+ (expt x1 2) x1 3))

(define f1
  '(+ (* 2 (expt x1 2)) (* 2 x1) 1))

(define g1
  '(+ (expt x1 2) (* 2 x1) 2))

;(show-time (lambda () (gcd-test d1 f1 g1)))
;process time: 10 (10 RUN + 0 GC); real time: 5
;Value: #t


(define d2
  '(+ (* 2 (expt x1 2) (expt x2 2))
      (* x1 x2)
      (* 2 x1)))

(define f2
  '(+ (expt x2 2)
      (* 2 (expt x1 2) x2)
      (expt x1 2)
      1))

(define g2
  '(+ (* (expt x1 2) (expt x2 2))
      (* (expt x1 2) x2)
      (* x1 x2)
      (expt x1 2)
      x1))

;(show-time (lambda () (gcd-test d2 f2 g2)))
;process time: 40 (40 RUN + 0 GC); real time: 34
;Value: #t

(define d3
  '(+ (* x2 x2 x3 x3)
      (* x2 x2 x3)
      (* 2 x1 x1 x2 x3)
      (* x1 x3)))

(define f3
  '(+ (* x3 x3)
      (* x2 x2 x3)
      (* x1 x1 x2 x3)
      (* x1 x3)
      (* x1 x1 x2 x2)))

(define g3
  '(+ (* x2 x3)
      (* 2 x1 x3)
      x3
      x1))

;(show-time (lambda () (gcd-test d3 f3 g3)))
;process time: 80 (80 RUN + 0 GC); real time: 83
;Value: #t


(define d4
  '(+ (* x1 x1 x4 x4)
      (* x2 x2 x3 x4)
      (* x1 x1 x2 x4)
      (* x2 x4)
      (* x1 x1 x2 x3)))

(define f4
  '(+ (* x1 x2 x3 x3 x4 x4)
      (* x1 x3 x3 x4 x4)
      (* x1 x4 x4)
      (* x4 x4)
      (* x1 x3 x4)))

(define g4
  '(+ (* x1 x3 x3 x4 x4)
      (* x3 x3 x4 x4)
      (* x4 x4)
      (* x1 x2 x2 x3 x4)
      (* x1 x2 x2)))

;(show-time (lambda () (gcd-test d4 f4 g4)))
;process time: 240 (240 RUN + 0 GC); real time: 238
;Value: #t

(define d5
  '(+ (* x1 x1 x1 x2 x2 x3 x3 x4 x5 x5)
      (* x1 x2 x2 x5 x5)
      (* x1 x1 x1 x3 x4 x4 x5)
      (* x1 x1 x1 x2 x3 x3 x4 x5)
      (* x1 x1 x2 x3 x3 x4 x4)))

(define f5
  '(+ (* x1 x2 x2 x5 x5)
      (* x1 x2 x3 x3 x4 x5)
      (* x1 x2 x3 x3 x4 x4)
      (* x1 x2 x2 x4 x4)
      1))

(define g5
  '(+ (* x1 x3 x3 x4 x5 x5)
      (* x2 x5 x5)
      (* x1 x2 x4 x5)
      (* x2 x5)
      (* x1 x2 x3 x4 x4)))

;(show-time (lambda () (gcd-test d5 f5 g5)))
;process time: 450 (450 RUN + 0 GC); real time: 453
;Value: #t


(define d6
  '(+ (* x1 x2 x4 x4 x5 x5 x6 x6)
      (* x1 x2 x2 x3 x3 x4 x5 x5 x6 x6)
      (* x1 x1 x3 x6 x6)
      (* x1 x1 x2 x3 x3 x4 x5 x5 x6)
      (* x1 x1 x3 x5 x6)))

(define f6
  '(+ (* x1 x1 x2 x4 x5 x5 x6 x6)
      (* x1 x3 x5 x5 x6 x6)
      (* x1 x2 x2 x6 x6)
      (* x1 x1 x2 x2 x3 x3 x5 x6)
      (* x1 x3 x3 x4 x5)))

(define g6
  '(+ (* x2 x2 x3 x3 x4 x5 x5 x6)
      (* x1 x4 x4 x5 x6)
      (* x2 x2 x3 x3 x4 x5 x6)
      (* x1 x2 x2 x3 x4 x4 x6)
      (* x1 x1 x3 x5 x5)))

;(show-time (lambda () (gcd-test d6 f6 g6)))
;process time: 460 (460 RUN + 0 GC); real time: 460
;Value: #t

(define d7
  '(+ (* x1 x2 x2 x4 x4 x6 x6 x7 x7)
      (* x1 x1 x3 x4 x6 x6 x7 x7)
      (* x3 x3 x4 x4 x7 x7)
      (* x1 x1 x2 x4 x4 x6)
      (* x3 x4 x5 x5)))

(define f7
  '(+ (* x1 x1 x2 x4 x4 x5 x6 x6 x7 x7)
      (* x1 x2 x3 x6 x7)
      (* x3 x4 x4 x5 x5 x7)
      (* x1 x1 x2 x3 x4 x4 x5 x6)))

(define g7
  '(+ (* x1 x3 x5 x6 x6 x7 x7)
      (* x2 x2 x3 x3 x4 x4 x5 x6 x7 x7)
      (* x4 x6 x7 x7)
      (* x1 x1 x2 x3 x5 x6 x7)
      (* x1 x1 x3 x3 x4 x5 x5)))


;(show-time (lambda () (gcd-test d7 f7 g7)))
;process time: 690 (690 RUN + 0 GC); real time: 685
;Value: #t


(define d8
  '(+ (* x2 x2 x4 x5 x6 x7 x8 x8)
      (* x1 x1 x2 x3 x3 x4 x4 x6 x6 x7 x7 x8)
      (* x1 x1 x3 x4 x4 x6 x6 x7 x7)
      (* x1 x1 x2 x2 x3 x3 x4 x5 x5 x6 x7 x7)
      (* x2 x2 x4 x6)))

(define f8
  '(+ (* x1 x1 x2 x2 x3 x4 x4 x5 x6 x6 x8 x8)
      (* x2 x5 x6 x6 x8 x8)
      (* x1 x1 x2 x2 x3 x3 x4 x4 x6 x6 x7 x7 x8)
      (* x1 x1 x3 x3 x4 x5 x5 x7 x7 x8)
      (* x1 x2 x2 x3 x3 x5 x5 x7)))

(define g8
  '(+ (* x1 x4 x4 x6 x6 x7 x8 x8)
      (* x1 x2 x2 x4 x4 x5 x5 x6 x6 x8)
      (* x1 x1 x2 x3 x4 x4 x6 x6 x8)
      (* x1 x1 x2 x2 x3 x3 x4 x5 x5 x8)
      (* x1 x2 x4 x4 x5 x5)))

;(show-time (lambda () (gcd-test d8 f8 g8)))
;process time: 900 (900 RUN + 0 GC); real time: 894
;Value: #t

(define d10
  '(+ (* x1 x2 x2 x4 x4 x8 x9 x9 x10 x10)
      (* x2 x2 x4 x5 x5 x6 x7 x9 x10 x10)
      (* x1 x1 x2 x3 x5 x5 x7 x7 x9 x9)
      (* x1 x3 x3 x4 x4 x7 x7 x9 x9)
      (* x1 x1 x3 x4 x7 x7 x8 x8)))

(define f10
  '(+ (* x1 x2 x3 x3 x4 x6 x7 x8 x9 x9 x10 x10)
      (* x2 x2 x3 x3 x4 x4 x6 x6 x9 x10 x10)
      (* x1 x2 x2 x3 x3 x4 x5 x6 x7 x8 x8 x9 x9 x10)
      (* x1 x1 x2 x4 x4 x5 x5 x8 x8 x9 x9 x10)
      (* x3 x4 x4 x5 x6 x7 x7 x9 x10)))

(define g10
  '(+ (* x1 x2 x2 x3 x3 x5 x5 x6 x6 x7 x8 x9 x9 x10 x10)
      (* x3 x8 x9 x9 x10 x10)
      (* x1 x2 x2 x3 x4 x5 x5 x6 x6 x8 x8 x9 x10)
      (* x1 x3 x6 x7 x8 x10)
      (* x4 x4 x5 x5 x6 x6 x7 x9 x9)))
 
;(show-time (lambda () (gcd-test d10 f10 g10)))
;process time: 1550 (1550 RUN + 0 GC); real time: 1553
;Value: #t

;;; These are a bit harder versions of the problem

(define d10a
  '(+ (* 2 x1 x2 x2 x4 x4 x8 x9 x9 x10 x10)
      (* 3 x2 x2 x4 x5 x5 x6 x7 x9 x10 x10)
      (* 4 x1 x1 x2 x3 x5 x5 x7 x7 x9 x9)
      (* 5 x1 x3 x3 x4 x4 x7 x7 x9 x9)
      (* 6 x1 x1 x3 x4 x7 x7 x8 x8)
      7))

(define f10a
  '(+ (* 8 x1 x2 x3 x3 x4 x6 x7 x8 x9 x9 x10 x10)
      (* 9 x2 x2 x3 x3 x4 x4 x6 x6 x9 x10 x10)
      (* 10 x1 x2 x2 x3 x3 x4 x5 x6 x7 x8 x8 x9 x9 x10)
      (* 11 x1 x1 x2 x4 x4 x5 x5 x8 x8 x9 x9 x10)
      (* 12 x3 x4 x4 x5 x6 x7 x7 x9 x10)
      13))

(define g10a
  '(+ (* 14 x1 x2 x2 x3 x3 x5 x5 x6 x6 x7 x8 x9 x9 x10 x10)
      (* 15 x3 x8 x9 x9 x10 x10)
      (* 16 x1 x2 x2 x3 x4 x5 x5 x6 x6 x8 x8 x9 x10)
      (* 17 x1 x3 x6 x7 x8 x10)
      (* 18 x4 x4 x5 x5 x6 x6 x7 x9 x9)
      19))

;(show-time (lambda () (gcd-test d10a f10a g10a)))
process time: 2540 (2540 RUN + 0 GC); real time: 2534
;Value: #t
|#

#|
(define *heuristic-sparse-gcd-enabled* #f)
(define *heuristic-sparse-gcd-win* 0)
(define *heuristic-sparse-gcd-lose* 0)
(define *heuristic-sparse-gcd-false-positive* 0)
(define *heuristic-sparse-gcd-false-negative* 0)

(define (reset-heuristic-sparse-gcd-test)
  (pp (list *heuristic-sparse-gcd-win*
	    *heuristic-sparse-gcd-lose*
	    *heuristic-sparse-gcd-false-positive*
	    *heuristic-sparse-gcd-false-negative*))
  (set! *heuristic-sparse-gcd-win* 0)
  (set! *heuristic-sparse-gcd-lose* 0)
  (set! *heuristic-sparse-gcd-false-positive* 0)
  (set! *heuristic-sparse-gcd-false-negative* 0)
  (set! *heuristic-sparse-gcd-enabled* #t))

(define (heuristic-sparse-gcd-test sparse-gcd)
  (define (the-gcd p1 p2)
    (if *heuristic-sparse-gcd-enabled*
	(let ((n (length (sparse-exponents (car p1)))))
	  (assert (fix:= n (length (sparse-exponents (car p2)))))
	  (let ((args (generate-list n interpolate-random)))
	    (let ((v1 (sparse-evaluate p1 args)) (v2 (sparse-evaluate p2 args)))
	      (cond ((= (base/gcd v1 v2) 1)
		     (set! *heuristic-sparse-gcd-win*
			   (fix:+ *heuristic-sparse-gcd-win* 1))
		     (let ((g (sparse-gcd p1 p2)))
		       (if (not (sparse-one? g))
			   (set! *heuristic-sparse-gcd-false-positive*
				 (fix:+ *heuristic-sparse-gcd-false-positive* 1)))
		       g))
		    (else
		     (set! *heuristic-sparse-gcd-lose*
			   (fix:+ *heuristic-sparse-gcd-lose* 1))
		     (let ((g (sparse-gcd p1 p2)))
		       (if (sparse-one? g)
			   (set! *heuristic-sparse-gcd-false-negative*
				 (fix:+ *heuristic-sparse-gcd-false-negative* 1)))
		       g))))))
	(sparse-gcd p1 p2)))
  the-gcd)

(define saved-sparse-multivariate-gcd sparse-multivariate-gcd)

#|
(set! sparse-multivariate-gcd
      (heuristic-sparse-gcd-test saved-sparse-multivariate-gcd))

;;; Test on a big problem reveals:

*heuristic-sparse-gcd-win*
;Value: 21538

*heuristic-sparse-gcd-lose*
;Value: 608

*heuristic-sparse-gcd-false-positive*
;Value: 0

*heuristic-sparse-gcd-false-negative*
;Value: 75
|#
|#