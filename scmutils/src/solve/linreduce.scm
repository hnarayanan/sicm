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

;;;; System solver based entirely on functions.

;;; We require the zeros of a function F:R^n --> R^m.
;;;  In general, there is a k-parameter family of solutions, 
;;;  S:R^k --> R^n, such that for all x in R^k, F(S(x)) = 0.
;;;  If the equations cannot be completely solved, there are
;;;  residuals.  These are functions of k variables.  If the 
;;;  equations are contradictory, we want to know it.  
;;;  However, this is hard if the residual is parametric.

;;; SOLVE takes three required arguments and two optional 
;;;  continuations.  If the optional continuations are not 
;;;  provided, defaults are used.
;;;  (lambda (F N M #!optional WIN LOSE) ...)

;;;   F: A vector-valued function of a vector.
;;;   N: the dimension of the domain of F.
;;;   M: the dimension of the range of F.

;;;   WIN: a continuation with two arguments called when a 
;;;     solution is found.  The default requires that NUM-PARAMS=0
;;;     (else it reports an error) and if OK, it returns the
;;;     value of (SOLUTIONS #()) -- not the procedure SOLUTIONS.
;;;       (lambda (num-params solutions) ... )
;;;        NUM-PARAMS:
;;;          the number of free parameters that generate the
;;;          solution space.
;;;        SOLUTIONS:
;;;          a procedure that when applied to a vector of length
;;;          NUM-PARAMS, generates a vector V in the partial
;;;          solution space.

;;;   LOSE: a continuation with four arguments called when the
;;;     equations cannot be solved (or if a contradiction is discovered)
;;;       (lambda (residual n m ifwin) ...)
;;;        RESIDUAL:
;;;          the offending residual function of a parameter vector 
;;;          that could not be solved.  The domain of the residual
;;;          has dimension N and the range has dimension M.
;;;        N: the dimension of the domain of the residual.
;;;        M: the number of unsolved residual equations.
;;;        IFWIN:  If the LOSE procedure can obtain a solution to the
;;;          offending kernel problem, it may call the continuation
;;;          IFWIN as above.  This continues the solution process.

;;;     Note that an appaent contradiction may just be a constraint on
;;;     the literals in the residual function that force it to be zero.

#|
;;; For a simple example, consider H:R^2 --> R^2, with a 
;;; unique solution

(define (H v)
  (let ((x (vector-ref v 0))
	(y (vector-ref v 1)))
    (vector (+ x y -3)			;x+y=3
	    (- x y 4))))		;x-y=4

(define foo (solve H 2 2 list))
;;; I used list for the WIN continuation here.
;;; The resulting value shows that the answer is
;;; unique (k=0).

foo
;Value: (0 #[compiled-closure 63])

;;; Thus, I can obtain the answer by applying the
;;; procedure returned to the empty vector.

((cadr foo) #())
;Value: #(7/2 -1/2)

;;; This is a solution:
(H ((cadr foo) #()))
;Value: #(0 0)


;;; Using the default WIN:

(solve H 2 2)
;Value: #(7/2 -1/2)

;;; For another example, consider F:R^3 --> R^2; (n=3, m=2)
(define (F v)
  (let ((x (vector-ref v 0))
	(y (vector-ref v 1))
	(z (vector-ref v 2)))
    (vector (+ (* 3 x) (* 2 y) z -4)
	    (+ (* 2 x) (* 2 y) (* -1 z) 5))))

(define foo (solve F 3 2 list))

foo
;Value: (1 #[compiled-closure 21])

;;; This tells us that there is a 1-parameter family of solutions
;;;  (and no residuals).   Suppose that we call the parameter a,
;;;  then the family of solutions is:
(simplify ((cadr foo) #(a)))
;Value: #((+ 9 (* -2 a)) (+ -23/2 (* 5/2 a)) a)

;;; Or simply, we can let the default WIN make up parameters.
(simplify (solve F 3 2))
;Value: #((+ 9 (* -2 p0)) (+ -23/2 (* 5/2 p0)) p0)

;;; Plugging these back into the original equations yields zero.
(simplify ((compose F (cadr foo)) #(a)))
;Value: #(0 0)

;;; Suppose we add in another, highly nonlinear constraint
(define (F* v)
  (let ((x (vector-ref v 0))
	(y (vector-ref v 1))
	(z (vector-ref v 2)))
    (vector (+ (* 3 x) (* 2 y) z -4)
	    (+ (square x) (cube y) (sqrt z) -6)
	    (+ (* 2 x) (* 2 y) (* -1 z) 5))))

(simplify (solve F* 3 3 list))
;Value: 
  (solve-lost
   #((+ -11567/8 (sqrt p0) (* p0 (+ 7647/8 (* p0 (+ -1693/8 (* 125/8 p0))))))))

;;; Thus there is a single residual equation and a one-parameter residual.


;;; Now we happen to know that this nonlinear equation has a solution
;;;   between 5 and 6.  Let's replace LOSE with a numerical search for
;;;   the root.  We must also supply the DEFAULT-WIN continuation.

(solve F* 3 3 default-win
       (lambda (F n m win)
	 (let ((f** (lambda (x) (vector-ref (F (vector x)) 0))))
	   (win 0 (no-constraints 0)
		0 (lambda (v)
		    (vector (false-position-search f** 5.0 6.0 1e-13)))))))
;Value: #(-1.2402976787780318 1.30037209847254 5.1201488393890155)

;;; And indeed, they are the correct roots:
(F* #(-1.2402976787780318 1.30037209847254 5.1201488393890155))
;Value 83: #(0. -8.08242361927114e-14 8.881784197001252e-16)

;;; More examples follow
|#

#|
PARTIAL-SOLVE is similar to solve, but it solves for a specified set
of variables.  F is a function of N vectors, each with a given arity,
as specified by arity-list.  The range of F is of dimension M.

The result of calling partial-solve is then applied to N arguments,
one of which is the symbol ?  and the rest of which are appropriate
vectors.  The result is a solution for the ?  vector in terms of the
others.  For example:

(se ((partial-solve
      (lambda (as bs xs ys)
	(vector (- (ref ys 0) (v:dot-product as xs))
		(- (ref ys 1) (v:dot-product bs xs))))
      '(2 2 2 2)
      2)
     (vector 'a_11 'a_12)
     (vector 'a_21 'a_22)
     '?
     (vector 'y_1 'y_2)))
(up (/ (+ (* -1 a_12 y_2) (* a_22 y_1))
       (+ (* a_11 a_22) (* -1 a_12 a_21)))
    (/ (+ (* a_11 y_2) (* -1 a_21 y_1))
       (+ (* a_11 a_22) (* -1 a_12 a_21))))


;;; For a hairier example:

(pe ((partial-solve
      (lambda (a c v p)
	(let ((A (vector-ref a 0))

	      (theta (vector-ref c 0))
	      (phi (vector-ref c 1))
	      (psi (vector-ref c 2))

	      (thetadot (vector-ref v 0))
	      (phidot (vector-ref v 1))
	      (psidot (vector-ref v 2))

	      (p_theta (vector-ref p 0))
	      (p_phi (vector-ref p 1))
	      (p_psi (vector-ref p 2)))
	  (vector (- p_theta (* A thetadot))
		  (- p_phi (+ (* A psidot (cos theta)) (* A phidot)))
		  (- p_psi (+ (* A phidot (cos theta)) (* A psidot))))))
      '(1 3 3 3)
      3)
     (vector 'A)
     (vector 'theta 'phi 'psi)
     '?
     (vector 'p_theta 'p_phi 'p_psi)))
(up (/ p_theta A)
    (/ (+ (* -1 p_psi (cos theta)) p_phi) (* A (expt (sin theta) 2)))
    (/ (+ (* -1 p_phi (cos theta)) p_psi) (* A (expt (sin theta) 2))))
|#

;;; The definitions start here.

(define (solve F n m #!optional win lose)
  (let ((win (if (default-object? win) default-win win))
	(lose (if (default-object? lose) default-lose lose)))		 
    (affine-reduce F n m win lose)))


;;; PARTIAL-SOLVE is built on SOLVE.

(define (partial-solve f arity-list m #!optional win lose flag)
  (let ((flag (if (default-object? flag) '? flag))
	(win (if (default-object? win) default-win win))
	(lose (if (default-object? lose) default-lose lose)))
    (lambda args
      (let ((n-1 (list-index-of flag args)))
	(solve (lambda (y)
		 (apply f (list-with-substituted-coord args n-1 y)))
	       (list-ref arity-list n-1)
	       m
	       win
	       lose)))))


;;; The default continuations are as follows

;;; Useful for generating parameter names

(define (make-parameters n)
  (generate-vector n
    (lambda (i)
      (string->symbol (string-append "p" (number->string i))))))

(define (default-win n S)
  (S (make-parameters n)))

(define (default-lose resid n m ifwin)
  (list 'Solve-lost (resid (make-parameters n))))

;;; AFFINE-REDUCE uses elimination, but on functions!

;;;   F: A vector-valued function of a vector.
;;;   n: the dimension of the domain of F.
;;;   m: the dimension of the range of F.

(define (affine-reduce F n m win lose)
  ;; win = (lambda (k S) ...)
  ;; lose = (lambda (F n m win) ...)
  (define (alp F n m cont)
    ;; cont = (lambda (k S r residuals) ...)
    (define (got-part nnn nnF nnm S)
      (alp nnF nnn nnm
	   (lambda (nnnn nS nnnm nnnF)
	     (cont nnnn (compose S nS)
		   nnnm (compose nnnF nS)))))
    (define (cannot-solve resid n m)	; I cannot solve this part
      (lose resid n m got-part))	; can lose win for me?
    (cond ((fix:= m 0)			; no more constraints
	   (cont n identity
		 m (no-constraints n)))
	  ((fix:= n 0)			; no more unknowns
	   (cond ((fix:= m 0)		; maybe over constrained?
		  (cont 0 identity m F));  no problem
		 ((all-tiny? (F #()))	; residuals OK?
		  (cont 0 identity
			0 (no-constraints 0)))
		 (else 
		  (lose F 0 m got-part))))
	  (else
	   (affine-classify F n m
	     (lambda (nF nn nm j i)
	       (affine-isolate nF nn nm j i got-part))
	     (lambda ()			; all are tautologies
	       (cont n identity
		     0 (no-constraints n)))
	     cannot-solve))))
  (clear-big!)				; kludge, see end!
  (alp F n m
       (lambda (k S m nF)
	 (if (not (fix:= m 0)) (error "Gack!"))
	 (win k S))))


;;; Given such an F, n, m affine-isolate performs one step of this
;;; process by pivoting on variable x_j in equation F_i.
;;; We return n, k, m, new-m, S to the continuation supplied.

(define (affine-isolate F n m j i cont)
  (assert (fix:< j n))
  (assert (fix:< i m))
  (let* ((F_i (compose (select-component i m) F))
	 (K_ij (compose F_i (with-fixed-component j n zero-function)))
	 (x_j
	  (/ K_ij
	     (- K_ij
		(compose F_i (with-fixed-component j n one-function))))) 
	 (S (with-fixed-component j n x_j))
	 (nF (compose (except-component i m) F S)))
    (cont (fix:- n 1) nF (fix:- m 1) S)))

;;; Given an F:R^n --> R^m, to check for contradictions, discard
;;; tautologies, and to choose an appropriate pivot.  FOUND-PIVOT
;;; is the normal return continuation.  Others are for the problems
;;; noted by the name of the continuation.

(define (affine-classify F n m found-pivot all-tautologies cannot-solve)
  ;; found-pivot = (lambda (nF nn nm j i) ...)
  (define ((g j) u v)			; if g_i linear, can isolate x_j
    (F (vector-with-substituted-coord v j u)))
  (let ((x1 (gensymbols n)) (x2 (gensymbols n)))
    (let ((fx1 (F x1)) (fx2 (F x2)))
      (let* ((tiny? (lambda (x) (exact-zero? (g:simplify x))))
	     (same? (lambda (x y) (tiny? (g:- x y))))
	     (interesting-equations-mask
	      (map (lambda (f1 f2)
		     (if (same? f1 f2)	; a constant?
			 (if (tiny? f1)	;  a tautology?
			     #f		;   yes -- uninteresting
			     '=><=)	;  a contradiction
			 #t))		; varies with inputs
		   (vector->list fx1)
		   (vector->list fx2))))
	(let ((interesting (select-from interesting-equations-mask))
	      (nm (reduce fix:+ 0
			  (map (lambda (x) (if (eq? x #t) 1 0))
			       interesting-equations-mask))))
	  (cond ((memq '=><= interesting-equations-mask)
		 (cannot-solve (compose interesting F) n nm))
	        ((fix:= nm 0) (all-tautologies))
		(else
		 (let vloop ((j 0))	; try each unknown variable
		   (if (fix:= j n)	; if cannot solve for any one
		       (cannot-solve (compose interesting F) n nm)
		       (let* ((gj (g j))
			      (a1 (gj 0 x1)) (b1 (- (gj 1 x1) a1))
			      (a2 (gj 0 x2)) (b2 (- (gj 1 x2) a2))
			      (c (generate-uninterned-symbol))
			      (lin1 (- (gj c x1) (+ a1 (* c b1))))
			      (lin2 (- (gj c x2) (+ a2 (* c b2))))
			      (lin3 (- b1 b2)))
			 (let eloop	; try each interesting equation
			     ((i 0)
			      (ecount 0)
			      (mask interesting-equations-mask))
			   (cond ((fix:= ecount nm) ; ran out of equations
				  (vloop (fix:+ j 1))) ;try next variable
				 ((not (car mask))
				  (eloop (fix:+ i 1) ecount (cdr mask)))
				 ((and (car mask)
				       (tiny? (vector-ref lin1 i))
				       (tiny? (vector-ref lin2 i))
				       (not (tiny? (vector-ref b1 i)))
				       (tiny? (vector-ref lin3 i)))
				  (found-pivot ; var j is linear in eqn i
				   (compose interesting F)
				   n nm j ecount))
				 (else
				  (eloop (fix:+ i 1)
					 (fix:+ ecount 1)
					 (cdr mask)))))))))))))))

#|
;;; This numerical thingy doesn't really work.

(define (affine-classify F n m found-pivot all-tautologies cannot-solve)
  ;; found-pivot = (lambda (nF nn nm j i) ...)
  (define ((g j) u v)			; if g_i linear, can isolate x_j
    (F (vector-with-substituted-coord v j u)))
  (let ((x1 (random-vector n)) (x2 (random-vector n)))
    (let ((fx1 (F x1)) (fx2 (F x2)))
      (let* ((big (find-biggest fx1 fx2))
	     (same? (make-same big))
	     (tiny? (make-tiny big))
	     (interesting-equations-mask
	      (map (lambda (f1 f2)
		     (if (same? f1 f2)	; a constant?
			 (if (tiny? f1)	;  a tautology?
			     #f		;   yes -- uninteresting
			     '=><=)	;  a contradiction
			 #t))		; varies with inputs
		   (vector->list fx1)
		   (vector->list fx2))))
	(let ((interesting (select-from interesting-equations-mask))
	      (nm (reduce fix:+ 0
			  (map (lambda (x) (if (eq? x #t) 1 0))
			       interesting-equations-mask))))
	  (cond ((memq '=><= interesting-equations-mask)
		 (cannot-solve (compose interesting F) n nm))
	        ((fix:= nm 0) (all-tautologies))
		(else
		 (let vloop ((j 0))	; try each unknown variable
		   (if (fix:= j n)	; if cannot solve for any one
		       (cannot-solve (compose interesting F) n nm)
		       (let* ((gj (g j))
			      (a1 (gj 0 x1)) (b1 (- (gj 1 x1) a1))
			      (a2 (gj 0 x2)) (b2 (- (gj 1 x2) a2))
			      (c (random-rational))
			      (lin1 (- (gj c x1) (+ a1 (* c b1))))
			      (lin2 (- (gj c x2) (+ a2 (* c b2))))
			      (lin3 (- b1 b2)))
			 (let eloop	; try each interesting equation
			     ((i 0)
			      (ecount 0)
			      (mask interesting-equations-mask))
			   (cond ((fix:= ecount nm) ; ran out of equations
				  (vloop (fix:+ j 1))) ;try next variable
				 ((not (car mask))
				  (eloop (fix:+ i 1) ecount (cdr mask)))
				 ((and (car mask)
				       (tiny? (vector-ref lin1 i))
				       (tiny? (vector-ref lin2 i))
				       (not (tiny? (vector-ref b1 i)))
				       (tiny? (vector-ref lin3 i)))
				  (found-pivot ; var j is linear in eqn i
				   (compose interesting F)
				   n nm j ecount))
				 (else
				  (eloop (fix:+ i 1)
					 (fix:+ ecount 1)
					 (cdr mask)))))))))))))))
|#

#|
;;; More examples...
;;;  An underdetermined system with excess dependent equations.

(define (F1 v)
  (let ((x (vector-ref v 0))
	(y (vector-ref v 1))
	(z (vector-ref v 2)))
    (vector (+ (* 3 x) (* 2 y) z -4)
	    (+ (* 2 x) (* 2 y) (* -1 z) 5)
	    (+ (* 6 x) (* 4 y) (* 2 z) -8)
	    (+ (* 8 x) (* 6 y) z -3))))

(simplify (solve F1 3 4))
;Value:  #((+ 9 (* -2 p0)) (+ -23/2 (* 5/2 p0)) p0)

;;; Check the solution
(simplify (F1 #((+ 9 (* -2 p0)) (+ -23/2 (* 5/2 p0)) p0)))
;Value: #(0 0 0 0)


;;; An overdetermined system with no solution.

(define (F2 v)
  (let ((x (vector-ref v 0))
	(y (vector-ref v 1))
	(z (vector-ref v 2)))
    (vector (+ (* 3 x) (* 2 y) z -4)
	    (+ (* 2 x) (* 2 y) (* -1 z) 5)
	    (+ (* 6 x) (* 3 y) (* 2 z) -8)
	    (+ (* 8 x) (* 6 y) -3))))


(solve F2 3 4)
;Value: (solve-lost #(-23/5))

;;; This shows a real contradiction.  There is no way to make -22/5 = 0!


;;; Slightly nonlinear systems.

(define (F3 v)
  (let ((x (vector-ref v 0))
	(y (vector-ref v 1))
	(z (vector-ref v 2)))
    (vector (- 3 (+ x y))
	    (- 5 (- x y))
	    (- 3 (+ (* (sqrt x) z) (square y))))))

(solve F3 3 3)
;Value: #(4 -1 1)

(simplify (F3 #(4 -1 1)))
;Value: #(0 0 0)

(define (F4 v)
  (let ((a (vector-ref v 0))
	(b (vector-ref v 1))
	(c (vector-ref v 2)))
    (vector (+ (* (+ a b) (- a c)) c)
	    (- 3 (+ a b)))))


(simplify (solve F4 3 2))
;Value: #((* 2/3 p0) (+ 3 (* -2/3 p0)) p0)

(simplify (F4  #((* 2/3 p0) (+ 3 (* -2/3 p0)) p0)))
;Value: #(0 0)


(define (F5 v)
  (let ((a (vector-ref v 0))
	(b (vector-ref v 1))
	(c (vector-ref v 2)))
    (vector (+ (* (+ a b) (- a c)) c)
	    (- 3 (- a c)))))

(simplify (solve F5 3 2))
;Value: #((+ 3 p0) (+ -3 (* -4/3 p0)) p0)

(simplify (F5 #((+ 3 p0) (+ -3 (* -4/3 p0)) p0)))
;Value: #(0 0)
|#

;;; Some utilities used in this code.

(define ((no-constraints n) v)
  (assert (fix:= (vector-length v) n))
  #())


(define ((select-component i m) v)
  (assert (fix:= (vector-length v) m))
  (assert (and (not (fix:negative? i)) (fix:< i m)))
  (vector-ref v i))

(define ((except-component j m) v)
  (assert (fix:= (vector-length v) m))
  (assert (and (not (fix:negative? j)) (fix:< j m)))
  (generate-vector (fix:- m 1)
		   (lambda (i)
		     (cond ((fix:< i j) (vector-ref v i))
			   (else (vector-ref v (fix:+ i 1)))))))

(define (with-fixed-component j n val)
  (let ((n-1 (fix:- n 1)))
    (assert (and (not (fix:negative? j)) (fix:< j n)))
    (define (fix-j v)
      (assert (fix:= (vector-length v) n-1))
      (generate-vector n
		       (lambda (i)
			 (cond ((fix:< i j) (vector-ref v i))
			       ((fix:= i j) (val v))
			       ((fix:> i j) (vector-ref v (fix:- i 1)))))))
    fix-j))


(define (zero-function . x) zero)
(define (one-function . x) one)


(define (select-from mask)
  (define (nf v)
    (list->vector
     (let lp ((l (vector->list v)) (m mask))
       (cond ((null? l) '())
	     ((car m) (cons (car l) (lp (cdr l) (cdr m))))
	     (else (lp (cdr l) (cdr m)))))))
  nf)

(define (gensymbols n)
  (make-initialized-vector n
    (lambda (i)
      (generate-uninterned-symbol 'x))))

(define (random-rational)
  (/ (- (random 20000) 10000)
     (let loop ()
       (let ((d (random 10000)))
	 (if (fix:zero? d)
	     (loop)
	     d)))))

(define (random-vector dimension)
  (generate-vector dimension
		   (lambda (ignore)
		     ignore
		     (random-rational))))


;;; The kludge is hidden here!

(define (find-biggest . vects)
  (let lp ((elts (apply append (map vector->list vects)))
	   (maxnum -1))
    (if (null? elts)
	(begin (set! *kludge:big-number* maxnum)
	       maxnum)
	(let ((x (car elts)))
	  (if (number? x)
	      (lp (cdr elts) (max maxnum (n:magnitude x)))
	      (lp (cdr elts) maxnum))))))

(define (make-same big)
  (let ((tol (n:* *solve-bugger-factor* big)))
    (lambda (x y)
      (if (and (number? x) (number? y))
	  (if (or (inexact? x) (inexact? y))
	      (< (n:magnitude (n:- x y)) tol)
	      (= x y))
	  (an:= x y)))))

(define (make-tiny big)
  (let ((tol (n:* *solve-bugger-factor* big)))
    (lambda (x)
      (if (number? x)
	  (if (inexact? x)
	      (< (n:magnitude x) tol)
	      (zero? x))
	  (an:zero? x)))))

(define (all-tiny? v)
  (for-all? (vector->list v) (make-tiny *kludge:big-number*)))


;;; The kludge!

(define *kludge:big-number*)

(define (clear-big!) (set! *kludge:big-number* 0))

(define *solve-bugger-factor* (* 100 *machine-epsilon*))

#|
;;; The following simple problem takes too long to simplify!

(set! *divide-out-terms* #f)

(pec ((partial-solve
       (lambda (as bs cs xs ys)
	 (vector (- (ref ys 0) (v:dot-product as xs))
		 (- (ref ys 1) (v:dot-product bs xs))
		 (- (ref ys 2) (v:dot-product cs xs))))
       '(3 3 3 3 3)
       3)
      (vector 'a_11 'a_12 'a_13)
      (vector 'a_21 'a_22 'a_23)
      (vector 'a_31 'a_32 'a_33)
      '?
      (vector 'y_1 'y_2 'y_3)))
#|
(up (/ (+ (* a_22 a_33 y_1)
	  (* -1 a_22 y_3 a_13)
	  (* -1 a_23 a_32 y_1)
	  (* a_23 y_3 a_12)
	  (* a_32 y_2 a_13)
	  (* -1 a_33 y_2 a_12))
       (+ (* a_22 a_33 a_11)
	  (* -1 a_22 a_13 a_31)
	  (* -1 a_23 a_32 a_11)
	  (* a_23 a_12 a_31)
	  (* a_32 a_13 a_21)
	  (* -1 a_33 a_12 a_21)))
    (/ (+ (* a_11 a_33 y_2)
	  (* -1 a_11 y_3 a_23)
	  (* -1 a_13 a_31 y_2)
	  (* a_13 y_3 a_21)
	  (* a_31 y_1 a_23)
	  (* -1 a_33 y_1 a_21))
       (+ (* a_11 a_33 a_22)
	  (* -1 a_11 a_23 a_32)
	  (* -1 a_13 a_31 a_22)
	  (* a_13 a_21 a_32)
	  (* a_31 a_12 a_23)
	  (* -1 a_33 a_12 a_21)))
    (/ (+ (* a_11 a_22 y_3)
	  (* -1 a_11 a_32 y_2)
	  (* -1 a_12 a_21 y_3)
	  (* a_12 a_31 y_2)
	  (* a_21 a_32 y_1)
	  (* -1 a_22 a_31 y_1))
       (+ (* a_11 a_22 a_33)
	  (* -1 a_11 a_32 a_23)
	  (* -1 a_12 a_21 a_33)
	  (* a_12 a_31 a_23)
	  (* a_13 a_21 a_32)
	  (* -1 a_13 a_22 a_31))))
|#
|#
