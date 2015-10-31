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

;;;; MULTIMIN.SCM -- n-dimensional minimization routines
;;; 9/22/89 (gjs) reduce->a-reduce

(declare (usual-integrations = + - * /
			     zero? 1+ -1+
			     ;; truncate round floor ceiling
			     sqrt exp log sin cos))

;;; Nelder-Mead downhill simplex algorithm.

;;; We have a function, f, defined on points in n-space.
;;; We are looking for a local minimum of f.

;;; The central idea -- We have a simplex of n+1 vertices where f is 
;;; known.  We want to deform the simplex until it sits on the minimum.

;;; A simplex is represented as a list of entries, each of which is a
;;; pair consisting of a vertex and the value of f at that vertex.

(define simplex-size length)

(define simplex-vertex car)
(define simplex-value cdr)
(define simplex-entry cons)

;;; Simplices are stored in sorted order
(define simplex-highest car)
(define simplex-but-highest cdr)
(define simplex-next-highest cadr)
(define (simplex-lowest s) (car (last-pair s)))

(define (simplex-add-entry entry s)
  (let ((fv (simplex-value entry)))
    (let loop ((s s))
      (cond ((null? s) (list entry))
            ((> fv (simplex-value (car s))) (cons entry s))
            (else (cons (car s) (loop (cdr s))))))))

(define (simplex-adjoin v fv s)
  (simplex-add-entry (simplex-entry v fv) s))

(define (simplex-sort s)
  (let lp ((s s) (ans '()))
    (if (null? s)
        ans
        (lp (cdr s) (simplex-add-entry (car s) ans)))))

(define simplex-centroid
  (lambda (simplex)
    (scalar*vector (/ 1 (simplex-size simplex))
		   (a-reduce vector+vector
			     (map simplex-vertex simplex)))))

(define	extender	
  (lambda (p1 p2)
    (let ((dp (vector-vector p2 p1)))
      (lambda (k)
        (vector+vector p1 (scalar*vector k dp))))))

(define (make-simplex point step f)
  (simplex-sort
    (map (lambda (vertex) (simplex-entry vertex (f vertex)))
         (cons point
	       (let ((n (vector-length point)))
		 (generate-list n
		   (lambda (i)
		     (vector+vector point
		       (scalar*vector step
			 (v:make-basis-unit n i))))))))))

(define (stationary? simplex epsilon)
  (close-enuf? (simplex-value (simplex-highest simplex))
               (simplex-value (simplex-lowest simplex))
               epsilon))

(define nelder-wallp? false)

(define (nelder-mead f start-pt start-step epsilon maxiter)
  (define shrink-coef 0.5)
  (define reflection-coef 2.0)
  (define expansion-coef 3.0)
  (define contraction-coef-1 1.5)
  (define contraction-coef-2 (- 2 contraction-coef-1))
  (define (simplex-shrink point simplex)
    (let ((pv (simplex-vertex point)))
      (simplex-sort
       (map (lambda (sp)
	      (if (eq? point sp) 
		  sp
		  (let ((vertex ((extender pv (simplex-vertex sp)) 
				 shrink-coef)))
		    (simplex-entry vertex (f vertex)))))
	    simplex))))
  (define (nm-step simplex)
    (let ((g (simplex-highest simplex))
          (h (simplex-next-highest simplex))
          (s (simplex-lowest simplex))
          (s-h (simplex-but-highest simplex)))
      (let* ((vg (simplex-vertex g)) (fg (simplex-value g))
             (fh (simplex-value h)) (fs (simplex-value s))
             (extend (extender vg (simplex-centroid s-h))))
        (let* ((vr (extend reflection-coef))
               (fr (f vr)))                 ;try reflection
          (if (< fr fh)                     ;reflection successful
              (if (< fr fs)                 ;new minimum 
                  (let* ((ve (extend expansion-coef))
                         (fe (f ve)))       ;try expansion
                    (if (< fe fs)           ;expansion successful
                        (simplex-adjoin ve fe s-h)
                        (simplex-adjoin vr fr s-h)))
                  (simplex-adjoin vr fr s-h))
              (let* ((vc (extend (if (< fr fg) 
                                     contraction-coef-1
                                     contraction-coef-2)))
                     (fc (f vc)))           ;try contraction
                (if (< fc fg)               ;contraction successful
                    (simplex-adjoin vc fc s-h)
                    (simplex-shrink s simplex))))))))
  (define (limit simplex count)
    (if nelder-wallp? (write-line (simplex-lowest simplex)))
    (if (stationary? simplex epsilon)
        (list 'ok (simplex-lowest simplex) count)
        (if (fix:= count maxiter)
            (list 'maxcount (simplex-lowest simplex) count)
            (limit (nm-step simplex) (fix:+ count 1)))))
  (limit (make-simplex start-pt start-step f) 0))


;;;(define (stationary? simplex epsilon)
;;;  (let ((np1 (length simplex)))
;;;    (let* ((mean (/ (a-reduce + (map simplex-value simplex)) np1))
;;;           (variance (/ (a-reduce + 
;;;                                  (map (lambda (e)
;;;                                         (square (- (simplex-value e) mean)))
;;;                                       simplex))
;;;                        np1)))
;;;      (< variance epsilon))))


;;; Variable Metric Methods:
;;;    Fletcher-Powell (choice of line search)
;;;    Broyden-Fletcher-Goldfarb-Shanno (Davidon's line search)


;;; The following utility procedure returns a gradient function given a
;;; differentiable function f of a vector of length n. In general, a gradient
;;; function accepts an n-vector and returns a vector of derivatives. In this
;;; case, the derivatives are estimated by richardson-extrapolation of a
;;; central difference quotient, with the convergence tolerance being a
;;; specified parameter.

(define (generate-gradient-procedure f n tol)
  (lambda (x)
    (generate-vector
      n
      (lambda (i)
        (richardson-limit
          (let ((fi (lambda (t)
                      (f (vector+vector x
			   (scalar*vector t
			     (v:make-basis-unit n i)))))))
            (lambda (h) (/ (- (fi h) (fi (- h))) 2 h)))
          (max 0.1 (* 0.1 (abs (vector-ref x i)))) ;starting h
          2  ;ord -- see doc for RE.SCM
          2  ;inc
          tol)))))



;;; The following line-minimization procedure is Davidon's original
;;; recommendation. It does a bracketing search using gradients, and
;;; then interpolates the straddled minimum with a cubic.

(define (line-min-davidon f g x v est)
  (define (t->x t) (vector+vector x (scalar*vector t v)))
  (define (linef t) (f (t->x t)))
  (define (lineg t) (g (t->x t)))
  (define f0 (linef 0))
  (define g0 (lineg 0))
  (define s0 (/ (- f0 est) -.5 (v:dot-product g0 v)))
  (let loop ((t (if (and (positive? s0) (< s0 1)) s0 1))
             (iter 0))
    (if (> iter 100)
        (list 'no-min)
        (let ((ft (linef t))
              (gt (lineg t)))
          (if (or (>= ft f0)
                  (>= (v:dot-product v gt) 0))
              (let* ((vg0 (v:dot-product v g0))
                     (vgt (v:dot-product v gt))
                     (z (+ (* 3 (- f0 ft) (/ 1 t)) vg0 vgt))
                     (w (sqrt (- (* z z) (* vg0 vgt))))
                     (tstar (* t (- 1 (/ (+ vgt w (- z)) 
                                         (+ vgt (- vg0) (* 2 w))))))
                     (fstar (linef tstar)))
                 (if (< fstar f0)
                     (list 'ok (t->x tstar) fstar)
                     (loop tstar (+ iter 1))))
              (loop (* t 2) (+ iter 1)))))))
  
  
  
;;; The following line-minimization procedure is based on Brent's
;;; algorithm.

(define (line-min-brent f g x v est)
  (define (t->x t) (vector+vector x (scalar*vector t v)))
  (define (linef t) (f (t->x t)))
  (define (lineg t) (g (t->x t)))
  (define f0 (linef 0))
  (define g0 (lineg 0))
  (define s0 (/ (- f0 est) -.5 (v:dot-product g0 v)))
  (let loop ((t (if (and (positive? s0) (< s0 1)) s0 1))
	     (iter 0))
    (if (> iter 100)
	(list 'no-min)
	(let ((ft (linef t))
	      (gt (lineg t)))
	  (if (or (>= ft f0)
		  (>= (v:dot-product v gt) 0))
	      (let* ((result (brent-min linef 0 t *sqrt-machine-epsilon*))
		     (tstar (car result))
		     (fstar (cadr result)))
		(list 'ok (t->x tstar) fstar))
	      (loop (* t 2) (+ iter 1)))))))


;;; In the following implementation of the Davidon-Fletcher-Powell
;;;  algorithm, f is a function of a single vector argument that returns
;;;  a real value to be minimized, g is the vector-valued gradient and
;;;  x is a (vector) starting point, and est is an estimate of the minimum
;;;  function value. If g is '(), then a numerical approximation is 
;;;  substituted using GENERATE-GRADIENT-PROCEDURE. ftol is the convergence 
;;;  criterion: the search is stopped when the relative change in f falls 
;;;  below ftol.

(define fletcher-powell-wallp? false)

(define (fletcher-powell line-search f g x est ftol maxiter)
  (let ((n (vector-length x)))
    (if (null? g) (set! g (generate-gradient-procedure 
                            f n (* 1000 *machine-epsilon*))))
    (let loop ((H (m:make-identity n))
               (x x)
               (fx (f x))
               (gx (g x))
               (count 0))
      (if fletcher-powell-wallp? (print (list x fx gx)))
      (let ((v (matrix*vector H (scalar*vector -1 gx))))
        (if (positive? (v:dot-product v gx))
          (begin 
            (if fletcher-powell-wallp? 
               (display (list "H reset to Identity at iteration" count)))
            (loop (m:make-identity n) x fx gx count))
          (let ((r (line-search f g x v est)))
            (if (eq? (car r) 'no-min)
              (list 'no-min (cons x fx) count)
              (let ((newx (cadr r))
                    (newfx (caddr r)))
                (if (close-enuf? newfx fx ftol) ;convergence criterion
                  (list 'ok (cons newx newfx) count)
                  (if (fix:= count maxiter)
                    (list 'maxcount (cons newx newfx) count)
                    (let* ((newgx (g newx))
                           (dx (vector-vector newx x))
                           (dg (vector-vector newgx gx))
                           (Hdg (matrix*vector H dg))
                           (A (matrix*scalar
			       (m:outer-product (vector->column-matrix dx)
						(vector->row-matrix dx))
			       (/ 1 (v:dot-product dx dg))))
                           (B (matrix*scalar
			       (m:outer-product (vector->column-matrix Hdg)
						(vector->row-matrix Hdg))
			       (/ -1 (v:dot-product dg Hdg))))
                           (newH (matrix+matrix H (matrix+matrix A B))))
                      (loop newH newx newfx newgx (fix:+ count 1)))))))))))))


;;; The following procedures, DFP and DFP-BRENT, call directly upon
;;; FLETCHER-POWELL. The first uses Davidon's line search which is
;;; efficient, and would be the normal choice. The second uses Brent's
;;; line search, which is less efficient but more reliable.

(define (dfp f g x est ftol maxiter)
  (fletcher-powell line-min-davidon f g x est ftol maxiter))

(define (dfp-brent f g x est ftol maxiter)
  (fletcher-powell line-min-brent f g x est ftol maxiter))

;;; The following is a variation on DFP, due (independently, we are told)
;;; to Broyden, Fletcher, Goldfarb, and Shanno. It differs in the formula
;;; used to update H, and is said to be more immune than DFP to imprecise
;;; line-search. Consequently, it is offered with Davidon's line search
;;; wired in.

(define bfgs-wallp? false)

(define (bfgs f g x est ftol maxiter)
  (let ((n (vector-length x)))
    (if (null? g) (set! g (generate-gradient-procedure 
                            f n (* 1000 *machine-epsilon*))))
    (let loop ((H (m:make-identity n))
               (x x)
               (fx (f x))
               (gx (g x))
               (count 0))
      (if bfgs-wallp? (print (list x fx gx)))
      (let ((v (matrix*vector H (scalar*vector -1 gx))))
        (if (positive? (v:dot-product v gx))
          (begin 
            (if bfgs-wallp? 
               (display (list "H reset to Identity at iteration" count)))
            (loop (m:make-identity n) x fx gx count))
          (let ((r (line-min-davidon f g x v est)))
            (if (eq? (car r) 'no-min)
              (list 'no-min (cons x fx) count)
              (let ((newx (cadr r))
                    (newfx (caddr r)))
                (if (close-enuf? newfx fx ftol) ;convergence criterion
                  (list 'ok (cons newx newfx) count)
                  (if (fix:= count maxiter)
                    (list 'maxcount (cons newx newfx) count)
                    (let* ((newgx (g newx))
                           (dx (vector-vector newx x))
                           (dg (vector-vector newgx gx))
                           (Hdg (matrix*vector H dg))
                           (dxdg (v:dot-product dx dg))
                           (dgHdg (v:dot-product dg Hdg))
                           (u (vector-vector (scalar*vector (/ 1 dxdg) dx)
					     (scalar*vector (/ 1 dgHdg) Hdg)))
                           (A (matrix*scalar
			       (m:outer-product (vector->column-matrix dx)
						(vector->row-matrix dx))
			       (/ 1 dxdg)))
                           (B (matrix*scalar
			       (m:outer-product (vector->column-matrix Hdg)
						(vector->row-matrix Hdg))
			       (/ -1 dgHdg)))
                           (C (matrix*scalar
			       (m:outer-product (vector->column-matrix u)
						(vector->row-matrix u))
			       dgHdg))
                           (newH
			    (matrix+matrix (matrix+matrix H A)
					   (matrix+matrix B C))))
                        (loop newH newx newfx newgx (fix:+ count 1)))))))))))))

