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

;;; 6/1/89 (mh) added EXTREMAL-ARG and EXTREMAL-VALUE
;;; modified by mh 12/6/87
;;; $Header: unimin.scm,v 1.4 88/01/26 07:45:38 GMT gjs Exp $
;;; 7/5/87 UNIMIN.SCM -- first compilation of routines for univariate
;;;			 optimization.
#|
(declare (usual-integrations = + - * /
			     zero? 1+ -1+
			     ;; truncate round floor ceiling
			     sqrt exp log sin cos))
|#

(declare (usual-integrations))

;;; The following univariate optimization routines typically return
;;; a list (x fx ...) where x is the argmument at which the extremal
;;; value fx is achieved. The following helps destructure this list.

(define extremal-arg car)
(define extremal-value cadr)


;;; Golden Section search, with supplied convergence test procedure,
;;; GOOD-ENUF?, which is a predicate accepting seven arguments: a, minx,
;;; b, fa, fminx, fb, count.

(define (golden-section-min f a b good-enuf?)
  (define g1 (/ (- (sqrt 5) 1) 2))
  (define g2 (- 1 g1))
  (define (new-left lo-x hi-x) 
    (+ (* g2 hi-x) (* g1 lo-x)))
  (define (new-right lo-x hi-x)
    (+ (* g2 lo-x) (* g1 hi-x)))
  (define (best-of a b c fa fb fc)
    (if (< fa (min fb fc))
        (list a fa)
        (if (<= fb (min fa fc))
            (list b fb)
            (list c fc))))
  (define (lp x0 x1 x2 x3 f0 f1 f2 f3 count)
    (if (< f1 f2)
        (if (good-enuf? x0 x1 x2 f0 f1 f2 count)
            (append (best-of x0 x1 x2 f0 f1 f2) (list count))
	    (let ((nx1 (new-left x0 x2)))
	      (lp x0 nx1 x1 x2 f0 (f nx1) f1 f2 (1+ count))))
	(if (good-enuf? x1 x2 x3 f1 f2 f3 count)
	    (append (best-of x1 x2 x3 f1 f2 f3) (list count))
	    (let ((nx2 (new-right x1 x3)))
	      (lp x1 x2 nx2 x3 f1 f2 (f nx2) f3 (1+ count))))))
  (let ((x1 (new-left a b)) (x2 (new-right a b)))
    (let ((fa (f a)) (fx1 (f x1)) (fx2 (f x2)) (fb (f b)))
      (lp a x1 x2 b fa fx1 fx2 fb 0))))


;;; For convenience, we also provide the sister-procedure for finding
;;; the maximum of a unimodal function.

(define (golden-section-max f a b good-enuf?)
  (let* ((-f (lambda (x) (- (f x))))
         (result (golden-section-min -f a b good-enuf?))
         (x (car result)) (-fx (cadr result)) (count (caddr result)))
    (list x (- -fx) count)))


;;; The following procedure allows keyword specification of typical
;;; convergence criteria.

(define (gsmin f a b . params)
  (let ((ok? 
           (if (null? params)
               (lambda (a minx b fa fminx fb count)
                 (close-enuf? (max fa fb) fminx (* 10 *machine-epsilon*)))
               (let ((type (car params))
  		     (value (cadr params)))
                 (cond ((eq? type 'function-tol)
                        (lambda (a minx b fa fminx fb count)
                          (close-enuf? (max fa fb) fminx value)))
 		       ((eq? type 'arg-tol)
		        (lambda (a minx b fa fminx fb count)
                          (close-enuf? a b value)))
		       ((eq? type 'count)
		        (lambda (a minx b fa fminx fb count)
		          (>= count value))))))))
    (golden-section-min f a b ok?)))

(define (gsmax f a b . params)
  (let ((ok? 
           (if (null? params)
               (lambda (a minx b fa fminx fb count)
                 (close-enuf? (max fa fb) fminx (* 10 *machine-epsilon*)))
               (let ((type (car params))
  		     (value (cadr params)))
                 (cond ((eq? type 'function-tol)
                        (lambda (a minx b fa fminx fb count)
                          (close-enuf? (max fa fb) fminx value)))
 		       ((eq? type 'arg-tol)
		        (lambda (a minx b fa fminx fb count)
                          (close-enuf? a b value)))
		       ((eq? type 'count)
		        (lambda (a minx b fa fminx fb count)
		          (>= count value))))))))
    (golden-section-max f a b ok?)))


;;; Brent's algorithm for univariate minimization -- transcribed from
;;; pages 79-80 of his book "Algorithms for Minimization Without Derivatives"

(define (brent-min f a b eps)
  (let ((a (min a b)) (b (max a b))
	(maxcount 100)
	(small-bugger-factor *sqrt-machine-epsilon*)
	(g (/ (- 3 (sqrt 5)) 2))
	(d 0) (e 0) (old-e 0) (p 0) (q 0) (u 0) (fu 0))
    (let* ((x (+ a (* g (- b a))))
	   (fx (f x))
	   (w x) (fw fx) (v x) (fv fx))
      (let loop ((count 0))
	(if (> count maxcount)
	    (list 'maxcount x fx count) ;failed to converge
	    (let* ((tol (+ (* eps (abs x)) small-bugger-factor))
		   (2tol (* 2 tol))
		   (m (/ (+ a b) 2)))
	      ;; test for convergence
	      (if (< (max (- x a) (- b x)) 2tol)
		  (list x fx count)
		  (begin
		    (if (> (abs e) tol)
			(let* ((t1 (* (- x w) (- fx fv)))
			       (t2 (* (- x v) (- fx fw)))
			       (t3 (- (* (- x v) t2) (* (- x w) t1)))
			       (t4 (* 2 (- t2 t1))))
			  (set! p (if (positive? t4) (- t3) t3))
			  (set! q (abs t4))
			  (set! old-e e)
			  (set! e d)))
		    (if (and (< (abs p) (abs (* 0.5 q old-e)))
			     (> p (* q (- a x)))
			     (< p (* q (- b x))))
			;; parabolic step
			(begin (set! d (/ p q))
			       (set! u (+ x d))
			       (if (< (min (- u a) (- b u)) 2tol)
				   (set! d (if (< x m) tol (- tol)))))
			;;else, golden section step
			(begin (set! e (if (< x m) (- b x) (- a x)))
			       (set! d (* g e))))
		    (set! u (+ x (if (> (abs d) tol) 
				     d
				     (if (positive? d) tol (- tol)))))
		    (set! fu (f u))
		    (if (<= fu fx)
			(begin (if (< u x) (set! b x) (set! a x))
			       (set! v w) (set! fv fw)
			       (set! w x) (set! fw fx)
			       (set! x u) (set! fx fu))
			(begin (if (< u x) (set! a u) (set! b u))
			       (if (or (<= fu fw) (= w x))
				   (begin (set! v w) (set! fv fw)
					  (set! w u) (set! fw fu))
				   (if (or (<= fu fv) (= v x) (= v w))
				       (begin (set! v u) (set! fv fu))))))
		    (loop (+ count 1))))))))))

(define (brent-max f a b eps)
  (define (-f x) (- (f x)))
  (let ((result (brent-min -f a b eps)))
    (list (car result) (- (cadr result)) (caddr result))))

                           
;;; Given a function f, a starting point and a step size, try to bracket
;;; a local extremum for f. Return a list (retcode a b c fa fb fc iter-count)
;;; where a < b < c, and fa, fb, fc are the function values at these
;;; points. In the case of a minimum, fb <= (min fa fc); the opposite
;;; inequality holds in the case of a maximum. iter-count is the number
;;; of function evaluations required. retcode is 'okay if the search
;;; succeeded, or 'maxcount if it was abandoned.

(define (bracket-min f start step max-tries)
  (define (reorder a b c fa fb fc) ;return with a < c
    (if (< a c)
        (list a b c fa fb fc)
        (list c b a fc fb fa)))
  (define (test-it a b c fa fb fc count) ;assumes b between a and c
    (if (> count max-tries)
        `(maxcount ,@(reorder a b c fa fb fc) ,max-tries)
        (if (<= fb (min fa fc))
            `(okay ,@(reorder a b c fa fb fc) ,count)
            (let ((d (+ c (- c a))))
              (test-it b c d fb fc (f d) (+ count 1))))))
  (let ((a start) (b (+ start step)))
    (let ((fa (f a)) (fb (f b)))
      (if (> fa fb)
          (let ((c (+ b (- b a))))
            (test-it a b c fa fb (f c) 0))
          (let ((c (+ a (- a b))))
            (test-it b a c fb fa (f c) 0))))))

(define (bracket-max f start step . max-tries)
  (define (-f x) (- (f x)))
  (apply bracket-min (append (list -f start step) max-tries)))


;;; Given a function f on [a, b] and N > 0, examine f at the endpoints
;;; a, b, and at N equally-separated interior points. From this form a
;;; list of brackets (p q) in each of which a local maximum is trapped. 
;;; Then apply Golden Section to all these brackets and return a list of
;;; pairs (x fx) representing the local maxima.

(define (local-maxima f a b n ftol)
  (let* ((h (/ (- b a) (+ n 1)))
         (xlist (generate-list 
                  (+ n 2) 
                  (lambda (i) (if (= i (+ n 1)) b (+ a (* i h))))))
         (flist (map f xlist))
         (xi (lambda(i) (list-ref xlist i)))
         (fi (lambda(i) (list-ref flist i)))
         (brack1 (if (> (fi 0) (fi 1))
                     (list (list (xi 0) (xi 1)))
                     '()))
         (brack2 (if (> (fi (+ n 1)) (fi n))
                     (cons (list (xi n) (xi (+ n 1))) brack1)
                     brack1))
         (bracketlist 
           (let loop ((i 1) (b brack2))
             (if (> i n)
                 b
                 (if (and (<= (fi (- i 1)) (fi i)) 
                          (>= (fi i) (fi (+ i 1))))
                     (loop (+ i 1) (cons (list (xi (- i 1)) 
                                               (xi (+ i 1))) b))
                     (loop (+ i 1) b)))))
         (locmax (lambda (int) (gsmax f (car int) (cadr int) 
                                      'function-tol ftol))))
    (map locmax bracketlist)))
                   
(define (local-minima f a b n ftol)
  (let* ((g (lambda (x) (- (f x))))
         (result (local-maxima g a b n ftol))
         (flip (lambda (r) (list (car r) (- (cadr r)) (caddr r)))))
    (map flip result)))
       

(define (estimate-global-max f a b n ftol)
  (let ((local-maxs (local-maxima f a b n ftol)))
    (let loop ((best-so-far (car local-maxs)) 
               (unexamined (cdr local-maxs)))
      (if (null? unexamined)
          best-so-far
          (let ((next (car unexamined)))
            (if (> (cadr next) (cadr best-so-far))
                (loop next (cdr unexamined))
                (loop best-so-far (cdr unexamined))))))))

(define (estimate-global-min f a b n ftol)
  (let ((local-mins (local-minima f a b n ftol)))
    (let loop ((best-so-far (car local-mins)) 
               (unexamined (cdr local-mins)))
      (if (null? unexamined)
          best-so-far
          (let ((next (car unexamined)))
            (if (< (cadr next) (cadr best-so-far))
                (loop next (cdr unexamined))
                (loop best-so-far (cdr unexamined))))))))

