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



;;; It is sometimes convenient to be able to split a state.

(define (->parts state cont)
  ;; cont = (lambda (t q p-or-qdot) ...)
  (apply cont (vector->list state)))


(define (with-state cont)
  (lambda (state)
    (->parts state cont)))

(define (with-dynamic-state cont)
  (lambda (state)
    (->parts state
	     (lambda (t q pv)
	       (cont t
		     q
		     (if (vector? pv)
			 (vector->row pv)
			 pv))))))

(define (flatten-state state)
  (list->vector
   (apply append
	  (map (lambda (x)
		 (cond ((vector? x)
			(vector->list x))
		       ((column? x)
			(vector->list (column->vector x)))
		       ((row? x)
			(vector->list (row->vector x)))
		       (else (list x))))
	       (vector->list state)))))

(define local->istate flatten-state)
(define H-state->istate flatten-state)

(define (->istate . args) (flatten-state (apply ->state args)))

(define (flat-state->t fstate) (vector-ref fstate 0))

;;; Assumes k state vector components: [t q q' ... q^(k)]
;;;  Needs to know n (degrees of freedom).

(define (unflatten-L-state flat-state #!optional n)
  (if (default-object? n)
      (set! n (quotient (fix:- (vector-length flat-state) 1) 2)))
  (let* ((kn+1 (vector-length flat-state))
	 (kn (fix:- kn+1 1))
	 (k (quotient kn n)))
    (assert (fix:= 0 (remainder kn n)))
    (if (fix:= n 1)
	flat-state
	(v:generate (fix:+ k 1)		; ->L-state
		    (lambda (i)
		      (if (fix:= i 0)
			  (vector-ref flat-state 0)
			  (vector->column
			   (subvector flat-state
				      (fix:+ (fix:* (fix:- i 1) n) 1)
				      (fix:+ (fix:* i n) 1)))))))))

(define istate->local unflatten-L-state)
(define istate->t time)


;;; Assumes that only t,q,p are present.

(define (unflatten-H-state flat-state)
  (let* ((2n+1 (vector-length flat-state))
	 (2n (fix:- 2n+1 1))
	 (n (quotient 2n 2)))
    (assert (odd? 2n+1))
    (if (fix:= n 1)
	flat-state
	(->H-state
	 (vector-ref flat-state 0)
	 (vector->column (subvector flat-state 1 (fix:+ n 1)))
	 (vector->row (subvector flat-state (fix:+ n 1) 2n+1))))))

(define istate->H-state unflatten-H-state)

;;; An alternative way to obtain Lagrange's equations arises from 
;;;  expanding the derivative of the momentum by the chain rule to
;;;  get the Lagrange operator.  Lagrange's equations are then
;;;  obtained by calling the Lagrange operator on the objects
;;;  q, qdot, qddot, all functions of time.

;;; ******* This stuff only works for L(t,q,qd)

(define (on-jet q qdot)
  (lambda (lfun)
    (compose lfun
	     (lambda (t)
	       (->local t
			(q t)
			(qdot t))))))

(define (Lagrange-operator Lagrangian)
  (let ((P ((partial 2) Lagrangian))
	(F ((partial 1) Lagrangian)))
    (let ((Pq ((partial 1) P))
	  (Pqdot ((partial 2) P))
	  (Pt ((partial 0) P)))
      (lambda (q qdot qddot)
	(let ((lift (on-jet q qdot)))
	  (+ (* (lift Pqdot) qddot)
	     (* (lift Pq) qdot)
	     (lift Pt)
	     (- (lift F))))))))

(define (Lagrange-equations-from-operator Lagrangian)
  (let ((lop (Lagrange-operator Lagrangian)))
    (lambda (q)
      (let ((qdot (D q)))
	(let ((qddot (D qdot)))
	  (lop q qdot qddot))))))

#|
(show-expression
 (((Lagrange-equations-from-operator
    (L-sliding-pend 'm_1 'm_2 'b 'g))
   (coordinate-tuple (literal-function 'x)
		     (literal-function 'theta)))
  't))
(down
 (+ (* -1 b m_2 (sin (theta t)) (expt ((D theta) t) 2))
    (* b m_2 (((expt D 2) theta) t) (cos (theta t)))
    (* m_1 (((expt D 2) x) t))
    (* m_2 (((expt D 2) x) t)))
 (+ (* (expt b 2) m_2 (((expt D 2) theta) t))
    (* b g m_2 (sin (theta t)))
    (* b m_2 (((expt D 2) x) t) (cos (theta t)))))
|#


(define (tz->tqp t z)
  (let* ((2n (vector-length z))
	 (n (quotient 2n 2)))
    (assert (even? 2n))
    (if (fix:= n 1)
	(->H-state t (vector-ref z 0) (vector-ref z 1))
	(->H-state t
		   (vector->column (subvector z 0 n))
		   (vector->row (subvector z n 2n))))))

(define (z->tqp z)
  (tz->tqp 'unknown-time z))

(define (tqp->z tqp)
  (let ((q (coordinate tqp))
	(p (momentum tqp)))
    (if (and (column? q) (row? p))
	(vector->up
	 (vector-append (column->vector q)
			(row->vector p)))
	(up q p))))

(define (tqp->tz tqp)
  (let ((t (time tqp))
	(q (coordinate tqp))
	(p (momentum tqp)))
    (if (and (column? q) (row? p))
	(up t
	    (vector->up
	     (vector-append (column->vector q)
			    (row->vector p))))
	(up t (up q p)))))
