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

;;; Almost right... Make patterns recursive...

(define-syntax let&
  (rsc-macro-transformer
   (lambda (form defn-env)
     (define (make-let&-match pattern access-chain source)
       (apply append
	      (map (lambda (v i)
		     (cond ((eq? v '_) '())
			   ((symbol? v)
			    (list `(,v (ref ,source ,@access-chain ,i))))
			   ((list? v)
			    (make-let&-match v (append access-chain (list i)) source))
			   (else (error "Ill-structured LET&" form))))
		   pattern (iota (length pattern)))))
     (if (list? (cadr form))
	 (let ((extends (filter (lambda (b) (list? (car b))) (cadr form))))
	   (if (null? extends)
	       `(let ,@(cdr form))
	       (let ((gens (map (lambda (x) (generate-uninterned-symbol)) extends)))
		 (let ((newbindings
			(let loop ((bindings (cadr form)) (gens gens))
			  (if (null? bindings)
			      '()
			      (let ((b (car bindings)))
				(cond ((symbol? (car b))
				       (cons b (loop (cdr bindings) gens)))
				      ((list? (car b))
				       (append (make-let&-match (car b) '() (car gens))
					       (loop (cdr bindings) (cdr gens))))
				      (else (error "Ill-structured LET&" form))))))))
		   `(let ,(map (lambda (gen extend)
				 `(,gen ,(cadr extend)))
			       gens extends)
		      (let ,newbindings
			  ,@(cddr form)))))))
	 (error "Cannot make named LET&, sorry..." form)))))

#|
(pp
 (unsyntax
  (syntax
   '(let& ((a b) (c d)) e)
   user-initial-environment)))
(let ((a b) (c d))
  e)

(pp
 (unsyntax
  (syntax
   '(let& ((a b) ((x y) foo) (c d)) e)
   user-initial-environment)))
(let ((|G1| foo))
  (let ((a b) (x (ref |G1| 0)) (y (ref |G1| 1)) (c d))
    e))

(pp
 (unsyntax
  (syntax
   '(let& ((a b) ((x y) foo) ((u v) bar) (c d)) e)
   user-initial-environment)))
(let ((|G2| foo) (|G3| bar))
  (let ((a b)
	(x (ref |G2| 0)) (y (ref |G2| 1))
	(u (ref |G3| 0)) (v (ref |G3| 1))
	(c d))
    e))

;;; Can use _ to make ignorable reference
(pp
 (unsyntax
  (syntax
   '(let& ((a b) ((x y) foo) ((u _ w) bar) (c d)) e)
   user-initial-environment)))
(let ((|G7| foo) (|G8| bar))
  (let ((a b)
	(x (ref |G7| 0)) (y (ref |G7| 1))
	(u (ref |G8| 0)) (w (ref |G8| 2))
	(c d))
    e))
|#

#|
;;; Actual use case...

(define ((L m g) state)
  (let& (((_ (x y) v) state))
	(- (* 1/2 m (square v))
	   (* m g y))))

(pp L)
(named-lambda (L m g)
  (lambda (state)
    (let ((G75 state))
      (let ((x (ref G75 1 0)) (y (ref G75 1 1)) (v (ref G75 2)))
        (- (* 1/2 m (square v)) (* m g y))))))

(((Lagrange-equations (L 'm 'g))
  (up (literal-function 'x) (literal-function 'y)))
 't)
#|
(down (* m (((expt D 2) x) t))
      (+ (* g m) (* m (((expt D 2) y) t))))
|#

|#
