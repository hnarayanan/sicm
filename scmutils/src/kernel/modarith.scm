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

;;;; Modular Integer Arithmetic

(declare (usual-integrations))

(define modular-type-tag '*modular-integer*)

(define (modint? x)
  (and (pair? x)
       (eq? (car x) modular-type-tag)))

(define (mod:make n p)
  (mod:make-internal (mod:reduce n p) p))

(define (mod:make-internal residue p)
  (list modular-type-tag residue p))

(define (mod:residue modint)
  (list-ref modint 1))

(define (mod:modulus modint)
  (list-ref modint 2))

(define (mod:reduce n p)
  (modulo n p))

(define (mod:unary-combine uop)
  (define (moduop x)
    (assert (modint? x)
	    "Not a modular integer" (list uop x))
    (let ((modulus (mod:modulus x)))
      (mod:make-internal
       (uop (mod:residue x) modulus)
       modulus)))
  moduop)


;;; Given a, p finds b such that a*b=1 mod p
;;;  by solving the linear Diophantine equation
;;;  b*a - c*p = 1 for b and c, and returning only b.

(define (modint:invert a p)
  (define (euclid a p cont)
    (if (int:= a 1)
        (cont 1 0)
	(let ((qr (integer-divide p a)))
	  (let ((q (integer-divide-quotient qr))
		(r (integer-divide-remainder qr)))
	    (euclid r a
		    (lambda (x y)
		      (cont (int:- y (int:* q x))
			    x)))))))
  (euclid a p
	  (lambda (x y)
	    (mod:reduce x p))))

#|
(define (testinv n p)
  (= 1 (modint:* n (modint:invert n p) p)))

(testinv 3 5)
;Value: #t
|#

(define mod:invert (mod:unary-combine modint:invert))

(define (mod:binary-combine bop)
  (define (modbop x y)
    (assert (and (modint? x) (modint? y))
	    "Not modular integers" (list bop x y))
    (let ((modulus (mod:modulus x)))
      (assert (int:= modulus (mod:modulus y))
	      "Not same modulus" (list bop x y))
      (mod:make-internal
       (bop (mod:residue x)
	    (mod:residue y)
	    modulus)
       modulus)))
  modbop)

(define (modint:+ x y p)
  (mod:reduce (int:+ x y) p))

(define (modint:- x y p)
  (mod:reduce (int:- x y) p))

(define (modint:* x y p)
  (mod:reduce (int:* x y) p))

(define (modint:/ x y p)
  (mod:reduce (int:* x (modint:invert y p)) p))

(define (modint:expt base exponent p)
  (define (square x)
    (modint:* x x p))
  (let lp ((exponent exponent))
    (cond ((int:= exponent 0) 1)
	  ((even? exponent)
	   (square (lp (quotient exponent 2))))
	  (else
	   (modint:* base (lp (int:- exponent 1)) p)))))


(define mod:+ (mod:binary-combine modint:+))

(define mod:- (mod:binary-combine modint:-))

(define mod:* (mod:binary-combine modint:*))
   
(define mod:/ (mod:binary-combine modint:/))

(define mod:expt (mod:binary-combine modint:expt))

(define (mod:= x y)
  (assert (and (modint? x) (modint? y))
	  "Not modular integers -- =" (list x y))
  (let ((modulus (mod:modulus x)))
    (assert (int:= modulus (mod:modulus y))
	    "Not same modulus -- =" (list x y))
    (int:= (modulo (mod:residue x) modulus)
	   (modulo (mod:residue y) modulus))))

;;; Chinese Remainder Algorithm
;;;   Takes a list of modular integers, m[i] (modulo p[i])
;;;   where the p[i] are relatively prime.
;;;   Finds x such that  m[i] = x mod p[i]

(define (mod:chinese-remainder . modints)
  (assert (for-all? modints modint?))
  (let ((moduli (map mod:modulus modints))
	(residues (map mod:residue modints)))
    ((modint:chinese-remainder moduli) residues)))

(define (modint:chinese-remainder moduli)
  (let ((prod (apply * moduli)))
    (let ((cofactors
	   (map (lambda (p)
		  (quotient prod p))
		moduli)))
      (let ((f
	     (map (lambda (c p)
		    (* c (modint:invert c p)))
		  cofactors
		  moduli)))
        (lambda (residues)
          (mod:reduce
	   (apply + (map * residues f))
	   prod))))))

#|
(define a1 (mod:make 2 5))

(define a2 (mod:make 3 13))

(mod:chinese-remainder a1 a2)

(mod:chinese-remainder a1 a2)
;Value: 42
|#



(assign-operation 'invert          mod:invert         modint?)


(assign-operation '+               mod:+              modint?  modint?)
(assign-operation '-               mod:-              modint?  modint?)
(assign-operation '*               mod:*              modint?  modint?)
(assign-operation '/               mod:/              modint?  modint?)

(assign-operation 'expt            mod:expt           modint?  modint?)

(assign-operation '=               mod:=              modint?  modint?)

#|
(define (test p)
  (let jlp ((j (- p)))
    (cond ((int:= j p) 'ok)
	  (else
	   (let ilp ((i (- p)))
	     ;;(write-line `(trying ,i ,j)) 
	     (cond ((int:= i p) (jlp (int:+ j 1)))
		   ((int:= (modulo i p) 0) (ilp (int:+ i 1)))
		   (else
		    (let ((jp (mod:make j p))
			  (ip (mod:make i p)))
		      (let ((b (mod:/ jp ip)))
			(if (mod:= (mod:* b ip) jp)
			    (ilp (int:+ i 1))
			    (begin (write-line `(problem dividing ,j ,i))
				   (write-line `((/ ,jp ,ip) =  ,(mod:/ jp ip)))
				   (write-line `((* ,b ,ip) = ,(mod:* b ip))))))))))))))

(test 47)
;Value: ok
|#

