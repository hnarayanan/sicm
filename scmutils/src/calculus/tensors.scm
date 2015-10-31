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

;;;; Testing a function for being a tensor field.

;;; To be a tensor field a function must be linear
;;; over the scalar function field in each of its
;;; arguments.  

;;; Each argument of a tensor field must be either
;;; a one-form field or a vector field.

;;; The test is done with respect to some coordinate
;;; system.  The types of the arguments are specified
;;; in a list.

(define (tensor-test T types coordsys)
  (let ((args (map (literal-field coordsys) types))
	(f ((literal-field coordsys) 'scalar)))
    (map (lambda (i)
	    (let ((thing
		   ((literal-field coordsys) (ref types i))))
	      ((- (apply T
			 (list-with-substituted-coord
			  args i
			   (+ (* f (ref args i))
			      thing)))
		  (+ (* f (apply T args))
		     (apply T
			    (list-with-substituted-coord
			     args i
			     thing))))
	       (typical-point coordsys))))
	  (iota (length types)))))


(define ((literal-field coordsys) type)
    (case type
      ((scalar function)
       (literal-manifold-function
	(generate-uninterned-symbol 'g)
	coordsys))
      ((up vector)
       (literal-vector-field
	(generate-uninterned-symbol 'v)
	coordsys))
      ((down 1form one-form)
       (literal-1form-field
	(generate-uninterned-symbol 'omega)
	coordsys))
      (else
       (error "Bad type list" types))))

#|
(tensor-test
 (Riemann (covariant-derivative (literal-Cartan 'G R3-rect)))
 '(1form vector vector vector)
 R3-rect)
#|
(0 0 0 0)
|#


(define ((F nabla) omega u v)
  (omega ((nabla u) v)))

(tensor-test
 (F (covariant-derivative (literal-Cartan 'G R3-rect)))
 '(1form vector vector)
 R3-rect)
#|
(0 0 <Mess>)
|#

(define ((G nabla) omega u v)
  (omega ((torsion-vector nabla) u v)))

(tensor-test
 (G (covariant-derivative (literal-Cartan 'G R3-rect)))
 '(1form vector vector)
 R3-rect)
#|
(0 0 0)
|#
|#