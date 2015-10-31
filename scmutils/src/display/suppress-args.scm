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

(define *suppressed-argument-list* '())

(define *suppressed-argument-list-counter* 0)

(define (suppress-arguments arguments)
  (let ((n (+ (length *suppressed-argument-list*) 1)))
    (set! *suppressed-argument-list-counter*
	  (+ *suppressed-argument-list-counter* 1))
    (set! *suppressed-argument-list*
	  (cons (cons arguments
		      (symbol "args."
			      *suppressed-argument-list-counter*))
		*suppressed-argument-list*))
    n))

(define (show-suppressed-arguments)
  (pp (map (lambda (al)
	     `(,(cdr al) = ,@(car al)))
	   *suppressed-argument-list*)))

(define (clear-arguments)
  (set! *suppressed-argument-list* '())
  (set! *suppressed-argument-list-counter* 0)
  0)

(define (arg-suppressor expression)
  (if (pair? expression)
      (let ((v (assoc (cdr expression) *suppressed-argument-list*)))
	(if v
	    (list (arg-suppressor (car expression)) (cdr v))
	    (cons (arg-suppressor (car expression))
		  (arg-suppressor (cdr expression)))))
      expression))

#|
;;; For example

(let ((t 't) (xy (up 'x 'y)) (uv (up 'r 's)))
  (* (((partial 2) Hp) (up t uv (- (((partial 2) F1) t xy uv))))
     (((partial 2) ((partial 1) F1)) 't xy uv)))
#|
(down
 (+
  (*
   (((partial 1 0) ((partial 2 1) F1)) t (up x y) (up r s))
   (((partial 2 1) Hp)
    (up t
	(up r s)
	(down (* -1 (((partial 2 0) F1) t (up x y) (up r s)))
	      (* -1 (((partial 2 1) F1) t (up x y) (up r s)))))))
  ...mess...)
 ...mess...)
|#

;;; We choose arguments to suppress:

(suppress-arguments '((up t
			  (up r s)
			  (down (* -1 (((partial 2 0) F1) t (up x y) (up r s)))
				(* -1 (((partial 2 1) F1) t (up x y) (up r s)))))))
#| 1 |#

(suppress-arguments '(t (up x y) (up r s)))
#| 2 |#


;;; Now look at the pretty result:

(let ((t 't) (xy (up 'x 'y)) (uv (up 'r 's)))
  (* (((partial 2) Hp) (up t uv (- (((partial 2) F1) t xy uv))))
     (((partial 2) ((partial 1) F1)) 't xy uv)))
#|
(down
 (+ (* (((partial 2 0) Hp) args.1) (((partial 1 0) ((partial 2 0) F1)) args.2))
    (* (((partial 2 1) Hp) args.1) (((partial 1 0) ((partial 2 1) F1)) args.2)))
 (+ (* (((partial 2 0) Hp) args.1) (((partial 1 1) ((partial 2 0) F1)) args.2))
    (* (((partial 2 1) Hp) args.1) (((partial 1 1) ((partial 2 1) F1)) args.2))))
|#

(show-suppressed-arguments)
((args.2 = t (up x y) (up r s))
 (args.1 = (up t
	       (up r s)
	       (down (* -1 (((partial 2 0) F1) t (up x y) (up r s)))
		     (* -1 (((partial 2 1) F1) t (up x y) (up r s)))))))
|#
