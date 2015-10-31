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

;;;;                  UTILS.SCM
;;; A few utilities
;;; 5/9/03 (gjs) -- redistributed most list, sets, and table procedures to GENERAL/.
;;; 9/15/89 (gjs) -- added FORALL, EXISTS; moved ACCUMULATION, INVERSE-ACCUMULATION here.
;;; 7/16/89 (mh) correcting bug in DEFAULT-LOOKUP
;;; 9/22/89 (gjs) reduce->a-reduce

(declare (usual-integrations))

(define (do-up low hi proc)
  ;; execute PROC for values beginning at LOW up to HI (exclusive)
  (if (fix:< low hi)
      (begin (proc low)
	     (do-up (fix:+ low 1) hi proc))))

(define (do-down hi low proc)
  ;; execute PROC for values beginning at HI down to LOW (exclusive)
  (if (fix:< low hi)
      (begin (proc hi)
	     (do-down (fix:- hi 1) low proc))))

(define (sign x)
   (cond ((> x 0) 1)
	((< x 0) -1)
	(else 0)))


(define (defer-application f)
  (lambda (x)
    (if (procedure? x)
	(defer-application (compose f x))
	(f x))))
#|
((((defer-application (lambda (x) (* 3 x))) (lambda (x) (+ x 2))) (lambda (x) (/ x 2))) 3)
#| 21/2 |#
|#

(define make-pairwise-test
  (lambda (pred)
    (lambda args
      (define (loop x y rem)
	(and (pred x y)
	     (or (null? rem)
		 (loop y (car rem) (cdr rem)))))
      (if (or (null? args) (null? (cdr args)))
	  (error "Pred needs 2 args" pred args)
	  (loop (car args) (cadr args) (cddr args))))))

(define (all-equal? lst)
  (define (lp lst)
    (if (null? (cdr lst))
	#t
	(and (equal? (car lst) (cadr lst))
	     (lp (cdr lst)))))
  (if (null? lst)
      #t
      (lp lst)))


#|
(define accumulation
  (lambda (operation identity)
    (lambda rest
      (define (loop accum rem)
	(if (null? rem)
	    accum
	    (loop (operation accum (car rem)) (cdr rem))))
      (cond ((null? rest) identity)
	    ((null? (cdr rest)) (car rest))
	    (else (operation (car rest) (loop (cadr rest) (cddr rest))))))))
|#

(define accumulation
  (lambda (operation identity)
    (lambda rest
      (define (loop accum rem)
	(if (null? rem)
	    accum
	    (loop (operation accum (car rem)) (cdr rem))))
      (cond ((null? rest) identity)
	    ((null? (cdr rest)) (car rest))
	    (else (loop (car rest) (cdr rest)))))))

(define inverse-accumulation
  (lambda (operation1 operation2 invop identity)
    (lambda rest
      (define (loop accum rem)
	(if (null? rem)
	    accum
	    (loop (operation2 accum (car rem)) (cdr rem))))
      (cond ((null? rest) identity)
	    ((null? (cdr rest)) (invop (car rest)))
	    ((null? (cddr rest)) (operation1 (car rest) (cadr rest)))
	    (else (operation1 (car rest) (loop (cadr rest) (cddr rest))))))))


(define (left-circular-shift l)
  (if (or (null? l) (null? (cdr l)))
      l
      (append (cdr l) (list (car l)))))

(define (right-circular-shift l)
  (if (or (null? l) (null? (cdr l)))
      l
      (let ((r (reverse l)))
        (cons (car r) (reverse! (cdr r))))))

;;; Functional operators

;;; Arity is important to special case.

(define *at-least-zero* '(0 . #f))
(define *exactly-zero* '(0 . 0))
(define *at-least-one* '(1 . #f))
(define *exactly-one* '(1 . 1))
(define *at-least-two* '(2 . #f))
(define *exactly-two* '(2 . 2))
(define *at-least-three* '(3 . #f))
(define *exactly-three* '(3 . 3))
(define *one-or-two*    '(1 . 2))

(define (exactly-n? arity)
  (fix:= (car arity) (cdr arity)))

(define (any-number? arity)
  (and (null? (cdr arity))
       (fix:= (car arity) 0)))

(define (compose . fs)
  (compose-n fs))

(define (compose-n fs)
  (define (lp fs)
    (cond ((null? (cdr fs)) (car fs))
	  (else (compose-2 (car fs) (lp (cdr fs))))))
  (cond ((null? fs) identity)
	((null? (cdr fs)) (car fs))
	(else				;compose-bin preserves arity
	 (compose-bin (lp (butlast fs))
		      (car (last-pair fs))))))

(define (identity x) x)

(define (compose-2 f g)
  (cond ((pair? g)
	 (lambda x
	   (apply f
		  (map (lambda (gi)
			 (apply gi x))
		       g))))
	(else
	 (lambda x
	   (f (apply g x))))))

(define (compose-bin f g)
  (cond ((pair? g)
	 (let ((a
		(a-reduce joint-arity
			  (map procedure-arity g))))
	   (cond ((equal? a *at-least-zero*)
		  (lambda x
		    (apply f
			   (map
			    (lambda (gi)
			      (apply gi x))
			    g))))
		 ((equal? a *exactly-zero*)
		  (lambda ()
		    (apply f
			   (map (lambda (gi)
				  (gi))
				g))))
		 ((equal? a *at-least-one*)
		  (lambda (x . y)
		    (apply f
			   (map (lambda (gi)
				  (apply gi x y))
				g))))
		 ((equal? a *exactly-one*)
		  (lambda (x)
		    (apply f
			   (map (lambda (gi)
				  (gi x))
				g))))

		 ((equal? a *at-least-two*)
		  (lambda (x y . z)
		    (apply f
			   (map (lambda (gi)
				  (apply gi x y z))
				g))))
		 ((equal? a *exactly-two*)
		  (lambda (x y)
		    (apply f
			   (map (lambda (gi)
				  (gi x y))
				g))))

		 ((equal? a *at-least-three*)
		  (lambda (u x y . z)
		    (apply f
			   (map (lambda (gi)
				  (apply gi u x y z))
				g))))
		 ((equal? a *exactly-three*)
		  (lambda (x y z)
		    (apply f
			   (map (lambda (gi)
				  (gi x y z))
				g))))
		 ((equal? a *one-or-two*)
		  (lambda (x #!optional y)
		    (if (default-object? y)
			(apply f
			       (map (lambda (gi)
				      (gi x))
				    g))
			(apply f
			       (map (lambda (gi)
				      (gi x y))
				    g)))))
		 (else
		  (lambda x
		    (apply f
			   (map
			    (lambda (gi)
			      (apply gi x))
			    g)))))))
	(else
	 (let ((a (procedure-arity g)))
	   (cond ((equal? a *at-least-zero*)
		  (lambda x
		    (f (apply g x))))
		 ((equal? a *exactly-zero*)
		  (lambda ()
		    (f (g))))
		 ((equal? a *at-least-one*)
		  (lambda (x . y)
		    (f (apply g x y))))
		 ((equal? a *exactly-one*)
		  (lambda (x)
		    (f (g x))))
		 ((equal? a *at-least-two*)
		  (lambda (x y . z)
		    (f (apply g x y z))))
		 ((equal? a *exactly-two*)
		  (lambda (x y)
		    (f (g x y))))
		 ((equal? a *at-least-three*)
		  (lambda (u x y . z)
		    (f (apply g u x y z))))
		 ((equal? a *exactly-three*)
		  (lambda (x y z)
		    (f (g x y z))))
		 ((equal? a *one-or-two*)
		  (lambda (x #!optional y)
		    (if (default-object? y)
			(f (g x))
			(f (g x y)))))
		 (else
		  (lambda x
		    (f (apply g x)))))))))

(define (any? . args) #t)
(define (none? . args) #f)

(define ((constant x) . y) x)

(define (joint-arity a1 a2)
  (if (and a1 a2)
      (let ((amin (max (car a1) (car a2)))
	    (amax
	     (let ((a1max (cdr a1)) (a2max (cdr a2)))
	       (if a1max
		   (if a2max
		       (min a1max a2max)
		       a1max)
		   a2max))))
	(if (and amax (fix:< amax amin))
	    #f
	    (cons amin amax)))
      #f))

(define (a-reduce f l)
  (define (loop l)
     (if (null? (cdr l))
         (car l)
         (loop (cons (f (car l) (cadr l)) (cddr l)))))
  (if (null? l)
      (error "Reduce no elements")
      (loop l)))

(define (filter pred l)
  (let lp ((l l))
    (cond ((null? l) '())
          ((pred (car l)) (cons (car l) (lp (cdr l))))
          (else (lp (cdr l))))))

(define (make-map f)		; very neat, e.g. ((make-map -) '(3 2) '(1 1)) = '(2 1)
  (lambda x (apply map (cons f x))))

(define ((bracket . fl) . x)
  (map (lambda (f) (apply f x))
       fl))

(define ((apply-to-all f) x)
  (map f x))

(define (((nary-combine fnary) . fs) . xs)
  (apply fnary
	 (map (lambda (f) (apply f xs))
	      fs)))

(define (((binary-combine fbin) f1 f2) . xs)
  (fbin (apply f1 xs) (apply f2 xs)))

(define (((unary-combine funary) f) . xs)
  (funary (apply f xs)))

(define (iterated f n #!optional id)
  (if (fix:< n 0)
      (error "I don't know how to invert -- ITERATED" f n)
      (let ((ident (if (default-object? id) identity id)))
	(if (fix:= n 0)
	    ident
	    (let lp ((n n))
	      (if (fix:= n 1)
		  f
		  (compose-2 f (lp (fix:- n 1)))))))))


;;; Generalization of fixed point stuff

(define (iterate-until-stable f done? x0)
  (let lp ((x x0))
    (let ((nx (f x)))
      (if (done? nx x)
	  nx
	  (lp nx)))))


;;; given a function f of a variable number of arguments, return a new 
;;; function that accepts a single vector argument and calls f with the 
;;; vector elements as arguments

(define (make-function-of-vector f)
  (lambda (v)
    (apply f (vector->list v))))

;;; given a function of a single vector argument, return a new function 
;;; that takes multiple arguments, being the vector elements

(define (make-function-of-arguments f)
  (lambda args
    (f (list->vector args))))


#|
;;; The following procedure came from SCHEME 6.1.2 RUNTIME
(define alphaless?
  (let ()
    (define (stringify object)
      (cond ((symbol? object) (symbol->string object))
	    ((string? object) object)
	    (else (error "ALPHALESS?: Wrong type argument" object))))

    (named-lambda (alphaless? x y)
      (string<? (stringify x) (stringify y)))))
|#

(define (alphaless? x y)
  (cond ((symbol? x)
	 (cond ((symbol? y) (symbol<? x y))
	       ((string? y) (string<? (symbol->string x) y))
	       (else
		(error "ALPHALESS?: Wrong type argument" y))))
	((string? x)
	 (cond ((string? y) (string<? x y))
	       ((symbol? y) (string<? x (symbol->string y)))
	       (else
		(error "ALPHALESS?: Wrong type argument" y))))
	(else
	 (error "ALPHALESS?: Wrong type argument" x))))

(define (concatenate-names-maker concat-string)
  (define (cn strings)
    (cond ((null? strings) "")
	  ((null? (cdr strings)) (car strings))
	  (else
	   (a-reduce (lambda (n1 n2)
		       (string-append n1 concat-string n2))
		     strings))))
  (define (concatenate-names . names)
    (cond ((null? names) the-null-symbol)
	  ((null? (cdr names)) (car names))
	  (else
	   (string->symbol
	    (cn (map symbol->string
		     (filter (lambda (x)
			       (not (eq? x the-null-symbol)))
			     names)))))))
  concatenate-names)


(define the-null-symbol (string->symbol ""))

(define concatenate-names (concatenate-names-maker "."))

;;; Special property of MIT CScheme

(define (print-depth #!optional newval)
  (if (default-object? newval) (set! newval #F))
  (if (or (not newval)
	  (and (integer? newval)
	       (positive? newval)))
      (set-fluid! *unparser-list-depth-limit* newval)
      (error "PRINT-DEPTH: Wrong type argument" newval)))

(define (print-breadth #!optional newval)
  (if (default-object? newval) (set! newval #F))
  (if (or (not newval)
	  (and (integer? newval)
	       (positive? newval)))
      (set-fluid! *unparser-list-breadth-limit* newval)
      (error "PRINT-BREADTH: Wrong type argument" newval)))


;;;for printing things out

(define (wallp-pp p? . objs)
  (if p? (for-each pp objs)))

(define (pp-it x)
  (pp x)
  x)

(define (watch-it wallp message)
  (lambda (e)
    (if wallp
	(begin (newline)
	       (display message)
	       (pp e)))
    e))

(define (cpp x #!optional port)
  (pp x
      (if (default-object? port)
	  (current-output-port)
	  port)
      ;; as code
      true))

;;; Programs may leave notes here

(define *taking-notes* #t)
(define *showing-notes* #f)

(define *notes* '())

(define (note-that! note)
  (and note                             ;fail if note is #f
       (begin
         (if *showing-notes*
             (display-note note))
         (if *taking-notes*
             (begin 
               (set! *notes* (lset-adjoin equal? *notes* note))
               'noted)
             'ignored))))

(define (clear-notes!)
  (set! *last-notes* *notes*)
  (set! *notes* '()))

(define (display-note note)
  (display "#| ")
  (newline)
  (pp note)
  (display "|#")
  (newline))

(define *last-notes*)
(define *last-notes-shown*)

(define (show-notes)
  (set! *last-notes-shown* *last-notes*)
  (newline)
  (display "#| ")
  (for-each (lambda (note)
              (newline)
              (pp note)
              (let ((sig (eq-get note 'rules)))
                (if sig (pp sig))))
            *last-notes*)
  (display "|#"))
