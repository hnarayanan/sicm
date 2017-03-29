#| -*-Scheme-*-

Copyright (C) 1986, 1987, 1988, 1989, 1990, 1991, 1992, 1993, 1994,
    1995, 1996, 1997, 1998, 1999, 2000, 2001, 2002, 2003, 2004, 2005,
    2006, 2007, 2008, 2009, 2010, 2011, 2012, 2013, 2014, 2015, 2016
    Massachusetts Institute of Technology

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

;;;; Scheme algebraic expression compiler to C syntax--Wisdom 2016

;;; all lets are interpreted incorrectly as let*
;;; c does not support parallel assignment.

(define (let? sexp)
  (and (pair? sexp)			;Possible worry! let*\=let
       (or (eq? (car sexp) 'let)
	   (eq? (car sexp) 'let*))))

(define (let-bindings sexp)
  (cadr sexp))

(define (let-body sexp)
  (caddr sexp))

(define (expt? sexp)
  (and (pair? sexp)
       (eq? (car sexp) 'expt)))

(define (expt-base sexp)
  (cadr sexp))

(define (expt-power sexp)
  (caddr sexp))

(define (struct-expr? sexp)
  (and (pair? sexp)
       (or (eq? (car sexp) 'up)
	   (eq? (car sexp) 'down))))

;;; To walk a structure returning an alist of index and terminal element

(define (walk-struct struct-expr)
  (let ((ans '()) (count 0))
    (define (walk struct-expr)
      (if (not (struct-expr? struct-expr))
	  (begin (set! ans (cons (cons count struct-expr) ans))
		 (set! count (+ count 1)))
	  (for-each (lambda (rand)
		      (walk rand))
		    (operands struct-expr))))
    (walk struct-expr)
    (reverse ans)))

(define (prefix->infix sexp vars language #!optional target)
  (if (default-object? target) (set! target 'target))
  (let lp ((sexp (cify-scheme-identifiers sexp)))
    (cond ((number? sexp)
	   (number->string (exact->inexact sexp)))
	  ((symbol? sexp)
	   (symbol->string sexp))
	  ((let? sexp)
	   (apply string-append
		  (list "{"
			(apply string-append
			       (append (map (lambda (bind)
					      (string-append "{"
							     (if (eq? language 'js) "var " "")
							     (symbol->string (car bind))
							     "="
							     (lp (cadr bind))
							     ";}\n "))
					    (let-bindings sexp))
				       (list (lp (let-body sexp)))))
			"}\n")))
	  ((sum? sexp)
	   (let ((ops (map lp (operands sexp))))
	     (string-append "("
			    (interpolate "+" ops)
			    ")")))
	  ((product? sexp)
	   (let ((ops (map lp (operands sexp))))
	     (string-append "("
			    (interpolate "*" ops)
			    ")")))
	  ;; fix for multiarry and unary operators.
	  ((quotient? sexp)
	   (let ((ops (map lp (operands sexp))))
	     (if (not (= (length ops) 2)) (error "quotient: wrong number of arguments"))
	     (string-append "("
			    (interpolate "/" ops)
			    ")")))
	  ((difference? sexp)
	   (let ((ops (map lp (operands sexp))))
	     (if (not (= (length ops) 2)) (error "difference: wrong number of arguments"))
	     (string-append "("
			    (interpolate "-" ops)
			    ")")))
	  ((expt? sexp)
	   (string-append "(pow(" 
			  (lp (expt-base sexp))
			  ","
			  (lp (expt-power sexp))
			  "))"))
	  ((square? sexp)
	   (string-append "(square(" 
			  (lp (cadr sexp))
			  "))"))
	  ((sqrt? sexp)
	   (string-append "(sqrt(" 
			  (lp (cadr sexp))
			  "))"))
	  ((struct-expr? sexp)
	   (string-append "{"
			  (apply string-append
				 (map (lambda (ass)
					(cond ((eq? language 'c)
					       (string-append (symbol->string target)
							      "[" (number->string (car ass)) "]"
							      "=" (lp (cdr ass)) ";\n"))
					      ((eq? language 'js)
					       (string-append "{var "
							      (symbol->string target)
							      "."
							      (symbol->string
							       (ref vars (car ass)))
							      "="  (lp (cdr ass)) ";}\n"))
					      (else (error "language?" language))))
				      (walk-struct sexp)))
			  "}"))
	  ((and (pair? sexp) (symbol? (car sexp))) ;(f . args)=> f(args)
	   (string-append "("
			  (symbol->string (car sexp))
			  "("
			  (interpolate ", " (map lp (operands sexp)))
			  "))"))
	  (else (error "unknown operation"))
	  )))

(define (cify-scheme-identifiers expr)
  (if (pair? expr)
      (cons (cify-scheme-identifiers (car expr))
	    (cify-scheme-identifiers (cdr expr)))
      (cify-atomic expr)))

(define (cify-atomic atom)
  (if (symbol? atom)
      (let ((name (symbol->string atom)))
	(if (fix:= (string-length name) 1)
	    atom
	    (begin 
	      (for-each (lambda (replacement)
			  (string-replace! name (car replacement) (cdr replacement)))
			scheme-c-replacements)
	      (string->symbol name))))
      atom))

;;; This is a bad idea.  Conflates variables.
(define scheme-c-replacements
  '( (#\- . #\_) (#\+ . #\_) (#\* . #\_) (#\/ . #\_) (#\: . #\_) ))

(define (prefix->variables sexp)
  (cond ((let? sexp)
	 (apply string-append
		(map (lambda (bind)
		       (string-append (symbol->string (car bind))
				      ","
				      ))
		     (let-bindings sexp))))
	(else sexp)))


(define (interpolate thing strings)
  (cond ((null? strings)
         '())
        ((null? (cdr strings))
         (car strings))
        (else
         (string-append (car strings)
                        thing
                        (interpolate thing (cdr strings))))))

#|
(display (prefix->infix '(let ((x (+ a b))
			       (y (* d f)))
			   (* x y))
			'(a b d f)
			'js))
{{var x=(a+b);}
 {var y=(d*f);}
 (x*y)}
|#

#|
(display (prefix->infix '(let ((x (+ a b))
			       (y (* d f)))
			   (up x y))
			'(a b d f)
			'js))
{{var x=(a+b);}
 {var y=(d*f);}
 {{var target.a=x;}
  {var target.b=y;}}}
|#

#|
(display (prefix->infix '(let ((x (+ a b))
			       (y (* d f)))
			   (up x y))
			'(a b d f)
			'c))
{{x=(a+b);}
 {y=(d*f);}
 {target[0]=x;
  target[1]=y;}}
|#

(define (jsify-system-derivative params dynamic-vars expression)
  (string-append "function eom(state){\n"
		 (apply string-append
			(map (lambda (var)
			       (string-append
				"{var "
				(symbol->string var)
				" = state."
				(symbol->string var)
				";}\n"))				
			     dynamic-vars))
		 (prefix->infix expression dynamic-vars 'js)
		 "var stated = {"
		 (interpolate ", "
			      (map (lambda (var)
				     (string-append (symbol->string var)
						    ":"
						    "target."
						    (symbol->string var)))
				   dynamic-vars))
		 "};\n"
		 "return(stated);}"))

#|
(display (jsify-system-derivative '()
				  '(a b d f)
				  '(let ((x (+ a b))
					 (y (* d f)))
				     (up x y))))
function eom(state){
{var a = state.a;}
{var b = state.b;}
{var d = state.d;}
{var f = state.f;}
{{var x=(a+b);}
 {var y=(d*f);}
 {{var target.a=x;}
  {var target.b=y;}}}
var stated = {a:target.a, b:target.b, d:target.d, f:target.f};
return(stated);}


;;; But if a, b are parameters

(display (jsify-system-derivative '(a b)
				  '(d f)
				  '(let ((x (+ a b))
					 (y (* d f)))
				     (up x y))))
function eom(state){
{var d = state.d;}
{var f = state.f;}
{{var x=(a+b);}
 {var y=(d*f);}
 {{var target.d=x;}
  {var target.f=y;}}}
var stated = {d:target.d, f:target.f};
return(stated);}
|#
