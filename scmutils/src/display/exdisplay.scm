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

;;;; DISPLAY-EXPRESSION

;;; This is a package of expression display programs for Scmutils.
;;; This printer is a modified version of code developed by Aubrey
;;; Jaffer for the JACAL symbolic mathematics system.  It also uses
;;; ideas from a printer developed by Kleanthes Koniaris.

;;; SHOW-EXPRESSION uses Tex to put up a window with the displayed
;;; expression.  It also pretty-prints the expression in the Scheme
;;; buffer.  It also prints the tex string used togenerate the tex output.
;;; The window is displayed using xdvi.  Type Q in the window to get rid
;;; of it.  If you quit in other ways (e.g., quitting from Scheme) this
;;; may leave garbage temp files in your directory.  The garbage files are
;;; named temp-display<n>.

;;; EXPRESSION->TEX-STRING returns the string that is input to tex

;;; 2D-SHOW-EXPRESSION is a simple 2D ASCII character printer for
;;; mathematical expressions.
;;; Example:

;;; (define test
;;; '(/
;;;  (+ alpha (/ ((derivative f) b) (+ alpha beta)))
;;;  (+ (/ (+ x y) 2) (expt (/ (+ a c (/ 2 x)) (* d e)) (+ f (/ g h))))))

;;; (display (expression->tex-string test))
;;; $${{\alpha + {{Df\left( b \right)}\over {\alpha + \beta}}}
;;; \over {{{x + y}\over {2}} + \left( {{a + c + {{2}\over {x}}}\over {d e}} \right)
;;; ^{f + {{g}\over {h}}}}}$$

;;; (2d-show-expression test)
;;;                Df(b)        
;;;     alpha + ------------    
;;;             alpha + beta    
;;; ----------------------------
;;;                            g
;;;                        f + -
;;;          /         2 \     h
;;;         |  a + c + -  |     
;;; x + y   |          x  |     
;;; ----- + |  ---------  |     
;;;   2      \    d e    /      

;;; Unlike Jaffer's code, this version does not handle line breaks.  We
;;; can extend it some day.

#|
;;; To make this stand-alone must add:

     (define derivative-symbol 'D)

     (define (up-maker? expr) (and (pair? expr) (eq? (car expr) 'up)))

     (define (vector-maker? expr) (and (pair? expr) (eq? (car expr) 'vector)))

     (define (down-maker? expr) (and (pair? expr) (eq? (car expr) 'down)))

     (define (matrix-by-rows-maker? expr)
       (and (pair? expr) (eq? (car expr) 'matrix-by-rows)))
|#

(declare (usual-integrations))

;;; exported functions
(define internal-show-expression)
(define 2d-show-expression)
(define expression->tex-string)
(define display-tex-string)

(define last-tex-string-generated)

(define enable-tex-display #t)

(let ()					;package all stuff internally

(define display-in-screen-window
  (let ((count 0))
    (lambda (tex-string)
      (let* ((dirname (->namestring (user-homedir-pathname)))
	     (file-name (string-append dirname
				       "temp-display"
				       (number->string count)))
	     (complete-tex-input (string-append
				  ;;" \\magnification=\\magstep2\n"
				  ;;"\\hsize=48pc  \\hoffset=-4pc  "
				  "\\voffset=-6pc "
				  "\\hsize=48pc " " \\hoffset=-6pc  "
				  boxit-string "\n"
				  tex-string
				  "\\vfil\\bye")))
	(with-output-to-file
	    (string-append file-name ".tex")
	  (lambda () (display complete-tex-input)))
	#|
	(working-unix/system (string-append "cd " dirname ";"
					    " tex " file-name
					    " > //dev//null 2>&1 "))
	(working-unix/system
	 (string-append "xdvi " file-name ".dvi "
			"-s 4 "
			"-yoffset 3.5 "
			"-geometry 900x400+1+1; "
			"//bin//rm " file-name ".*"
			))
	|#
	(run-shell-command
	 (string-append "cd " dirname ";"
			" tex " file-name
			" > /dev/null 2>&1 ")
	 'output #f
	 'shell-file-name "/bin/sh")
	(run-shell-command
	 (string-append "xdvi " file-name ".dvi "
			"-s 4 "
			"-yoffset 3.5 "
			"-geometry 900x400+1+1"
			" > /dev/null 2>&1; "
			"/bin/rm " file-name ".*"
			)
	 'output #f
	 'shell-file-name "/bin/sh")
	(set! count (+ count 1))
	))))


(define boxit-string
  "\\def\\boxit#1{\\vbox{\\hrule\\hbox{\\vrule\\kern5pt
     \\vbox{\\kern5pt#1\\kern5pt}\\kern3pt\\vrule}\\hrule}}\n")


;;; A couple of utility procedures:

#|
;;some magic from Jinx
(define (working-unix/system string)
  (let ((old #f))
    (dynamic-wind
     (lambda ()
       (set! old (thread-timer-interval))
       (set-thread-timer-interval! #f))
     (lambda ()
       (unix/system string))
     (lambda ()
       (set-thread-timer-interval! old)))))
|#


(define (2d-display-box box)
  (newline)
  (newline)
  (for-each (lambda (line)
	      (for-each display (line-elements line))
	      (newline))
	    (box-lines box))
  (newline))


;;; The program works by gluing together boxes, which are lists of
;;; lines, all of the same width (the box-width).  A box also has a
;;; vertical offset (box-voffset), which is used to align boxes when
;;; gluing them together horizontally--the vertical offset represents
;;; the height of the top line of the box with respect to a designated
;;; "zero" line.  There is also a binding power (box-binding-power)
;;; that is used in unparsing expressions from infix to prefix.  The
;;; binding power represents how how tightly the box is "held together".  If
;;; the binding power is less than the required binding power, the box
;;; will be enclosed in parentheses.

;;; A character string is considered to be a special kind of box with
;;; voffset 0 and binding power the maximum binding power.

;;; Data structure definitions for boxes

(define (make-box voffset width binding-power lines)
  (append (list 'box voffset width binding-power)
	  lines))

(define (explicit-box? elt)
  (and (pair? elt)
       (eq? (car elt) 'BOX)))

(define (box-voffset box)
  (if (explicit-box? box)
      (list-ref box 1)
      0))

(define (box-width box)
  (if (explicit-box? box)
      (list-ref box 2)
      (string-length box)))

(define (box-binding-power box)
  (if (explicit-box? box)
      (list-ref box 3)
      max-bp))

(define (box-lines box)
  (if (explicit-box? box)
      (list-tail box 4)
      (list (make-line (list box)))))

(define (box-nlines box)
  (length (box-lines box)))

;;;make a box just like the given one, but with the designated binding
;;;power
(define (make-box-with-bp bp box)
  (make-box (box-voffset box)
	    (box-width box)
	    bp
	    (box-lines box)))

(define (make-empty-box width height)
  (let ((lines (make-list height (make-blank-line width))))
    (make-box 0		       ;v-offset arbitrary
	      width
	      max-bp	       ;binding power arbitrary
	      lines)))


;;; A LINE is a list of strings (the line-elements of the line)

(define (make-line elements)
  (cons 'line elements))

(define (line-elements line)
  (cdr line))

(define (make-blank-line width)
  (make-line (make-blank-line-elts width)))

(define (make-blank-line-elts width)
  (if (= width 0)
      '()
      (list (make-string width #\SPACE))))


;;;; Operations for combining boxes

;;;Join boxes horizontally, aligned according to the vertical offsets.
;;;Resulting box will have the binding power of the first box and a
;;;v-offset equal to the max of the v-offsets

(define (glue-horiz boxes)
  (if (null? (cdr boxes))
      (car boxes)
      (join2-right (car boxes) (glue-horiz (cdr boxes)))))

(define (join2-right box1 box2)
  (let ((v1 (box-voffset box1))
	(v2 (box-voffset box2))
	(blank1 (make-blank-line (box-width box1)))
	(blank2 (make-blank-line (box-width box2))))
    (make-box (max v1 v2)
	      (+ (box-width box1) (box-width box2))
	      (box-binding-power box1)
	      (cond ((> v1 v2)
		     ;;must pad box2 on top to start
		     (join-lines-horiz
		      (box-lines box1)
		      (append (make-list (- v1 v2) blank2)
			      (box-lines box2))
		      blank1
		      blank2))
		    ((> v2 v1)
		     ;;must pad box1 on top
		     (join-lines-horiz
		      (append (make-list (- v2 v1) blank1)
			      (box-lines box1))
		      (box-lines box2)
		      blank1
		      blank2))
		    (else (join-lines-horiz (box-lines box1)
					    (box-lines box2)
					    blank1
					    blank2))))))

(define (join-lines-horiz lines1 lines2 blank1 blank2)
  (cond ((null? lines1)
	 (map (lambda (line2) (make-line (append (line-elements blank1)
						 (line-elements line2))))
	      lines2))
	((null? lines2)
	 (map (lambda (line1) (make-line (append (line-elements line1)
						 (line-elements blank2))))
	      lines1))
	(else (cons (make-line (append (line-elements (car lines1))
				       (line-elements (car lines2))))
		    (join-lines-horiz (cdr lines1) (cdr lines2) blank1 blank2)))))


;;; Glue boxes vertically.  The boxes will all be extended to the
;;; width of the maximum width box, and centered within that width.
;;; The voffset will be the voffset of the first box.  (I.e., the
;;; first box will stay at the same level, and the other boxes will be
;;; appended below it.)  The binding power will be the binding power
;;; of the first box. 

(define (glue-vert boxes)
  (if (null? (cdr boxes))
      (car boxes)
      (glue-below (car boxes) (glue-vert (cdr boxes)))))


(define (glue-below box1 box2)
  (let* ((new-width (max (box-width box1) (box-width box2)))
	 (nbox1 (pad-box-centered-to-width new-width box1))
	 (nbox2 (pad-box-centered-to-width new-width box2)))
    (make-box
     (box-voffset box1)
     new-width
     (box-binding-power box1)
     (append (box-lines nbox1) (box-lines nbox2)))))
    

;;; Glue-above is similar to glue-below below, except that the 
;;; v-offset of the top line in box2 remains
;;; what it was, and box1 is glued in above it.

(define (glue-above box1 box2)
  (let* ((new-width (max (box-width box1) (box-width box2)))
	 (nbox1 (pad-box-centered-to-width new-width box1))
	 (nbox2 (pad-box-centered-to-width new-width box2)))
    (make-box
     (+ (box-voffset box2) (length (box-lines box1)))
     new-width
     (box-binding-power box1)
     (append (box-lines nbox1) (box-lines nbox2)))))


;;;pad the box on both the left and the right so it is centered in a
;;;box of the given width

(define (pad-box-centered-to-width width box)
  (let* ((extra (- width (box-width box)))
	 (extra-left (floor->exact (/ extra 2)))
	 (extra-right (- extra extra-left))
	 (pad-left (make-blank-line-elts extra-left))
	 (pad-right (make-blank-line-elts extra-right)))
    (make-box (box-voffset box)
	      width
	      (box-binding-power box)
	      (map (lambda (line)
		     (make-line 
		      (append pad-left
			      (line-elements line)
			      pad-right)))
		   (box-lines box)))))


;;; pad the box on both the top and the bottom so it will be centeted
;;; in a box of the given height.  "Centered" here means that the box
;;; will appear in the center of the expanded box, regardles of where
;;; the zero line was.

(define (pad-box-centered-to-height height box)
  (let* ((extra (- height (box-nlines box)))
	 (extra-top (floor->exact (/ extra 2)))
	 (extra-bottom (- extra extra-top))
	 (width (box-width box)))
    (let ((padded-box
	   (glue-below (glue-above (make-empty-box width extra-top)
				   box)
		       (make-empty-box width extra-bottom))))
      (shift-top-to (- (box-nlines padded-box) 1)
		    padded-box))))

;;; Offsetting boxes vertically

;;; Make the voffset of the bottom of the box be at n
(define (shift-bottom-to n box)
  (shift-top-to (+ n -1 (box-nlines box)) box))


;;; Shift the box so that its zero line is now at n
(define (shift-zero-to n box)
  (shift-top-to (+ n (box-voffset box)) box))


;;; Shift the box so that the top of the box is now at n
(define (shift-top-to n box)
  (make-box n
	    (box-width box)
	    (box-binding-power box)
	    (box-lines box)))


;;;Create a box from a list of strings, on string per line.  The
;;;strings are padded on the right to be all the same width.

(define (strings->vbox voffset strings)
  (let* ((width (apply max (map string-length strings)))
	 (padded-strings
	  (map (lambda (string)
		 (string-append (make-string
				 (- width (string-length string))
				 #\SPACE)
				string))
	       strings))
	 (lines (map (lambda (string)
		       (make-line (list string)))
		     padded-strings)))
    (make-box voffset
	      width
	      max-bp
	      lines)))

;;; List utility: 
;;;Interpolate element between all items in the list

(define (interpolate element list)
  (cond ((null? list) '())
	((null? (cdr list)) list)
	(else (cons (car list)
		    (cons element
			  (interpolate element (cdr list)))))))

;;; Binding powers of elements, and required binding powers.  An element
;;; on the left will be parenthesized if it is used in a context on the
;;; right that appears above it in the table.
;;; 
;;; max-bp                    200
;;; one-char symbol           200
;;; parenthesized thing       200
;;; 
;;; n-char symbol             190        product set off with dots in Tex
;;; 
;;; subscripted               140
;;; superscripted             140
;;; derivative                140
;;; 2nd derviv                140
;;; partial deriv             140
;;; nth deriv                 140
;;;                                      base of exponentiation  140
;;;                                      item to be subscripted 140
;;;                                      subscript 140
;;;                                      item to be differentiated 140                
;;; expt                      130
;;; application               130
;;;                                      operator of application 130
;;; product                   120
;;; quotient                  120
;;;                                      term in product 120
;;; sum/diff                  100
;;;                                      exponent of exponentiation 100
;;;                                      term in sum 100
;;; negation                   99

(define max-bp 200)

;;; Enclose the box in parentheses if its binding power is less than
;;; the required bp. Uptable is the unparsing table (needed in order
;;; to know how to parenthesize).

(define (insure-bp uptable required box)
  (if (< (box-binding-power box) required)
      ((cadr (assq 'parenthesize uptable)) uptable box)
      box))

;;; Create a new box by enclosing the given box in vertically
;;; expanding parentheses.  The binding power of the new box is the
;;; maximum binding power.  The voffset will be the voffset of the
;;; original box.

(define (2d:parenthesize uptable box)
  (let ((n (box-nlines box)))
    (cond ((= n 0) "()")
	  ((<= n 2)
	   (make-box-with-bp max-bp
			     (glue-horiz (list "(" box ")"))))
	  (else
	   (let ((left-paren
		  (strings->vbox
		   (box-voffset box)
		   (append           '(" / ")
		    (make-list (- n 2) "|  ")
                                     '(" \\ "))))
		 (right-paren
		  (strings->vbox
		   (box-voffset box)
		   (append           '(" \\ ")
		    (make-list (- n 2) "  |")
                                     '(" / ")))))
	     (make-box-with-bp max-bp (glue-horiz (list left-paren box right-paren))))))))

(define (tex:parenthesize uptable box)
  (make-box-with-bp max-bp (glue-horiz (list "\\left( " box " \\right)"))))

;;;; Unparsing handlers.  Each handler takes the expression and the
;;;; unparsing table.

;;; Some of the handlers here work both for tex-display and for
;;; 2d-display, others are specific to one or the other.

(define (unparse-default uptable args)
  (make-box-with-bp 130
		    (glue-horiz
		     (list (insure-bp uptable 130 (car args))
			   ((cadr (assq 'parenthesize uptable))
			    uptable
			    (if (null? (cdr args))
				""
				(glue-horiz (interpolate ", " (cdr args)))))))))

(define (unparse-sum uptable args)
  (let ((args (map (lambda (a) (insure-bp uptable 100 a)) args)))
    (make-box-with-bp 100 (glue-horiz (interpolate " + " args)))))

(define (unparse-difference uptable args)
  (let ((args (map (lambda (a) (insure-bp uptable 100 a)) args)))
    (make-box-with-bp 100 (glue-horiz (interpolate " - " args)))))

(define (unparse-negation uptable args)
  (make-box-with-bp 99
		    (glue-horiz
		     (list  "- " (insure-bp uptable 101 (car args))))))

(define (unparse-signed-sum uptable signs terms)
  (let ((args (map (lambda (a) (insure-bp uptable 100 a)) terms)))
    (make-box-with-bp 100 (glue-horiz (interpolate-signs signs args)))))

;;number of signs should equal number of args
(define (interpolate-signs signs args)
  (define (interp signs args)
    (cond ((null? args) '())
	  ((null? (cdr args)) args)
	  (else (cons (car args)
		      (cons (if (eq? (car signs) '-) " - " " + ")
			    (interp (cdr signs) (cdr args)))))))
  (let ((after-first-sign (interp (cdr signs) args)))
    (if (eq? (car signs) '-)
	(cons " - " after-first-sign)
	after-first-sign)))


(define (2d:unparse-product uptable args)
  (let ((args (map (lambda (a) (insure-bp uptable 120 a)) args)))
    (make-box-with-bp 120 (glue-horiz (interpolate " " args)))))


;;; For a product, if an element is a >1-char symbol (binding power
;;; 190), we set it off from the other factors by dots
(define (tex:unparse-product uptable args)
  (let ((args (map (lambda (a) (insure-bp uptable 120 a)) args)))
    (make-box-with-bp 120 (glue-horiz (interpolate-for-tex-product args)))))

(define (interpolate-for-tex-product list)
  (define (separator a1 a2)
    (if (or (= (box-binding-power a1) 190)
	    (= (box-binding-power a2) 190))
	" \\cdot "
	" "))
  (cond ((null? list) '())
	((null? (cdr list)) list)
	(else (cons (car list)
		    (cons (separator (car list) (cadr list))
			  (interpolate-for-tex-product (cdr list)))))))

(define (2d:unparse-quotient uptable args)
  (let* ((box1 (car args))
	 (box2 (cadr args))
	 (width (max (box-width box1) (box-width box2)))
	 (rule-box (make-string width #\-)))
    (make-box-with-bp 120
		      (glue-below
		       (glue-above box1 rule-box)
		       box2))))

(define (tex:unparse-quotient uptable args)
  (let ((box1 (car args))
	(box2 (cadr args)))
    (make-box-with-bp 120
		      (glue-horiz
		       (list  "{" "{" box1 "}"
			     "\\over "
			       "{" box2 "}" "}")))))


(define (2d:unparse-expt uptable  args)
  ;;if base is not shorter than expt, raise expt
  ;;so that its zero is one above the top of the base.
  ;;otherwise raise so that bottom of expt is one above the top of the
  ;;base
  (let* ((base (insure-bp uptable 140 (car args)))
	 (expt (insure-bp uptable 100 (cadr args)))
	 (shift-expt
	  (if (>= (box-nlines base) (box-nlines expt))
	      (shift-zero-to (+ (box-voffset base) 1)
			     expt)
	      (shift-bottom-to (+ (box-voffset base) 1)
			       expt))))
    (make-box-with-bp
     130
     (glue-horiz (list base shift-expt)))))

(define (tex:unparse-expt uptable args)
  (let ((base (insure-bp uptable 140 (car args)))
	(expt (insure-bp uptable 100 (cadr args))))
    (make-box-with-bp
     130
     (glue-horiz (list "{" base "}^{" expt "}")))))


(define (2d:unparse-superscript uptable args)
  (let ((top (insure-bp uptable 140 (car args)))
	(script (insure-bp uptable 140 (cadr args))))
    (make-box-with-bp
     140
     (glue-horiz
      (list top
	    (shift-top-to (+ (box-voffset top) (box-nlines top))
			  script))))))


(define (tex:unparse-superscript uptable args)
  (let ((top (insure-bp uptable 140 (car args)))
	(scripts
	 (map (lambda (ss)
		(insure-bp uptable 140 ss))
	      (cdr args))))
    (make-box-with-bp
     140
     (glue-horiz
      (append (list "{")
	      (list top)
	      (list "}^{")
	      (let lp ((scripts scripts))
		(if (null? (cdr scripts))
		    (list (car scripts))
		    (append (list (car scripts))
			    (list ", ")
			    (lp (cdr scripts)))))
	      (list "}"))))))

(define (2d:unparse-subscript uptable args)
  (let ((top (insure-bp uptable 140 (car args)))
	(script (insure-bp uptable 140 (cadr args))))
    (make-box-with-bp
     140
     (glue-horiz
      (list top
	    (shift-top-to (- (box-voffset top) (box-nlines top))
			  script))))))


(define (tex:unparse-subscript uptable args)
  (let ((top (insure-bp uptable 140 (car args)))
	(scripts
	 (map (lambda (ss)
		(insure-bp uptable 140 ss))
	      (cdr args))))
    (make-box-with-bp
     140
     (glue-horiz
      (append (list "{")
	      (list top)
	      (list "}_{")
	      (let lp ((scripts scripts))
		(if (null? (cdr scripts))
		    (list (car scripts))
		    (append (list (car scripts))
			    (list ", ")
			    (lp (cdr scripts)))))
	      (list "}"))))))

#|
(define (tex:unparse-subscript uptable args)
  (let ((top (insure-bp uptable 140 (car args)))
	(script (insure-bp uptable 140 (cadr args))))
    (make-box-with-bp
     140
     (glue-horiz (list top "_{" script "}")))))
|#

(define (unparse-derivative uptable args)
  (make-box-with-bp
   140
   (glue-horiz (list "D" (insure-bp uptable 140 (car args))))))

#|
(define (unparse-derivative uptable args)
  (make-box-with-bp
   140
   (glue-horiz (list (insure-bp uptable 140 (car args)) "'"))))
|#

(define (tex:unparse-sqrt uptable args)
  (make-box-with-bp
   140
   (glue-horiz (list "\\sqrt{" (insure-bp uptable 90 (car args)) "}"))))

(define (tex:unparse-dotted uptable args)
  (make-box-with-bp
   140
   (glue-horiz (list "\\dot{" (insure-bp uptable 140 (car args)) "}"))))

(define (2d:unparse-dotted uptable args)
  (make-box-with-bp
   140
   (glue-horiz (list "(dot " (insure-bp uptable 140 (car args)) ")"))))

(define (tex:unparse-dotdotted uptable args)
  (make-box-with-bp
   140
   (glue-horiz (list "\\ddot{" (insure-bp uptable 140 (car args)) "}"))))

(define (2d:unparse-dotdotted uptable args)
  (make-box-with-bp
   140
   (glue-horiz (list "(ddot " (insure-bp uptable 140 (car args)) ")"))))

(define (tex:unparse-primed uptable args)
  (let ((top (insure-bp uptable 140 (car args))))
    (make-box-with-bp 140
     (glue-horiz (list "{" top "}^\\prime")))))

(define (2d:unparse-primed uptable args)
  (make-box-with-bp
   140
   (glue-horiz (list "(prime " (insure-bp uptable 140 (car args)) ")"))))

(define (tex:unparse-primeprimed uptable args)
  (let ((top (insure-bp uptable 140 (car args))))
    (make-box-with-bp 140
     (glue-horiz (list "{" top "}^{\\prime\\prime}")))))

(define (2d:unparse-primeprimed uptable args)
  (make-box-with-bp
   140
   (glue-horiz (list "(primeprime " (insure-bp uptable 140 (car args)) ")"))))

#|
(define (unparse-second-derivative uptable args)
  (make-box-with-bp
   140
   (glue-horiz (list (insure-bp uptable 140 (car args)) "''"))))
|#

(define (2d:unparse-second-derivative uptable args)
  (make-box-with-bp
   140
   (glue-horiz (list (2d:unparse-expt uptable (list "D" "2"))
		     (insure-bp uptable 140 (car args))))))


(define (tex:unparse-second-derivative uptable args)
  (make-box-with-bp
   140
   (glue-horiz (list (tex:unparse-expt uptable (list "D" "2"))
		     (insure-bp uptable 140 (car args))))))

;;; This does not work with multiple subscripts
(define (2d:unparse-partial-derivative uptable args)
  (make-box-with-bp
   140
   (glue-horiz (list (2d:unparse-subscript uptable (list "D" (cadr args)))
		     (insure-bp uptable 140 (car args))))))

(define (tex:unparse-partial-derivative uptable args)
  (make-box-with-bp
   140
   (glue-horiz
    (list (tex:unparse-subscript uptable (cons "\\partial" (cdr args)))
	  (insure-bp uptable 140 (car args))))))

(define (2d:unparse-nth-derivative uptable args)
  (let ((op (2d:unparse-expt uptable (list "D" (cadr args)))))
    (make-box-with-bp
     140
     (glue-horiz (list op (insure-bp uptable 140 (car args)))))))

(define (tex:unparse-nth-derivative uptable args)
  (let ((op (tex:unparse-expt uptable (list "D" (cadr args)))))
    (make-box-with-bp
     140
     (glue-horiz (list op (insure-bp uptable 140 (car args)))))))

;;; vector is printed as column matrix
(define (2d:unparse-vector uptable args)
  (2d:unparse-matrix uptable
		     (map list args)))


;;; a vector will be displayed as a 1-column matrix
(define (tex:unparse-vector uptable args)
  ;;args here is the list of vector elements
  (tex:unparse-matrix uptable
		      (map list args)))

;;; matrix list is a list of rows where each row is a list
(define (2d:unparse-matrix uptable matrix-list)
  ;;first pad all elements in each column to the max width in the
  ;;column
  (define (transpose matrix-lists)
    (apply map (cons list matrix-lists)))
  (let* ((matrix-with-widended-columns
	  (transpose
	   (map (lambda (column)
		  (let ((width (apply max (map box-width column))))
		    (map (lambda (element)
			   (pad-box-centered-to-width width element))
			 column)))
		(transpose matrix-list))))
	 ;;pad all elts in each row to the max height
	 (matrix-with-lengthened-rows
	  (map (lambda (row)
		 (let ((height  (apply max (map box-nlines row))))
		   (map (lambda (element)    
			  (pad-box-centered-to-height height element))
			row)))
	       matrix-with-widended-columns))
	 ;;glue elts in each row together, separated by two spaces
	 (row-boxes
	  (map (lambda (row) (glue-horiz (interpolate "  " row)))
	       matrix-with-lengthened-rows))
	 ;;glue the rows together, with separated by blank lines
	 (separated-row-boxes
	  (interpolate (make-empty-box (box-width (car row-boxes)) 1)
		       row-boxes))
	 (all-elts
	  (glue-vert separated-row-boxes))
	 ;;surround matrix by brackets
	 (with-brackets
	  (glue-horiz
	   (list (strings->vbox (box-voffset all-elts)
				(make-list (box-nlines all-elts) "["))
		 all-elts
		 (strings->vbox (box-voffset all-elts)
				(make-list (box-nlines all-elts) "]"))))))
    ;;center matrix vertically
    (shift-top-to
     (floor->exact (/ (box-nlines with-brackets) 2))
     with-brackets)))
	 
(define (tex:unparse-matrix uptable matrix-list)
  (let* ((displaystyle-rows
	  (map (lambda (row)
		 (map (lambda (elt)
			(glue-horiz (list "\\displaystyle{ "
					  elt
					  "}")))
		      row))
	       matrix-list))
	 (separated-rows
	  (map (lambda (row) (glue-horiz (interpolate " & " row)))
	       displaystyle-rows))
	 (separated-columns
	  (glue-horiz (interpolate " \\cr \\cr " separated-rows))))
    #;
    (glue-horiz
     (list "\\left\\{ \\matrix{ "
	   separated-columns
	   "} \\right\\}"))
    (glue-horiz
     (list "\\left\\lgroup \\matrix{ "
	   separated-columns
	   "} \\right\\rgroup"))))
	 
(define (tex:unparse-up uptable matrix-list)
  (let* ((displaystyle-rows
	  (map (lambda (row)
		 (map (lambda (elt)
			(glue-horiz (list "\\displaystyle{ "
					  elt
					  "}")))
		      row))
	       matrix-list))
	 (separated-rows
	  (map (lambda (row) (glue-horiz (interpolate " & " row)))
	       displaystyle-rows))
	 (separated-columns
	  (glue-horiz (interpolate " \\cr \\cr " separated-rows))))
    (glue-horiz
     (list left-up-delimiter separated-columns right-up-delimiter))))

(define (tex:unparse-down uptable matrix-list)
  (let* ((displaystyle-rows
	  (map (lambda (row)
		 (map (lambda (elt)
			(glue-horiz (list "\\displaystyle{ "
					  elt
					  "}")))
		      row))
	       matrix-list))
	 (separated-rows
	  (map (lambda (row) (glue-horiz (interpolate " & " row)))
	       displaystyle-rows))
	 (separated-columns
	  (glue-horiz (interpolate " \\cr \\cr " separated-rows))))
    (glue-horiz
     (list left-down-delimiter separated-columns right-down-delimiter))))


;;; Unparsing table for 2D displays

(define 2d:unparse-table
  `((parenthesize ,2d:parenthesize)
    (default ,unparse-default)
    (+ ,unparse-sum)
    ;;need sum (in addition to +) as an internal hook for 
    ;;process-sum
    (sum ,unparse-sum)
    (- ,unparse-difference)
    (* ,2d:unparse-product)
    (negation ,unparse-negation)
    (/ ,2d:unparse-quotient)
    (signed-sum ,unparse-signed-sum)
    (expt ,2d:unparse-expt)
    (,derivative-symbol ,unparse-derivative)
    (derivative ,unparse-derivative)
    (second-derivative ,2d:unparse-second-derivative)
    (nth-derivative ,2d:unparse-nth-derivative)
    (partial-derivative ,2d:unparse-partial-derivative)
    (subscript ,2d:unparse-subscript)
    (superscript ,2d:unparse-superscript)
    (vector ,2d:unparse-vector)
    (row ,2d:unparse-matrix)
    (column ,2d:unparse-matrix)
    (down ,2d:unparse-matrix)
    (up ,2d:unparse-matrix)
    (matrix ,2d:unparse-matrix)
    (dotted ,2d:unparse-dotted)
    (dotdotted ,2d:unparse-dotdotted)
    (primed ,2d:unparse-primed)
    (primeprimed ,2d:unparse-primeprimed)
    ))

(define 2d:symbol-substs
  `((derivative "D")
    ))

(define tex:unparse-table
  `((parenthesize ,tex:parenthesize)
    (default ,unparse-default)
    (+ ,unparse-sum)
    ;;need sum (in addition to +) as an internal hook for 
    ;;process-sum
    (sum ,unparse-sum)
    (- ,unparse-difference)
    (* ,tex:unparse-product)
    (& ,tex:unparse-product)
    (negation ,unparse-negation)
    (/ ,tex:unparse-quotient)
    (signed-sum ,unparse-signed-sum)
    (expt ,tex:unparse-expt)
    (,derivative-symbol ,unparse-derivative)
    (derivative ,unparse-derivative)
    (second-derivative ,tex:unparse-second-derivative)
    (nth-derivative ,tex:unparse-nth-derivative)
    (partial-derivative ,tex:unparse-partial-derivative)
    (subscript ,tex:unparse-subscript)
    (superscript ,tex:unparse-superscript)
    (vector ,tex:unparse-vector)
    (column ,tex:unparse-up)
    (row ,tex:unparse-down)
    (up ,tex:unparse-up)
    (down ,tex:unparse-down)
    (matrix ,tex:unparse-matrix)
    (sqrt ,tex:unparse-sqrt)
    (dotted ,tex:unparse-dotted)
    (dotdotted ,tex:unparse-dotdotted)
    (primed ,tex:unparse-primed)
    (primeprimed ,tex:unparse-primeprimed)
    ))

(define tex:symbol-substs
  (append `((derivative "D")
	    (acos "\\arccos")
	    (asin "\\arcsin")
	    (atan "\\arctan")
	    )
	  (map (lambda (string)
		 (list (string->symbol string)
		       (string-append "\\" string)))
	       '(
		 "alpha" "beta" "gamma" "delta" "epsilon" "zeta" "eta" "theta"
		 "iota" "kappa" "lambda" "mu" "nu" "xi"
		 ;; "omicron" does not appear in tex
		 "pi" "rho" "tau" "upsilon" "phi" "chi" "psi" "omega"
		 "varepsilon" "vartheta" "varpi" "varrho" "varsigma" "varphi"

		 ;;"Alpha" "Beta"
		 "Gamma" "Delta"
		 ;;"Epsilon" "Zeta" "Eta" 
		 "Theta"
		 ;;"Iota" "Kappa"
		 "Lambda"
		 ;;"Mu" "Nu"
		 "Xi"
		 ;;"Omicron"
		 "Pi"
		 ;;"Rho" "Tau"
		 "Upsilon" "Phi"
		 ;;"Chi"
		 "Psi" "Omega"

		 "aleph" "hbar" "nabla" "top" "bot" "mho" "Re" "Im"
		 "infty" "Box" "diamond" "Diamond" "triangle"

		 "sin" "cos" "tan" "cot" "sec" "csc" "log" "exp"
		 ))
	  (map (lambda (string)
		 (list (string->symbol string)
		       (string-append "{\\rm\\ " string " }")))
               '("&meter" "&kilogram" "&second"
                 "&ampere" "&kelvin" "&mole"
                 "&candela" "&radian"

                 "&newton" "&joule" "&coulomb"
                 "&watt" "&volt" "&ohm"
                 "&siemens" "&farad" "&weber"
                 "&henry" "&hertz" "&tesla"
                 "&pascal" "&katal" "&becquerel"
                 "&gray" "&sievert" "&inch"
                 "&pound" "&slug" "&foot"
                 "&mile" "&dyne" "&calorie"
                 "&day" "&year" "&sidereal-year"
                 "&AU" "&arcsec" "&pc"
                 "&ly" "&esu" "&ev"))))

;;; Actual unparsing procedure.  Symbol-substs is a table of special symbols
;;; to be substituted for.  UPtable is an unparsing table.

(define (unparse exp symbol-substs uptable)
  (let ((exp (unparse-special-convert exp))
	(up (lambda (exp) (unparse exp symbol-substs uptable))))
    (cond ((null? exp) "")
	  ((number? exp) (unparse-number exp symbol-substs uptable))
	  ((symbol? exp) (unparse-symbol exp symbol-substs uptable))
	  ((up-maker? exp)
	   ((cadr (assq 'column uptable))
	    uptable
	    (map list (map up (cdr exp)))))
	  ((down-maker? exp)
	   ((cadr (assq 'row uptable))
	    uptable
	    ;; For horizontal format
	    ;;(list (map up (cdr exp)))
	    ;; For vertical format
	    (map list (map up (cdr exp)))))
	  ((vector-maker? exp)
	   ((cadr (assq 'vector uptable))
	    uptable
	    (map up (cdr exp))))
	  ((matrix-by-rows-maker? exp)
	   ((cadr (assq 'matrix uptable))
	    uptable
	    (map (lambda (row) (cdr (map up row)))
		 (cdr exp))))
	  ((eq? (car exp) '+)
	   (process-sum exp symbol-substs uptable))
	  ((symbol? (car exp))
	   (let ((proc (assq (car exp) uptable)))
	     (if proc
		 ((cadr proc) uptable (map up (cdr exp)))
		 ((cadr (assq 'default uptable)) uptable (map up exp)))))
	  (else
	   (let ((proc (assq 'default uptable)))
	     ((cadr proc) uptable (map up exp)))))))

	   
(define (unparse-number n symbol-substs uptable)
  (cond ((and (real? n) (< n 0))
	 (unparse `(- ,(- n)) symbol-substs uptable))
	((and (rational? n) (exact? n) (not (= (denominator n) 1)))
	 (unparse `(/ ,(numerator n) ,(denominator n))
		  symbol-substs
		  uptable))
	(else (number->string n))))


;;; symbols are treated as follows: some symbols are looked for and
;;; substituted specially (e.g., alpha turns into \alpha for tex).
;;; Other symbols have implied subscripts, e.g., m_1.  Symbols that
;;; are more than one character long have a lower binding power than
;;; symbols that are one character long, so that the product of foo
;;; and x, for example, will be written as foo <dot> x in Tex, or the
;;; square of dt will be (dt)^2.

(define (unparse-symbol symbol symbol-substs uptable)
  (let ((s (assq symbol symbol-substs)))
    (if s
	(cadr s)
	(let ((string (symbol->string symbol)))
	  (split-at-underscore-or-caret
	   string
	   (lambda (before at after)
	     (if (not before)		;no underscore or caret in symbol
		 (unparse-string string symbol-substs uptable)
		 (unparse `(,at ,(string->symbol before) ,(string->symbol after))
			  symbol-substs
			  uptable))))))))


(define dotdot-string "dotdot")
(define dotdot-string-length (string-length dotdot-string))

(define dot-string "dot")
(define dot-string-length (string-length dot-string))

(define primeprime-string "primeprime")
(define primeprime-string-length (string-length primeprime-string))

(define prime-string "prime")
(define prime-string-length (string-length prime-string))

#|
(define (unparse-string string symbol-substs uptable)
  (if (= (string-length string) 1)
      string
      (cond ((string-search-forward dotdot-string string)
	     => (lambda (n)
		  (if (= (+ n dotdot-string-length) ;terminal dotdot
			 (string-length string))
		      (unparse `(dotdotted
				 ,(string->symbol (string-head string n)))
			       symbol-substs uptable)
		      (make-box-with-bp 190 string))))
            ((string-search-forward dot-string string)
	     => (lambda (n)
		  (if (= (+ n dot-string-length) ;terminal dot
			 (string-length string))
		      (unparse `(dotted
				 ,(string->symbol (string-head string n)))
			       symbol-substs uptable)
		      (make-box-with-bp 190 string))))
	    ((string-search-forward primeprime-string string)
	     => (lambda (n)
		  (if (= (+ n primeprime-string-length) ;terminal primeprime
			 (string-length string))
		      (unparse `(primeprimed
				 ,(string->symbol (string-head string n)))
			       symbol-substs uptable)
		      (make-box-with-bp 190 string))))
            ((string-search-forward prime-string string)
	     => (lambda (n)
		  (if (= (+ n prime-string-length) ;terminal prime
			 (string-length string))
		      (unparse `(primed
				 ,(string->symbol (string-head string n)))
			       symbol-substs uptable)
		      (make-box-with-bp 190 string))))
	    (else
	     (make-box-with-bp 190 string)))))
|#

(define (unparse-string string symbol-substs uptable)
  (define (for-terminal special-string special-string-length special-symbol)
    (let ((n (string-search-forward special-string string)))
      (if (and n (= (+ n special-string-length) (string-length string)))
	  (unparse `(,special-symbol
		     ,(string->symbol (string-head string n)))
		   symbol-substs uptable)
	  #f)))
  (cond ((= (string-length string) 1) string)
	((for-terminal dotdot-string     dotdot-string-length     'dotdotted))
	((for-terminal dot-string        dot-string-length        'dotted))
	((for-terminal primeprime-string primeprime-string-length 'primeprimed))
	((for-terminal prime-string      prime-string-length      'primed))
	(else (make-box-with-bp 190 string))))

#|
(define (split-at-underscore-or-caret string cont)
  ;;cont = (lambda (before after) ...)
  (let ((index (string-find-next-char string #\_)))
    (if (not index)
	(cont #f #f)
	(cont (string-head string index)
	      (string-tail string (+ index 1))))))
|#

(define (split-at-underscore-or-caret string cont)
  ;;cont = (lambda (before at after) ...)
  (let ((index (string-find-next-char-in-set string (char-set #\^ #\_))))
    (if (not index)
	(cont #f #f #f)
	(cont (string-head string index)
	      (if (char=? (string-ref string index) #\^)
		  'superscript
		  'subscript)
	      (string-tail string (+ index 1))))))



;;; Some forms have funny rules.  For example, ((expt A 2) f) would
;;;             2
;;; unparse to A (f), but ((expt derivative 2) f) is traditionally
;;;                             2
;;; written without parens, as D f.  Partial derivatives are also
;;; funny, since we will generally convert them to subscripts.  The
;;; following procedure catches these special forms.  If there were
;;; more of these, we would use a pattern matcher here.


(define (unparse-special-convert exp)
  (cond ((and
	  ;;((expt derivative n) f) --> (nth-derivative f n)
	  (pair? exp)
	  (pair? (car exp))
	  (= (length exp) 2)
	  (= (length (car exp)) 3)
	  (eq? (caar exp) 'expt)
	  (or (eq? (cadar exp) 'derivative)
	      (eq? (cadar exp) derivative-symbol)))
	 (let ((exponent (list-ref (car exp) 2))
	       (base (cadr exp)))
	   (if (eq? exponent 2)
	       `(second-derivative ,base)
	       `(nth-derivative ,base ,exponent))))
	((and
	  ;;((partial x) f) --> (partial-derivative f x)
	  (pair? exp)
	  (pair? (car exp))
	  (= (length exp) 2)
	  (eq? (caar exp) 'partial))
	 `(partial-derivative ,(cadr exp) ,@(cdr (car exp))))
	((and
	  ;;(- x) --> (negation x)
	  (pair? exp)
	  (eq? (car exp) '-)
	  (eq? (length exp) 2))
	 `(negation ,(cadr exp)))
	(else exp)))

;;; for a sum, find all terms of the form (* -1 .....) and make them
;;; appear without the -1 and with a negative sign in the sum

(define (process-sum exp symbol-substs uptable)
  (let ((terms (cdr exp)))
    (cond ((null? terms)
	   (unparse 0 symbol-substs uptable))
	  ((null? (cdr terms))
	   (unparse (car terms) symbol-substs uptable))
	  (else
	   (let ((signed-terms
		  (map (lambda (term)
			 (cond ((and (pair? term) (eq? (car term) '*))
				(let ((first-factor (cadr term)))
				  (if (and (real? first-factor) (negative? first-factor))
				      (if (and (= first-factor -1) (not (null? (cddr term))))
					  (list '- (cons '* (cddr term)))
					  (list '- (cons '* (cons (- first-factor) (cddr term)))))
				      (list '+ term))))
			       ((and (pair? term) (eq? (car term) '/))
				(let ((numer (cadr term)))
				  (cond ((and (real? numer) (negative? numer))
					 (list '- (cons '/ (cons (- numer) (cddr term)))))
					((and (pair? numer) (eq? (car numer) '*))
					 (let ((first-factor (cadr numer)))
					   (if (and (real? first-factor) (negative? first-factor))
					       (if (and (= first-factor -1) (not (null? (cddr numer))))
						   (list '-
							 (cons '/
							       (cons (cons '* (cddr numer))
								     (cddr term))))
						   (list '-
							 (cons '/
							       (cons (cons '*
									   (cons (- first-factor)
										 (cddr numer)))
								     (cddr term)))))
					       (list '+ term))))
					(else
					 (list '+ term)))))
			       (else
				(list '+ term))))
		       terms)))
	     (let ((processed-terms
		    (map (lambda (exp) (unparse exp symbol-substs uptable))
			 (map cadr signed-terms))))
	       ((cadr (assq 'signed-sum uptable))
		uptable
		(map car signed-terms)
		processed-terms
		)))))))

(set! internal-show-expression
  (lambda (exp)
    (set! last-tex-string-generated (expression->tex-string exp))
    (let ((name (graphics-type-name (graphics-type #f))))
      (if (and (eq? name 'X) enable-tex-display)
	  (begin (display-in-screen-window last-tex-string-generated)
		 (newline)
		 (newline)
		 ;;  (display tex-string)
		 ;;  (newline)
		 ;;  (newline)
		 )
	  (2d-show-expression exp)))))


(set! 2d-show-expression
      (lambda (exp)
	(2d-display-box
	 (unparse exp 2d:symbol-substs 2d:unparse-table))))


(set! expression->tex-string
      (lambda (exp)
	(let* ((one-line-box (unparse exp tex:symbol-substs tex:unparse-table))
	       (tex-string
		(with-output-to-string
		  (lambda ()
		    (for-each display
			      (line-elements (car (box-lines one-line-box))))))))
	  (string-append "\\boxit{ " "$$" tex-string "$$" "}"))))

#|
;;; Beal's folly.
(set! expression->tex-string
      (lambda (exp)
	(let* ((one-line-box (unparse exp tex:symbol-substs tex:unparse-table))
	       (tex-string
		(with-output-to-string
		  (lambda ()
		    (for-each display
			      (line-elements (car (box-lines one-line-box))))))))
	  (string-append "\\boxit{ " "$" tex-string "$" "}"))))
|#

(set! display-tex-string display-in-screen-window)

)       ;end let()


;;;(define left-up-delimiter "\\left \\lceil \\matrix{ ")
;;;(define right-up-delimiter "} \\right \\rceil")
;;;(define left-down-delimiter "\\left \\lfloor \\matrix{ ")
;;;(define right-down-delimiter "} \\right \\rfloor")


(define left-up-delimiter "\\left( \\matrix{ ")
(define right-up-delimiter "} \\right)")

(define left-down-delimiter "\\left[ \\matrix{ ")
(define right-down-delimiter "} \\right]")


#|

(define test
  '(/
    (+ alpha (/ ((derivative f) b) (+ alpha beta)))
    (+ (/ (+ x y) 2) (expt (/ (+ a c (/ 2 x)) (* d e)) (+ f (/ g h))))))



|#
