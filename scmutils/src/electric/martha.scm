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

;;;;          Admittance and Impedance Calculator

;;; Ripoff of Penfield's Martha, by Gerald Jay Sussman
;;; To be loaded into an NSCMUTILS.  

;;;   A branch is represented as an impedance or an admittance
;;; tagged with its type.

(define (contents branch) (cadr branch))
(define (tag type-tag contents) (list type-tag contents))

(define (make-impedance contents)
  (tag 'impedance contents))
(define (make-admittance contents)
  (tag 'admittance contents))

(define (impedance? x)
  (and (pair? x) (eq? (car x) 'impedance)))
(define (admittance? x)
  (and (pair? x) (eq? (car x) 'admittance)))


;;; Conversions

(define (admittance branch)
  (coerce-to-function
   (cond ((admittance? branch) (contents branch))
	 ((impedance? branch) (/ 1 (contents branch)))
	 ((1-port? branch)
	  (/ (vector-ref (contents branch) 1)
	     (vector-ref (contents branch) 0)))
	 (else
	  (error "Unknown branch type -- ADMITTANCE"
		 branch)))))

(define (impedance branch)
  (coerce-to-function
   (cond ((admittance? branch) (/ 1 (contents branch)))
	 ((impedance? branch) (contents branch))
	 ((1-port? branch)
	  (/ (vector-ref (contents branch) 0)
	     (vector-ref (contents branch) 1)))
	 (else
	  (error "Unknown branch type -- IMPEDANCE"
		 branch)))))

;;; Useful:
(define (zerofun s) 0)
(define (onefun s) 1)
(define (monefun s) -1)

;;; Branch Impedances

(define (resistor R)			;R
  (make-impedance (lambda (s) R)))

(define (capacitor C)			;C
  (make-impedance (lambda (s) (/ 1 (* C s)))))

(define (inductor L)			;L
  (make-impedance (lambda (s) (* L s))))


(define (short)
  (make-impedance zerofun))

(define (open)
  (make-admittance zerofun))


;;; Wiring functions for combining branches 

(define (series b1 b2)			;S
  (make-impedance
   (+ (impedance b1) (impedance b2))))

(define (parallel b1 b2)		;P
  (make-admittance
   (+ (admittance b1) (admittance b2))))


;;;; A 1-port object is a vector [V I]

(define (make-1-port vi)
  (tag '1-port vi))

(define (branch->1-port-vector branch)
  (cond ((admittance? branch)
	 (vector onefun (contents branch)))
	((impedance? branch)
	 (vector (contents branch) onefun))
	(else
	 (error "Unknown branch type -- MAKE-1-PORT"
		branch))))

(define (1-port? x)
  (and (pair? x)
       (eq? (car x) '1-port)))

(define (1-port-vector x)
  (cond ((1-port? x) (contents x))
	(else (branch->1-port-vector x))))

;;;; 2-port matrices

;;; V1 = Z11*I1 + Z12*I2;  V2 = Z21*I1 + Z22*I2

(define (make-z-matrix z11 z12 z21 z22)
  (tag 'z-matrix
       (array->matrix
	(vector (vector z11 z12)
		(vector z21 z22)))))


;;; I1 = Y11*V1 + Y12*V2;  I2 = Y21*I1 + Y22*I2

(define (make-y-matrix y11 y12 y21 y22)
  (tag 'y-matrix
       (array->matrix
	(vector (vector y11 y12)
		(vector y21 y22)))))


;;; V1 = H11*I1 + H12*V2;  I2 = H21*I1 + H22*V2

(define (make-h-matrix h11 h12 h21 h22)
  (tag 'h-matrix
       (array->matrix
	(vector (vector h11 h12)
		(vector h21 h22)))))


;;; I1 = G11*V1 + G12*I2;  V2 = G21*V1 + G22*I2

(define (make-g-matrix g11 g12 g21 g22)
  (tag 'g-matrix
       (array->matrix
	(vector (vector g11 g12)
		(vector g21 g22)))))


;;; V1 = A*V2 - B*I2;  I1 = C*V2 - D*I2

(define (make-abcd-matrix A B C D)
  (tag 'abcd-matrix
       (array->matrix
	(vector (vector A B)
		(vector C D)))))

(define (z-matrix? x)
  (and (pair? x) (eq? (car x) 'z-matrix)))

(define (y-matrix? x)
  (and (pair? x) (eq? (car x) 'y-matrix)))

(define (h-matrix? x)
  (and (pair? x) (eq? (car x) 'h-matrix)))

(define (g-matrix? x)
  (and (pair? x) (eq? (car x) 'g-matrix)))

(define (abcd-matrix? x)
  (and (pair? x) (eq? (car x) 'abcd-matrix)))


;;; Conversions

(define (y->z y-matrix)
  (let ((y11 (matrix-ref y-matrix 0 0))
	(y12 (matrix-ref y-matrix 0 1))
	(y21 (matrix-ref y-matrix 1 0))
	(y22 (matrix-ref y-matrix 1 1)))
    (let ((d (- (* y11 y22) (* y12 y21))))
      (let ((z11 (/ y22 d))      (z12 (/ (- y12) d))
	    (z21 (/ (- y21) d))  (z22 (/ y11 d)))
	(array->matrix
	 (vector (vector z11 z12)
		 (vector z21 z22)))))))

(define (z->y z-matrix)
  (let ((z11 (matrix-ref z-matrix 0 0))
	(z12 (matrix-ref z-matrix 0 1))
	(z21 (matrix-ref z-matrix 1 0))
	(z22 (matrix-ref z-matrix 1 1)))
    (let ((d (- (* z11 z22) (* z12 z21))))
      (let ((y11 (/ z22 d))      (y12 (/ (- z12) d))
	    (y21 (/ (- z21) d))  (y22 (/ z11 d)))
	(array->matrix
	 (vector (vector y11 y12)
		 (vector y21 y22)))))))

#|
(define general-y-matrix
  (matrix-by-rows (list 'y11 'y12)
		  (list 'y21 'y22)))
(print-expression (z->y (y->z general-y-matrix)))
(matrix-by-rows (list y11 y12) (list y21 y22))
|#

(define (z->abcd z-matrix)
  (let ((z11 (matrix-ref z-matrix 0 0))
	(z12 (matrix-ref z-matrix 0 1))
	(z21 (matrix-ref z-matrix 1 0))
	(z22 (matrix-ref z-matrix 1 1)))
    (let ((a (/ z11 z21))   (b (/ (- (* z11 z22) (* z12 z21)) z21))
	  (c (/ 1 z21))     (d (/ z22 z21)))
      (array->matrix
       (vector (vector a b)
	       (vector c d))))))

(define (y->abcd y-matrix)
  (let ((y11 (matrix-ref y-matrix 0 0))
	(y12 (matrix-ref y-matrix 0 1))
	(y21 (matrix-ref y-matrix 1 0))
	(y22 (matrix-ref y-matrix 1 1)))
    (let ((a (/ (- y22) y21))                     (b (/ -1 y21))
	  (c (/ (- (* y12 y21) (* y11 y22)) y21)) (d (/ (- y11) y21)))
      (array->matrix
       (vector (vector a b)
	       (vector c d))))))

#|
(print-expression
 (- (z->abcd (y->z general-y-matrix))
    (y->abcd general-y-matrix)))
(matrix-by-rows (list 0 0) (list 0 0))
|#


(define (abcd->z abcd-matrix)
  (let ((a (matrix-ref abcd-matrix 0 0))
	(b (matrix-ref abcd-matrix 0 1))
	(c (matrix-ref abcd-matrix 1 0))
	(d (matrix-ref abcd-matrix 1 1)))
    (let ((z11 (/ a c))   (z12 (- (/ (* a d) c) b))
	  (z21 (/ 1 c))   (z22 (/ d c)))
      (array->matrix
       (vector (vector z11 z12)
	       (vector z21 z22))))))

#|
(define general-z-matrix
  (matrix-by-rows (list 'z11 'z12)
		  (list 'z21 'z22)))

(print-expression
 (abcd->z (z->abcd general-z-matrix)))
(matrix-by-rows (list z11 z12) (list z21 z22))
|#

(define (abcd->y abcd-matrix)
  (let ((a (matrix-ref abcd-matrix 0 0))
	(b (matrix-ref abcd-matrix 0 1))
	(c (matrix-ref abcd-matrix 1 0))
	(d (matrix-ref abcd-matrix 1 1)))
    (let ((y11 (/ d b))   (y12 (- c (/ (* a d) b)))
	  (y21 (/ -1 b))   (y22 (/ a b)))
      (array->matrix
       (vector (vector y11 y12)
	       (vector y21 y22))))))

#|
(print-expression (abcd->y (y->abcd general-y-matrix)))
(matrix-by-rows (list y11 y12) (list y21 y22))
|#

(define (h->z h-matrix)
  (let ((h11 (matrix-ref h-matrix 0 0))
	(h12 (matrix-ref h-matrix 0 1))
	(h21 (matrix-ref h-matrix 1 0))
	(h22 (matrix-ref h-matrix 1 1)))
    (let ((z11 (/ (- (* h11 h22) (* h12 h21)) h22))
	  (z12 (/ h12 h22))
	  (z21 (/ (- h21) h22))
	  (z22 (/ 1 h22)))
      (array->matrix
       (vector (vector z11 z12)
	       (vector z21 z22))))))

(define (h->y h-matrix)
  (let ((h11 (matrix-ref h-matrix 0 0))
	(h12 (matrix-ref h-matrix 0 1))
	(h21 (matrix-ref h-matrix 1 0))
	(h22 (matrix-ref h-matrix 1 1)))
    (let ((y11 (/ 1 h11))
	  (y12 (/ (- h12) h11))
	  (y21 (/ h21 h11))
	  (y22 (/ (- (* h11 h22) (* h12 h21)) h11)))
      (array->matrix
       (vector (vector y11 y12)
	       (vector y21 y22))))))
#|
(define general-h-matrix
  (matrix-by-rows (list 'h11 'h12)
		  (list 'h21 'h22)))

(print-expression
  (- (h->z general-h-matrix)
     (y->z (h->y general-h-matrix))))
(matrix-by-rows (list 0 0) (list 0 0))
|#

(define (z->h z-matrix)
  (let ((z11 (matrix-ref z-matrix 0 0))
	(z12 (matrix-ref z-matrix 0 1))
	(z21 (matrix-ref z-matrix 1 0))
	(z22 (matrix-ref z-matrix 1 1)))
    (let ((d (- (* z11 z22) (* z12 z21))))
      (let ((h11 (/ d z22))        (h12 (/ z12 z22))
	    (h21 (/ (- z21) z22))  (h22 (/ 1 z22)))
	(array->matrix
	 (vector (vector h11 h12)
		 (vector h21 h22)))))))

#|
(print-expression (z->h (h->z general-h-matrix)))
(matrix-by-rows (list h11 h12) (list h21 h22))
|#

(define (y->h y-matrix)
  (let ((y11 (matrix-ref y-matrix 0 0))
	(y12 (matrix-ref y-matrix 0 1))
	(y21 (matrix-ref y-matrix 1 0))
	(y22 (matrix-ref y-matrix 1 1)))
    (let ((d (- (* y11 y22) (* y12 y21))))
      (let ((h11 (/ 1 y11))    (h12 (/ (- y12) y11))
	    (h21 (/ y21 y11))  (h22 (/ d y11)))
	(array->matrix
	 (vector (vector h11 h12)
		 (vector h21 h22)))))))

#|
(print-expression (y->h (h->y general-h-matrix)))
(matrix-by-rows (list h11 h12) (list h21 h22))
|#

(define (h->abcd h-matrix)
  (let ((h11 (matrix-ref h-matrix 0 0))
	(h12 (matrix-ref h-matrix 0 1))
	(h21 (matrix-ref h-matrix 1 0))
	(h22 (matrix-ref h-matrix 1 1)))
    (let ((a (/ (- (* h12 h21) (* h11 h22)) h21))
	  (b (/ (- h11) h21))
	  (c (/ (- h22) h21))
	  (d (/ -1 h21)))
      (array->matrix
       (vector (vector a b)
	       (vector c d))))))

(define (abcd->h abcd-matrix)
  (let ((a (matrix-ref abcd-matrix 0 0))
	(b (matrix-ref abcd-matrix 0 1))
	(c (matrix-ref abcd-matrix 1 0))
	(d (matrix-ref abcd-matrix 1 1)))
    (let ((h11 (/ b d))  (h12 (/ (- (* a d) (* b c)) d))
	  (h21 (/ -1 d)) (h22 (/ c d)))
      (array->matrix
       (vector (vector h11 h12)
	       (vector h21 h22))))))

#|
(print-expression (abcd->h (h->abcd general-h-matrix)))
(matrix-by-rows (list h11 h12) (list h21 h22))
|#


;;; Temporary kludge

(define g->h m:invert)

(define h->g m:invert)

(define g->z (compose h->z g->h))

(define g->y (compose h->y g->h))

(define g->abcd (compose h->abcd g->h))

(define z->g (compose h->g z->h))

(define y->g (compose h->g y->h))

(define abcd->g (compose h->g abcd->h))

(define (z-matrix 2-port)
  (cond ((abcd-matrix? 2-port) (abcd->z (contents 2-port)))
	((z-matrix? 2-port) (contents 2-port))
	((y-matrix? 2-port) (y->z (contents 2-port)))
	((h-matrix? 2-port) (h->z (contents 2-port)))
	((g-matrix? 2-port) (g->z (contents 2-port)))
	(else
	 (error "Unknown 2-port type -- Z-matrix" 2-port))))

(define (y-matrix 2-port)
  (cond ((abcd-matrix? 2-port) (abcd->y (contents 2-port)))
	((z-matrix? 2-port) (z->y (contents 2-port)))
	((y-matrix? 2-port) (contents 2-port))
	((h-matrix? 2-port) (h->y (contents 2-port)))
	((g-matrix? 2-port) (g->y (contents 2-port)))
	(else
	 (error "Unknown 2-port type -- Y-matrix" 2-port))))

(define (h-matrix 2-port)
  (cond ((abcd-matrix? 2-port) (abcd->h (contents 2-port)))
	((z-matrix? 2-port) (z->h (contents 2-port)))
	((y-matrix? 2-port) (y->h (contents 2-port)))
	((h-matrix? 2-port) (contents 2-port))
	((g-matrix? 2-port) (g->h (contents 2-port)))
	(else
	 (error "Unknown 2-port type -- H-matrix" 2-port))))

(define (g-matrix 2-port)
  (cond ((abcd-matrix? 2-port) (abcd->g (contents 2-port)))
	((z-matrix? 2-port) (z->g (contents 2-port)))
	((y-matrix? 2-port) (y->g (contents 2-port)))
	((h-matrix? 2-port) (h->g (contents 2-port)))
	((g-matrix? 2-port) (contents 2-port))
	(else
	 (error "Unknown 2-port type -- G-matrix" 2-port))))

(define (abcd-matrix 2-port)
  (cond ((abcd-matrix? 2-port) (contents 2-port))
	((z-matrix? 2-port) (z->abcd (contents 2-port)))
	((y-matrix? 2-port) (y->abcd (contents 2-port)))
	((h-matrix? 2-port) (h->abcd (contents 2-port)))
	((g-matrix? 2-port) (g->abcd (contents 2-port)))
	(else
	 (error "Unknown 2-port type -- ABCD-matrix" 2-port))))

;;; Wiring functions for 2 ports

(define (shunt->2-port branch)		;WP
  (let ((y (admittance branch)))
    (make-abcd-matrix onefun zerofun y onefun)))

(define (series->2-port branch)		;WS
  (let ((z (impedance branch)))
    (make-abcd-matrix onefun z zerofun onefun)))

(define (cascade 2-port-1 2-port-2)	;WC
  (tag 'abcd-matrix
       (matrix*matrix (abcd-matrix 2-port-1)
		      (abcd-matrix 2-port-2))))


;;; Terminate takes a 2-port and a 1-port and makes a 1-port

(define (terminate 2-port 1-port)	;WT
  (make-1-port
   (matrix*vector (abcd-matrix 2-port)
		  (1-port-vector 1-port))))

(define (terminate-open 2-port)		;WTO
  (make-impedance
   (matrix-ref (z-matrix 2-port) 0 0)))

(define (terminate-short 2-port)	;WTS
  (make-admittance
   (matrix-ref (y-matrix 2-port) 0 0)))


(define (parallel-parallel tp-1 tp-2)	;WPP
  (tag 'y-matrix
       (matrix+matrix (y-matrix tp-1)
		      (y-matrix tp-2))))

(define (series-series tp-1 tp-2)	;WSS
  (tag 'z-matrix
       (matrix+matrix (z-matrix tp-1)
		      (z-matrix tp-2))))

(define (series-parallel tp-1 tp-2)	;WSP
  (tag 'h-matrix
       (matrix+matrix (h-matrix tp-1)
		      (h-matrix tp-2))))

(define (parallel-series tp1 tp2)	;WPS
  (tag 'g-matrix
   (matrix+matrix (g-matrix tp1)
		  (g-matrix tp2))))

(define (flip-i/o tp)			;WN
  (cond ((z-matrix? tp)
	 (tag 'z-matrix (matrix-rot (contents tp))))
	((y-matrix? tp)
	 (tag 'y-matrix (matrix-rot (contents tp))))
	((h-matrix? tp)
	 (tag 'h-matrix (h-inv (contents tp))))
	((abcd-matrix? tp)
	 (tag 'abcd-matrix (abcd-inv (contents tp))))
	(else
	 (error "Unknown 2-port type -- FLIP-I/O"
		tp))))

(define (matrix-rot m)
  (let ((m11 (matrix-ref m 0 0))
	(m12 (matrix-ref m 0 1))
	(m21 (matrix-ref m 1 0))
	(m22 (matrix-ref m 1 1)))
    (array->matrix
     (vector (vector m22 m21)
	     (vector m12 m11)))))

(define (h-inv h)
  (let ((h11 (matrix-ref h 0 0))
	(h12 (matrix-ref h 0 1))
	(h21 (matrix-ref h 1 0))
	(h22 (matrix-ref h 1 1)))
    (let ((det (- (* h11 h22) (* h21 h12))))
      (let ((m11 (/ h22 det))
	    (m12 (/ (- h12) det))
	    (m21 (/ (- h21) det))
	    (m22 (/ h11 det)))
	(array->matrix
	 (vector (vector m22 m21)
		 (vector m12 m11)))))))

(define (abcd-inv abcd-matrix)
  (let ((a (matrix-ref abcd-matrix 0 0))
	(b (matrix-ref abcd-matrix 0 1))
	(c (matrix-ref abcd-matrix 1 0))
	(d (matrix-ref abcd-matrix 1 1)))
    (let ((det (- (* a d) (* b c))))
      (array->matrix
       (vector (vector (/ d det) (/ (- b) det))
	       (vector (/ (- c) det) (/ a det)))))))

;;; Important 2-port formation procedures

;;; From parameters

(define (make-2-port-from-z z11 z12 z21 z22)
  (make-z-matrix (lambda (s) z11) (lambda (s) z12)
		 (lambda (s) z21) (lambda (s) z22)))

(define (make-2-port-from-y y11 y12 y21 y22)
  (make-y-matrix (lambda (s) y11) (lambda (s) y12)
		 (lambda (s) y21) (lambda (s) y22)))

(define (make-2-port-from-h h11 h12 h21 h22)
  (make-h-matrix (lambda (s) h11) (lambda (s) h12)
		 (lambda (s) h21) (lambda (s) h22)))

(define (make-2-port-from-abcd A B C D)
  (make-abcd-matrix (lambda (s) A) (lambda (s) B)
		    (lambda (s) C) (lambda (s) D)))


;;; From branches

(define (t-network left mid right)
  (let ((zleft (impedance left))
	(zmid (impedance mid))
	(zright (impedance right)))
    (make-z-matrix (+ zleft zmid) zmid
		   zmid (+ zright zmid))))

(define (pi-network left mid right)
  (let ((yleft (admittance left))
	(ymid (admittance mid))
	(yright (admittance right)))
    (make-y-matrix (+ yleft ymid) (- ymid)
		   (- ymid) (+ yright ymid))))

(define (l-network across down)
  (cascade (series->2-port across)
	   (shunt->2-port down)))



(define (connect-thru)			;WTHRU
  (make-abcd-matrix onefun zerofun zerofun onefun))

(define (polarity-reverse)		;WR
  (make-abcd-matrix monefun 0 0 monefun))

(define (mutual-inductor L1 M L2)	;L'
  (make-z-matrix (lambda (s) (* L1 s))
		 (lambda (s) (* M s))
		 (lambda (s) (* M s))
		 (lambda (s) (* L2 s))))

(define (ideal-transformer N)		;IT
  (make-abcd-matrix (constant N) (constnt 0) zerofun (constant (/ 1 N))))

(define (vcvs A)
  (make-abcd-matrix (/ 1 A) zerofun zerofun zerofun))

(define (ccvs rm)
  (make-z-matrix zerofun zerofun rm zerofun))

(define (vccs gm)
  (make-y-matrix zerofun zerofun gm zerofun0))

(define (cccs A)
  (make-h-matrix zerofun zerofun A zerofun))


(define (opamp A #!optional Rout Rin principal-pole)	;OPAMP
  (let ((o1
	 (if (default-object? principal-pole)
	     (vcvs A)
	     (make-abcd-matrix
	      (lambda (s)
		(/ (+ s principal-pole) A))
	      zerofun zerofun zerofun))))
    (if (default-object? Rout)
	o1
	(let ((o2 (cascade o1 (series->2-port (resistor Rout)))))
	  (if (default-object? Rin)
	      o2
	      (cascade (shunt->2-port (resistor Rin))
		       o2))))))

(define (fet Cgs Cgd gm)		;FET
  (make-y-matrix (lambda (s) (* (+ Cgs Cgd) s))
		 (lambda (s) (* -1 Cgd s))
		 (lambda (s) (- gm (* Cgd s)))
		 (lambda (s) (* Cgd s))))

(define (bjt Rx Rpi Cpi Cmu gm)		;HYBRIDPI
  (cascade (series->2-port (resistor Rx))
	   (cascade (shunt->2-port (resistor Rpi))
		    (fet Cpi Cmu gm))))

;;; Transmission lines

(define Z_0				; impedance of free space
  (sqrt (/ :mu_0 :epsilon_0)))

;;; k = dielectric constant of insulator

;;; Rs = surface resistivity of conductor
(define    Ag:Rs 2.52e-7)
(define    Cu:Rs 2.61e-7)
(define    Al:Rs 3.26e-7)
(define Brass:Rs 5.01e-7)

;;; Rdc = bulk resistivity of conductor
(define    Ag:Rdc 1.629e-8)
(define    Cu:Rdc 1.724e-8)		;Ohm-m
(define    Al:Rdc 2.828e-8)
(define Brass:Rdc 7.00e-8)


;;; f = frequency

;;; Coax

;;; ro = radius of outer conductor; 
;;; ri = radius of inner conductor

(define (coax-L ro ri)			; inductance/meter
  (* (/ :mu_0 :2pi) (log (/ ro ri))))

(define (coax-C ro ri #!optional k)	; capacitance/meter
  (set! k (if (default-object? k) 1 k))
  (/ (* :2pi k :epsilon_0)
     (log (/ ro ri))))

(define (coax-R ro ri f #!optional Rs Rdc) ; resistance/meter
  (set! Rs (if (default-object? Rs) Cu:Rs Rs))
  (set! Rdc (if (default-object? Rdc) 0 Rdc))	
  (+ (/ Rdc (* :pi (square ri)))	; Neglects shield resistance.
     (* (/ 1 :2pi) Rs (sqrt f)
	(+ (/ 1 ro) (/ 1 ri)))))
  
(define (coax-Z_0 ro ri #!optional k)
  (set! k (if (default-object? k) 1 k))
  (* (/ 1 (* :2pi (sqrt k))) Z_0 (log (/ ro ri))))

(define (coax length ro ri #!optional k Rs Rdc)
  (set! k (if (default-object? k) 1 k))
  (set! Rs (if (default-object? Rs) Cu:Rs Rs))
  (set! Rdc (if (default-object? Rdc) 0 Rdc))	
  (define (Z0 s)
    (sqrt (/ (+ (coax-R ro ri (/ (imag-part s) :2pi) Rs Rdc)
		(* (coax-L ro ri) s))
	     (* (coax-C ro ri k) s))))
  (define (gamma s)
    (sqrt (* (+ (coax-R ro ri (/ (imag-part s) :2pi) Rs Rdc)
		(* (coax-L ro ri) s))
	     (* (coax-C ro ri k) s))))
  (make-abcd-matrix
   (lambda (s)
     (cosh (* (gamma s) length)))
   (lambda (s)
     (* (Z0 s) (sinh (* (gamma s) length))))
   (lambda (s)
     (/ (sinh (* (gamma s) length)) (Z0 s)))
   (lambda (s)
     (cosh (* (gamma s) length)))))

#|
(define m/inch 0.0254)

;;; Air-filled copper coax (Martha 1973 addendum, p. 59)

(pp
 (map (lambda (f)
	(dBw
	 (* 4
	    (square
	     ((magnitude
	       (voltage-transfer-ratio
		(cascade (series->2-port (resistor 50))
			 (cascade (coax (* 1200 m/inch)
					(* 0.2805 m/inch)
					(* 0.122 m/inch))
				  (shunt->2-port (resistor 50))))))
	      (* :2pi +i f 1e9))))))	;GHz
      '(0.5 1.0 1.5 2.0 2.5 3.0)))
(-1.140579449589373
 -1.613016234061262
 -1.9755274686625077
 -2.2811481982734976
 -2.5503994449178933
 -2.7938176030040824)
|#

;;; Twin Lead

;;; b = spacing of conductors, center to center; 
;;; a = radius of conductors

(define (twin-lead-L b a)		; inductance/meter
  (* (/ :mu_0 :pi) (log (/ b (* 2 a)))))

(define (twin-lead-C b a #!optional k)	; capacitance/meter
  (set! k (if (default-object? k) 1 k))
  (/ (* :pi k :epsilon_0)
     (log (/ b (* 2 a)))))

(define (twin-lead-R a f #!optional Rs Rdc)	; resistance/meter
  (set! Rs (if (default-object? Rs) Cu:Rs Rs))
  (set! Rdc (if (default-object? Rdc) Cu:Rdc Rdc))	
  (+ (/ (* 2 Rdc) (* :pi (square a)))
     (* (/ 1 :2pi) Rs (sqrt f) (/ 2 a))))

(define (twin-lead-Z_0 b a #!optional k)
  (set! k (if (default-object? k) 1 k))
  (* (/ 1 (* :pi (sqrt k))) Z_0 (log (/ b (* 2 a)))))

(define (twin-lead length b a #!optional k Rs Rdc)
  (set! k (if (default-object? k) 1 k))
  (set! Rs (if (default-object? Rs) Cu:Rs Rs))
  (set! Rdc (if (default-object? Rdc) Cu:Rdc Rdc))	
  (define (Z0 s)
    (sqrt (/ (+ (twin-lead-R a (/ (imag-part s) :2pi) Rs Rdc)
		(* (twin-lead-L b a) s))
	     (* (twin-lead-C b a k) s))))
  (define (gamma s)
    (sqrt (* (+ (twin-lead-R a (/ (imag-part s) :2pi) Rs Rdc)
		(* (twin-lead-L b a) s))
	     (* (twin-lead-C b a k) s))))
  (make-abcd-matrix
   (lambda (s)
     (cosh (* (gamma s) length)))
   (lambda (s)
     (* (Z0 s) (sinh (* (gamma s) length))))
   (lambda (s)
     (/ (sinh (* (gamma s) length)) (Z0 s)))
   (lambda (s)
     (cosh (* (gamma s) length)))))

;;; A few response functions

;;; Input Impedance Z11 = V1/I1 | I2=0

(define (Z11 tp)
  (matrix-ref (z-matrix tp) 0 0))

(define (Z21 tp)
  (matrix-ref (z-matrix tp) 1 0))

(define (Z12 to)
  (matrix-ref (z-matrix tp) 0 1))

;;; Output Impedance Z22 = V2/I2 | I1=0

(define (Z22 tp)
  (matrix-ref (z-matrix tp) 1 1))


;;; The VTR = V2/V1 | I2=0

(define (voltage-transfer-ratio tp)
  (/ 1 (matrix-ref (abcd-matrix tp) 0 0)))


;;; The CTR = I2/I1 | V2=0

(define (current-transfer-ratio tp)
  (/ 1 (matrix-ref (abcd-matrix tp) 1 1)))


;;; The Transconductance Gm = I2/V1 | V2=0

(define (transconductance tp)
  (/ 1 (matrix-ref (abcd-matrix tp) 0 1)))


;;; The Transresistance Rm = V2/I1 | I2=0

(define (transresistance tp)
  (/ 1 (matrix-ref (abcd-matrix tp) 1 0)))


;;; Output format functions:

(define (dBv V1/V2)
  (* 20 (log10 V1/V2)))

(define (dBw P1/P2)
  (* 10 (log10 P1/P2)))



#|
(show-expression
  ((matrix-ref (z-matrix
		(cascade (shunt->2-port (resistor 'R_1))
			 (cascade (t-network (inductor 'L_1)
					     (capacitor 'C)
					     (inductor 'L_2))
				  (shunt->2-port
				   (resistor 'R_2)))))
	       0 1)
   's))
(/
 (* R_1 R_2)
 (+ (* C L_1 L_2 (expt s 3))
    (* C L_1 R_2 (expt s 2))
    (* C L_2 R_1 (expt s 2))
    (* C R_1 R_2 s)
    (* L_1 s)
    (* L_2 s)
    R_1
    R_2))
|#

#|
;;; Elliptic filter from Omar Wing "Circuit Theory with Computer
;;; Methods" p510

(define wing
  (cascade (l-network (resistor 1)
		      (capacitor 0.89318))
	   (cascade (l-network (parallel (inductor 1.26033)
					 (capacitor 0.1022))
			       (capacitor 1.57677))
		    (l-network (parallel (inductor 1.03950)
					 (capacitor 0.29139))
			       (parallel (capacitor 0.74177)
					 (resistor 1))))))

((impedance (terminate-open wing)) 0+1.05i)
;Value: 2.3191742137504785+.6118662772640898i

((impedance (terminate-open wing)) 0+1.1025i)
;Value: 3.423764666920801+1.3751580013280909i

;;; Difference with Wing: my real part is one more than 
;;;   his, because I count the resistance of the The'venin
;;;   source.  Of course, he is doing it right.


;;; Now the gain of this circuit is 1/A

((magnitude (voltage-transfer-ratio wing)) 0+1.05i)
;Value: .4788569560973999
 
((angle (voltage-transfer-ratio wing)) 0+1.05i)
;Value: 2.4028769579869986

;;; Omar Wing gets -.7387156956027944 for the angle
;;;  or in degrees:

(/ (* 360 -.7387156956027944) 2pi)
;Value: -42.32529161811094

;;; He is wrong.  He screwed up his definition of the network.

|#

#|
;;; Twin-tee filter network from Fig. 2.14 of the Martha manual.

(define microFarad 1/1000000)

(define twin-tee
  (parallel-parallel (t-network (resistor 2000)
				(capacitor (* 2 microFarad))
				(resistor 2000))
		     (t-network (capacitor (* 1 microFarad))
				(resistor 1000)
				(capacitor (* 1 microFarad)))))

(pp
 (map (lambda (f)
	(dBv ((magnitude (voltage-transfer-ratio twin-tee))
	      (* 2pi +i f))))
      '(50 74 75 76 77 78 79 80 81 82 83 84)))
(-12.611400600644222
 -28.79207876789273
 -30.566604182340335
 -32.76511464658994
 -35.66953125218555
 -39.990216657836285
 -48.77416038920447
 -51.54228192969944
 -41.05213224645407
 -36.48126228034761
 -33.53234170141037
 -31.35788373997261)

(print-expression
 ((magnitude (voltage-transfer-ratio twin-tee)) 's))
(magnitude
 (/ (+ 250000 (expt s 2))
    (+ 250000 (* 2000 s) (expt s 2))))

(define Vo/Vi
  (abstract-to-function (list 's)
    (simplify ((magnitude (voltage-transfer-ratio twin-tee)) 's))))

(print-expression (Vo/Vi (* 2pi +i 'f)))
(magnitude
 (/ (+ -6332.573977646111 (expt f 2))
    (+ -6332.573977646111 (expt f 2) (* -318.30988618379064i f))))

(define win (frame 50.0 100.0 0.0 .25))

(plot-function win
  (lambda (f)
    (magnitude
     (/ (+ -6332.57397764611 (expt f 2))
	(+ -6332.573977646111 (* -318.3098861837907i f) (expt f 2)))))
  50.0 
  100.0
  .1)

;;; Result is now in nscmutils/src/electric/twin-tee.xwd
|#
