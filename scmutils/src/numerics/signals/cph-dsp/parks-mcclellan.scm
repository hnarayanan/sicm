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

;;; -*-Scheme-*-
;;;
;;; $Id: parks-mcclellan.scm,v 1.1 1994/07/20 19:42:38 cph Exp $
;;;
;;; Copyright (c) 1993-94 Massachusetts Institute of Technology
;;;
;;; This material was developed by the Scheme project at the
;;; Massachusetts Institute of Technology, Department of Electrical
;;; Engineering and Computer Science.  Permission to copy this
;;; software, to redistribute it, and to use it for any purpose is
;;; granted, subject to the following restrictions and understandings.
;;;
;;; 1. Any copy made of this software must include this copyright
;;; notice in full.
;;;
;;; 2. Users of this software agree to make their best efforts (a) to
;;; return to the MIT Scheme project any improvements or extensions
;;; that they make, so that these may be included in future releases;
;;; and (b) to inform MIT of noteworthy uses of this software.
;;;
;;; 3. All materials developed as a consequence of the use of this
;;; software shall duly acknowledge such use, in accordance with the
;;; usual standards of acknowledging credit in academic research.
;;;
;;; 4. MIT has made no warrantee or representation that the operation
;;; of this software will be error-free, and MIT is under no
;;; obligation to provide any services, by way of maintenance, update,
;;; or otherwise.
;;;
;;; 5. In conjunction with products arising from the use of this
;;; material, there shall be no use of the name of the Massachusetts
;;; Institute of Technology nor of any adaptation thereof in any
;;; advertising, promotional, or sales literature without prior
;;; written consent from MIT in each case.

;;;; Parks-McClellan FIR Filter Design Program

;;; Loosely translated from "FIR Linear Phase Filter Design Program",
;;; McClellan, Parks, and Rabiner, section 5.1 of "Programs for
;;; Digital Signal Processing", ed. DSP Committee, IEEE Press, 1979.
;;; A cogent description of the design appears in "Discrete-Time
;;; Signal Processing", Oppenheim and Schafer, Prentice-Hall, 1989,
;;; section 7.6.

(declare (usual-integrations))

;;;; Language Notes

;;; This program is written in MIT Scheme, and uses several
;;; non-standard procedures.  This page describes those procedures and
;;; how to alter this program for use in other Scheme implementations.

;;; Most notably, variables whose names start with "fix:" or "flo:"
;;; are arithmetic procedures that are restricted to fixnums or
;;; flonums, respectively -- these restrictions act as declarations
;;; that allow the compiler to produce more efficient code.  Deleting
;;; these prefixes with a text editor is sufficient to change these
;;; procedures to standard Scheme arithmetic procedures.

;;; Some of the "flo:" procedures manipulate vectors -- this is a
;;; special efficient representation for vectors of floating-point
;;; numbers.  The procedure VECTOR->FLONUM-VECTOR maps an ordinary
;;; vector of flonums to one of these special vectors.

;;; The COMPUTE-INVERSE-DFT procedure uses some special procedures not
;;; included with this file; see the comment near COMPUTE-INVERSE-DFT
;;; for replacement code that does not use these special procedures.

;;; ERROR is MIT Scheme's error-signalling procedure.  Most Scheme
;;; implementations have something similar.

;;; DECLARE is used to give declarations to the MIT Scheme compiler.
;;; These declarations do not change the semantics of the program,
;;; only its performance.  Delete any instances of DECLARE if you are
;;; using another implementation.

;;; The reader syntax #| ... |# defines a multi-line comment exactly
;;; as in Common Lisp, similar to C's /* ... */.

;;; The REMEZ-TRACE-foo procedures use graphics facilities that are
;;; specific to MIT Scheme; if tracing is disabled (the default),
;;; these graphics procedures will not be called.

;;; The following definitions can be used to emulate some of the other
;;; non-standard procedures used here:

;;; (DEFINE (CEILING->EXACT X) (INEXACT->EXACT (CEILING X)))
;;; (DEFINE (EXACT-INTEGER? X) (AND (EXACT? X) (INTEGER? X)))
;;; (DEFINE (10LOG10 X) (* 10 (/ (LOG X) (LOG 10))))
;;; (DEFINE PI (* 4 (ATAN 1 1)))
;;; (DEFINE 2PI (* 2 PI))
;;; (DEFINE (APPEND-MAP! P . X) (APPLY APPEND! (APPLY MAP P X)))
;;; (DEFINE UNSPECIFIC #F)
;;; (DEFINE (VECTOR-MAP V P) (LIST->VECTOR (MAP P (VECTOR->LIST V))))
;;; (DEFINE (VALUES . X) X)
;;; (DEFINE (CALL-WITH-VALUES GENERATOR RECEIVER) (APPLY RECEIVER (GENERATOR)))

;;; (DEFINE (SUBVECTOR-MOVE-LEFT! V1 S1 E1 V2 S2)
;;;   (DO ((I1 S1 (+ I1 1))
;;;        (I2 S2 (+ I2 1)))
;;;       ((= I1 E1))
;;;     (VECTOR-SET! V2 I2 (VECTOR-REF V1 I1))))

(define (pm-lowpass-filter M omega-p omega-t p-ripple s-attenuation)
  (with-pm-K p-ripple s-attenuation
    (lambda (K)
      (pm-multiple-band-filter M
			       `((0 ,omega-p 1 1)
				 (,(+ omega-p omega-t) ,pi 0 ,K))))))

(define (pm-highpass-filter M omega-p omega-t p-ripple s-attenuation)
  (with-pm-K p-ripple s-attenuation
    (lambda (K)
      (pm-multiple-band-filter M
			       `((0 ,(- omega-p omega-t) 0 ,K)
				 (,omega-p ,pi 1 1))))))

(define (pm-bandpass-filter M omega-c bw-p omega-t p-ripple s-attenuation)
  (with-pm-K p-ripple s-attenuation
    (lambda (K)
      (pm-multiple-band-filter
       M
       (let* ((bw-p/2 (/ bw-p 2))
	      (bw-s/2 (+ bw-p/2 omega-t)))
	 `((0 ,(- omega-c bw-s/2) 0 ,K)
	   (,(- omega-c bw-p/2) ,(+ omega-c bw-p/2) 1 1)
	   (,(+ omega-c bw-s/2) ,pi 0 ,K)))))))

(define (with-pm-K passband-ripple stopband-attenuation procedure)
  (let ((K
	 (/ (passband-ripple->delta passband-ripple)
	    (stopband-attenuation->delta stopband-attenuation))))
    (let ((result (procedure K)))
      (pm-write-line
       `(PASSBAND-RIPPLE ,(passband-delta->ripple remez-deviation)))
      (pm-write-line
       `(STOPBAND-ATTENUATION
	 ,(stopband-delta->attenuation (/ remez-deviation K))))
      result)))

(define (pm-lowpass-filter-order omega-t passband-ripple stopband-attenuation)
  (ceiling->exact
   (/ (- (+ (10log10 (* (passband-ripple->delta passband-ripple)
			(stopband-attenuation->delta stopband-attenuation)))
	    13))
      (* 2.324 omega-t))))

(define (pm-multiple-band-filter M bands)
  (parks-mcclellan-program M #t bands))

(define (pm-hilbert-transform-filter M bands)
  (parks-mcclellan-program M #f bands))

(define (pm-differentiator M bands)
  (parks-mcclellan-program
   M
   #f
   (map (lambda (band)
	  (list (band/lower-edge band)
		(band/upper-edge band)
		(let ((value (/ (band/value band) 2pi)))
		  (lambda (omega)
		    (* value omega)))
		(if (< (band/value band) .0001)
		    (band/weight band)
		    (let ((weight (* (band/weight band) 2pi)))
		      (lambda (omega)
			(/ weight omega))))))
	bands)))

(define (parks-mcclellan-program M symmetric? bands)
  (if (not (and (exact-integer? M) (positive? M)))
      (error "System order must be exact positive integer:" M))
  (check-bands bands)
  (%parks-mcclellan-program M symmetric? bands))

(define (%parks-mcclellan-program M symmetric? bands)
  (let ((fir-type (if symmetric? (if (even? M) 1 2) (if (even? M) 3 4))))
    (let ((L+1
	   (if (= fir-type 1)
	       (+ (quotient M 2) 1)
	       (quotient (+ M 1) 2))))
      (compute-impulse-response
       (compute-inverse-dft
	(call-with-values
	    (lambda () (compute-grids bands (/ pi (* lgrid L+1)) fir-type))
	  (lambda (grid value weight)
	    (remez L+1 grid value weight (length bands))))
	L+1)
       fir-type))))

(define lgrid
  ;; A scale factor that controls the spacing between frequency
  ;; samples in the algorithm.  The spacing is inversely proportional
  ;; to this constant.
  16)

(define band/lower-edge
  ;; The lowest frequency (inclusive) of the band, in radians.
  car)

(define band/upper-edge
  ;; The highest frequency (inclusive) of the band, in radians.
  cadr)

(define band/value
  ;; The desired response value within the band.  Either a real number
  ;; or a procedure that accepts a frequency as an argument and
  ;; returns a real number which is the value for that frequency
  ;; within the band.
  caddr)

(define band/weight
  ;; The error-function weighting within the band.  Either a positive
  ;; real number of a procedure that accepts a frequency as an
  ;; argument and returns a positive real number.  Larger weights
  ;; decrease the maximum error within the band relative to smaller
  ;; weights.
  cadddr)

(define (check-bands bands)
  (if (or (not (list? bands))
	  (null? bands)
	  (null? (cdr bands)))
      (error "Bands must be a list of length > 1:" bands))
  (let ((check-omega
	 (lambda (omega)
	   (if (or (< omega 0) (> omega pi))
	       (error "Band frequency must be between 0 and pi:" omega)))))
    (for-each (lambda (band)
		(if (not (= 4 (length band)))
		    (error "Band must be a list of four elements:" band))
		(check-omega (band/lower-edge band))
		(check-omega (band/upper-edge band))
		(if (> (band/lower-edge band) (band/upper-edge band))
		    (error "Band edges reversed:"
			   (band/lower-edge band)
			   (band/upper-edge band)))
		(if (not (or (real? (band/value band))
			     (procedure? (band/value band))))
		    (error "Band value must be a real number or a procedure:"
			   (band/value band)))
		(if (not (or (and (real? (band/weight band))
				  (positive? (band/weight band)))
			     (procedure? (band/weight band))))
		    (error
		     "Band weight must be a a positive real or a procedure:"
		     (band/weight band))))
	      bands))
  (do ((bands bands (cdr bands)))
      ((null? (cdr bands)))
    (if (>= (band/upper-edge (car bands))
	    (band/lower-edge (cadr bands)))
	(error "Bands improperly ordered:" (car bands) (cdr bands)))))

(define (compute-grids bands delta-omega fir-type)
  (let ((grids (compute-grid-frequencies bands delta-omega fir-type)))
    (let ((value (map-over-grid-frequencies grids bands band/value))
	  (weight (map-over-grid-frequencies grids bands band/weight)))
      (let ((grid (list->vector (apply append! grids))))
	(let ((ngrid (vector-length grid)))
	  (let ((adjust!
		 (lambda (f)
		   (do ((i 0 (+ i 1)))
		       ((= i ngrid))
		     (let ((x (f (vector-ref grid i))))
		       (vector-set! value i (/ (vector-ref value i) x))
		       (vector-set! weight i (* (vector-ref weight i) x)))))))
	    (case fir-type
	      ((2) (adjust! (lambda (omega) (cos (/ omega 2)))))
	      ((3) (adjust! (lambda (omega) (sin omega))))
	      ((4) (adjust! (lambda (omega) (sin (/ omega 2)))))))
	  (values grid value weight))))))

(define (compute-grid-frequencies bands delta-omega fir-type)
  (let loop ((bands* bands))
    (if (null? bands*)
	'()
	(cons (let ((band (car bands*)))
		(let ((upper-edge
		       (if (and (null? (cdr bands*))
				(memq fir-type '(2 3)))
			   (min (- pi delta-omega) (band/upper-edge band))
			   (band/upper-edge band))))
		  (let loop
		      ((omega
			(if (and (eq? bands* bands)
				 (memq fir-type '(3 4)))
			    (max delta-omega (band/lower-edge band))
			    (band/lower-edge band))))
		    (cons omega
			  (let ((omega* (+ omega delta-omega)))
			    (if (< omega* upper-edge)
				(loop omega*)
				(list upper-edge)))))))
	      (loop (cdr bands*))))))

(define (map-over-grid-frequencies grids bands selector)
  (list->vector (append-map! (lambda (grid band)
			       (map (let ((value (selector band)))
				      (if (procedure? value)
					  value
					  (lambda (omega) omega value)))
				    grid))
			     grids
			     bands)))

(define (remez L+1 grid value weight nband)
  (let ((L+2 (+ L+1 1))
	(ngrid (vector-length grid))
	(value (vector->flonum-vector value))
	(weight (vector->flonum-vector weight))
	(x (vector->flonum-vector (vector-map grid cos)))
	(extrema (make-vector (+ L+1 1)))
	(xe (flo:make-vector (+ L+1 1)))
	(extrema* (make-vector (+ (- L+1 2) (* nband 2))))
	(c (flo:make-vector (+ L+1 1)))
	(d (flo:make-vector L+1))
	(errors (flo:make-vector (vector-length grid))))

    (define (remez-body)
      (compute-initial-guess)
      (let loop ((n-iterations 0) (deviation-limit -1))
	(let ((delta (abs (initialize-iteration))))
	  (pm-write-line `(DEVIATION ,delta))
	  (if (<= delta deviation-limit)
	      (error "Failure to converge:" n-iterations))
	  (exclude-excess-extrema (find-extrema))
	  (if (done?)
	      (set! remez-deviation delta)
	      (loop (+ n-iterations 1) delta))))
      (maybe-record-state)
      sample-frequency-response!)

    (define (compute-initial-guess)
      ;; Space the initial frequencies evenly along the grid.
      (let ((ngrid-1 (fix:- ngrid 1)))
	(do ((i 0 (fix:+ i 1))
	     (k 0 (fix:+ k ngrid-1)))
	    ((fix:= i L+2))
	  (vector-set! extrema* i (fix:quotient k L+1)))))

    (define (initialize-iteration)
      (subvector-move-left! extrema* 0 L+2 extrema 0)
      (do ((i 0 (fix:+ i 1)))
	  ((fix:= i L+2))
	(flo:vector-set! xe i (flo:vector-ref x (vector-ref extrema i))))
      (let ((delta (compute-deviation)))
	(do ((i 0 (fix:+ i 1))
	     (sign 1. (flo:negate sign)))
	    ((fix:= i L+2))
	  (let ((k (vector-ref extrema i)))
	    (flo:vector-set! c
			     i
			     (flo:- (flo:vector-ref value k)
				    (flo:/ (flo:* sign delta)
					   (flo:vector-ref weight k))))))
	(let ((L+3 (fix:+ L+2 1)))
	  (do ((i 0 (fix:+ i 1)))
	      ((fix:= i L+3))
	    (let ((khi (if (fix:= i L+2) ngrid (vector-ref extrema i))))
	      (do ((k (if (fix:= i 0)
			  0
			  (fix:+ (vector-ref extrema (fix:- i 1)) 1))
		      (fix:+ k 1)))
		  ((fix:= k khi))
		(flo:vector-set!
		 errors
		 k
		 (flo:* (flo:- (interpolate (flo:vector-ref x k))
			       (flo:vector-ref value k))
			(flo:vector-ref weight k))))
	      (if (not (fix:= i L+2))
		  (flo:vector-set!
		   errors
		   khi
		   (flo:* (flo:- (flo:vector-ref c i)
				 (flo:vector-ref value khi))
			  (flo:vector-ref weight khi)))))))
	(if remez-trace?
	    (remez-trace-errors grid errors))
	delta))

    (define (compute-deviation)
      ;; This code corresponds to the description on page 477 of
      ;; Oppenheim and Schafer.  It replaces the code that the
      ;; McClellan-Parks-Rabiner program computes.
      (let ((xL+1 (flo:vector-ref xe L+1)))
	(let loop ((k 0) (sign 1.) (numer 0.) (denom 0.))
	  (if (fix:= k L+2)
	      (flo:/ numer denom)
	      (let ((j (vector-ref extrema k))
		    (xk (flo:vector-ref xe k)))
		(let ((1/bk
		       (do ((i 0 (fix:+ i 1))
			    (1/bk 1.
				  (if (fix:= i k)
				      1/bk
				      (flo:* 1/bk
					     (flo:- xk
						    (flo:vector-ref xe i))))))
			   ((fix:= i L+2) 1/bk))))
		  (if (fix:< k L+1)
		      (flo:vector-set! d k (flo:/ (flo:- xk xL+1) 1/bk)))
		  (loop (fix:+ k 1)
			(flo:negate sign)
			(flo:+ numer (flo:/ (flo:vector-ref value j) 1/bk))
			(flo:+ denom
			       (flo:/ sign
				      (flo:* (flo:vector-ref weight j)
					     1/bk))))))))))

    (define (interpolate cos-omega)
      ;; This is ugly but it has to be fast.
      (let ((t (flo:make-vector 3)))
	(flo:vector-set! t 0 0.)
	(flo:vector-set! t 1 0.)
	(do ((k 0 (fix:+ k 1)))
	    ((fix:= k L+1))
	  (flo:vector-set! t 2
			   (flo:/ (flo:vector-ref d k)
				  (flo:- cos-omega (flo:vector-ref xe k))))
	  (flo:vector-set! t 0
			   (flo:+ (flo:vector-ref t 0)
				  (flo:* (flo:vector-ref t 2)
					 (flo:vector-ref c k))))
	  (flo:vector-set! t 1
			   (flo:+ (flo:vector-ref t 1)
				  (flo:vector-ref t 2))))
	(flo:/ (flo:vector-ref t 0) (flo:vector-ref t 1))))

    (define (find-extrema)
      ;; Due to numerical inaccuracies, it is possible for two or more
      ;; adjacent error points to be equal to one another.  If such a
      ;; string of points is an extremum, the smallest of the points
      ;; is chosen as "the" extremum.
      (let ((k 0))
	(let ((klim (vector-length extrema*))
	      (ngrid-1 (fix:- ngrid 1)))
	  (let ((include
		 (lambda (j)
		   (if (< k klim)
		       (begin
			 (vector-set! extrema* k j)
			 (set! k (fix:+ k 1))
			 unspecific)
		       (warn "Too many extrema found (reduce filter order):"
			     k j ngrid)))))
	    (let ((e0 (flo:vector-ref errors 0)))
	      (if (and (not (flo:= e0 0.))
		       (let loop ((i 1))
			 (let ((ei (flo:vector-ref errors i)))
			   (if (flo:= e0 ei)
			       (or (fix:= i ngrid-1)
				   (loop (fix:+ i 1)))
			       (if (flo:> e0 0.)
				   (flo:> e0 ei)
				   (flo:< e0 ei))))))
		  (include 0)))
	    (do ((j 1 (fix:+ j 1)))
		((fix:= j ngrid-1))
	      (let ((ej-1 (flo:vector-ref errors (fix:- j 1)))
		    (ej (flo:vector-ref errors j)))
		(if (and (if (flo:< ej 0.)
			     (flo:< ej ej-1)
			     (and (flo:> ej 0.)
				  (flo:> ej ej-1)))
			 (let loop ((i (fix:+ j 1)))
			   (let ((ei (flo:vector-ref errors i)))
			     (if (flo:= ej ei)
				 (or (fix:= i ngrid-1)
				     (loop (fix:+ i 1)))
				 (if (flo:> ej ej-1)
				     (flo:> ej ei)
				     (flo:< ej ei))))))
		    (include j))))
	    (let ((en-2 (flo:vector-ref errors (fix:- ngrid 2)))
		  (en-1 (flo:vector-ref errors ngrid-1)))
	      (if (if (flo:< en-1 0.)
		      (flo:< en-1 en-2)
		      (and (flo:> en-1 0.)
			   (flo:> en-1 en-2)))
		  (include ngrid-1)))))
	(if (< k L+2) (error "Too few extrema found:" k L+2))
	(if remez-trace? (pm-write-line `(EXCESS-EXTREMA ,(- k L+2))))
	k))

    (define (exclude-excess-extrema k)
      ;; Heuristic rules for excluding excess extrema:
      ;;
      ;; 1. If there are any adjacent extrema that aren't alternating,
      ;; exclude those first.
      ;;
      ;; 2. If all extrema are alternating, and there is more than one
      ;; excess extrema, exclude the one with the smallest error.
      ;; This creates a non-alternation that can then be excluded
      ;; using rule 1.
      ;;
      ;; 3. If all extrema are alternating, and there is exactly one
      ;; excess extremus, exclude the smaller of the first and last
      ;; extrema.
      (let ((e
	     (lambda (j)
	       (flo:vector-ref errors (vector-ref extrema* j)))))
	(declare (integrate-operator e))
	(let ((e<0
	       (lambda (j)
		 (flo:< (e j) 0.)))
	      (emin
	       (lambda (i j)
		 (if (flo:< (flo:abs (e i)) (flo:abs (e j))) i j)))
	      (exclude
	       (lambda (j)
		 (subvector-move-left! extrema* (fix:+ j 1) k extrema* j)
		 (set! k (fix:- k 1))
		 unspecific))
	      (L+3 (fix:+ L+2 1)))
	  (declare (integrate-operator e<0 emin))
	  (do () ((= k L+2))
	    (cond ((let ((k-1 (fix:- k 1)))
		     (do ((i 0 (fix:+ i 1))
			  (j #f
			     (if (or (and (not (fix:= i 0))
					  (if (e<0 i)
					      (e<0 (fix:- i 1))
					      (not (e<0 (fix:- i 1)))))
				     (and (not (= i k-1))
					  (if (e<0 i)
					      (e<0 (fix:+ i 1))
					      (not (e<0 (fix:+ i 1))))))
				 (if j (emin i j) i)
				 j)))
			 ((fix:= i k) j)))
		   => exclude)
		  ((fix:= k L+3)
		   (exclude (emin 0 L+2)))
		  (else
		   (do ((i 1 (fix:+ i 1))
			(j 0 (emin i j)))
		       ((fix:= i k) (exclude j)))))))))

    (define (done?)
      (if (eq? remez-trace? #t)
	  (remez-trace-extrema
	   (vector-map (vector-head extrema* L+2)
		       (lambda (k) (vector-ref grid k)))))
      (let ((e0 (abs (flo:vector-ref errors (vector-ref extrema* 0)))))
	(let loop ((j 1) (min-error e0) (max-error e0))
	  (if (= j L+2)
	      (let ((error-ratio (/ (- max-error min-error) max-error)))
		(if remez-trace?
		    (begin
		      (pm-write-line `(MAX-ERROR ,max-error))
		      (pm-write-line `(ERROR-RATIO ,error-ratio))))
		(if remez-trace-break?
		    (breakpoint "Trace break"))
		(<= error-ratio remez-tolerance))
	      (let ((ej (abs (flo:vector-ref errors (vector-ref extrema* j)))))
		(loop (+ j 1)
		      (min min-error ej)
		      (max max-error ej)))))))

    (define (sample-frequency-response! Aek n m)
      (let ((delta-omega (/ 2pi n)))
	(do ((j 0 (fix:+ j 1))
	     (omega 0. (flo:+ omega delta-omega)))
	    ((fix:= j m))
	  (flo:vector-set! Aek j (frequency-response omega)))))

    (define (frequency-response omega)
      (let ((cos-omega (flo:cos omega)))
	(let loop ((low 0) (high L+1))
	  (if (fix:< high low)
	      (interpolate cos-omega)
	      (let ((k (fix:quotient (fix:+ high low) 2)))
		(let ((xk (flo:vector-ref x (vector-ref extrema k))))
		  (cond ((flo:< (flo:abs (flo:- cos-omega xk)) 1e-6)
			 (flo:vector-ref c k))
			((flo:< cos-omega xk)
			 (loop (fix:+ k 1) high))
			(else
			 (loop low (fix:- k 1))))))))))

    (define (maybe-record-state)
      (if remez-record-state?
	  (begin
	    (set! remez-grid grid)
	    (set! remez-value value)
	    (set! remez-weight weight)
	    (set! remez-errors errors)
	    (set! remez-extrema extrema)
	    (set! remez-get-Aek! sample-frequency-response!))))

    (remez-body)))

(define remez-tolerance 1e-6)

(define remez-record-state? #f)
(define remez-grid)
(define remez-value)
(define remez-weight)
(define remez-deviation)
(define remez-errors)
(define remez-extrema)
(define remez-get-Aek!)

(define remez-trace? #f)
(define remez-trace-break? #f)

(define (remez-trace-errors grid errors)
  (let ((device (get-plotting-device))
	(ngrid (vector-length grid)))
    (graphics-clear device)
    (let ((xl (apply min (vector->list grid)))
	  (xu (apply max (vector->list grid)))
	  (yl (apply min (flonum-vector->list errors)))
	  (yu (apply max (flonum-vector->list errors))))
      (graphics-set-coordinate-limits device xl yl xu yu)
      (graphics-draw-line device xl 0 xu 0))
    (graphics-move-cursor device
			  (vector-ref grid 0)
			  (flo:vector-ref errors 0))
    (do ((i 1 (+ i 1)))
	((= i ngrid))
      (graphics-drag-cursor device
			    (vector-ref grid i)
			    (flo:vector-ref errors i)))
    (graphics-flush device)))

(define (remez-trace-extrema extrema)
  (let ((device (get-plotting-device))
	(L+2 (vector-length extrema)))
    (graphics-bind-line-style device 2
      (lambda ()
	(call-with-values (lambda () (graphics-coordinate-limits device))
	  (lambda (xl yb xr yt)
	    xl xr
	    (do ((i 0 (+ i 1)))
		((= i L+2))
	      (let ((x (vector-ref extrema i)))
		(graphics-draw-line device x yb x yt)))))))
    (graphics-flush device)))

(define (pm-write-line object)
  (newline)
  (write-char #\;)
  (write object))

(define (compute-inverse-dft get-Aek! L+1)
  (let ((N (fix:ceiling-lg (- (* 2 L+1) 1))))
    (let ((reals (flo:make-vector N))
	  (N/2 (quotient N 2)))
      (get-Aek! reals N (+ N/2 1))
      (do ((i 1 (fix:+ i 1)))
	  ((fix:= i N/2))
	(flo:vector-set! reals (fix:- N i) (flo:vector-ref reals i)))
      (flo:real-inverse-fft! reals)
      (flo:subvector reals 0 L+1))))

#|
;;; FLO:REAL-INVERSE-FFT! and FLO:SUBVECTOR are supplied by an
;;; external package.  If you don't have them, replace the above
;;; procedure with this one:

(define (compute-inverse-dft get-Aek! L+1)
  (let ((N (- (* 2 L+1) 1))
	(Aek (flo:make-vector L+1))
	(a (flo:make-vector L+1)))
    (get-Aek! Aek N (flo:vector-length Aek))
    ;; Take the inverse DFT of vector Aek and put the results in A.
    ;; Use direct evaluation since the number of samples is small, and
    ;; since Ae is real and even, we don't have to compute the
    ;; imaginary components of the impulse response.
    (flo:vector-set! a 0
		     (/ (+ (flo:vector-ref Aek 0)
			   (* 2 
			      (do ((k 1 (+ k 1))
				   (d 0 (+ d (flo:vector-ref Aek k))))
				  ((= k L+1) d))))
			N))
    (let ((delta-omega (/ 2pi N)))
      (do ((i 1 (+ i 1))
	   (sign -1 (- sign))
	   (omega delta-omega (+ omega delta-omega)))
	  ((= i L+1))
	(flo:vector-set! a
			 i
			 (/ (+ (flo:vector-ref Aek 0)
			       (* 2
				  (do ((k 1 (+ k 1))
				       (omega* omega (+ omega* omega))
				       (d 0
					  (+ d
					     (* (flo:vector-ref Aek k)
						(cos omega*)))))
				      ((= k L+1) d))))
			    N))))
    a))
|#

(define (compute-impulse-response hek fir-type)
  ;; HEK is the set of coefficients h_e[k] that are the impulse
  ;; response of the non-causal type I system.  Translating this to
  ;; the impulse response of a causal system of the appropriate type
  ;; is what this procedure performs.
  (let* ((L+1 (flo:vector-length hek))
	 (L (fix:- L+1 1))
	 (2L (fix:+ L L)))
    (case fir-type
      ((1)
       (let* ((M 2L)
	      (h (flo:make-vector (fix:+ M 1))))
	 (do ((j 0 (fix:+ j 1)))
	     ((fix:= j L))
	   (flo:vector-set! h j (flo:vector-ref hek (fix:- L j))))
	 (flo:vector-set! h L (flo:vector-ref hek 0))
	 (do ((j 0 (fix:+ j 1)))
	     ((fix:= j L+1))
	   (flo:vector-set! h (fix:- M j) (flo:vector-ref h j)))
	 h))
      ((2)
       (let* ((M (fix:+ 2L 1))
	      (h (flo:make-vector (fix:+ M 1))))
	 (flo:vector-set! h 0 (flo:/ (flo:vector-ref hek L) 2.))
	 (do ((j 1 (fix:+ j 1)))
	     ((fix:= j L))
	   (flo:vector-set! h j
			    (flo:/ (flo:+ (flo:vector-ref hek (fix:- L j))
					  (flo:vector-ref hek (fix:- L+1 j)))
				   2.)))
	 (flo:vector-set! h L
			  (flo:/ (flo:+ (flo:vector-ref hek 0)
					(flo:vector-ref hek 1))
				 2.))
	 (do ((j 0 (fix:+ j 1)))
	     ((fix:= j L+1))
	   (flo:vector-set! h (fix:- M j) (flo:vector-ref h j)))
	 h))
      ((3)
       (let* ((L+2 (fix:+ L+1 1))
	      (M (fix:+ 2L 2))
	      (h (flo:make-vector (fix:+ M 1))))
	 (flo:vector-set! h 0 (flo:/ (flo:vector-ref hek L) 2.))
	 (flo:vector-set! h 1 (flo:/ (flo:vector-ref hek (fix:- L 1)) 2.))
	 (do ((j 2 (fix:+ j 1)))
	     ((fix:= j L))
	   (flo:vector-set! h j
			    (flo:/ (flo:- (flo:vector-ref hek (fix:- L j))
					  (flo:vector-ref hek (fix:- L+2 j)))
				   2.)))
	 (flo:vector-set! h L
			  (flo:/ (flo:- (flo:vector-ref hek 0)
					(flo:vector-ref hek 2))
				 2.))
	 (flo:vector-set! h L+1 0.)
	 (do ((j 0 (fix:+ j 1)))
	     ((fix:= j L+1))
	   (flo:vector-set! h (fix:- M j) (flo:- 0. (flo:vector-ref h j))))
	 h))
      ((4)
       (let* ((M (fix:+ 2L 1))
	      (h (flo:make-vector (fix:+ M 1))))
	 (flo:vector-set! h 0 (flo:/ (flo:vector-ref hek L) 2.))
	 (do ((j 1 (fix:+ j 1)))
	     ((fix:= j L))
	   (flo:vector-set! h j
			    (flo:/ (flo:- (flo:vector-ref hek (fix:- L j))
					  (flo:vector-ref hek (fix:- L+1 j)))
				   2.)))
	 (flo:vector-set! h L
			  (flo:/ (flo:- (flo:vector-ref hek 0)
					(flo:vector-ref hek 1))
				 2.))
	 (do ((j 0 (fix:+ j 1)))
	     ((fix:= j L+1))
	   (flo:vector-set! h (fix:- M j) (flo:- 0. (flo:vector-ref h j))))
	 h)))))