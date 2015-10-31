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

(declare (usual-integrations))

;;;;    Discrete Fourier Transform -- GJS -- 8 April 1987

;;; Simple FFT for records whose length is a power of 2.
;;;   Data records are represented as lists of (complex) numbers.
;;;   All arithmetic is assumed to be complex generic, 
;;;   unless explicitly noted by "fix:" for indices.

#|;;; Note speedup caveat on a subsequent page.

(define (make-transform-pair period)
  (define (fft-internal data ws)
    (if (null? (cdr data))
	data
	(let ((ews (if (null? (cdr ws)) ws (evens ws))))
	  (let ((even-fft (fft-internal (evens data) ews))
		(odd-fft (map * ws (fft-internal (odds data) ews))))
	    (append (map + even-fft odd-fft)
		    (map - even-fft odd-fft))))))
  (if (power-of-2? period)
      (let* ((w-list (roots-of-unity period))
	     (w*-list (map conjugate w-list))
	     (fperiod (exact->inexact period)))
	(define (ft data)
	  (if (fix:= period (length data))
	      (map (lambda (z) (/ z fperiod))
		   (fft-internal data w*-list))
	      (error "Wrong length data record -- FT" period)))
	(define (ift data)
	  (if (fix:= period (length data))
	      (fft-internal data w-list)
	      (error "Wrong length data record -- IFT" period)))
	(list period ft ift))
      (error "Period is not a power of 2 -- MAKE-TRANSFORM-PAIR")))
|#

;;; FFTs like to get power-of-2 data points.

(define (power-of-2? n)
  (and (exact? n) (integer? n) (positive? n)
       (let lp ((n n))
	 (or (fix:= n 1)
	     (and (even? n)
		  (lp (quotient n 2)))))))


;;; Power of two data can be split into even and odd sublists

(define (evens list)
  (if (null? list)
      '()
      (cons (car list) (evens (cddr list)))))

(define (odds list)
  (if (null? list)
      '()
      (cons (cadr list) (odds (cddr list)))))

;;; Roots of unity are needed in FFT programs

(define (roots-of-unity n)
  (let ((n/2 (quotient n 2))
	(2pi/n (/ 2pi (exact->inexact n))))
    (let loop ((k 0))
      (if (fix:= k n/2)
	  '()
	  (let ((fk (exact->inexact k)))
	    (cons (make-rectangular (cos (* 2pi/n fk))
				    (sin (* 2pi/n fk)))
		  (loop (fix:1+ k))))))))


;;; Useful for testing FFT programs

(define (m-cycles-cos-in-n-samples m n)
  (let ((w (/ (* 2pi m) n)))
    (make-initialized-list
     n
     (lambda (i)
       (cos (* w i))))))


;;; FFTs grow as A*n*log_2(n)
;;;  We estimate A with the following code:

(define (constant-of-growth n t)
  (/ t (* n (/ (log n) (log 2)))))

;;; Actually, by putting EVENS and ODDS procedures into stuff it runs faster:

(define (make-transform-pair period)
  (define (evens list)
    (if (null? list)
	'()
	(cons (car list) (evens (cddr list)))))
  (define (odds list)
    (if (null? list)
	'()
	(cons (cadr list) (odds (cddr list)))))
  (define (fft-internal data ws)
    (if (null? (cdr data))
	data
	(let ((ews (if (null? (cdr ws)) ws (evens ws))))
	  (let ((even-fft (fft-internal (evens data) ews))
		(odd-fft (map * ws (fft-internal (odds data) ews))))
	    (append (map + even-fft odd-fft)
		    (map - even-fft odd-fft))))))
  (if (power-of-2? period)
      (let* ((w-list (roots-of-unity period))
	     (w*-list (map conjugate w-list))
	     (fperiod (exact->inexact period)))
	(define (ft data)
	  (if (fix:= period (length data))
	      (map (lambda (z) (/ z fperiod))
		   (fft-internal data w*-list))
	      (error "Wrong length data record -- FT" period)))
	(define (ift data)
	  (if (fix:= period (length data))
	      (fft-internal data w-list)
	      (error "Wrong length data record -- IFT" period)))
	(list period ft ift))
      (error "Period is not a power of 2 -- MAKE-TRANSFORM-PAIR")))

#|
;;; For example, we may:

(define 2pi (* 8 (atan 1 1)))

(define fts (make-transform-pair 64))
(define ft (cadr fts))			; This gets the transform.
(define ift (caddr fts))		; This gets the inverse transform.


(define foo
  (m-cycles-cos-in-n-samples 4 64))

(pp foo)
(1
 .9238795325112867
 .7071067811865476
 .38268343236508984
 6.123233995736766e-17
 -.3826834323650897
 -.7071067811865475
 -.9238795325112867
 -1.
 -.9238795325112868
 -.7071067811865477
 -.38268343236509034
 -1.8369701987210297e-16
 .38268343236509
 .7071067811865474
 .9238795325112865
 1.
 .9238795325112867
 .7071067811865477
 .38268343236509045
 3.061616997868383e-16
 -.3826834323650899
 -.7071067811865467
 -.9238795325112864
 -1.
 -.9238795325112867
 -.7071067811865471
 -.38268343236509056
 -4.286263797015736e-16
 .3826834323650898
 .7071067811865466
 .9238795325112864
 1.
 .9238795325112867
 .7071067811865472
 .38268343236509067
 5.51091059616309e-16
 -.38268343236508967
 -.7071067811865465
 -.9238795325112864
 -1.
 -.9238795325112875
 -.7071067811865474
 -.3826834323650908
 -2.4499125789312946e-15
 .38268343236508956
 .7071067811865464
 .923879532511287
 1.
 .9238795325112875
 .7071067811865475
 .3826834323650909
 -9.803364199544708e-16
 -.38268343236508945
 -.7071067811865464
 -.923879532511287
 -1.
 -.9238795325112876
 -.7071067811865476
 -.382683432365091
 -2.6948419387607653e-15
 .38268343236508934
 .7071067811865462
 .9238795325112868)

(pp (ft foo))
(-1.2561523886109383e-16+0.i
 -7.194813380339634e-17-7.086252452433498e-17i
 -4.755322322060515e-17-1.3793326022032788e-17i
 -5.83341391632138e-17-1.8018183092462435e-17i
 .5-2.5552136926677007e-16i
 1.1586064979361383e-16+1.089702111422959e-17i
 -1.1873231118751674e-17-2.0239773911048592e-17i
 4.512855110144377e-17-6.255141134152987e-17i
 9.58273082749445e-17+5.748367925732733e-18i
 3.5694588106975706e-17+6.199387627557606e-17i
 1.3757783427383357e-18+1.4290236579584324e-17i
 1.6878935539584702e-17-2.4910490867645194e-17i
 -1.6004798298033718e-17+1.6604098399003017e-17i
 -3.4262965998359476e-18+4.549316112455987e-17i
 2.2011235859554076e-17+3.601347807542881e-17i
 -5.445987949241084e-18+4.310411348056533e-17i
 -7.704298153374324e-17+1.3877787807814457e-17i
 -2.96453526982685e-17-6.616462217832386e-17i
 3.5611883689982354e-17-2.7326144464208437e-17i
 -5.312532674753407e-17-4.548113318454349e-18i
 1.3986314276751338e-16-4.1898558117279617e-17i
 -2.006313433668442e-17+2.1833880092687754e-17i
 -2.4934295892384985e-17+7.547029960573702e-18i
 3.092207877681236e-17-4.5990700402709215e-17i
 8.60142304081709e-17+3.3503943541361643e-17i
 2.3964718499917352e-17+7.709287355040676e-17i
 2.249285459842277e-19+5.152164240412953e-17i
 9.813852419933036e-18+7.523983342109689e-17i
 -1.3877787807814457e-16-1.2177051777973827e-18i
 -7.604540905863336e-18-5.0461016188942277e-17i
 2.513692379348282e-17+1.3716743725186983e-18i
 -2.867046203424327e-17+6.749760138699778e-17i
 -8.398187543765047e-17+0.i
 -2.867046203424324e-17-6.749760138699776e-17i
 2.5136923793482817e-17-1.371674372518686e-18i
 -7.604540905863339e-18+5.0461016188942265e-17i
 -1.3877787807814457e-16-3.347676434173876e-17i
 9.813852419933073e-18-7.52398334210969e-17i
 2.2492854598423925e-19-5.1521642404129534e-17i
 2.3964718499917377e-17-7.709287355040676e-17i
 8.60142304081709e-17-3.3503943541361643e-17i
 3.092207877681235e-17+4.599070040270922e-17i
 -2.4934295892384985e-17-7.547029960573716e-18i
 -2.00631343366844e-17-2.183388009268778e-17i
 1.825382519918072e-16+8.716066861031106e-18i
 -5.312532674753406e-17+4.54811331845434e-18i
 3.5611883689982354e-17+2.732614446420845e-17i
 -2.964535269826851e-17+6.616462217832388e-17i
 -7.704298153374324e-17-1.3877787807814457e-17i
 -5.4459879492410746e-18-4.310411348056531e-17i
 2.2011235859554076e-17-3.601347807542881e-17i
 -3.4262965998359383e-18-4.549316112455986e-17i
 2.667031092626009e-17-2.505497056619788e-17i
 1.68789355395847e-17+2.4910490867645207e-17i
 1.3757783427383326e-18-1.4290236579584315e-17i
 3.569458810697573e-17-6.199387627557607e-17i
 9.58273082749445e-17-5.74836792573273e-18i
 4.5128551101443756e-17+6.255141134152987e-17i
 -1.1873231118751684e-17+2.0239773911048586e-17i
 1.158606497936138e-16-1.0897021114229534e-17i
 .5+3.318492022097496e-16i
 -5.833413916321382e-17+1.8018183092462423e-17i
 -4.7553223220605156e-17+1.3793326022032773e-17i
 -7.194813380339636e-17+7.086252452433494e-17i)
|#

#|
;;;  PPA = (200 MHz single-processor P6, with 65MB ram).
;Summary of configuration options:
;  heap size: 3000
;  constant-space size: 500
;  stack size: 100
;  library path: /usr/local/lib/mit-scheme
;  band: /usr/local/lib/mit-scheme/runtime.com
;  microcode tables: /usr/local/lib/mit-scheme/utabmd.bin
;  emacs subprocess: yes
;  force interactive: no
;  disable core dump: no
;  unused arguments: 
;Scheme Microcode Version 11.154
;MIT Scheme running under Linux

;Scheme saved on Thursday July 25, 1996 at 11:28:28 PM
;  Release 7.5a
;  Microcode 11.154
;  Runtime 14.170

;;; Bezalel =  (33MHz 486 with 36MB ram)
Scheme saved on Wednesday April 10, 1996 at 1:44:48 AM
  Release 7.4.2
  Microcode 11.151
  Runtime 14.168
  Edwin 3.90

;;; The code we tried is:
(show-time
 (let ((ft (cadr (make-transform-pair 1024)))
       (sig (m-cycles-cos-in-n-samples 120 1024)))
   (lambda ()
     (ft sig)
     'done)))

;;; Interpreted, on PPA
process time: 220 (220 RUN + 0 GC); real time: 244
process time: 260 (260 RUN + 0 GC); real time: 267

;;; Interpreted, on Bezalel
process time: 3180 (3180 RUN + 0 GC); real time: 3180

;;; Compiled, on Bezalel 
;;; n=1024
process time: 940 (940 RUN + 0 GC); real time: 940
process time: 900 (900 RUN + 0 GC); real time: 900
;;; n=2048
process time: 1970 (1970 RUN + 0 GC); real time: 1970
;;; n=4096
process time: 4750 (4250 RUN + 500 GC); real time: 4750

(constant-of-growth 4096 4250)
;Value: .08646647135416667
(constant-of-growth 2048 1970)
;Value: .08744673295454546
(constant-of-growth 1024 920)
;Value: .08984375


;;; Compiled, on PPA, with Liar (Intel i386) 4.106
(show-time
 (let ((ft (cadr (make-transform-pair 8192)))
       (sig (m-cycles-cos-in-n-samples 120 8192)))
   (lambda ()
     (ft sig)
     'done)))
process time: 1140 (890 RUN + 250 GC); real time: 1169
process time: 1030 (900 RUN + 130 GC); real time: 1038
process time: 1150 (910 RUN + 240 GC); real time: 1165
process time: 990 (870 RUN + 120 GC); real time: 1121

(constant-of-growth 8192 900)
;Value: 8.451021634615384e-3

;;; Comparing PPA with Bezalel
(/ .08646647135416667 8.451021634615384e-3)
;Value: 10.231481481481483
|#

#|
;;; CPH's FFT is much better, as expected! (On PPA)
(show-time
 (let ((sig
	(list->flonum-vector
	 (m-cycles-cos-in-n-samples 120 32768))))
   (lambda ()
     (flo:real-fft sig)
     'done)))
process time: 480 (480 RUN + 0 GC); real time: 486
process time: 500 (500 RUN + 0 GC); real time: 495
process time: 380 (380 RUN + 0 GC); real time: 376


(constant-of-growth 32768 450)
;Value: .00091552734375


;;; CPH's compiled on Bezalel:
;;; n=1024
process time: 60 (60 RUN + 0 GC); real time: 60
;;; n=2048
process time: 160 (160 RUN + 0 GC); real time: 160
;;; n=4096
process time: 280 (280 RUN + 0 GC); real time: 280
;;; n=8192
process time: 620 (620 RUN + 0 GC); real time: 620
;;; n=32768
process time: 2810 (2810 RUN + 0 GC); real time: 2810

(constant-of-growth 32768 2810)
;Value: 5.716959635416667e-3

(constant-of-growth 4096 280)
;Value: 5.696614583333333e-3

;;; Comparing PPA and Bezalel
(/ 5.716959635416667e-3 .00091552734375)
;Value: 6.2444444444444445

;;; Comparing CPH with GJS
(/ .08646647135416667			; GJS Bezalel Compiled
   5.716959635416667e-3			; CPH Bezalel Compiled
   )
;Value: 15.124555160142348              ; a nice improvement!
|#

