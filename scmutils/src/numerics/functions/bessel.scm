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

;;;; Bessel functions of integer order.

;;; This file may be compiled with the Scheme compiler.
;;;  It does not depend upon any other code to work.
(declare (usual-integrations))
#|
;;; The following three are redundant with nscmutils/kernel/udefs

(define pi/4 (atan 1 1))
(define pi/2 (* 2 pi/4))
(define pi (* 4 pi/4))
|#

(define 2/pi (/ 1 2 pi/4))
(define 3pi/4 (* 3 pi/4))


;;; Utilities for special functions

(define (round-to-even x)
  (let ((xn (inexact->exact (round x))))
    (if (odd? xn)
	(fix:+ xn 1)
	xn)))


(define (poly-by-coeffs->value x . coeffs)
  (let lp ((coeffs coeffs))
    (if (null? (cdr coeffs))
	(car coeffs)
	(+ (car coeffs)
	   (* x
	      (lp (cdr coeffs)))))))


;;; From John F. Hart, et.al., Computer Approximations, QA297.C64 1968
;;;           These are all good to at least 15 digits.

(define (bessj0 x)
  (let ((ax (magnitude x)))
    (if (< ax 8.0)
	(let ((y (* x x)))		;Jzero 5845
	  (/ (poly-by-coeffs->value y
	       +.1859623176218978035283999449e+18
	       -.4414582939181598183458448718e+17
	       +.2334489171877869744571586698e+16
	       -.4776555944267358775465713161e+14
	       +.4621722250317180263694186830e+12
	       -.2271490439553603267422190396e+10
	       +.5513584564770752154116759317e+7
	       -.5292617130384557364907747176e+4)
	     (poly-by-coeffs->value y
	       +.1859623176218977331294574009e+18
	       +.2344750013658996756881142774e+16
	       +.1501546244976975197238575580e+14
	       +.6439867453513325627846468877e+11
	       +.2042514835213435736159365899e+9
	       +.4940307949181397241772754336e+6
	       +.8847203675617550401186701293e+3
	       +.1e+1)))
	(let* ((z (/ 8.0 ax))		
	       (y (* z z))
	       (xx (- ax pi/4))
	       (p0			;Pzero 6546
		(/ (poly-by-coeffs->value y
		     +.8554822541506661710252074e+4
		     +.8894437532960619440762804e+4
		     +.2204501043965180428995069e+4
		     +.1286775857487141932988510e+3
		     +.9004793474802880316384000e+0)
		   (poly-by-coeffs->value y
		     +.8554822541506662842462151e+4
		     +.8903836141709595355210343e+4
		     +.2214048851914710419825683e+4
		     +.1308849004999238828351090e+3
		     +.1e+1)))
	       (q0			;Qzero 6946
		(/ (poly-by-coeffs->value y
		     -.37510534954957111594836e+2
		     -.46093826814625174976377e+2
		     -.13990976865960680088016e+2
		     -.10497327982345548331260e+1
		     -.93525953294031893049e-2)
		   (poly-by-coeffs->value y
		     +.2400674237117267479318819e+4
		     +.2971983745208491990065486e+4
		     +.921566975526530895082307e+3
		     +.74428389741411178824152e+2
		     +.1e1))))
	  (* (sqrt (/ 2/pi ax))
	     (- (* (cos xx) p0)
		(* z (sin xx) q0)))))))

(define (bessj1 x)
  (let ((ax (magnitude x)))
    (if (< ax 8.0)			;Jone 6045
	(let ((y (* x x)))
	  (/ (* x
		(poly-by-coeffs->value y
		  +.695364226329838502166085207e+8
		  -.8356785487348914291918495672e+7
		  +.3209027468853947029888682298e+6
		  -.58787877666568200462723094e+4
		  +.6121876997356943874446879769e+2
		  -.3983107983952332023421699105e+0
		  +.1705769264349617107854016566e-2
		  -.4910599276555129440130592573e-5
		  +.9382193365140744507653268479e-8
		  -.1107352224453730633782671362e-10
		  +.63194310317443161294700346e-14))
	     (poly-by-coeffs->value y
	       +.139072845265967685120764336e+9
	       +.6705346835482299302199750802e+6
	       +.1284593453966301898121332163e+4
	       +.1e+1)))
	(let* ((z (/ 8.0 ax))
	       (y (* z z))
	       (xx (- ax 3pi/4))
	       (p1			;Pone 6747
		(/ (poly-by-coeffs->value y
		     +.1290918471896188077350689e+5
		     +.1309042051103506486292571e+5
		     +.313275295635506951011069e+4
		     +.17431379748379024599685e+3
		     +.122850537643590432633e+1)
		   (poly-by-coeffs->value y
		     +.1290918471896187879332737e+5
		     +.1306678308784402036110575e+5
		     +.310928141677002883350924e+4
		     +.16904721775008609992033e+3
		     +.1e+1)))
	       (q1			;Qone 7147
		(/ (poly-by-coeffs->value y
		    ;+.14465282874995208675225e+3        ;This line or the next is in error
		     +.14465282874995208765225e+3
		     +.1744291689092425885102e+3
		     +.5173653281836591636536e+2
		     +.379944537969806734901e+1
		     +.36363466476034710809e-1)
		   (poly-by-coeffs->value y
		     +.308592701333231723110639e+4
		     +.373434010601630179517765e+4
		     +.11191098527047487025919e+4
		     +.8522392064341340397334e+2
		     +.1e+1)))
	       (ans
		(* (sqrt (/ 2/pi ax))
		   (- (* (cos xx) p1)
		      (* z (sin xx) q1)))))
	  (if (< x 0.0)
	      (- ans)
	      ans)))))

;;;  Although this is implemented with the usual recurrences, it is 
;;; very subtle.

(define (bessj n x)
  (let ((acc 40) (bigno 1.0e10) (bigni 1.0e-10))  
    (cond ((fix:= n 0) (bessj0 x))
	  ((fix:= n 1) (bessj1 x))
	  ((= x 0.0) 0.0)
	  ((< x 0.0)
	   (if (even? n) (bessj n (- x)) (- (bessj n (- x)))))
	  ((fix:< n 0)
	   (if (even? n) (bessj (- n) x) (- (bessj (- n) x))))
	  (else
	   (let* ((ax (magnitude x)) (tox (/ 2.0 ax)))
	     (if (> ax n)
		 (let lp ((j 1) (bjm (bessj0 ax)) (bj  (bessj1 ax)))
		   (if (fix:= j n)
		       (if (and (< x 0.0) (odd? n)) (- bj) bj)
		       (lp (fix:+ j 1)
			   bj
			   (- (* j tox bj) bjm))))
		 (let ((m (round-to-even (+ n (sqrt (* acc n)))))
		       (bj 1.0) (bjp 0.0) (ans 0.0) (sum 0.0))
		   (let lp ((j m))
		     (if (fix:= j 0)
			 (let ((ans (/ ans (- (* 2.0 sum) bj))))
			   (if (and (< x 0.0) (odd? n)) (- ans) ans))
			 (let ((bjm (- (* j tox bj) bjp)))
			   (set! bjp bj)
			   (set! bj bjm)
			   (if (> (magnitude bj) bigno)
			       (begin (set! bj (* bj bigni))
				      (set! bjp (* bjp bigni))
				      (set! ans (* ans bigni))
				      (set! sum (* sum bigni))))
			   (if (odd? j)
			       (set! sum (+ sum bj)))
			   (if (fix:= j n)
			       (set! ans bjp))
			   (lp (fix:- j 1))))))))))))

#|
;;; Assymptotic formulae, for testing:

(define (fact n)
  (if (< n 2)
      1
      (* n (fact (- n 1)))))

(define (bessj:0<x<<n n x)
  (/ (expt (/ x 2) n) (fact n)))

(define (bessj:n<<x n x)
  (* (sqrt (/ 2 pi x))
     (cos (- x (* pi/2 n) pi/4))))
|#

(define (bessy0 x)
  (let ((ax (magnitude x)))
    (if (< ax 8.0)
	(let* ((y (* x x))		;Yzero 6243
	       (r0
		(poly-by-coeffs->value y
		     -.122848349966864707119444888e+8  ;p00
		     +.2950673961329634647867906439e+8 ;p01
		     -.2540763578168434015208700066e+7 ;p02
		     +.7768806299511773765193176993e+5 ;p03
		     -.1193299661108745921129349868e+4 ;p04
		     +.10753556131901778914962135e+2   ;p05
		     -.6186687126256085875960782886e-1 ;p06
		     +.2379830688791742855598085169e-3 ;p07
		     -.6227852796374134180786140767e-6 ;p08
		     +.1091804574277522610752537393e-8 ;p09
		     -.119120749069566983004259626e-11 ;p10
		     +.6321369945552678896098605e-15   ;p11
		     ))
	       (s0
		(poly-by-coeffs->value y
		     +.1664514914558198835968659363e+9 ;q00
		     +.75939734950276133884153062e+6   ;q01
		     +.137072109013171838997378226e+4  ;q02
		     +1.0		               ;q03
		     )))
	  (+ (/ r0 s0)
	     (* 2/pi (bessj0 x) (log x))))
	(let* ((z (/ 8.0 ax))		
	       (y (* z z))
	       (xx (- ax pi/4))
	       (p0			;Pzero 6546
		(/ (poly-by-coeffs->value y
		     +.8554822541506661710252074e+4
		     +.8894437532960619440762804e+4
		     +.2204501043965180428995069e+4
		     +.1286775857487141932988510e+3
		     +.9004793474802880316384000e+0)
		   (poly-by-coeffs->value y
		     +.8554822541506662842462151e+4
		     +.8903836141709595355210343e+4
		     +.2214048851914710419825683e+4
		     +.1308849004999238828351090e+3
		     +.1e+1)))
	       (q0			;Qzero 6946
		(/ (poly-by-coeffs->value y
		     -.37510534954957111594836e+2
		     -.46093826814625174976377e+2
		     -.13990976865960680088016e+2
		     -.10497327982345548331260e+1
		     -.93525953294031893049e-2)
		   (poly-by-coeffs->value y
		     +.2400674237117267479318819e+4
		     +.2971983745208491990065486e+4
		     +.921566975526530895082307e+3
		     +.74428389741411178824152e+2
		     +.1e1))))
	  (* (sqrt (/ 2/pi ax))
	     (+ (* (sin xx) p0)
		(* z (cos xx) q0)))))))

(define (bessy1 x)
  (let ((ax (magnitude x)))
    (if (< ax 8.0)
	(let* ((y (* x x))		;Yone 6442
	       (r0
		(poly-by-coeffs->value y
                     -.2493247725431151099221863985e+8  ;p00
		     +.678814979608784027013965029e+7   ;p01
		     -.3418758685257648961162234201e+6  ;p02
		     +.731876663732252704384675659e+4   ;p03
		     -.8477929772037826827977473917e+2  ;p04
		     +.5973109455969101918912869725e+0  ;p05
		     -.2723714918114737334647812804e-2  ;p06
		     +.8253358475754237969740284405e-5  ;p07
		     -.1645797675540583390670878295e-7  ;p08
		     +.2013974632712911344982964612e-10 ;p09
		     -.118503924369772697029706524e-13  ;p10
		     ))
	       (s0
		(poly-by-coeffs->value y
                     +.1271694748306711445338693265e+9  ;q00
		     +.6291245810783959009675422035e+6  ;q01
		     +.1241151964683171602676430057e+4  ;q02
		     +1.0                               ;q03
		     )))
	  (+ (/ (* x r0) s0)
	     (* 2/pi
		(- (* (bessj1 x) (log x))
		   (/ 1.0 x)))))
	(let* ((z (/ 8.0 ax))		
	       (y (* z z))
	       (xx (- ax 3pi/4))
	       (p1			;Pone 6747
		(/ (poly-by-coeffs->value y
		     +.1290918471896188077350689e+5
		     +.1309042051103506486292571e+5
		     +.313275295635506951011069e+4
		     +.17431379748379024599685e+3
		     +.122850537643590432633e+1)
		   (poly-by-coeffs->value y
		     +.1290918471896187879332737e+5
		     +.1306678308784402036110575e+5
		     +.310928141677002883350924e+4
		     +.16904721775008609992033e+3
		     +.1e+1)))
	       (q1			;Qone 7147
		(/ (poly-by-coeffs->value y
		     +.14465282874995208675225e+3
		     +.1744291689092425885102e+3
		     +.5173653281836591636536e+2
		     +.379944537969806734901e+1
		     +.36363466476034710809e-1)
		   (poly-by-coeffs->value y
		     +.308592701333231723110639e+4
		     +.373434010601630179517765e+4
		     +.11191098527047487025919e+4
		     +.8522392064341340397334e+2
		     +.1e+1))))
	  (* (sqrt (/ 2/pi ax))
	     (+ (* (sin xx) p1)
		(* z (cos xx) q1)))))))

#|
;;; These coefficients are from Numerical Recipes... not so good... 7digits.

(define (bessy0 x)
  (let ((ax (magnitude x)))
    (if (< ax 8.0)
	(let* ((y (* x x))
	       (r0
		(poly-by-coeffs->value y
				       -2957821389.0
				       +7062834065.0
				       -512359803.6
				       +10879881.29
				       -86327.92757
				       +228.4622733))
	       (s0
		(poly-by-coeffs->value y
				       +40076544269.0
				       +745249964.8
				       +7189466.438
				       +47447.26470
				       +226.1030244
				       +1.0)))
	  (+ (/ r0 s0)
	     (* 2/pi (bessj0 x) (log x))))
	(let* ((z (/ 8.0 ax))		
	       (y (* z z))
	       (xx (- ax pi/4))
	       (p0
		(poly-by-coeffs->value y
				       +1.0
				       -0.1098628627e-2
				       +0.2734510407e-4
				       -0.2073370639e-5
				       +0.2093887211e-6))
	       (q0
		(poly-by-coeffs->value y
				       -0.1562499995e-1
				       +0.1430488765e-3
				       -0.6911147651e-5
				       +0.7621095161e-6
				       -0.934945152e-7)))
	  (* (sqrt (/ 2/pi ax))
	     (+ (* (sin xx) p0)
		(* z (cos xx) q0)))))))

(define (bessy1 x)
  (let ((ax (magnitude x)))
    (if (< ax 8.0)
	(let* ((y (* x x))
	       (r0
		(poly-by-coeffs->value y
				       -0.4900604943e13
				       +0.1275274390e13
				       -0.5153438139e11
				       +0.7349264551e9
				       -0.4237922726e7
				       +0.8511937935e4))
	       (s0
		(poly-by-coeffs->value y
				       +0.2499580570e14
				       +0.4244419664e12
				       +0.3733650367e10
				       +0.2245904002e8
				       +0.1020426050e6
				       +0.3549632885e3
				       +1.0)))
	  (+ (/ (* x r0) s0)
	     (* 2/pi
		(- (* (bessj1 x) (log x))
		   (/ 1.0 x)))))
	(let* ((z (/ 8.0 ax))		
	       (y (* z z))
	       (xx (- ax 3pi/4))
	       (p0
		(poly-by-coeffs->value y
				       +1.0
				       +0.183105e-2
				       -0.3516396496e-4
				       +0.2457520174e-5
				       -0.240337019e-6))
	       (q0
		(poly-by-coeffs->value y
				       +0.04687499995
				       -0.2002690873e-3
				       +0.8449199096e-5
				       -0.88228987e-6
				       +0.105787412e-6)))
	  (* (sqrt (/ 2/pi ax))
	     (+ (* (sin xx) p0)
		(* z (cos xx) q0)))))))
|#

(define (bessy n x)
  (cond ((fix:= n 0) (bessy0 x))
	((fix:= n 1) (bessy1 x))
	((not (> x 0))
	 (error "Argument out of range -- Bessel Y" n x))
	((fix:< n 0)
	 (if (even? n) (bessy (- n) x) (- (bessy (- n) x))))	
	(else
	 (let lp ((i 1) (yn (bessy1 x)) (yn-1 (bessy0 x)))
	   (if (fix:= i n)
	       yn
	       (lp (fix:+ i 1)
		   (- (/ (* 2 i yn) x) yn-1)
		   yn))))))


(define (bessh n x)
  (+ (bessj n x) (* +i (bessy n x))))

#|

(define (bessh:n<<x n x)
  (* (sqrt (/ 2 pi z))
     (exp (* +i (- z (* pi/2 n) pi/4)))))

|#

#|
;;; Consistency check based on Wronskian:

;;; J_{n+1}(x) Y_n(x) - J_n(x) Y_{n+1}(x) = 2/(pi x)

(define (bessel-check n x)
  (/ (- (* (bessj (+ n 1) x) (bessy n x))
	(* (bessj n x) (bessy (+ n 1) x))
	(/ 2 (* pi x)))
     (/ 2 (* pi x))))


(let lp ((x .0001) (worstx 0.0) (relerr 0.0))
  (if (> x 20)
      (list `(worst-x ,worstx relative-error ,relerr))
      (let ((err (bessel-check 0 x)))
	(if (> (magnitude err) (magnitude relerr))
	    (lp (+ x .0001) x err)
	    (lp (+ x .0001) worstx relerr)))))
;Value: ((worst-x 7.950999999994808 relative-error -1.1214144656680372e-13))

;;; Interestingly, there appears to be a problem just below 8.0, because 
;;; except near here the errors are close to roundoff noise.


(define win
  (frame 7.90 8.00 -2e-13 2e-13))

(plot-function win
	       (lambda (x)
		 (bessel-check 0 x))
	       7.90
	       8.0
	       0.000001)

(graphics-clear win)
(graphics-close win)


(define win
  (frame 0.0 20.0 -2e-13 2e-13))

(plot-function win
	       (lambda (x)
		 (bessel-check 0 x))
	       0.1
	       20.0
	       0.0001)

;;; Graph is stored in bessel-error.jpg


(define (foo z)				;Approx to J0 9.1.18 A&S
  (/ (integrate-closed-closed
      (lambda (theta)
	(cos (* z (sin theta))))
      0.0
      pi
      1e-16)
     pi))


(define win
  (frame 0.0 20.0 -2e-15 2e-15))




|#