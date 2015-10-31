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

;;; The following order incrementer was mentioned in a lecture by
;;;  William Kahan on 16 October 1993 at the Fields Institute in
;;;  Waterloo, Canada.  He claimed that this made any reflexive 
;;;  integration method into a higher order reflexive method.



(define (ordinc:aa-baa alpha beta)
  (lambda (oldmethod)
    (define (newmethod . args)
      (let ((oldmap (apply oldmethod args)))
	(define (newmap state)
	  (define (newstep h succeed fail)
	    (let ((ah (* alpha h)))
	      ((oldmap state)
	       ah
	       (lambda (state1 n1)
		 ((oldmap state1)
		  ah
		  (lambda (state2 n2)
		    ((oldmap state2)
		     (* -1 beta h)
		     (lambda (state3 n3)
		       ((oldmap state3)
			ah
			(lambda (state4 n4)
			  ((oldmap state4)
			   ah
			   (lambda (state5 n5)
			     (succeed state5
				      (+ n1 n2 n3 n4 n5)))
			   fail))
			fail))
		     fail))
		  fail))
	       fail)))
	  newstep)
	newmap))
    newmethod))

(define ordinc/kahan
  ;; (assert (= (- (* 4 a) b) 1))
  ;; (assert (= (- (* 4 (expt a 3)) (expt b 3)) 0))
  (ordinc:aa-baa (/ 1 (- 4 (expt 4 1/3)))
		 (/ (expt 4 1/3) (- 4 (expt 4 1/3)))))


#|
;;; For example:

((advance-generator
  ((quality-control n-trapezoid 2)	;integration method
   (lambda (v cont)			;x' = x
     (cont v #(#(1.0))))
   0.0001				;qc-error tolerated
   1					;state dimension
   1.0e-5))				;corrector convergence
 #(1.0)					;initial state (at t = t0)
 1.0					;proceed to t = t0 + 1
 0.1					;first step no larger than .1
 0.5					;no step larger than .5
 (lambda (ns dt h cont)
   (pp ns)
   (cont))
 (lambda (ns dt sdt)
   ;; assert ns = #(2.718...)
   ;; assert dt = 1.000...+-
   (list ns dt sdt)))
#(1.)
#(1.1051708824988178)
#(1.259109256732086)
#(1.4307187104000767)
#(1.6219876372976312)
#(1.8350462370303233)
#(2.0722642596439016)
#(2.3362773683519777)
#(2.630016590666874)
;Value 4: (#(2.718279922395027) 1. .11684285320335219)

((advance-generator
  ((quality-control (ordinc/kahan n-trapezoid) 4)	;integration method
   (lambda (v cont)			;x' = x
     (cont v #(#(1.0))))
   0.0001				;qc-error tolerated
   1					;state dimension
   1.0e-5))				;corrector convergence
 #(1.0)					;initial state (at t = t0)
 1.0					;proceed to t = t0 + 1
 0.1					;first step no larger than .1
 0.5					;no step larger than .5
 (lambda (ns dt h cont)
   (pp ns)
   (cont))
 (lambda (ns dt sdt)
   ;; assert ns = #(2.718...)
   ;; assert dt = 1.000...+-
   (list ns dt sdt)))
#(1.)
#(1.105170918076203)
#(1.822118873940917)
;Value 5: (#(2.7182819609566646) 1. .6191614739685206)

;;; Order incrementation is not very valuable with low-precision
;;; and large steps, but ---
;;; In the next example, where the step is small
;;;  we get 40 times the step size for only 5 times the work:

((advance-generator
  ((quality-control c-trapezoid 2)	;integration method
   (lambda (v) v)			;x' = x
   1e-10				;qc error tolerated
   1.0e-14))				;corrector convergence
 #(1.0)					;initial state (at t = t0)
 1.0					;proceed to t = t0 + 1
 0.1					;first step no larger than .1
 0.5					;no step larger than .5
 (lambda (ns dt h cont)
   (pp ns)
   (cont))
 (lambda (ns dt sdt)
   ;; assert ns = #(2.718...)
   ;; assert dt = 1.000...+-
   (list ns dt sdt)))
#(1.)
#(1.0013048975052876)
#(1.0026334717515324)
#(1.003963514294337)
;;;... a zillion steps
#(2.7088039676905655)
#(2.7119720704653294)
#(2.71514354396058)
;Value 18: (#(2.7182818284589105) 1. 1.1685081910932198e-3)


((advance-generator
  ((quality-control (ordinc/kahan c-trapezoid) 4)	;integration method
   (lambda (v) v)			;x' = x
   1e-10				;qc error tolerated
   1.0e-14))				;corrector convergence
 #(1.0)					;initial state (at t = t0)
 1.0					;proceed to t = t0 + 1
 0.1					;first step no larger than .1
 0.5					;no step larger than .5
 (lambda (ns dt h cont)
   (pp ns)
   (cont))
 (lambda (ns dt sdt)
   ;; assert ns = #(2.718...)
   ;; assert dt = 1.000...+-
   (list ns dt sdt)))
#(1.)
#(1.0427739743980482)
#(1.0876422117483189)
#(1.1342464187255308)
#(1.1826506633852774)
#(1.2329214198807332)
#(1.285127653928507)
#(1.3393409047561384)
#(1.3956353449351588)
#(1.454088054093883)
#(1.5147789241479352)
#(1.5777908760514507)
#(1.643209919435115)
#(1.7111253803933226)
#(1.7816298116681306)
#(1.8548193541820144)
#(1.930793770860625)
#(2.0096564104059995)
#(2.0915147920691073)
#(2.176480233700457)
#(2.264668388028462)
#(2.356199389461111)
#(2.451197472160678)
#(2.549791982801217)
#(2.6521168244562565)
;Value 17: (#(2.71828182845908) 1. .0392083162662634)


;;; By comparision, 
;;;  RK4 does 4 derivs/step
;;;  Kahan's order incrementer on a pre-solved trapezoid does
;;;   10 derivatives/step.
;;;  So RK4 is actually better here.

((advance-generator
  ((quality-control rk4 4)		;integration method
   (lambda (v) v)			;x' = x
   1e-10))				;error tolerated
 #(1.0)					;initial state (at t = t0)
 1.0					;proceed to t = t0 + 1
 1.0					;first step no larger than .1
 0.5					;no step larger than .5
 (lambda (ns dt h cont)
   (pp ns)
   (cont))
 (lambda (ns dt sdt)
   ;; assert ns = #(2.718...)
   ;; assert dt = 1.000...+-
   (list ns dt sdt)))
#(1.)
#(1.0288641270359913)
#(1.0573808628038246)
#(1.0866028952728164)
#(1.1165521074874276)
#(1.1472457790033497)
#(1.1787016001122053)
#(1.2109376670073848)
#(1.2439725445479952)
#(1.2778252219689556)
#(1.3125151338299665)
#(1.348062195185723)
#(1.3844868290249117)
#(1.4218099143751641)
#(1.4600528732516973)
#(1.4992375982084383)
#(1.539386555897929)
#(1.5805227175506802)
#(1.622669656685209)
#(1.665851482523242)
#(1.7100928938366995)
#(1.7554192006945573)
#(1.8018562983432844)
#(1.849430742587183)
#(1.8981696907608068)
#(1.9481010081550414)
#(1.9992532143508546)
#(2.051655492521119)
#(2.10533776741998)
#(2.1603306441207266)
#(2.216665531967081)
#(2.274374563258408)
#(2.3334906709463925)
#(2.3940475644598562)
#(2.456079733164678)
#(2.519622563252926)
#(2.5847122979489776)
#(2.651385996809997)
;Value 21: (#(2.718281828451157) 1. 2.5395554474610716e-2)




|#