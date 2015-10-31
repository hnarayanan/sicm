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

;;; In active region, a mosfet has the characteristic:

(define ((iD-sat K VT) vG)
  (* 1/2 K (square (- vG VT))))


(- ((iD-sat 'K 'V_T) (- 'v_I (* 'i_D 'R_S))) 'i_D)
#|
(+ (* 1/2 K (expt R_S 2) (expt i_D 2))
   (* K R_S V_T i_D)
   (* -1 K R_S i_D v_I)
   (* 1/2 K (expt V_T 2))
   (* -1 K V_T v_I)
   (* 1/2 K (expt v_I 2))
   (* -1 i_D))
|#

(let ((K 'K) (RS 'R_S) (id 'i_D) (vt 'V_T) (vi 'v_I))
  (-
   (- ((iD-sat K vt) (- vi (* id RS))) id)
   (+ (* 1/2 K (square RS) (square id))
      (* -1 (+ (* K RS (- vi vt)) 1) id)
      (* 1/2 K (square (- vi vt))))))

(define (quadratic-solution a b c)
  (list (/ (+ (- b) (sqrt (- (square b) (* 4 a c))))
	   (* 2 a))
	(/ (- (- b) (sqrt (- (square b) (* 4 a c))))
	   (* 2 a))))

(let ((K 'K) (RS 'R_S) (vt 'V_T) (vi 'v_I))
  (quadratic-solution (* 1/2 K (square RS))
		      (* -1 (+ (* K RS (- vi vt)) 1))
		      (* 1/2 K (square (- vi vt)))))




(let ((K (& 'K (/ ampere volt)))
      (RS (& 'R_S ohm))
      (id (& 'i_D ampere))
      (vt (& 'V_T volt))
      (vi (& 'v_I volt)))
  (-
   (- ((iD-sat K vt) (- vi (* id RS))) id)
   (+ (* 1/2 K (square RS) (square id))
      (* -1 (+ (* K RS (- vi vt)) 1) id)
      (* 1/2 K (square (- vi vt))))))


(let ((K (& 'K (/ ampere (square volt))))
      (RS (& 'R_S ohm))
      (id (& 'i_D ampere))
      (vt (& 'V_T volt))
      (vi (& 'v_I volt)))
  ((iD-sat K vt) (- vi (* id RS))))



(let ((K (& 'K (/ ampere (square volt))))
      (RS (& 'R_S ohm))
      (id (& 'i_D ampere))
      (vt (& 'V_T volt))
      (vi (& 'v_I volt)))
  (-
   (- ((iD-sat K vt) (- vi (* id RS))) id)
   (+ (* 1/2 K (square RS) (square id))
      (* -1 (+ (* K RS (- vi vt)) 1) id)
      (* 1/2 K (square (- vi vt))))))



(let ((K (& 'K (/ ampere (square volt))))
      (RS (& 'R_S ohm))
      (vt (& 'V_T volt))
      (vi (& 'v_I volt)))
  (quadratic-solution (* 1/2 K (square RS))
		      (* -1 (+ (* K RS (- vi vt)) 1))
		      (* 1/2 K (square (- vi vt)))))
#|
((&
  (/ (+ 1 (* -1 K R_S V_T) (* K R_S v_I)
	(sqrt (+ 1 (* -2 K R_S V_T) (* 2 K R_S v_I))))
     (* K (expt R_S 2)))
  ampere)
 (&
  (/ (+ 1 (* -1 K R_S V_T) (* K R_S v_I)
	(* -1 (sqrt (+ 1 (* -2 K R_S V_T) (* 2 K R_S v_I)))))
     (* K (expt R_S 2)))
  ampere))
|#


;;; Power MOSFET

(let ((K (& 2 (/ ampere (square volt))))
      (RS (& 8 ohm))
      (vt (& 2 volt))
      (vi (& 3 volt)))
  (quadratic-solution (* 1/2 K (square RS))
		      (* -1 (+ (* K RS (- vi vt)) 1))
		      (* 1/2 K (square (- vi vt)))))

#|
((& .17769189567607835 ampere) (& .08793310432392165 ampere))
|#

(let ((K (& 2 (/ ampere (square volt))))
      (RS (& 20 ohm))
      (vt (& 2 volt))
      (vi (& 6.5 volt)))
  (quadratic-solution (* 1/2 K (square RS))
		      (* -1 (+ (* K RS (- vi vt)) 1))
		      (* 1/2 K (square (- vi vt)))))
#|
((& .25 ampere) (& .2025 ampere))
|#



(let ((K (& 2 (/ ampere (square volt))))
      (RS (& 8 ohm))
      (vt (& 2 volt))
      (vi (& 2 volt)))
  (quadratic-solution (* 1/2 K (square RS))
		      (* -1 (+ (* K RS (- vi vt)) 1))
		      (* 1/2 K (square (- vi vt)))))
#|
((& 1/64 ampere) (& 0 ampere))
|#
;;; So negative root is correct.

;;; vO 
(let ((K (& 2 (/ ampere (square volt))))
      (RS (& 8 ohm))
      (vt (& 2 volt))
      (vi (& 3 volt)))
  (* RS
     (cadr
      (quadratic-solution (* 1/2 K (square RS))
			  (* -1 (+ (* K RS (- vi vt)) 1))
			  (* 1/2 K (square (- vi vt)))))))
#|
(& .7034648345913732 volt)
|#

(let ((K (& 2 (/ ampere (square volt))))
      (RS (& 8 ohm))
      (vt (& 2 volt))
      (vi (& 4 volt)))
  (* RS
     (cadr
      (quadratic-solution (* 1/2 K (square RS))
			  (* -1 (+ (* K RS (- vi vt)) 1))
			  (* 1/2 K (square (- vi vt)))))))
#|
(& 1.5586088907313407 volt)
|#

;;; Source Follower

(define (vOS vI)
  (let ((K (& 2 (/ ampere (square volt))))
	(RS (& 8 ohm))
	(VT (& 2 volt)))
    (* RS
       (cadr
	(quadratic-solution (* 1/2 K (square RS))
			    (* -1 (+ (* K RS (- vI VT)) 1))
			    (* 1/2 K (square (- vI VT))))))))

(vOS (& 4 volt))
#|
(& 1.863490283019151 volt)
|#


((D vOS) (& 4 volt))
#| .8759652654107916 |#

((D vOS) (& 3 volt))
#| .8259223440443022 |#

((D vOS) (& 5 volt))
#| .8984653834866381 |#

;;; Symbolic

(define (vOSs vI)
  (let ((K (& 'K (/ ampere (square volt))))
	(RS (& 'R_S ohm))
	(VT (& 'V_T volt)))
    (* RS
       (cadr
	(quadratic-solution (* 1/2 K (square RS))
			    (* -1 (+ (* K RS (- vI VT)) 1))
			    (* 1/2 K (square (- vI VT))))))))
(vOSs (& 'v_I volt))
#|
(&
 (/ (+ 1 (* -1 K R_S V_T) (* K R_S v_I)
       (* -1 (sqrt (+ 1 (* -2 K R_S V_T) (* 2 K R_S v_I)))))
    (* K R_S))
 volt)
|#

((D vOSs) (& 'v_I volt))
#|
(+ 1 (/ -1 (sqrt (+ 1 (* -2 K R_S V_T) (* 2 K R_S v_I)))))
|#



;;; Common Source Voltage Amplifier with source degeneration

(define (vOD vI)
  (let ((K (& 2 (/ ampere (square volt))))
	(VDD (& 50 volt))
	(RS (& 20 ohm))
	(RL (& 100 ohm))
	(VT (& 2 volt)))
    (- VDD
       (* RL
	  (cadr
	   (quadratic-solution (* 1/2 K (square RS))
			       (* -1 (+ (* K RS (- vI VT)) 1))
			       (* 1/2 K (square (- vI VT)))))))))

(vOD (& 6.5 volt))
#|
(& 29.75 volt)
|#


((D vOD) (& 6.5 volt))
#| -4.7368421052631575 |#

(define (vODs vI)
  (let ((K (& 'K (/ ampere (square volt))))
	(VDD (& 'VDD volt))
	(RS (& 'R_S ohm))
	(RL (& 'R_L ohm))
	(VT (& 'V_T volt)))
    (- VDD
       (* RL
	  (cadr
	   (quadratic-solution (* 1/2 K (square RS))
			       (* -1 (+ (* K RS (- vI VT)) 1))
			       (* 1/2 K (square (- vI VT)))))))))


((D vODs) (& 'V_I volt))
#|
(/ (+ (* -1 R_L (sqrt (+ 1 (* 2 K R_S V_I) (* -2 K R_S V_T)))) R_L)
   (* R_S (sqrt (+ 1 (* 2 K R_S V_I) (* -2 K R_S V_T)))))
|#


;;; Differential Amplifier

#|

(let ((K 'K) (VT 'VT) (I 'I) (e1 'e1) (e2 'e2) (e3 'e3))
  (- (+ (* 1 (square e3))
	(* (- (* 2 VT) e1 e2) e3)
	(+ (square VT)
	   (* -1 (+ e1 e2) VT)
	   (* 1/2 (+ (square e1) (square e2)))
	   (* -1 (/ I K))))
     (/ (+ ((iD-sat K VT) (- e1 e3))
	   ((iD-sat K VT) (- e2 e3))
	   (* -1 I))
	K)))
#| 0 |#


(let ((K 'K) (VT 'VT) (I 'I) (e1 'e1) (e2 'e2) )
  (let ((e3
	 (cadr
	  (quadratic-solution
	   1
	   (- (* 2 VT) e1 e2)
	   (+ (square VT)
	      (* -1 (+ e1 e2) VT)
	      (* 1/2 (+ (square e1) (square e2)))
	      (* -1 (/ I K)))))))
    (- e3
       (+ (/ (+ e1 e2) 2)
	  (* -1 VT)
	  (* -1
	     (sqrt (- (/ I K)
		      (square (/ (- e1 e2) 2)))))))))
#| 0 |#

(let ((K 'K) (VT 'VT) (I 'I) (e1 'e1) (e2 'e2) )
  (let ((e3
	 (cadr
	  (quadratic-solution
	   1
	   (- (* 2 VT) e1 e2)
	   (+ (square VT)
	      (* -1 (+ e1 e2) VT)
	      (* 1/2 (+ (square e1) (square e2)))
	      (* -1 (/ I K)))))))
    (let ((i1 ((iD-sat K VT) (- e1 e3)))
	  (i2 ((iD-sat K VT) (- e2 e3))))
      (+ i1 i2))))
#| I |#

(let ((K 'K) (VT 'VT) (I 'I) (e1 'e1) (e2 'e2) )
  (let ((e3
	 (cadr
	  (quadratic-solution
	   1
	   (- (* 2 VT) e1 e2)
	   (+ (square VT)
	      (* -1 (+ e1 e2) VT)
	      (* 1/2 (+ (square e1) (square e2)))
	      (* -1 (/ I K)))))))
    (let ((i1 ((iD-sat K VT) (- e1 e3)))
	  (i2 ((iD-sat K VT) (- e2 e3))))
      i1)))
#|
(/ (+ (*
       (- e1 e2)
       (sqrt
	(+ (* -1 (expt K 2) (expt e1 2))
	   (* 2 (expt K 2) e1 e2)
	   (* -1 (expt K 2) (expt e2 2))
	   (* 4 I K))))
      (* 2 I))
   4)
|#

;;; eD = (e1-e2)/2; eC = (e1+e2)/2

(let ((eD 'eD) (eC 'eC))
  (let ((K 'K) (VT 'VT) (I 'I) (e1 (+ eC eD)) (e2 (- eC eD) ))
    (let ((e3
	   (cadr
	    (quadratic-solution
	     1
	     (- (* 2 VT) e1 e2)
	     (+ (square VT)
		(* -1 (+ e1 e2) VT)
		(* 1/2 (+ (square e1) (square e2)))
		(* -1 (/ I K)))))))
      (let ((i1 ((iD-sat K VT) (- e1 e3)))
	    (i2 ((iD-sat K VT) (- e2 e3))))
	i1))))
#|
(/ (+ (* eD
	 (sqrt (+ (* -4 (expt K 2) (expt eD 2))
		  (* 4 I K))))
      I)
   2)
|#

(let ((eD 'eD) (eC 'eC))
  (let ((K 'K) (VT 'VT) (I 'I) (e1 (+ eC eD)) (e2 (- eC eD) ))
    (let ((e3
	   (cadr
	    (quadratic-solution
	     1
	     (- (* 2 VT) e1 e2)
	     (+ (square VT)
		(* -1 (+ e1 e2) VT)
		(* 1/2 (+ (square e1) (square e2)))
		(* -1 (/ I K)))))))
      (let ((i1 ((iD-sat K VT) (- e1 e3)))
	    (i2 ((iD-sat K VT) (- e2 e3))))
	i2))))
#|
(+ (/ (* -1
	 eD
	 (sqrt (+ (* -4 (expt K 2) (expt eD 2)) (* 4 I K))))
      2)
   (/ I 2))
|#


|#