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

(define (continued-fraction x)
  (let ((a (floor->exact x)))
    (cons-stream a (continued-fraction (/ 1 (- x a))))))

(define (convergents cfrac)
  (define (gen A1 A2 B1 B2 cfrac)
    (let ((b (head cfrac)))
      (let ((An (+ (* b A1) A2))
	    (Bn (+ (* b B1) B2)))
	(cons-stream (/ An Bn)
		     (gen An A1 Bn B1 (tail cfrac))))))
  (let ((b (head cfrac)))
    (cons-stream b
		 (gen b 1 1 0 (tail cfrac)))))


(define stream-ones (cons-stream 1 stream-ones))

#|

(print-stream (continued-fraction pi) 10)
3
7
15
1
292
1
1
1
2
1

(print-stream (continued-fraction (exp 1)) 30)

2
1
2
1
1
4
1
1
6
1
1
8
1
1
10
1
1
12
1
1
11 xxx
3
2

(print-stream
 (convergents
  (continued-fraction pi))
 10)
3
22/7
333/106
355/113
103993/33102
104348/33215
208341/66317
312689/99532
833719/265381
1146408/364913
;Value: ...


(print-stream
 (map-stream 
  (lambda (x) (/ (- x pi) pi))
  (convergents
   (continued-fraction pi)))
 10)
-.04507034144862795
4.024994347707008e-4
-2.648963016704766e-5
8.49136787674061e-8
-1.8394829882935047e-10
1.0556048950798648e-10
-3.894723497538627e-11
9.276617820935258e-12
-2.7741504721654008e-12
5.127054146519189e-13
;Value: ...

(print-stream
 (convergents
  stream-ones)
 10)
1
2
3/2
5/3
8/5
13/8
21/13
34/21
55/34
89/55
;Value: ...

(print-stream
 (map-stream 
  (lambda (x) 
    (let ((gm (/ (+ 1 (sqrt 5)) 2)))
      (/ (- x gm) gm)))
  (convergents stream-ones))
 10)

(print-stream
 (map-stream 
  (lambda (x) 
    (let ((gm (/ (+ 1 (sqrt 5)) 2)))
      (/ (- x gm) gm)))
  (convergents stream-ones))
 10)
-.38196601125010515
.23606797749978967
-.07294901687515776
3.0056647916491423e-2
-.01114561800016822
4.305231718579094e-3
-1.6374027886314115e-3
6.264579760202099e-4
-2.391358457583512e-4
9.136361346616536e-5
;Value: ...


(print-stream
 (continued-fraction (sqrt 2))
 20)
1
2
2
2
2
2
2
2
2
2
2
2
2
2
2
2
2
2
2
2
;Value: ...

(print-stream
 (convergents
  (continued-fraction (sqrt 2)))
 10)
1
3/2
7/5
17/12
41/29
99/70
239/169
577/408
1393/985
3363/2378
;Value: ...

(print-stream
 (map-stream 
  (lambda (x) 
    (let ((r2 (sqrt 2)))
      (/ (- x r2) r2)))
  (convergents
   (continued-fraction (sqrt 2))))
 10)
-.29289321881345254
.06066017177982121
-1.0050506338833596e-2
.00173460668094231
-2.973093569501634e-4
5.101910668863161e-5
-8.753233225833026e-6
1.5018250929351827e-6
-2.576722227077141e-7
4.420957066726649e-8
;Value: ...

;; check that the error is less than 1/d^2
(print-stream
 (let ((number (sqrt 2)))
   (map-stream 
    (lambda (c)
      (let ((error (abs (- c number))))
	(- error (/ 1 (square (denominator c))))))
    (convergents 
     (continued-fraction number))))
 10)
; ok
(print-stream
 (let ((number (/ (+ 1 (sqrt 5)) 2)))
   (map-stream 
    (lambda (c)
      (let ((error (abs (- c number))))
	(- error (/ 1 (square (denominator c))))))
    (convergents 
     (continued-fraction number))))
 10)
;ok

(print-stream
 (let ((number pi))
   (map-stream 
    (lambda (c)
      (let ((error (abs (- c number))))
	(- error (/ 1 (square (denominator c))))))
    (convergents 
     (continued-fraction number))))
 20)
-.8584073464102069
-1.9143673997956443e-2
-5.780016472316501e-6
-7.804790414797462e-5
-3.347326313231945e-10
-5.747961301800781e-10
-1.0502260270843243e-10
-7.179925756609443e-11
-5.4838338802403175e-12
-5.89896667061906e-12
-1.3644086097276364e-13
-3.1384653391315194e-13
-1.0925052623624082e-15
-1.6328867414587005e-16
-1.4760984937032654e-17
-2.718809562442266e-20
-2.499649407750906e-20
-6.51444233883359e-21
-1.154885932677522e-22
-6.760478215475832e-24
;Value: ...

|#


