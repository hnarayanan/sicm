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

;;;; Structure iterators

(declare (usual-integrations))

;;;           Structural Lists

#|
(define (generate-list n proc) ; ==> ( (proc 0) (proc 1) ... (proc n-1) )
  (let loop ((i (fix:- n 1)) (list '()))
    (if (fix:< i 0)
        list
        (loop (fix:- i 1) (cons (proc i) list)))))
|#

(define generate-list make-initialized-list)
(define list:generate make-initialized-list)

(define (list-with-substituted-coord lst i x)
  (append (list-head lst i)
	  (list x)
	  (cdr (list-tail lst i))))

;;;           Structural Vectors
#|
;;; Scheme supplies
vector-ref
vector-set!
vector
make-initialized-vector

(define (generate-vector size proc)
  (let ((ans (make-vector size)))
    (let loop ((i 0))
      (if (fix:= i size)
	  ans
	  (begin (vector-set! ans i (proc i))
		 (loop (fix:+ i 1)))))))
|#

(define generate-vector make-initialized-vector)

(define ((vector-elementwise f) . vectors)
  (make-initialized-vector
    (vector-length (car vectors))
    (lambda (i)
      (apply f
	     (map (lambda (v) (vector-ref v i))
		  vectors)))))

(define (vector-forall p? . vectors)
  (let lp ((i (fix:- (vector-length (car vectors)) 1)))
    (cond ((fix:= i 0)
	   (apply p? (map (lambda (v) (vector-ref  v i))
			  vectors)))
	  ((apply p? (map (lambda (v) (vector-ref  v i))
			  vectors))
	   (lp (fix:- i 1)))
	  (else #f))))

(define (vector-exists p? . vectors)
  (let lp ((i (fix:- (vector-length (car vectors)) 1)))
    (cond ((fix:= i 0)
	   (apply p? (map (lambda (v) (vector-ref  v i))
			  vectors)))
	  ((apply p? (map (lambda (v) (vector-ref  v i))
			  vectors))
	   #t)
	  (else 
	   (lp (fix:- i 1))))))


(define (vector-accumulate acc fun init v)
  (let ((l (vector-length v)))
    (if (fix:= l 0)
	init
	(let loop ((i 1)
		   (ans (fun (vector-ref v 0))))
	  (if (fix:= i l)
	      ans
	      (loop (fix:+ i 1)
		    (acc ans (fun (vector-ref v i)))))))))


(define (vector-with-substituted-coord v i x)
  (make-initialized-vector (vector-length v)
    (lambda (j)
      (if (fix:= j i)
	  x
	  (vector-ref v j)))))

;;;      Structural 2-dimensional arrays

;;; Structrure procedures -- operate on raw array material

(define (array-ref m i j)
  (vector-ref (vector-ref m i) j))

(define (array-set! m i j v)
  (vector-set! (vector-ref m i) j v))

(define (generate-array rows cols proc)
  (make-initialized-vector
   rows
   (lambda (row)
     (make-initialized-vector
      cols
      (lambda (col)
	(proc row col))))))
    
(define ((array-elementwise f) . arrays)
  (generate-array
    (num-rows (car arrays))
    (num-cols (car arrays))
    (lambda (i j)
      (apply f
	     (map (lambda (m)
		    (array-ref m i j))
		  arrays)))))

(define (array-copy m)
  (generate-array (num-rows m) (num-cols m)
    (lambda (i j) (array-ref m i j))))


(define (num-rows array)
  (vector-length array))

(define (num-cols array)
  (vector-length (vector-ref array 0)))


(define (nth-row M n) ;return as a vector
  (vector-ref M n))

(define (nth-col M j)
  (generate-vector (num-rows M)
		   (lambda (i)
		     (array-ref M i j))))

(define (array-with-substituted-row A i V)
  (vector-with-substituted-coord A i V))

(define (array-with-substituted-col A k V)
  (generate-array (num-rows A) (num-cols A)
    (lambda (i j)
      (if (fix:= j k)
	  (vector-ref v i)
	  (array-ref A i j)))))

(define (array-by-rows M)
  (apply vector (map list->vector M)))

(define (array-by-cols M)
  (transpose-array (array-by-rows M)))

(define (transpose-array M)
  (generate-array (num-cols M) (num-rows M)
    (lambda (i j) (array-ref M j i))))

  