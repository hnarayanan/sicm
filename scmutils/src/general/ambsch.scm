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

;;;; Extension of Scheme for amb
;;;   amb is the ambiguous operator of McCarthy.

;;; (load "stack-queue.scm")

(define-syntax amb
  (sc-macro-transformer
   (lambda (form uenv)
     `(amb-list
       (list ,@(map (lambda (arg)
		      `(lambda ()
			 ,(close-syntax arg uenv)))
		    (cdr form)))))))

(define *number-of-calls-to-fail* 0)	;for metering.

(define (amb-list alternatives)
  (if (null? alternatives)
      (set! *number-of-calls-to-fail*
	    (+ *number-of-calls-to-fail* 1)))
  (call-with-current-continuation
   (lambda (k)
     (add-to-search-schedule
      (map (lambda (alternative)
             (lambda ()
               (within-continuation k alternative)))
           alternatives))
     (yield))))


;;; amb-set! is an assignment operator
;;;  that gets undone on backtracking.

(define-syntax amb-set!
  (sc-macro-transformer
   (lambda (form uenv)
     (compile-amb-set (cadr form) (caddr form) uenv))))

(define (compile-amb-set var val-expr uenv)
  (let ((var (close-syntax var uenv))
        (val (close-syntax val-expr uenv)))
    `(let ((old-value ,var))
       (effect-wrapper
        (lambda ()
          (set! ,var ,val))
        (lambda ()
          (set! ,var old-value))))))


;;; A general wrapper for undoable effects, when doing depth-first only!

(define (effect-wrapper doer undoer)
  (assert (eq? add-to-search-schedule add-to-depth-first-search-schedule)
	  "Undoable effects make sense only in depth-first search")
  (force-next
   (lambda () (undoer) (yield)))
  (doer))

;;; Alternative search strategy wrappers

(define (with-depth-first-schedule thunk)
  (call-with-current-continuation
   (lambda (k)
     (fluid-let ((add-to-search-schedule
		  add-to-depth-first-search-schedule)
		 (*search-schedule* (empty-search-schedule))
		 (*top-level* k))
       (thunk)))))

(define (with-breadth-first-schedule thunk)
  (call-with-current-continuation
   (lambda (k)
     (fluid-let ((add-to-search-schedule
		  add-to-breadth-first-search-schedule)
		 (*search-schedule* (empty-search-schedule))
		 (*top-level* k))
       (thunk)))))


;;; Representation of the search schedule

(define *search-schedule*)

(define (empty-search-schedule)
  (make-stack&queue))

(define (yield)
  (if (stack&queue-empty? *search-schedule*)
      (*top-level* #f)
      ((pop! *search-schedule*))))

(define (force-next thunk)
  (push! *search-schedule* thunk))

;;; Alternative search strategies

(define (add-to-depth-first-search-schedule alternatives)
  (for-each (lambda (alternative)
	      (push! *search-schedule* alternative))
	    (reverse alternatives)))

(define (add-to-breadth-first-search-schedule alternatives)
  (for-each (lambda (alternative)
	      (add-to-end! *search-schedule* alternative))
	    alternatives))

;;; For incremental interactive experiments from REPL.

(define (init-amb)
  (set! *search-schedule* (empty-search-schedule))
  (set! *number-of-calls-to-fail* 0)
  'done)

(define add-to-search-schedule ;; Default is depth 1st
  add-to-depth-first-search-schedule)

(define *top-level*
  (lambda (ignore)
    (display ";No more alternatives\n")
    (abort->top-level unspecific)))



;;; Utilities

(define (require p)
  (if (not p) (amb)))


(define (distinct? items)
  (cond ((null? items) #t)
        ((null? (cdr items)) #t)
        ((member (car items) (cdr items)) #f)
        (else (distinct? (cdr items)))))

#|
;;; AX 1 - Elementary backtrack test.

(define elementary-backtrack-test
  (lambda ()
    (let ((x (amb 1 2 3)))
      (pp (list x))
      (let ((y (amb 'a 'b)))
        (pp (list x y))
        (let ((z (amb #t #f)))
          (pp (list x y z)))))
    (amb)))
#|
;; AX 1.d - Elementary backtrack test.  [Depth First]

(with-depth-first-schedule elementary-backtrack-test)
(1)
(1 a)
(1 a #t)
(1 a #f)
(1 b)
(1 b #t)
(1 b #f)
(2)
(2 a)
(2 a #t)
(2 a #f)
(2 b)
(2 b #t)
(2 b #f)
(3)
(3 a)
(3 a #t)
(3 a #f)
(3 b)
(3 b #t)
(3 b #f)
;Value: #f

;; AX 1.b - Elementary backtrack test.  [Breadth First]

(with-breadth-first-schedule elementary-backtrack-test)
(1)
(2)
(3)
(1 a)
(1 b)
(2 a)
(2 b)
(3 a)
(3 b)
(1 a #t)
(1 a #f)
(1 b #t)
(1 b #f)
(2 a #t)
(2 a #f)
(2 b #t)
(2 b #f)
(3 a #t)
(3 a #f)
(3 b #t)
(3 b #f)
;Value: #f
|#

;;; AX 2 - Testing undoable assignment.

(define testing-undoable-assignment
  (lambda ()
    (let ((x (amb 1 2 3)) (y 0) (z 0))
      (pp `(before ,x ,y ,z))
      (amb-set! y x)
      (pp `(after ,x ,y ,z))
      (amb-set! z (amb 3.14 2.718))
      (pp `(zset ,x ,y ,z))
      (amb-set! x (+ y z))
      (pp `(xset ,x ,y ,z))
      (amb))))
#|
;;; AX 2.d - Testing undoable assignment.  [Depth First]

(with-depth-first-schedule testing-undoable-assignment)
(before 1 0 0)
(after 1 1 0)
(zset 1 1 3.14)
(xset 4.140000000000001 1 3.14)
(zset 1 1 2.718)
(xset 3.718 1 2.718)
(before 2 0 0)
(after 2 2 0)
(zset 2 2 3.14)
(xset 5.140000000000001 2 3.14)
(zset 2 2 2.718)
(xset 4.718 2 2.718)
(before 3 0 0)
(after 3 3 0)
(zset 3 3 3.14)
(xset 6.140000000000001 3 3.14)
(zset 3 3 2.718)
(xset 5.718 3 2.718)
;Value: #f
|#

;;; AX 3 - Pythagorean triples

;; In breadth-first we get useful results here.
;; None from depth-first.

;; AX 3.f - A Pythagorean triple from...

(define (a-pythagorean-triple-from low)
  (let ((i (an-integer-from low)))
    (let ((j (an-integer-from i)))
      (let ((k (an-integer-from j)))
        (require (= (+ (* i i) (* j j)) (* k k)))
        (list i j k)))))

(define (an-integer-from low)
  (amb low (an-integer-from (+ low 1))))

#|
(with-breadth-first-schedule
    (lambda ()
      (pp (a-pythagorean-triple-from 1))
      (amb)))
(3 4 5)
(6 8 10)
(5 12 13)
(9 12 15)
(8 15 17)
(12 16 20)
(7 24 25)
(15 20 25)
(10 24 26)
(20 21 29)
(18 24 30)
(16 30 34)
(21 28 35)
(12 35 37)
(15 36 39)
(24 32 40)
(9 40 41)
(27 36 45)
(14 48 50)
(30 40 50)
(24 45 51)
(20 48 52)
(28 45 53)
(33 44 55)
(40 42 58)
(36 48 60)
(11 60 61)
(16 63 65)
(25 60 65)
(33 56 65)
;Quit!
|#

;; AX 3.b - A Pythagorean triple between...

;; For example, for controlling search:

(define (a-pythagorean-triple-between low high)
  (let ((i (an-integer-between low high)))
    (let ((j (an-integer-between i high)))
      (let ((k (an-integer-between j high)))
        (require (= (+ (* i i) (* j j)) (* k k)))
        (list i j k)))))
|#

(define (an-integer-between low high)
  (require (<= low high))
  (amb low
       (an-integer-between (+ low 1) high)))

;; A useful device:

(define (amb-collect-values result-thunk #!optional limit)
  (call-with-current-continuation
   (lambda (k)
     (let ((values '()) (count 0))
       (fluid-let ((*top-level* (lambda (ignore) (k values)))
                   (*search-schedule* (empty-search-schedule)))
         (let ((value (result-thunk)))
           (set! values (cons value values))
           (set! count (+ count 1))
           (if (and (not (default-object? limit))
                    (>= count limit))
               (k values))
           (amb)))))))
#|
(with-depth-first-schedule
    (lambda ()
      (let ((mid (amb-collect-values
                  (lambda ()
                    (a-pythagorean-triple-between 1 20))
                  ;; I want only 3, and
                  ;; I don't want to backtrack into this.
                  3)))
        (pp (list (a-pythagorean-triple-between 1 10)
                  mid
                  (a-pythagorean-triple-between 10 30)))
        (amb))))
((3 4 5) ((6 8 10) (5 12 13) (3 4 5)) (10 24 26))
((6 8 10) ((6 8 10) (5 12 13) (3 4 5)) (10 24 26))
((3 4 5) ((6 8 10) (5 12 13) (3 4 5)) (12 16 20))
((6 8 10) ((6 8 10) (5 12 13) (3 4 5)) (12 16 20))
((3 4 5) ((6 8 10) (5 12 13) (3 4 5)) (15 20 25))
((6 8 10) ((6 8 10) (5 12 13) (3 4 5)) (15 20 25))
((3 4 5) ((6 8 10) (5 12 13) (3 4 5)) (18 24 30))
((6 8 10) ((6 8 10) (5 12 13) (3 4 5)) (18 24 30))
((3 4 5) ((6 8 10) (5 12 13) (3 4 5)) (20 21 29))
((6 8 10) ((6 8 10) (5 12 13) (3 4 5)) (20 21 29))
;Value: #f
|#
