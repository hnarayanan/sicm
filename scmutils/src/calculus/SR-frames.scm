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

;;;;              Special-relativity frames.

;;; A frame is defined by a Poincare transformation from a given
;;; background 4-space frame (the "ancestor-frame").  The
;;; transformation is specified by a boost magnitude and a unit-vector
;;; boost direction, relative to the ancestor frame, and the position
;;; of the origin of the frame being defined in the ancestor frame.

;;; The events are absolute, in that it is always possible to compare
;;; them to determine if two are the same.  They will be represented
;;; with coordinates relative to some arbitrary absolute frame,
;;; "the-ether".

;;; To keep us from going nuts, an SR frame has a name, which it uses
;;; to label coordinates in its frame.

;;; ...
;;; Implementation of the coordinates uses a put/get table.

(define (make-SR-coordinates frame 4tuple)
   (assert (vector? 4tuple))
   (assert (fix:= (vector-length 4tuple) 4))
   (eq-put! 4tuple 'SR-coordinates #t)
   (claim! 4tuple frame)
   4tuple)

(define (SR-coordinates? coords)
  (eq-get coords 'SR-coordinates))

(define (SR-name coords)
  ((frame-owner coords) 'name))

;;; SR frames

(define (coordinates->event ancestor-frame this-frame
			    boost-direction v/c origin)
  (assert (eq? (frame-owner origin) ancestor-frame))
  (define (c->e coords)
    (assert (SR-coordinates? coords))
    ((point ancestor-frame)
     (make-SR-coordinates ancestor-frame
			  (+ ((general-boost2 boost-direction v/c)
			      coords)
			     origin))))
  c->e)


(define (event->coordinates ancestor-frame this-frame
			    boost-direction v/c origin)
  (assert (eq? (frame-owner origin) ancestor-frame))
  (define (e->c event)
    (assert (event? event))
    (make-SR-coordinates this-frame
			 ((general-boost2 (- boost-direction) v/c)
			  (- ((chart ancestor-frame) event)
			     origin))))
  e->c)


#|
;;; Galilean test

(define (this->ancestor x) x)
(define (ancestor->this x) x)

(define (coordinates->event ancestor-frame this-frame
			    boost-direction v/c origin)
  (assert (eq? (frame-owner origin) ancestor-frame))
  (define (c->e coords)
    (assert (SR-coordinates? coords))
    ((point ancestor-frame)
     (make-SR-coordinates ancestor-frame
			  (+ (this->ancestor coords)
			     origin))))
  c->e)


(define (event->coordinates ancestor-frame this-frame
			    boost-direction v/c origin)
  (assert (eq? (frame-owner origin) ancestor-frame))
  (define (e->c event)
    (assert (event? event))
    (make-SR-coordinates this-frame
			 (ancestor->this
			  (- ((chart ancestor-frame) event)
			     origin))))
  e->c)
|#


(define (boost-direction frame)
  (list-ref (frame-params frame) 0))

(define (v/c frame)
  (list-ref (frame-params frame) 1))

(define (coordinate-origin frame)
  (list-ref (frame-params frame) 2))


(define make-SR-frame
  (frame-maker coordinates->event event->coordinates))

;;; The background frame


(define ((base-frame-point ancestor-frame this-frame) coords)
  (assert (SR-coordinates? coords))
  (assert (eq? this-frame (frame-owner coords)))
  (make-event coords)
  coords)

(define ((base-frame-chart ancestor-frame this-frame) event)
  (assert (event? event))
  (make-SR-coordinates this-frame event))

(define the-ether
   ((frame-maker base-frame-point base-frame-chart)
    'the-ether 'the-ether))

#|
(symbolic-constants #f)
(set! *divide-out-terms* #f)

;;; Velocity addition formula

(define A
   (make-SR-frame 'A the-ether
		  (up 1 0 0)
		  (/ 'va :c)
		  (make-SR-coordinates the-ether
				       #(0 0 0 0))))

(define B
   (make-SR-frame 'B A
		  (up 1 0 0)
		  (/ 'vb :c)
		  (make-SR-coordinates A
				       #(0 0 0 0))))

(let ((foo ((chart the-ether)
	    ((point B)
	     (make-SR-coordinates B
	       (up (* :c 'tau) 0 0 0))))))
   (/ (ref foo 1) (/ (ref foo 0) :c)))
#|
(/ (+ (* (expt :c 2) va)
      (* (expt :c 2) vb))
   (+ (expt :c 2) (* va vb)))
;;; Hand simplified to:
(/ (+ va vb)
   (+ 1 (* (/ va :c) (/ vb :c))))
|#

|#

(define (add-v/cs v1/c v2/c)
   (/ (+ v1/c v2/c)
      (+ 1 (* v1/c v2/c))))

(define (add-velocities v1 v2)
   (/ (+ v1 v2)
      (+ 1 (* (/ v1 :c) (/ v2 :c)))))

#|
;;; Simple test of reversibility

(define A
   (make-SR-frame 'A the-ether (up 1 0 0) 'va/c
		  (make-SR-coordinates the-ether #(cta xa ya za))))


((chart A)
  ((point A)
   (make-SR-coordinates A #(ct x y z))))
#|
(up ct x y z)
|#

;;; The ether coordinates of the origin of A relative to "the ether"
;;; is

(define origin-A
  (coordinate-origin A))

(frame-name (frame-owner origin-A))
#| the-ether |#

(define B
   (make-SR-frame 'B A (up 1 0 0) 'vba/c
		  (make-SR-coordinates A #(ctba xba yba zba))))

((chart B)
  ((point B)
   (make-SR-coordinates B
	  #(ct x y z))))
#|
(up ct x y z)
|#
|#

#|
;;; Poincare formula


(define A
   (make-SR-frame 'A the-ether (up 1 0 0) 'va/c
		  (make-SR-coordinates the-ether #(cta xa ya za))))

(define B
   (make-SR-frame 'B A (up 1 0 0) 'vba/c
		  (make-SR-coordinates A #(ctba xba yba zba))))



;;; The ether coordinates of the origin of B relative to "the ether"
;;; is

(define origin-B
  ((chart the-ether)
   ((point A)
    (coordinate-origin B))))

origin-B
#|
(up
 (/ (+ (* cta (sqrt (+ 1 (* -1 (expt va/c 2))))) (* va/c xba) ctba)
    (sqrt (+ 1 (* -1 (expt va/c 2)))))
 (/ (+ (* ctba va/c) (* xa (sqrt (+ 1 (* -1 (expt va/c 2))))) xba)
    (sqrt (+ 1 (* -1 (expt va/c 2)))))
 (+ ya yba)
 (+ za zba))
|#

(define C
  (make-SR-frame 'C the-ether
		 (up 1 0 0)
		 (add-v/cs 'va/c 'vba/c)
		 origin-B))


;;; A typical event.

(define foo
  ((point the-ether)
   (make-SR-coordinates the-ether
			(up 'ct 'x 'y 'z))))
|#
