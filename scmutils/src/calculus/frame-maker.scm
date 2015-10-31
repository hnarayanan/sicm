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

;;;; System code for making frames

;;; Every frame has a name, and a frame that it is built on (which may be #f).
;;; Every frame owns coordinates that it may coerce to an absolute event or that
;;; it may export as its representation of an absolute event.

(define ((frame-maker c->e e->c) name ancestor-frame . params)

   (define (coordinates->event coords)
     (assert (eq? (frame-owner coords) this-frame))
     (let ((event
	    ((apply c->e ancestor-frame this-frame params) coords)))
       (assert (event? event))
       event))

   (define (event->coordinates event)
     (assert (event? event))
     (let ((coords 
	    ((apply e->c ancestor-frame this-frame params) event)))
       (assert (eq? (frame-owner coords) this-frame))
       coords))

   (define (this-frame m)
     (case m
       ((coords->event) coordinates->event)
       ((event->coords) event->coordinates)
       ((name) name)
       ((ancestor-frame) ancestor-frame)
       ((params) params)
       ((manifold) #f)			;Kludge.  See frame? in manifold.scm
       (else (error "Unknown message: " name m))))
   this-frame)

(define (event->coords frame) (frame 'event->coords))
(define (coords->event frame) (frame 'coords->event))
(define (ancestor-frame frame) (frame 'ancestor-frame))

(define (make-event e)
  (eq-put! e 'event #t)
  e)

(define (event? e)
  (eq-get e 'event))

(define (frame-owner coords)
  (eq-get coords 'owner))

(define (claim! coords owner)
  (let ((other (frame-owner coords)))
    (if other
	(if (not (eq? other owner))
	    (error "Someone else owns these coords" coords owner))
	(eq-put! coords 'owner owner))
    coords))

(define (frame-params frame) (frame 'params))
(define (frame-name frame) (frame 'name))

