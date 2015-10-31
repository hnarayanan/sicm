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

;;; Some elementary unit conversions

(define (degrees->radians degrees)
  (* (/ :2pi 360) degrees))

(define (radians->degrees radians)
  (* (/ 360 :2pi) radians))


(define (xms->x xms)
  (+ (car xms)
     (/ (cadr xms) 60)
     (/ (caddr xms) 3600)))

(define (x->xms x)
  (let* ((d (truncate x))
	 (dd (- x d))
	 (m (truncate (* 60 dd)))
	 (ddm (- dd (/ m 60)))
	 (s (* 3600 ddm)))
    (list d m s)))

(define dms->d xms->x)
(define d->dms x->xms)

(define (dms->radians dms)
  (degrees->radians (dms->d dms)))

(define (radians->dms radians)
  (d->dms (radians->degrees radians)))


(define (hours->radians hours)
  (* (/ :2pi 24) hours))

(define (radians->hours radians)
  (* (/ 24 :2pi) radians))


(define hms->h xms->x)
(define h->hms x->xms)

(define (hms->radians hms)
  (* 15 (dms->radians hms)))

(define (radians->hms radians)
  (radians->dms (/ radians 15)))
