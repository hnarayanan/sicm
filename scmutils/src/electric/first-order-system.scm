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

;;; for t0<t<inf
;;; x(t) = x(inf) + (x(t0)-x(inf))*exp(-(t-t0)/tau)

(define ((xfun t-initial x-initial x-final tau) t)
  (+ x-final
     (* (- x-initial x-final)
	(exp (- (/ (- t t-initial)
		   tau))))))

;;; Time to get to x(t) in [x(t0), x(inf)]

(define ((xtime t-initial x-initial x-final tau) x-goal)
  (+ t-initial
     (* -1 tau
	(log (/ (- x-goal x-final)
		(- x-initial x-final))))))


#|
(let ((t0 't0) (vi 'vi) (vf 'vf) (tau 'tau))
  ((compose (xfun t0 vi vf tau)
	    (xtime t0 vi vf tau))
   't))
#| t |#

(let ((t0 't0) (vi 'vi) (vf 'vf) (tau 'tau))
  ((compose (xtime t0 vi vf tau)
	    (xfun t0 vi vf tau))
   'xo))
#| xo |#
|#