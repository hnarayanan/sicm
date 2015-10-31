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

;;;; Jeff Lang's model of a 2N7000
;;;    using mosfet.scm

(define win (frame 0 5 0 0.02))
#| win |#

(define 2N7000
  (n-channel_enhancement-mode_mosfet-current 0.138 2.0 (/ 1.0 65)))
#| 2N7000 |#

(2N7000 2.3 3)
#| 6.496615384615378e-3 |#

(for-each (lambda (vGS)
	    (plot-function win
			   (lambda (vDS)
			     (2N7000 vGS vDS))
			   0. 5.
			   .001))
	  '(1.90 2.0 2.1 2.2 2.3 2.4 2.5))
