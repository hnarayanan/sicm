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

(define fmod
  (sigfun:make (general-bump -1/2 0 1/2)
	       (sigfun:make-span -10 10)))

(plot-trace 1 (magnitude fmod) #t)
;Value: (1 (-10. 10. 0. 1.))

(define tmod
  (signal->time-function (frequency-function->signal fmod)))

(plot-trace 2 (real-part tmod) #t)
;Value: (2 (-25.6 25.6 -1.1402029382176215e-2 .38381726398295835))

(plot-trace 3 (imag-part tmod) #t)
;Value 124: (3 (-25.6 25.6 0. 0.))

(define tcarrier  
  (sigfun:make (lambda (t) (cos (* 10 t)))
	       (sigfun:span tmod)))

(plot-trace 4 tcarrier)
;Value: (4 (-25.6 25.6 -.9999902731420214 1.))

(define fcarrier
  (signal->frequency-function (time-function->signal tcarrier)))

(plot-trace 5 (magnitude fcarrier))
;Value: (5 (-10. 10. 1.2756984956419672e-2 16.75378514012112))

(define tmodcarrier (* tmod tcarrier))

(plot-trace 6 (real-part tmodcarrier))
;Value: (6 (-25.6 25.6 -.7894410066332435 .9833808129167028))

(define fmodcarrier
  (signal->frequency-function (time-function->signal tmodcarrier)))

(plot-trace 7 fmodcarrier)
;Value: (7 (-10. 10. -6.33107267778147e-9 .4999094037156))
