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

(fluid-let ((compiler:use-multiclosures? #f))
  (for-each cf-conditionally
	    '("simplify"
	      )))

(for-each cf-conditionally
	  '("pcf"
	    "rcf"
	    "split-poly"
	    "fpf"

	    "symbenv"
	    "rule-syntax"
	    "matcher"
	    "rule-simplifier"

	    ))

(let ((fn "syntax"))
  (if (not (file-processed? fn "scm"
			    (compiler:compiled-code-pathname-type)))
      (cf fn))
  (let ((environment (nearest-repl/environment)))
    (load fn environment)
    (load "rule-syntax" environment)
    (for-each (lambda (name)
		(link-variables system-global-environment name
				environment name))
	      '(rule-system))))

(for-each cf-conditionally
	  '("rules"
	    "default"
	    ))

(for-each cf-conditionally
	  '( "sparse"
	     "sparse-interpolate"
	     "sparse-gcd"
	     
	     "pcf-fpf"))

