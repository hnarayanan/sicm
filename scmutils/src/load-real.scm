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

;;;; Scmutils top-level loader

(set! load-debugging-info-on-demand? #t)

(ge user-initial-environment)

(add-subsystem-identification! "ScmUtils" '("Mechanics" "Summer 2015"))

(define scmutils-base-environment user-initial-environment)

(environment-define scmutils-base-environment
		    '*environment*
		    'scmutils-base-environment)

(load "general/comutils" scmutils-base-environment)

(start-canonicalizing-symbols!)


(environment-define scmutils-base-environment
		    'derivative-symbol
		    (string->symbol "D"))

(define (in-scmutils-directory relative-path thunk)
  (with-working-directory-pathname
      (merge-pathnames (pathname-as-directory relative-path)
		       (directory-pathname (current-load-pathname)))
    thunk))

(load-option 'hash-table)
(load-option 'synchronous-subprocess)

(in-scmutils-directory "./general"
		       (lambda ()
			 (load "load" scmutils-base-environment)))
(in-scmutils-directory "./kernel"
		       (lambda ()
			 (load "load" scmutils-base-environment)))

;;; kernel/genenv.scm defines the generic environment
(environment-define system-global-environment 'generic-environment
		    (access generic-environment scmutils-base-environment))

(environment-define system-global-environment 'user-generic-environment
		    (extend-top-level-environment
		     (access generic-environment scmutils-base-environment)))

(environment-define user-generic-environment
		    '*environment*
		    'user-generic-environment)

(in-scmutils-directory "./simplify"
		       (lambda ()
			 (load "load" scmutils-base-environment)))

(environment-define system-global-environment
		    'symbolic-environment
		    (access symbolic-environment scmutils-base-environment))

(environment-define system-global-environment
		    'rule-environment
		    (access rule-environment scmutils-base-environment))

(define symbolic-operators
  (hash-table/key-list symbolic-operator-table))

(in-scmutils-directory "./display"
		       (lambda ()
			 (load "load" scmutils-base-environment)))

(in-scmutils-directory "./enclose"
		       (lambda ()
			 (load "load" scmutils-base-environment)))
(in-scmutils-directory "./numerics"
		       (lambda ()
			 (load "load" scmutils-base-environment)))
(in-scmutils-directory "./poly"
		       (lambda ()
			 (load "load" scmutils-base-environment)))


(start-preserving-case!)

(in-scmutils-directory "./kernel"
		       (lambda ()
			 (load "litfun" scmutils-base-environment)))

(in-scmutils-directory "./solve"
                       (lambda ()
			 (load "load" scmutils-base-environment)))

(in-scmutils-directory "./units"
		       (lambda ()
			 (load "load" scmutils-base-environment)))      

(in-scmutils-directory "./mechanics"
		       (lambda ()
			 (load "load" scmutils-base-environment)))

(in-scmutils-directory "./calculus"
		       (lambda ()
			 (load "load" scmutils-base-environment)))

;;; Should be a place to put useful stuff like this.
(define (Sigma a b proc)
  (g:sigma proc a b))

(in-scmutils-directory "./general"
		       (lambda ()
			 (load "let-macro"
			       user-generic-environment)))

;;; To fix the problem that students cannot change find-path.

(in-scmutils-directory "./mechanics"
		       (lambda ()
			 (load "find-path-patch"
			       user-generic-environment)))


;;; To fix bugs, e.g. in X11graphics 

(in-scmutils-directory "."
		       (lambda ()
			 (load "patch"
			       user-initial-environment)))

(start-scmutils-print)
(init-amb)
(ge user-generic-environment)
