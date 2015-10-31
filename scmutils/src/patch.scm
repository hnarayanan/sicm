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

(start-canonicalizing-symbols!)

;;; Patch to /scheme/v7/src/runtime/X11graph by Taylor Campbell
;;;    Fixes timing error in creation of windows.

((lambda (patch)
   (eval patch (->environment '(runtime x-graphics))))
 '(define (%read-and-process-event display)
    (let ((event
           (or (x-display-process-events (x-display/xd display) 2)
               (and (eq? 'read
                         (test-for-io-on-descriptor
                          (x-display-descriptor (x-display/xd display))
                          #t
                          'read))
                    (x-display-process-events (x-display/xd display)
                    1)))))
      (if event
          (process-event display event)))))


(start-preserving-case!)

#|
;;; Patch to make F->CT work with structured coordinates.
;;; Now in system.

(define (F->CT F)
  (define (CT H-state)
    (let ((t (time H-state))
	  (q (coordinate H-state))
	  (p (momentum H-state)))
      (let ((qo (F H-state)))
	(->H-state t
		   qo
		   (* p
		      (s:inverse (compatible-shape qo)
				 (((partial 1) F) H-state)
				 (compatible-shape p)))))))
  CT)
|#