#| -*-Scheme-*-

Copyright (C) 1986, 1987, 1988, 1989, 1990, 1991, 1992, 1993, 1994,
    1995, 1996, 1997, 1998, 1999, 2000, 2001, 2002, 2003, 2004, 2005,
    2006, 2007, 2008, 2009, 2010, 2011, 2012, 2013, 2014, 2015, 2016
    Massachusetts Institute of Technology

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

;;;; Patches for a running Scheme Mechanics System.

;;; For MACs, to fix problem with pointer coordinates in a running Scheme.
;;; Put the next procedure in a file, say mac-patch.scm, then execute 
;;;  (load "mac-patch" scmutils-base-environment)

(define (make-window/X11 width height x y #!optional display)
  (if (default-object? display) (set! display #f))
  (let ((window
	 (make-graphics-device 'X 
			       display
			       (x-geometry-string x y width height)
			       true)))
    ;; Prevent this window from receiving the keyboard focus.
    (if (not (string-ci=? "OS X" microcode-id/operating-system-variant))
	(x-graphics/disable-keyboard-focus window))
    ;; Inform the window manager that this window does not do any
    ;; keyboard input.
    (x-graphics/set-input-hint window false)
    ;; OK, now map the window onto the screen.
    (x-graphics/map-window window)
    (x-graphics/flush window)
    window))


;;; To fix pendulum action-angle procedures in a running Scheme.
;;; Put the next two procedures in a file, say pendulum-patch.scm, then execute 
;;;  (load "pendulum-patch" generic-environment)

(define ((pendulum-oscillating-state-to-aa-state alpha beta) state)
  (let ((E ((Hpendulum alpha beta) state)))
    (let ((action (pendulum-oscillating-action alpha beta E))
	  (angle ((pendulum-oscillating-phase alpha beta) state)))
      (up (time state) angle action))))

(define ((pendulum-circulating-state-to-aa-state alpha beta) state)
  (let ((E ((Hpendulum alpha beta) state)))
    (let ((action (pendulum-circulating-action alpha beta E))
	  (angle ((pendulum-circulating-phase alpha beta) state)))
      (up (time state) angle action))))

