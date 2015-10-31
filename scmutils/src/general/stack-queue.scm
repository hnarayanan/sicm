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

;;;; Simple stack&queue Abstraction

(declare (usual-integrations))

(define-record-type <stack&queue>
    (%make-stack&queue front back)
    stack&queue?
  (front stack&queue-front set-stack&queue-front!)
  (back stack&queue-back set-stack&queue-back!))
  

(define (make-stack&queue)
  (%make-stack&queue '() '()))

(define (stack&queue-empty? stq)
  (not (pair? (stack&queue-front stq))))

(define (stack&queued? stq item)
  (memq item (stack&queue-front stq)))

(define (push! stq object)
  (if (pair? (stack&queue-front stq))
      (set-stack&queue-front! stq
        (cons object (stack&queue-front stq)))
      (begin
	(set-stack&queue-front! stq
	  (cons object (stack&queue-front stq)))
	(set-stack&queue-back! stq
	  (stack&queue-front stq))))
  unspecific)

(define (add-to-end! stq object)
  (let ((new (cons object '())))
    (if (pair? (stack&queue-back stq))
	(set-cdr! (stack&queue-back stq) new)
	(set-stack&queue-front! stq new))
    (set-stack&queue-back! stq new)
    unspecific))

(define (pop! stq)
  (let ((next (stack&queue-front stq)))
    (if (not (pair? next))
	(error "Empty stack&queue -- POP"))
    (if (pair? (cdr next))
	(set-stack&queue-front! stq (cdr next))
	(begin
	  (set-stack&queue-front! stq '())
	  (set-stack&queue-back! stq '())))
    (car next)))



