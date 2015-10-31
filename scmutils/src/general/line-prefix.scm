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

(declare (usual-integrations))

(define (make-line-prefix-port port prefix)
 (make-port line-prefix-port-type
            (vector port prefix #t)))

(define line-prefix-port-type
  (make-port-type
   `((write-char
      ,(lambda (port char)
	 (let ((v (port/state port)))
	   (let ((port* (vector-ref v 0)))
	     (if (vector-ref v 2)
		 (write-string (vector-ref v 1) port*))
	     (let ((n (output-port/write-char port* char)))
	       (vector-set! v 2 (char=? char #\newline))
	       n)))))
     (x-size
      ,(lambda (port)
	 (let ((v (port/state port)))
	   (let ((port* (vector-ref v 0)))
	     (let ((op (port/operation port* 'X-SIZE)))
	       (and op
		    (let ((n (op port*)))
		      (and n
			   (max (- n (string-length (vector-ref v 1)))
				0)))))))))
     (column
      ,(lambda (port)
	 (let ((v (port/state port)))
	   (let ((port* (vector-ref v 0)))
	     (let ((op (port/operation port* 'COLUMN)))
	       (and op
		    (let ((n (op port*)))
		      (and n
			   (max (- n (string-length (vector-ref v 1)))
				0)))))))))
     (flush-output
      ,(lambda (port)
	 (let ((v (port/state port)))
	   (output-port/flush-output (vector-ref v 0)))))
     (discretionary-flush-output
      ,(lambda (port)
	 (let ((v (port/state port)))
	   (output-port/discretionary-flush (vector-ref v 0))))))
   #f))

(define ((pp-line-prefix prefix) object #!optional port . rest)
  (apply pp
	 object
	 (make-line-prefix-port (if (default-object? port)
				    (current-output-port)
				    port)
				prefix)
	 rest))

(define pp-comment (pp-line-prefix ";"))



(set! repl:write-result-hash-numbers? #f)

