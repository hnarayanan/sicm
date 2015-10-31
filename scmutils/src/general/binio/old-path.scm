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

(define (canonicalize-input-filename filename)
  (->namestring (canonicalize-input-pathname filename)))

(define (canonicalize-input-pathname filename)
  (let ((pathname (->pathname filename)))
    (or (pathname->input-truename pathname)
	(canonicalize-input-pathname
	 (error:file-operation pathname
			       "find"
			       "file"
			       "file does not exist"
			       canonicalize-input-pathname
			       (list filename))))))

(define (pathname->input-truename pathname)
  (let ((pathname (merge-pathnames pathname)))
    (and (eq? true (file-exists? pathname))
	 pathname)))

(define (canonicalize-output-filename filename)
  (->namestring (canonicalize-output-pathname filename)))

(define (canonicalize-output-pathname filename)
  (pathname->output-truename (->pathname filename)))

(define (pathname->output-truename pathname)
  (merge-pathnames pathname))

(define (canonicalize-overwrite-filename filename)
  (->namestring (canonicalize-overwrite-pathname filename)))

(define (canonicalize-overwrite-pathname filename)
  (pathname->overwrite-truename (->pathname filename)))

(define (pathname->overwrite-truename pathname)
  (merge-pathnames pathname))

(define (pathname-components pathname receiver)
  (receiver (pathname-host pathname)
	    (pathname-device pathname)
	    (pathname-directory pathname)
	    (pathname-name pathname)
	    (pathname-type pathname)
	    (pathname-version pathname)))

(define (pathname-relative? pathname pathname*)
  (let ((diff (enough-pathname pathname pathname*)))
    (and (not (equal? (pathname-directory pathname) (pathname-directory diff)))
	 (make-pathname (pathname-host diff)
			(pathname-device diff)
			(pathname-directory diff)
			(pathname-name pathname)
			(pathname-type pathname)
			(pathname-version pathname)))))

(define home-directory-pathname user-homedir-pathname)
(define init-file-truename init-file-pathname)
(define pathname->absolute-pathname merge-pathnames)
(define pathname->string ->namestring)
(define pathname-directory-path directory-pathname)
(define pathname-directory-string directory-namestring)
(define pathname-name-path file-pathname)
(define pathname-name-string file-namestring)
(define string->pathname ->pathname)