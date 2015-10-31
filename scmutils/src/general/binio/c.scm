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

Return-Path: <gjs@altdorf.ai.mit.edu>
Date: Tue, 2 Jul 91 15:49:44 edt
From: "Gerald Jay Sussman" <gjs@altdorf.ai.mit.edu>
To: wisdom@altdorf.ai.mit.edu
Subject: [jinx@altdorf.ai.mit.edu: binary IO and C structure utilities]

Date: Mon, 25 Mar 91 22:20:06 est
From: "Guillermo J. Rozas" <jinx@altdorf.ai.mit.edu>
Return-Path: <jinx@altdorf.ai.mit.edu>
To: gjs@altdorf.ai.mit.edu, wisdom@altdorf.ai.mit.edu
Cc: chb@altdorf.ai.mit.edu, cph@altdorf.ai.mit.edu, markf@altdorf.ai.mit.edu,
        aab@altdorf.ai.mit.edu
Subject: binary IO and C structure utilities
Reply-To: jinx@zurich.ai.mit.edu

~jinx/scheme/random/binio contains procedures for reading and writing
some binary data.
~jinx/scheme/random/cstruct contains the DEFINE-C-STRUCTURE macro to
map between scheme and (limited) C structs.

After loading both, you can do things like

(define-c-structure collection
  (char foo)
  (double bar)
  (long baz))

(define my-vec (make-vector 4))

(let ((binfile (binary-file/open-input "foo.dat")))
  (vector-set! my-vec 0 (binary-file/read-collection binfile))
  (vector-set! my-vec 1 (binary-file/read-collection binfile))
  (vector-set! my-vec 2 (binary-file/read-collection binfile))
  (vector-set! my-vec 3 (binary-file/read-collection binfile))
  (binary-file/close binfile))

and then you can do things like

(pp (vector-ref my-vec 2))

(collection/baz (vector-ref my-vec 1))

(set-collection/baz! (vector-ref my-vec 3) -23)

(binary-file/write-collection <some bin file> (vector-ref my-vec 0))

etc.

PS: Currently DEFINE-C-STRUCTURE does not handle arrays, structs, or
pointers, but it handles alignment correctly.

The alignment is machine-dependent, and the default is the S300
alignment.

To switch to S800 alignment, type

(set! data-description s800-data-description)

after loading both files, but before using DEFINE-C-STRUCTURE.

