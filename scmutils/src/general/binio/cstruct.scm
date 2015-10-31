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

;;; -*- Scheme -*-

(declare (usual-integrations))

(define int-type 'signed-long)
(define unsigned-int-type 'unsigned-long)

(define s300-data-description
  '((char 1 1)
    (signed-byte 1 1)
    (unsigned-byte 1 1)
    (signed-short 2 2)
    (unsigned-short 2 2)
    (signed-long 4 2)
    (unsigned-long 4 2)
    (double 8 2)
    (struct () 2)
    ))

(define s800-data-description
  '((char 1 1)
    (signed-byte 1 1)
    (unsigned-byte 1 1)
    (signed-short 2 2)
    (unsigned-short 2 2)
    (signed-long 4 4)
    (unsigned-long 4 4)
    (double 8 8)
    (struct () -4)
    ))

(define mips-data-description
  '((char 1 1)
    (signed-byte 1 1)
    (unsigned-byte 1 1)
    (signed-short 2 2)
    (unsigned-short 2 2)
    (signed-long 4 4)
    (unsigned-long 4 4)
    (double 8 8)
    (struct () -4)
    ))

(define data-description mips-data-description)

;; This currently does not handle internal structs, arrays, or pointers.
;; It is essentially impossible to make it handle pointers, but the others
;; could be fixed.

(define (c-struct/process-fields fields recvr)
  (define (round-up position alignment)
    (* alignment (quotient (+ position (-1+ alignment))
			   alignment)))

  (define (handle fields position constraint accum)
    (if (null? fields)
	(let ((stdefn (assq 'struct data-description)))
	  (if (not stdefn)
	      (error "c-struct/process-fields: Cannot find struct")
	      (let ((alignment (caddr stdefn)))
		(recvr (round-up position (if (< alignment 0)
					      (max constraint (- 0 alignment))
					      alignment))
		       (reverse accum)))))
	(let ((field (car fields)))
	  (let ((name (if (not (pair? field))
			  field
			  (cadr field)))
		(type (if (not (pair? field))
			  int-type
			  (case (car field)
			    ((char) 'char)
			    ((short) 'signed-short)
			    ((unsigned-int) unsigned-int-type)
			    ((int) int-type)
			    ((long) 'signed-long)
			    (else
			     (car field))))))
	    (let ((defn (assq type data-description)))
	      (if (not defn)
		  (error "c-struct/process-fields: unknown type" type))
	      (let ((size (cadr defn))
		    (alignment (caddr defn)))
		(let ((posn (round-up position alignment)))
		  (handle (cdr fields)
			  (+ size posn)
			  (max alignment constraint)
			  (cons
			   (list name posn
				 (intern (string-append (symbol->string type)
							"/read"))
				 (intern (string-append (symbol->string type)
							"/write")))
			   accum)))))))))
  (handle fields 0 0 '()))

(syntax-table-define system-global-syntax-table 'DEFINE-C-STRUCTURE
  (macro (name . fields)
    (let* ((str (symbol->string name))
	   (conc (string-append str "/")))
      (c-struct/process-fields
       fields
       (lambda (total-size fields)
	 (let ((fread (intern (string-append "binary-file/read-" str)))
	       (fwrite (intern (string-append "binary-file/write-" str)))
	       (read (intern (string-append conc "read")))
	       (write (intern (string-append conc "write")))
	       (make (intern (string-append conc "make")))
	       (buffer (generate-uninterned-symbol "buffer"))
	       (channel (generate-uninterned-symbol "channel"))
	       (struct (generate-uninterned-symbol "struct"))
	       (posn (generate-uninterned-symbol "posn")))
	       
	   `(begin

	      (define-structure (,name (conc-name ,(intern conc))
				       (constructor ,make ()))
		,@(map car fields))

	      (define (,read ,buffer ,posn)
		(let ((,struct (,make)))
		  ,@(map (lambda (field)
			   `(,(intern
			       (string-append "set-"
					      conc
					      (symbol->string
					       (car field))
					      "!"))
			     ,struct
			     (,(caddr field) ,buffer
					     (+ ,posn
						,(cadr field)))))
			 fields)
		  ,struct))

	      (define (,write ,buffer ,posn ,struct)
		,@(map (lambda (field)
			 `(,(cadddr field)
			   ,buffer
			   (+ ,posn ,(cadr field))
			   (,(intern
			      (string-append conc
					     (symbol->string (car field))))
			    ,struct)))
		       fields))

	      (define ,fread
		(let ((,buffer (make-string ,total-size)))
		  (named-lambda (,fread ,channel)
		    (if (not (fix:= (channel-read ,channel ,buffer 0 ,total-size)
				    ,total-size))
			binary-file/eof-object
			(,read ,buffer 0)))))

	      (define ,fwrite
		(let ((,buffer (make-string ,total-size)))
		  (named-lambda (,fwrite ,channel ,struct)
		    (,write ,buffer 0 ,struct)
		    (if (not (fix:= (channel-write ,channel ,buffer 0
						   ,total-size)
				    ,total-size))
			(error ,(string-append (symbol->string fwrite)
					       ": Error writing")
			       ,struct
			       ,channel))))))))))))