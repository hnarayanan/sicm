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

(declare (usual-integrations)
	 (integrate-primitive-procedures
	  (primitive-object-set! 3)
	  (primitive-object-ref 2)
	  (integer->flonum 2)))

(define-integrable (byte/unsigned->signed byte)
  (if (fix:< byte 128)
      byte
      (fix:- byte 256)))

(define-integrable (char/read string posn)
  (string-ref string posn))

(define-integrable (char/write string posn value)
  (string-set! string posn value))

(define-integrable (unsigned-byte/read string posn)
  (vector-8b-ref string posn))

(define-integrable (unsigned-byte/write string posn value)
  (vector-8b-set! string posn value))

(define-integrable (signed-byte/read string posn)
  (byte/unsigned->signed (unsigned-byte/read string posn)))

(define-integrable (signed-byte/write string posn value)
  (unsigned-byte/write string posn (fix:and value #xff)))

(define (unsigned-short/read string posn)
  (fix:or (unsigned-byte/read string (fix:+ 1 posn))
	  (fix:lsh (unsigned-byte/read string posn)
		   8)))

(define (unsigned-short/write string posn value)
  (unsigned-byte/write string (fix:+ 1 posn) (fix:and value #xff))
  (unsigned-byte/write string posn (fix:lsh value -8)))

(define (signed-short/read string posn)
  (fix:or (unsigned-byte/read string (fix:+ 1 posn))
	  (fix:lsh (signed-byte/read string posn)
		   8)))

(define (signed-short/write string posn value)
  (signed-byte/write string posn (fix:and (fix:lsh value -8) #xff))
  (unsigned-byte/write string (fix:+ 1 posn) (fix:and value #xff)))

(define (unsigned-long/read string posn)
  (+ (unsigned-short/read string (fix:+ posn 2))
     (* (unsigned-short/read string posn)
	65536)))

(define (unsigned-long/write string posn value)
  (let ((both (integer-divide value 65536)))
    (unsigned-short/write string posn (integer-divide-quotient both))
    (unsigned-short/write string (fix:+ posn 2) (integer-divide-remainder both))))

(define (signed-long/read string posn)
  (+ (unsigned-short/read string (fix:+ posn 2))
     (* (signed-short/read string posn)
	65536)))

(define (signed-long/write string posn value)
  (if (< value 0)
      (let ((both (integer-divide (- value 65535) 65536)))
	(signed-short/write string posn (integer-divide-quotient both))
	(unsigned-short/write string (fix:+ posn 2)
			      (fix:+ (integer-divide-remainder both) 65535)))
      (unsigned-long/write string posn value)))

(define (double/read string posn)
  ;; This could use (flo+ 0. 0.) if the compiler didn't optimize it out.
  (if (not (fix:= (fix:remainder posn 4) 0))
      (double/read (substring string posn (fix:+ posn 8)) 0)
      (let ((float (integer->flonum 0 0))
	    (index (fix:quotient posn 4)))
	(primitive-object-set! float 1 (primitive-object-ref string (fix:+ index 2)))
	(primitive-object-set! float 2 (primitive-object-ref string (fix:+ index 3)))
	float)))

(define (double/write string posn value)
  (if (not (fix:= (fix:remainder posn 4) 0))
      (let ((temp (make-string 8)))
	(double/write temp 0 value)
	(substring-move-right! temp 0 8 string posn))
      (let ((index (fix:quotient posn 4)))
	(primitive-object-set! string (fix:+ index 2)
			       (primitive-object-ref value 1))
	(primitive-object-set! string (fix:+ index 3)
			       (primitive-object-ref value 2)))))
  

(let-syntax
    ((define-binary-io
       (lambda (name size)
	 (let ((read-name
		(intern (string-append "binary-file/read-"
				       (symbol->string name))))
	       (write-name
		(intern (string-append "binary-file/write-"
				       (symbol->string name))))
	       (fetch-name
		(intern (string-append (symbol->string name)
				       "/read")))
	       (store-name
		(intern (string-append (symbol->string name)
				       "/write"))))
	   `(begin
	      (define ,read-name
		(let ((buffer (make-string ,size)))
		  (named-lambda (,read-name channel)
		    (if (fix:= (channel-read channel buffer 0 ,size) ,size)
			(,fetch-name buffer 0)
			binary-file/eof-object))))
	      (define ,write-name
		(let ((buffer (make-string ,size)))
		  (named-lambda (,write-name channel value)
		    (,store-name buffer 0 value)
		    (if (not (fix:= (channel-write channel buffer 0 ,size) ,size))
			(error ,(string-append (symbol->string write-name)
					       ": Error writing" )
			       value channel))))))))))

(define-binary-io char 1)
(define-binary-io signed-byte 1)
(define-binary-io unsigned-byte 1)
(define-binary-io signed-short 2)
(define-binary-io unsigned-short 2)
(define-binary-io signed-long 4)
(define-binary-io unsigned-long 4)
;; (define-binary-io float 4) ;; can't do.
(define-binary-io double 8)
)

(define binary-file/eof-object
  (make-eof-object "This argument is ignored"))

(define (binary-file/open-input fname)
  (file-open-input-channel (canonicalize-input-filename fname)))

(define (binary-file/open-output fname)
  (file-open-output-channel (canonicalize-output-filename fname)))

(define (binary-file/close channel)
  (channel-close channel))