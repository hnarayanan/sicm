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

;;; -*-Scheme-*-
;;;
;;; $Id: sound.scm,v 1.2 1997/01/08 22:05:53 cph Exp $
;;;
;;; Copyright (c) 1993-97 Massachusetts Institute of Technology
;;;
;;; This material was developed by the Scheme project at the
;;; Massachusetts Institute of Technology, Department of Electrical
;;; Engineering and Computer Science.  Permission to copy this
;;; software, to redistribute it, and to use it for any purpose is
;;; granted, subject to the following restrictions and understandings.
;;;
;;; 1. Any copy made of this software must include this copyright
;;; notice in full.
;;;
;;; 2. Users of this software agree to make their best efforts (a) to
;;; return to the MIT Scheme project any improvements or extensions
;;; that they make, so that these may be included in future releases;
;;; and (b) to inform MIT of noteworthy uses of this software.
;;;
;;; 3. All materials developed as a consequence of the use of this
;;; software shall duly acknowledge such use, in accordance with the
;;; usual standards of acknowledging credit in academic research.
;;;
;;; 4. MIT has made no warrantee or representation that the operation
;;; of this software will be error-free, and MIT is under no
;;; obligation to provide any services, by way of maintenance, update,
;;; or otherwise.
;;;
;;; 5. In conjunction with products arising from the use of this
;;; material, there shall be no use of the name of the Massachusetts
;;; Institute of Technology nor of any adaptation thereof in any
;;; advertising, promotional, or sales literature without prior
;;; written consent from MIT in each case.

;;;; I/O for Sony/HP Digitized Audio Files

(declare (usual-integrations))

(define (transform-raw-l16-file from-file to-file
				transformation buffer-size overlap)
  (call-with-input-file from-file
    (lambda (input-port)
      (call-with-output-file to-file
	(lambda (output-port)
	  (let ((ibs (fix:+ buffer-size overlap)))
	    (let ((reader (make-input-reader input-port buffer-size))
		  (writer (make-output-writer output-port buffer-size))
		  (input-buffer (flo:make-vector ibs))
		  (output-buffer (flo:make-vector buffer-size)))
	      (flo:subvector-fill! input-buffer 0 overlap 0.)
	      (let loop ()
		(let ((ndata (reader input-buffer overlap ibs)))
		  (if (> ndata 0)
		      (begin
			(transformation input-buffer
					overlap
					(fix:+ overlap ndata)
					output-buffer
					0)
			(writer output-buffer 0 ndata)
			(loop))))))))))))

(define (make-input-reader input-port buffer-size)
  (let ((read-substring (port/operation input-port 'READ-SUBSTRING))
	(raw-buffer (make-string (fix:* buffer-size 2))))
    (lambda (buffer start end)
      (let ((nread
	     (read-substring input-port
			     raw-buffer 0 (fix:* 2 (fix:- end start)))))
	(let ((ndata (fix:quotient nread 2)))
	  (do ((i 0 (fix:+ i 2))
	       (j start (fix:+ j 1)))
	      ((fix:>= i nread))
	    (flo:vector-set!
	     buffer
	     j
	     (l16->flonum
	      (let ((unsigned-integer
		     (fix:+ (vector-8b-ref raw-buffer (fix:+ i 1))
			    (fix:lsh (vector-8b-ref raw-buffer i) 8))))
		(if (fix:> unsigned-integer #x7FFF)
		    (fix:- unsigned-integer #x10000)
		    unsigned-integer)))))
	  ndata)))))

(define (make-output-writer output-port buffer-size)
  (let ((write-substring (port/operation output-port 'WRITE-SUBSTRING))
	(raw-buffer (make-string (fix:* buffer-size 2))))
    (lambda (buffer start end)
      (do ((i start (fix:+ i 1))
	   (j 0 (fix:+ j 2)))
	  ((fix:= i end))
	(let ((n (flonum->l16 (flo:vector-ref buffer i))))
	  (let ((n (if (fix:< n 0) (fix:+ n #x10000) n)))
	    (vector-8b-set! raw-buffer j (fix:lsh n -8))
	    (vector-8b-set! raw-buffer (fix:+ j 1) (fix:and n #xFF)))))
      (write-substring output-port raw-buffer 0 (fix:* 2 (fix:- end start))))))

(define-structure (sound-file-header
		   (conc-name sound-file-header/))
  (version #f read-only #t)
  (offset #f read-only #t)
  (size #f read-only #t)
  (block-size #f read-only #t)
  (mode #f read-only #t)
  (format #f read-only #t)
  (compress #f read-only #t)
  (rate #f read-only #t)
  (channel #f read-only #t)
  (bit-width #f read-only #t)
  (emphasis #f read-only #t)
  (extend #f read-only #t))

(define (read-sony-sound-file filename)
  (call-with-input-file filename
    (lambda (port)
      (let ((header (read-sound-file-header port)))
	(cons header
	      (read-16-data port (sound-file-header/size header)))))))

(define (read-raw-l16-file filename)
  (call-with-input-file filename
    (lambda (port)
      (read-16-data port ((port/operation port 'LENGTH) port)))))

(define (read-sound-file-header port)
  (let ((magic (read-string port 16)))
    (if (not (string=? magic "@!sound\000\000\000\000\000\000\000\000\000"))
	(error "Not sound file magic:" magic)))
  (let ((header (read-string port 48)))
    (make-sound-file-header (convert-32-bit-integer header 0)
			    (convert-32-bit-integer header 4)
			    (convert-32-bit-integer header 8)
			    (convert-32-bit-integer header 12)
			    (convert-32-bit-integer header 16)
			    (convert-32-bit-integer header 20)
			    (convert-32-bit-integer header 24)
			    (convert-32-bit-integer header 28)
			    (convert-32-bit-integer header 32)
			    (convert-32-bit-integer header 36)
			    (convert-32-bit-integer header 40)
			    (convert-32-bit-integer header 44))))

(define (convert-32-bit-integer string index)
  (let ((unsigned-integer
	 (+ (vector-8b-ref string (fix:+ index 3))
	    (* 256
	       (+ (vector-8b-ref string (fix:+ index 2))
		  (* 256
		     (+ (vector-8b-ref string (fix:+ index 1))
			(* 256 (vector-8b-ref string index)))))))))
    (if (> unsigned-integer #x7FFFFFFF)
	(- unsigned-integer #x100000000)
	unsigned-integer)))

(define (read-16-data port n-bytes)
  (let ((bytes (read-string port n-bytes))
	(n-data (fix:quotient n-bytes 2)))
    (let ((result (flo:vector-cons n-data)))
      (do ((i 0 (fix:+ i 1)))
	  ((fix:= i n-data))
	(flo:vector-set!
	 result
	 i
	 (l16->flonum
	  (let ((unsigned-integer
		 (fix:+ (vector-8b-ref bytes (fix:+ (fix:+ i i) 1))
			(fix:lsh (vector-8b-ref bytes (fix:+ i i)) 8))))
	    (if (fix:> unsigned-integer #x7FFF)
		(fix:- unsigned-integer #x10000)
		unsigned-integer)))))
      result)))

(define (write-sony-sound-file data filename)
  (call-with-output-file filename
    (lambda (port)
      (write-sound-file-header (flo:vector-length data) port)
      (write-16-data data port))))

(define (write-raw-l16-file data filename)
  (call-with-output-file filename
    (lambda (port)
      (write-16-data data port))))

(define (write-sound-file-header n-samples port)
  (write-string "@!sound\000\000\000\000\000\000\000\000\000" port)
  (write-32-bit-integer 1 port)
  (write-32-bit-integer 64 port)
  (write-32-bit-integer (* n-samples 2) port)
  (write-32-bit-integer 2 port)
  (write-32-bit-integer 1 port)
  (write-32-bit-integer 0 port)
  (write-32-bit-integer 0 port)
  (write-32-bit-integer 8000 port)
  (write-32-bit-integer 1 port)
  (write-32-bit-integer 16 port)
  (write-32-bit-integer 0 port)
  (write-32-bit-integer 0 port))

(define (write-32-bit-integer n port)
  (let* ((qr1 (integer-divide (if (< n 0) (+ n #x100000000) n) 256))
	 (qr2 (integer-divide (integer-divide-quotient qr1) 256))
	 (qr3 (integer-divide (integer-divide-quotient qr2) 256)))
    (write-char (integer->char (integer-divide-quotient qr3)) port)
    (write-char (integer->char (integer-divide-remainder qr3)) port)
    (write-char (integer->char (integer-divide-remainder qr2)) port)
    (write-char (integer->char (integer-divide-remainder qr1)) port)))

(define (write-16-data data port)
  (let ((n (flo:vector-length data)))
    #|
    (do ((i 0 (fix:+ i 1)))
	((fix:= i n) unspecific)
      (write-16-bit-integer (flonum->l16 (flo:vector-ref data i)) port))
    |#
    (let ((buffer (make-string (fix:+ n n))))
      (do ((i 0 (fix:+ i 1)))
	  ((fix:= i n) unspecific)
	(let ((n (flonum->l16 (flo:vector-ref data i))))
	  (let ((n (if (fix:< n 0) (fix:+ n #x10000) n)))
	    (vector-8b-set! buffer (fix:+ i i) (fix:lsh n -8))
	    (vector-8b-set! buffer (fix:+ (fix:+ i i) 1) (fix:and n #xFF)))))
      (write-string buffer port))))

(define (write-16-bit-integer n port)
  (let ((n (if (fix:< n 0) (fix:+ n #x10000) n)))
    (write-char (integer->char (fix:lsh n -8)) port)
    (write-char (integer->char (fix:and n #xFF)) port)))

(define (strpcm->mulaw n)
  (let ((n (flo:/ (exact->inexact n) 32768.)))
    (if (flo:< n 0.)
	(flo:/ (flo:log (flo:+ 1. (flo:* -255. n)))
	       -5.545177444479562)
	(flo:/ (flo:log (flo:+ 1. (flo:* 255. n)))
	       5.545177444479562))))

(define (convert-l16-vector->flonum-vector data)
  (let ((n (vector-length data)))
    (let ((result (flo:vector-cons n)))
      (do ((i 0 (fix:+ i 1)))
	  ((fix:= i n))
	(flo:vector-set! result i (l16->flonum (vector-ref data i))))
      result)))

(define-integrable (l16->flonum n)
  (flo:/ (int:->flonum n) 32768.))

(define (convert-flonum-vector->l16-vector data)
  (let ((n (flo:vector-length data)))
    (let ((result (make-vector n)))
      (do ((i 0 (fix:+ i 1)))
	  ((fix:= i n))
	(vector-set! result i (flonum->l16 (flo:vector-ref data i))))
      result)))

(define-integrable (flonum->l16 x)
  (flo:round->exact (flo:* x 32768.)))

(define (read-string port n)
  (let ((buffer (make-string n)))
    (let ((result ((port/operation port 'READ-CHARS) port buffer)))
      (if (not (eqv? result n))
	  (error "Bad result while reading string:" result)))
    buffer))