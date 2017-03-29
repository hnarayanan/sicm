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


;;; To find the limit of (f x) as x goes to zero, rationally extrapolated 
;;;  from the given x-list.

(define (extrapolate-function-to-zero f x-list eps)
  (build-tableau-f '() '() f x-list eps 666.0))	; a beastly estimate.

(define (build-tableau-f dt dx-list f x-list eps estimate)
  (if (null? x-list) 
      (error "RATIONAL-FUNCTIONS: INACCURATE" estimate)
      (let ((dx-new (car x-list)))
	(let ((c (f dx-new)))
	  (let ((new-dt
		 (cons c (rational-interpolation dt c dx-list dx-new eps))))
	    (let ((new-estimate (sum-list-flo new-dt)))
	      (if (and (close-enuf? new-estimate estimate eps)
		       (> (length new-dt) 2))
		  (list new-estimate (length dt))
		  (build-tableau-f new-dt (cons dx-new dx-list) 
				   f (cdr x-list) eps new-estimate))))))))



;;;----------------------------------------------------------------
;;; Rational interpolation on a pair of lists

(define ((rational-function-interpolation x-list y-list eps) x)
  (let ((data (map (lambda (xl y) (cons (- x xl) y)) x-list y-list)))
    (let ((sdata (sort data (lambda (a b) (< (abs (car a)) (abs (car b)))))))
      (let ((cd (car sdata)))
	(let ((sdx (map car sdata))
	      (sdy (map cdr sdata)))
	  (if (= (car cd) 0.0)
	      (cdr cd)
	      (build-tableau-lists '() '() sdx sdy eps (cdr cd))))))))


(define (build-tableau-lists dt dx-list x-list y-list eps estimate)
  (if (null? x-list) 
      (error "RATIONAL-FUNCTIONS: INACCURATE" estimate)
      (let ((dx-new (car x-list))
	    (c (car y-list)))
	(let ((new-dt
	       (cons c (rational-interpolation dt c dx-list dx-new eps))))
	  (let ((new-estimate (sum-list-flo new-dt)))
	    (if (and (close-enuf? new-estimate estimate eps)
		     (> (length new-dt) 2))
		new-estimate
		(build-tableau-lists new-dt
				     (cons dx-new dx-list) 
				     (cdr x-list) (cdr y-list)
				     eps
				     new-estimate)))))))

(define (sum-list-flo l)  ; from small end up
  (if (null? (cdr l)) 
      (car l) 
      (flo:+ (car l) (sum-list-flo (cdr l)))))

;;;----------------------------------------------------------------
;;; The following is the core of the rational interpolation,
;;;   with the zero denominator fix used by BS and Henon.

;;; Version in Scmutils, with all flo: and .0 removed

(define (rational-interpolation dt c dx-list dx-new eps)
  (if (null? dt) 
      '()
      (let* ((dt1 (car dt))
	     (w (- c dt1))
	     (b1 (* (/ (car dx-list) dx-new) dt1))
	     (den (- b1 c)))
	(if (= den 0)
	    (begin (if zd-wallp? (display "zd "))
		   (cons dt1
			 (rational-interpolation (cdr dt)
						 c
						 (cdr dx-list)
						 dx-new
						 eps)))
	    (let* ((b (/ w den))
		   (new-d (* c b)))
	      (cons new-d
		    (rational-interpolation (cdr dt)
					    (* b1 b)
					    (cdr dx-list)
					    dx-new
					    eps)))))))
(define zd-wallp? #f)
