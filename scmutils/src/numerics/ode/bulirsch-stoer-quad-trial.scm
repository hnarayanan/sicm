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


;;; Quad precision in rational extrapolation

(define (bulirsch-stoer-setup max-depth max-width)
  (define (bsi n)
    (cons-stream (expt 2 (+ n 1))
		 (cons-stream (* 3 (expt 2 n))
			      (bsi (+ n 1)))))
  (set! *max-tableau-depth* max-depth)
  (set! *max-tableau-width* max-width)
  (let ((bulirsch-stoer-integers (cons-stream 1 (bsi 0))))
    (pp (stream-head bulirsch-stoer-integers max-depth))
    (set! bulirsch-stoer-steps
	  (list->vector
	   (map (lambda (x) (fix:* 2 x))
		(stream-head bulirsch-stoer-integers max-depth))))
    (set! bulirsch-stoer-magic-vectors
	  (make-initialized-vector *max-tableau-depth*
	    (lambda (m)
	      (make-initialized-vector (min m *max-tableau-width*)
		(lambda (k)
		  (quad:square (quad:/ (->quad
					(stream-ref bulirsch-stoer-integers m))
				       (->quad
					(stream-ref bulirsch-stoer-integers
						    (fix:- m (fix:1+ k)))))))))))
    'done))

;;; Quad precision in rational extrapolation

(define (bulirsch-stoer-floating-lisptran f n error-measure)
  (let ((mm (vector-Gragg f n))
	(state-estimate1 (flo:make-vector n 0.0))
	(state-estimate2 (flo:make-vector n 0.0))
	(gragg-output1 (flo:make-vector n 0.0))
	(gragg-output2 (flo:make-vector n 0.0))
	(tableau
	 (make-initialized-vector n
	    (lambda (i) (make-vector *max-tableau-width* quad:0)))))
    (lambda (state delta-t-suggested continuation)
      ;; continuation = (lambda (new-state actual-delta-t suggested-delta-t) ...)
      (if bulirsch-stoer-state-wallp
	  (pp `(bulirsch-stoer-state ,state ,delta-t-suggested)))
      (let outside ((delta-t delta-t-suggested))
	(let ((modified-midpoint (mm state delta-t)))
	  (modified-midpoint 2 state-estimate1)
	  (flo:vector-copy-into-vector n state-estimate1 gragg-output1)
	  (let m-loop ((m 1)
		       (old-verr)
		       (old-state-estimate state-estimate1)
		       (new-state-estimate state-estimate2)
		       (old-out gragg-output1)
		       (new-out gragg-output2)
		       (fail #f))	;zero divide would have happened
	    (if (fix:< m *max-tableau-depth*)
		(let ((m1 (min m *max-tableau-width*))
		      (d (vector-ref bulirsch-stoer-magic-vectors m)))
		  (modified-midpoint (vector-ref bulirsch-stoer-steps m) new-out)

		  (for 0 (less-than n) fix:1+
		       (lambda (i)	;coordinates
			 (declare (integrate i))
			 (let* ((dta (->quad (flo:vector-ref old-out i)))
				(yb (->quad (flo:vector-ref new-out i)))
				(c yb))
			   (for 0 (less-than m1) fix:1+
				(lambda (k) ;width of tableau
				  (declare (integrate k))
				  (let* ((b1 (quad:* (vector-ref d k) dta))
					 (den (quad:- b1 c))
					 (dtn dta))
				    (if (not (flo:= (quad:high den) 0.0))
					(let ((b (quad:/ (quad:- c dta) den)))
					  (set! dtn (quad:* c b))
					  (set! c (quad:* b1 b)))
					(set! fail #t))
				    (set! dta (vector-ref (vector-ref tableau i) k))
				    (vector-set! (vector-ref tableau i) k dtn)
				    (set! yb (quad:+ yb dtn)))))
			   (flo:vector-set! new-state-estimate i (quad:high yb)))))

		  (let ((verr (error-measure new-state-estimate old-state-estimate)))
		    (if bulirsch-stoer-error-wallp
			(pp `(bulirsch-stoer-error level: ,m error: ,verr h: ,delta-t)))
		    ;; In Jack's C program the first two conditions
		    ;; below are interchanged and the minimum number
		    ;; of iterations is set to (fix:< m 4)
		    (cond ;;(fail) 
                          ;;not good to (outside (* 0.9 delta-t)) or to m-loop with m+1
			  ((flo:< verr 2.0)
			   (continuation (flo:vector-copy new-state-estimate)
					 delta-t
					 (flo:* (flo:* delta-t bulirsch-stoer-magic-multiplier)
						(flo:expt bulirsch-stoer-magic-base
							  (exact->inexact (fix:- m m1))))))
			  ((fix:< m 2)
			   (m-loop (fix:1+ m) verr
				   new-state-estimate old-state-estimate
				   new-out old-out #f))
			  ((not (flo:< verr old-verr))
			   (outside (flo:* 0.5 delta-t)))
			  (else
			   (m-loop (fix:1+ m) verr
				   new-state-estimate old-state-estimate
				   new-out old-out #f)))))

		(outside (flo:* 0.5 delta-t)))))))))