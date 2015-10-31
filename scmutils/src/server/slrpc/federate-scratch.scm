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
 
;(load "slrpc")
(load "slrpc-cps")
;(load "../propagator/propagator-bleeding3/support/load.scm")
;(load-relative-compiled "slrpc")


(define *federated-id*
  (string->number (get-environment-variable "SLRPC_FEDERATED_ID")))

(define *federated-port*
  (string->number (get-environment-variable "SLRPC_FEDERATED_PORT")))
  

(define remote-eval)

(define (start-master)
  (define thread-targets '())
  (define server-lock (make-lock))

  (define (wait-as-thread eval-proc)
	(let ((message-target (make-message-target)))
	  (with-lock server-lock
				 (lambda () 
				   (set! thread-targets (cons message-target thread-targets))))
	  (fluid-let ((remote-eval eval-proc));(lambda args (pp `(calling ,eval-proc on ,args))
										;	   (apply eval-proc args))))
		(wait-apply message-target))
	  (with-lock server-lock
				 (lambda () 
				   (set! thread-targets (delq message-target thread-targets))))))

  (define server (create-rpc-server))
  (register-rpc-procedure server "wait-as-thread" wait-as-thread)
  (start-rpc-server server *federated-port* (host-address-loopback))
  (list server (lambda () thread-targets)))

(define (connect-to-master) 
  (define client (create-rpc-client))
  (connect-rpc-client client *federated-port* "localhost")
  (define wait-as-thread (bind-rpc-call client "wait-as-thread"))
  (wait-as-thread (lambda (expr) (eval expr user-initial-environment))))

(define (wait-apply target)
  (let* ((message (wait-message target))
		 (cmd (car message)))
	(if (eq? cmd 'apply)
		(begin
		  ((cdr message))
		  (wait-apply target))
		(if (eq? cmd 'die)
			'terminated
			(error 'unrecognized-apply-message)))))

(define (send-apply-async target thunk)
  (let ((reply-target (make-message-target)))
	(send-message
	 target 
	 (cons 'apply
		   (lambda ()
			 (call-with-current-continuation
			  (lambda (k)		
				(send-message reply-target
							  (cons 'ok
									(with-error-filter
									 (lambda (condition)
									   (send-message reply-target condition)
									   (k condition))
									 thunk))))))))
	(lambda ()
	  (let ((reply (wait-message reply-target)))
		(set! reply-target #f) ; Should be unnecessary!!
							   ; But, helps prevent cryptic memory leaks thru interpreter.
		(if (and (pair? reply) (eq? (car reply) 'ok))
			(cdr reply)
			(begin
			  (signal-condition reply)
			  (standard-error-handler reply)))))))

(define (send-apply target thunk)
  ((send-apply-async target thunk)))

(define (map-threads func . arglist)
  (let ((delayed-replies
		 (apply map
				(cons
				 (lambda (target . args)
				   (send-apply-async target (lambda () (apply func args))))
				 (cons
				  ((second master))
				  arglist)))))				 
	(map apply delayed-replies)))


(define (eval-each-thread expr)
  (map-threads
   (lambda () (remote-eval expr))))

(define master)

(if (= *federated-id* 0)
	(set! master (start-master))
	(with-error-filter
	 (lambda (condition)
	   (pp `(error on ,*federated-id*))
	   (pp condition)
	   (stack-trace (condition/continuation condition) console-i/o-port)
	   (%exit))
	 connect-to-master))



; this serves no purpose except to work around the REPL/GC issue:
(define (synchronous-punt thunk)
  (let ((target (make-message-target)))
	(spawn 
	 (lambda () 
	   (send-message target (thunk))))
	(wait-message target)))

#|
1 ]=> (synchronous-punt (lambda () (for-each (lambda (n) (WITH-TIMINGS (lambda () (for-each (lambda (i) (map-threads apply funs2)) (iota 500))) (lambda (run-time gc-time real-time) (pp (list run-time gc-time real-time))))) (iota 12)))) ; funs2
(1290 30 2033)
(1320 20 2376)
(1390 40 2562)
(1370 20 2422)
(1410 60 2459)
(1290 20 2308)
(1220 40 2508)
(1460 30 2363)
(1420 40 2500)
(1430 20 2310)
(1380 40 2548)
(1430 20 2341)
;Unspecified return value

1 ]=> ((lambda () (for-each (lambda (n) (WITH-TIMINGS (lambda () (for-each (lambda (i) (map-threads apply funs2)) (iota 500))) (lambda (run-time gc-time real-time) (pp (list run-time gc-time real-time))))) (iota 12)))) ; funs2
(1080 40 2148)
(1440 110 2336)
(1440 80 2529)
(1460 190 2556)
(1480 220 2570)
(1490 270 2606)
(1440 440 2855)
(1520 330 2579)
(1430 550 2834)
(1540 810 3085)
(1400 890 3265)
(1250 940 3229)
;Unspecified return value

|#



#| ; Example:


#|
$ SLRPC_FEDERATED_PORT=4321 SLRPC_FEDERATED_ID=0 \
  scheme --load federate-scratch.scm
$ for id in 1 2 3 4 5; do \
  ( SLRPC_FEDERATED_PORT=4321 SLRPC_FEDERATED_ID=$id \
  scheme --load federate-scratch.scm & ); done
|#

(define fibs
  (eval-each-thread
   '(letrec ((fib
			  (lambda (n) (if (<= n 2) 1 (+ (fib (- n 1)) (fib (- n 2)))))))
	  fib)))
;Value: fibs

(map-threads apply fibs (map list (map (lambda (n) (+ n 27)) (iota 5))))
;Value: (196418 317811 514229 832040 1346269)

|#

; this cross-thread nonsense is really obnoxious, as is having to eval everything
; and string it together. what can be done better?
; can lambdas check what thread they're running on and proxy to the correct one if
; incorrect? can all session operations proxy themselves in such a manner?
; (maybe this requires a threading model where the session threads behave
; like workers? or maybe the messages to run the lambdas and such can be queued
; to the ordinary blocking read alert target?) that kind of prevents parallel
; invocation, though.




#|

(define funs (eval-each-thread '(lambda () 1)))          

(WITH-TIMINGS (lambda () (for-each (lambda (i) (map-threads apply funs)) (iota 500))) (lambda (run-time gc-time real-time) (pp (list run-time gc-time real-time))))
;(1000 20 1694)

55% cpu in master process, 10% in each of five worker processes
85% user cpu, 15% kernel cpu

So, why is run-time / real-time very close to master cpu / workers
cpu? coincidence? Or are they getting serialized for short RPCs? (I
wouldn't be surprised if they're serialized, but I don't quite see why
one thread should wait for the other's return even though each thread
does wait.)

The timing volatility does confuse me, though. Some compiles seem
worse than others, maybe as much as 20%? Or maybe it's machine state?
Master CPU usage goes up by a few percent, but clock time is even
worse.
Also, GC time seems to grow superlinearly in test run size...


Infrequent, sporadic error (~1 in 10,000+ map-threads null rpcs,
sometimes master, usually workers):

(error on 2)
#[condition 13 no-current-thread]
(type #[condition-type 14 no-current-thread])
(continuation #[continuation 15])
(restarts (#[restart 16 abort]))
(field-values #())
(properties #[1d-table 17])



Even rarer: master hangs wile sleeping
0                            (begin (%record-set! thread 4 #t) (event) (set ...
1                            (begin (if event (let ((block? (%record-ref th ...
2                            (let ((any-events? (handle-thread-events threa ...
3                            (let ((value (%suspend-current-thread))) (set- ...
4                            (begin (suspend-current-thread) (do-loop))
5    loop                    (begin (sleep 5000) (loop))
6                            (set! retval (with-alertable (message-target-a ...
7                            (let ((reply (wait-message reply-target))) (if ...
8    loop                    (cons (procedure (car l)) (quote ()))
9                            (begin (let loop ((l (cdr l)) (previous head)) ...
10   map-1                   (begin (procedure (car l)) (map-1 (cdr l)))
11                           (let ((value (thunk))) (let ((process-end (sys ...
12   %repl-eval              (let ((value (hook/repl-eval s-expression envi ...
13   %repl-eval/write        (hook/repl-write (%repl-eval s-expression envi ...
14   do-loop                 (begin (if (queue-empty? queue) (let ((environ ...
15   loop                    (loop (bind-abort-restart cmdl (lambda () (der ...

|#


; todo add rpc-proto-wallp to slrpc
