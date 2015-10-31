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


(load "slrpc-cps")

;;;FIXME test manager load path
(load "/scratch/micahbro/propagator/propagator-bleeding3/testing/load.scm")


(define (linux-command-line)
  (with-input-from-file
	  (string-append "/proc/" (number->string (unix/current-pid)) "/cmdline")
	(lambda ()
	  (let ((null-char-set (string->char-set "")))
		(read-string null-char-set)))))

(define (linux-command-line-args)
  (let ((zt-char-set (string->char-set "\000")))
	(with-input-from-string
		(linux-command-line)
	  (lambda ()
		(let readmore ()
		  (if (input-port/eof? (current-input-port))
			  '()	
			  (let ((next-input (read-string zt-char-set)))
				(read-char) ; discard null
				(cons next-input
					  (readmore)))))))))

(define *subscheme-command-line-args* (linux-command-line-args))

(load-option 'synchronous-subprocess)

(define (launch-subscheme #!optional extra-options output-port)
  (if (default-object? extra-options) (set! extra-options '()))
  (if (default-object? output-port) (set! output-port #f))
  (spawn 
   (lambda ()
	 (run-synchronous-subprocess (car *subscheme-command-line-args*)
								 (append 
								  (cdr *subscheme-command-line-args*)
								  extra-options)
								 'input #f
								 'output output-port
								 'environment
								 (vector-append scheme-subprocess-environment
												(vector "_SLRPC_IS_CHILD_WORKER=1"))
								 ; Not very composable, oh well...
								 ))))
								 



(define reporting-for-duty-target (make-message-target))

(define master-server (create-rpc-server))

(define *shutdown-worker-continuation*)

(register-rpc-procedure master-server "report-for-duty"
						(lambda (evalfunc)
						  (call-with-current-continuation
						   (lambda (k)					
							 (fluid-let ((*shutdown-worker-continuation* k))
							   (send-message
								reporting-for-duty-target
								(cons *thread-associated-rpcsession* evalfunc))
							   (rpcsession-wait-indefinitely
								*thread-associated-rpcsession*))))))

(define (shutdown-worker session)
  (send-thunk-call-async (rpcsession-async-command-target session)
						 (lambda ()
						   (*shutdown-worker-continuation* unspecific))))
; this may need an acknowledgment to prevent premature server termination...
; or at least a dumb delay loop :P
; possible signals to wait for: all clients disconnected, all spawned
; processes terminated, *async* client-shutting-down call (can't be
; synchronous because that pushes the problem back as a race against
; return)
; or, just make the damn client process quit cleanly on error :P

(define *master-server-port* #f)

(define (start-master-server)
  (set! *master-server-port*
		(start-rpc-server-free-port master-server 18300 18350 (host-address-loopback))))
; Port range is narrow as a hedge against forkbombs...
; (Also improves identifiability)

(define (stop-master-server)
  (set! *master-server-port* #f)
  (stop-rpc-server master-server))


(define (enable-most-rpc-wallps!)
  (set! *rpc-threading-wallp* #t)
  (set! *rpc-client-side-wallp* #t)
  (set! *rpc-server-side-wallp* #t)
  (set! *rpc-inbound-command-wallp* #t))

(define (make-rpc-wallp-log!)
  (set! *rpc-wallp-port* (open-output-string))
  (enable-most-rpc-wallps!))

;;(make-rpc-wallp-log!) ;;;
; (this actually only has an effect on the server because the client,
;  if it sees this file at all, stomps on those settings when
;  (re-)loading slrpc-cps)

  
(define *log-subscheme-output* #t) ;;;;;;!
(define *subscheme-output-ports* '())

(define (launch-simple-test-worker)
  (if (not *master-server-port*)
	  (error 'server-port-not-set))
  (launch-subscheme `("--eval" "(load \"slrpc-cps\")"
					  "--eval" "(define test-client (create-rpc-client))"
					  ;"--eval" "(set! *rpc-client-side-wallp* #t)" ;;;
                      ;"--eval" "(set! *rpc-server-side-wallp* #t)" ;;;
                      ;"--eval" "(set! *rpc-inbound-command-wallp* #t))" ;;;
					  "--eval" ,(string-append "(connect-rpc-client test-client "
											  (number->string *master-server-port*)
											  " \"localhost\")")
					  "--eval" "(define go (bind-rpc-call test-client \"report-for-duty\"))"
					  "--eval" "(define (die-on-error thunk) (with-error-filter (lambda (c) (ignore-errors (lambda () (pp `(exiting-on-error ,(condition/report-string c))))) (%exit 1)) thunk))"
					  "--eval" "(die-on-error (lambda () (go (lambda (expr) (eval expr user-initial-environment)))))"
					  "--eval" "(pp 'worker-done)"
					  "--eval" "(%exit 0)") ;; (does not seem to help)
					(if *log-subscheme-output*
						(let ((outport (open-output-string)))
						  (set! *subscheme-output-ports* 
								(cons outport *subscheme-output-ports*))
						  outport)
						(current-output-port))))

(define (with-simple-test-worker handler)
  (launch-simple-test-worker)
  (let* ((message (wait-message reporting-for-duty-target))
		 (session (car message))
		 (evalfunc (cdr message)))
	(handler session evalfunc)
	(shutdown-worker session)))
	;(ignore-errors (lambda () (evalfunc '(disconnect-rpc-client test-client)))))) ; hacky :P
    ;; test framework still picks up errors! dunno why...


(define (with-test-worker-ensemble n handler)
  (define (wtwe-internal n sessions evalfuncs)
	(if (> n 0)
		(with-simple-test-worker
		 (lambda (session evalfunc)
		   (wtwe-internal (- n 1)
						  (cons session sessions)
						  (cons evalfunc evalfuncs))))
		(handler sessions evalfuncs)))
  (wtwe-internal n '() '()))
					   

;;; The actual tests
(in-test-group
 test-launch-framework
 
 (define-test (smoke)
   (with-simple-test-worker
	(lambda (workersession reval)
	  (check (rpcsession-open? workersession))
	  (check (procedure? reval)))))

 (define-test (self-evaluating)
   (with-simple-test-worker
	(lambda (workersession reval)
	  (define (selfeval-ok predicate value)
		(predicate (reval value) value))
	  (check (selfeval-ok = 1))
	  (check (selfeval-ok = -1/2))
	  (check (selfeval-ok = 2.5))
	  (check (selfeval-ok = 99e-6))
	  (check (selfeval-ok eq? #t))
	  (check (selfeval-ok eq? #f))
	  (check (selfeval-ok equal? "foo bar"))
	  (check (selfeval-ok equal? "\\a\n\r\t#',@!z zz\z\\\000\111\"FOO"))
	  (check (selfeval-ok equal? #(1 2 3)))
	  (check (selfeval-ok equal? #(foo bar)))
	  (check (selfeval-ok equal? #((1) () 2 #(4 5 6))))
	  (check (selfeval-ok equal? #((x . y))))
	  (check (selfeval-ok equal? #(`foo ,bar)))
	  (check (selfeval-ok equal? #(`(a b ,c d ,@f `g 'h `(i ,j)))))
	  (check (selfeval-ok equal? #((x . ,y))))
	  (check (selfeval-ok equal? #(((unquote . quote)))))
	  (check (selfeval-ok eq? (if #f #f))))))

 (define-test (quoted-exprs)
   (with-simple-test-worker
	(lambda (workersession reval)
	  (define (quoted-ok predicate value)
		(predicate (reval (list 'quote value)) value))
	  (check (quoted-ok = 1))
	  (check (quoted-ok = 3/300))
	  (check (quoted-ok = 1))
	  (check (quoted-ok eq? 'someatom))
	  (check (quoted-ok eq? '()))
	  (check (quoted-ok equal? "strrrrrING!"))
	  (check (quoted-ok equal? #()))
	  (check (quoted-ok equal? '(a b ,c d ,@f `g 'h `(i ,j))))
	  )))
 
 (define-test (simple-funcs)
   (with-simple-test-worker
	(lambda (workersession reval)
	  (define rcons (reval 'cons))
	  (define r+ (reval +))
	  (check (equal? (rcons 'a '()) '(a)))
	  (check (= (r+ 1 2) 3))
	  (check (= (apply r+ (iota 100)) (apply + (iota 100)))))))

 (define-test (callbacks)
   (with-simple-test-worker
	(lambda (workersession reval)
	  (define rapply (reval 'apply))   
	  (define r* (reval *))
	  (check (= (rapply + (iota 10)) (apply + (iota 10))))
	  (check (= (rapply r* '(5 2 3)) 30))
	  (let ((result #f))
		(rapply (lambda (n) (set! result (r* (r* 3 4) n))) '(2))
		(check (= result 24)))
	  (check (= (rapply (reval '(lambda (f n) (* (f (+ n 1)) 2)))
						(list (lambda (x) (* x x)) 3))
				32))
	  )))
	  
 (define-test (simple-errors)
   (with-simple-test-worker
	(lambda (workersession reval)
	  (define rcons (reval 'cons))
	  (define (get-error thunk)
		(call-with-current-continuation
		 (lambda (k)
		   (let ((value
				  (with-error-filter
				   (lambda (condition) (k condition))
				   thunk)))
			 (error 'error-expected value)))))
	  (check (get-error (lambda () (reval '*this-is-not-bound*))))
	  (check (get-error (lambda () (reval '('not-a-procedure)))))
	  (check (get-error (lambda () (rcons))))
	  (check (rcons 1 2))
	  (check (get-error (lambda () (rcons 1 2 3))))
	  (define rapply (reval 'apply))
	  (check (get-error (lambda () (rapply))))
	  (check (get-error (lambda () (rapply (lambda (x) (/ 1 x)) '(0)))))
	  (check (get-error (lambda ()
						  (rapply
						   (reval '(lambda (f) (f 1)))
						   (list cons)))))
	  (check (equal? (rcons 0 (rcons 1 '(2 3 4))) (iota 5))) ; Make sure it still works!	  
	  )))

 (define-test (uninterned-eq)
   (with-simple-test-worker
	(lambda (workersession reval)
	  (define rgensym (reval 'generate-uninterned-symbol))
	  (define rapply (reval 'apply))
	  (check (symbol? (rgensym)))
	  (check ((reval 'symbol?) (generate-uninterned-symbol)))

	  (let ((sym (rgensym)))
		(check ((reval 'eq?) sym sym)))
	  (let ((sym (generate-uninterned-symbol)))
		(check ((reval 'eq?) sym sym)))
	  
	  (define (all-different? l)
		(not
		 (any (lambda (i)
				(any (lambda (j)
					   (and (not (= i j))
							(eq? (list-ref l i)
								 (list-ref l j))))
					 (iota (length l))))
			  (iota (length l)))))
	  
	  (define remotesyms (map (lambda (i) (rgensym)) (iota 100)))
	  (define localsyms (map (lambda (i) (generate-uninterned-symbol)) (iota 100)))
	  (define bothsyms (append localsyms remotesyms))
	  (check (all-different? bothsyms))
	  (check (rapply (reval '(lambda (f arg) (f (reverse arg))))
					 (list
					  (lambda (arg) (equal? arg (reverse bothsyms)))
					  bothsyms)))
	  )))

 (define-test (continuations)
   (with-simple-test-worker
	(lambda (workersession reval)
	  (define rapply (reval 'apply))
	  (check (= (call-with-current-continuation 
				 (lambda (k) (rapply k '(42)))) 42))
	  (check (= (call-with-current-continuation 
				 (lambda (k)
				   (rapply
					(reval '(lambda (f n) (f (+ n 1))))
					(list (lambda (n)
							(k (* n 2)))
						  7))))
				16))

	  (define *test-log-results* '())
	  (define (log-result value)
		(set! *test-log-results*
			  (cons value *test-log-results*)))
	  (define (get-log-results!)
		(let ((results (reverse *test-log-results*)))
		  (set! *test-log-results* '())
		  results))
	  
	  (reval '(define %worker-log))
	  (rapply (reval '(lambda (f) (set! %worker-log f)))
			  (list log-result))

	  (define localfluid 'dummy)
	  (define test-coroutine (reval 
							  '(lambda (k l rdp)
								 (for-each
								  (lambda (x)
									(%worker-log `(hi ,x))
									(rdp x)
									(call-with-current-continuation
									 (lambda (j)
									   (k (list j x))))
									(%worker-log `(ho ,x)))
								  l))))
	  
	  (check
	   (eq?
		(fluid-let ((localfluid 'baz))
		  (let ((thepair
				 (call-with-current-continuation
				  (lambda (k)
					(mobile-fluid-bind
					 'snoog 'boog
					 (lambda () 
					   (fluid-let ((localfluid 'fuzz))
						 (test-coroutine
						  k
						  (iota 4)
						  (lambda (x)
							(log-result `(rdp ,x ,localfluid
											  ,(mobile-fluid-query 'snoog))))))))))))
			(if (pair? thepair)
				(begin (log-result `(got ,(cadr thepair) ,localfluid 
										 ,(mobile-fluid-query 'snoog #f)))
					   (fluid-let ((localfluid 'bork))
						 (mobile-fluid-bind 'snoog 'choog
											(lambda ()
											  ((car thepair) 'dummy2)))))
				'done)))
		'done))
	  
	  (check (equal? (get-log-results!)
					 '((hi 0)
					   (rdp 0 fuzz boog)
					   (got 0 baz #f)
					   (ho 0)
					   (hi 1)
					   (rdp 1 fuzz boog)
					   (got 1 baz #f)
					   (ho 1)
					   (hi 2)
					   (rdp 2 fuzz boog)
					   (got 2 baz #f)
					   (ho 2)
					   (hi 3)
					   (rdp 3 fuzz boog)
					   (got 3 baz #f)
					   (ho 3))))

	  (check (= (rapply (reval '+) '(1 2 3)) 6)) ; Make sure it still works!
	  )))

 )

(define (reps-with-timings n thunk)
  (for-each
   (lambda (rep)
	 (with-timings
	  thunk
	  (lambda (run-time gc-time real-time)
		(pp (list run-time gc-time real-time)))))
   (iota n)))

(in-test-group
 stress-tests

 (define-test (simple-stress)
   (with-simple-test-worker
	(lambda (workersession reval)
	  (define rapply (reval 'apply))
	  (define fun (reval '(lambda (i) (+ i 1))))
	  (reps-with-timings 5 (lambda ()
							 (for-each fun (iota 1000)))))))

#|
   ; doesn't work, causes erratic errors, don't know why!
   ; mostly unparseable command and session closed
   ; (actually, this may be a general gc-related issue that just
   ;  shows more frequently here than anywhere else...
   ;  crops up with low frequency in the gc test, following a
   ;  remote invoke of a gc-flip. appears, bizarrely,
   ;  as receiving OID folloped by (drop), rather than (drop
   ;  OID)... but also rop followed by (dr OID).
   ; have not managed to produce in old federated tests.)
|# ; apparent cause: quasi-reentrant calls to (read)

 (define-test (simple-threaded-stress)
   (with-test-worker-ensemble
	5
	(lambda (workersessions revals)
	  (define funs (map (lambda (reval)
						  (reval '(lambda (i) (+ i 1))))
						revals))
	  (define done-targets (map (lambda (i) (make-message-target))
								(iota (length funs))))
	  (reps-with-timings
	   5
	   (lambda ()
		 (for-each
		  (lambda (f done-target) 
			(spawn 
			 (lambda () 
			   (for-each f (iota 300))
			   (send-message done-target 'done))))
		  funs done-targets)
		 (for-each wait-message done-targets))))))


 (define-test (simple-gc-stress)
   (with-simple-test-worker
	(lambda (workersession reval)
	  (define rapply (reval 'apply))
	  (define fun (reval '(lambda (i)
							(call-with-current-continuation
							 (lambda (k)
							   (list
								k 
								(lambda () k)
								(iota 100)))))))
	  (newline)
	  (pp `(local-heap before ,(gc-flip)))
	  (reval '(pp `(remote-heap before ,(gc-flip))))
	  (reps-with-timings
	   20
	   (lambda ()
		 (for-each (lambda (i) (fun (lambda () i)))
				   (iota 1000))))
	  (pp `(local-heap after-1 ,(gc-flip)))
	  (reval '(pp `(remote-heap after-1 ,(gc-flip))))
	  (pp `(local-heap after-2 ,(gc-flip)))
	  (reval '(pp `(remote-heap after-2 ,(gc-flip))))
	  (sleep 500)
	  (pp `(local-heap after-3 ,(gc-flip)))
	  (reval '(pp `(remote-heap after-3 ,(gc-flip))))
	  ; fixme it's a little screwey to look at repeated GCs
	  ; (object table cleanup is asynchronous)
	  (newline)
	  )))
 )



;todo objects
;todo gc
;todo reconnection	  
;todo parallel stress
;todo butterfly
;todo multihop object passing
;todo client-side calls
;todo threading unit
;todo threading stress
;todo network & proto errors


(if (not (get-environment-variable "_SLRPC_IS_CHILD_WORKER"))
	(dynamic-wind
		start-master-server ; this isn't really re-doable for a given test,
						    ; but it should allow later tests to run...
		run-registered-tests
		stop-master-server))
