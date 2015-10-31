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

;;;;;;;;;;;;;;;;;;;;;;;;;;;;
;;; Scheme Light-weight RPC 
;;; CPS hack-a-roo...
;;;;;;;;;;;;;;;;;;;;;;;;;;;;

;;; Author: Micah Brodsky
;;; Version: 0.2b, spring 2012
;;; GJS fixed fluid references 22Nov2014

;;; Examples
;;;;;;;;;;;;

;;; Client-side example:
#| 
(load "slrpc.scm")
;Loading "slrpc.scm"...
;... done
;Value: marshal-data

(define client (create-rpc-client))
;Value: client

;; Substitute your server IP address, or "localhost" if same machine

(connect-rpc-client client 3457 "128.30.16.146") 
;Unspecified return value

(define simp (bind-rpc-call client 'simplify))
;Value: simp

(simp 3)
;Value: 3

(simp '(+ a 3))
;Value 1: (+ 3 a)

;; Example of a remote error -- incorrect number of args:

(simp)
; rpc-remote-error-raised #(remote-condition "The procedure
; #[compiled-procedure 15 (\"print\" #x6) #x1a #x32d3652] has been
; called with 0 arguments; it requires exactly 1 argument.")
;To continue, call RESTART with an option number:
; (RESTART 1) => Return to read-eval-print level 1.

(disconnect-rpc-client client)
;Unspecified return value
|#


;;; Server-side example:
#| 
(load "slrpc.scm")
;Loading "slrpc.scm"...
;... done
;Value: marshal-data

(define server (create-rpc-server))
;Value: server

(register-rpc-procedure server "simplify" simplify)
;Unspecified return value

; Runtime package threading version:
(start-rpc-server server 3457 (host-address-any))
;Unspecified return value

; Comment: Use (host-address-loopback) to restrict to local
; connections as a security measure.

; ... do stuff from client ...

(stop-rpc-server server)
;Unspecified return value

;; Conspiracy threading version (does not presently work!):
;(with-time-sharing-conspiracy
; (lambda ()
;   (start-rpc-server server 3457 (host-address-any))
;   (conspire:null-job)))
;; ... (doesn't return until CTRL+C) ...

|#


;;; Common defitions
;;;;;;;;;;;;;;;;;;;;

(declare (usual-integrations))

(define (self-relatively thunk) ; courtesy of axch
  (let ((place (ignore-errors current-load-pathname)))
    (if (pathname? place)
        (with-working-directory-pathname
         (directory-namestring place)
         thunk)
        (thunk))))

(define (load-relative filename)
  (self-relatively (lambda () (load filename))))


;;; Session structure definition:
(define (%make-rpcsession socket dispatch-table)
  (list 'rpcsession socket (make-strong-eq-hash-table) dispatch-table 0
		(make-message-target) #t #f #f (make-reentrant-wind)))
; [Treat session as thread-local, with the exception of the dispatch table,
;  which has a global lock, and the async command target.]
(define (rpcsession-socket session)
  (cadr session))
(define (rpcsession-strong-object-table session)
  (third session))
(define (rpcsession-dispatch-lookup session procname default)
  (if (string=? procname "")
	  rpc-object-invoke-stub ; hackhack :P
	  (if (string=? procname "%raise-condition")
		  stub-raise-condition ; hackhack again
		  (let ((dispatch-table (fourth session)))
			(if dispatch-table
				(with-lock *dispatch-tables-global-lock*
						   (lambda ()
							 (hash-table/get dispatch-table procname default)))
				default)))))
(define (rpcsession-next-seqno session)
  (fifth session))
(define (set-rpcsession-next-seqno! session seqno)
  (set-car! (cddddr session) seqno))
; sequence number isn't so useful anymore... maybe it should be removed.
(define (rpcsession-async-command-target session)
  (sixth session))
(define (rpcsession-open? session)
  (not (message-target-closed?
		(rpcsession-async-command-target session))))
; Field 7 no longer in use:
;(define (rpcsession-open? session)
;  (seventh session))
;(define (set-rpcsession-open session flag) 
;  (set-car! (cddr (cddddr session)) flag))
;; (this flag partly overlaps in functionality with session == #f on the client...)
;; (note that this does not reflect socket errors, only whether the session has been
;;  closed by the user)
;; (uhh... why no bang?)
;; (annd.. now it's also redundant with the closed flag on the async command target??)
;; (FIXME?)
(define (rpcsession-root-continuation session)
  (eighth session))
(define (set-rpcsession-root-continuation! session continuation)
  (set-car! (cdddr (cddddr session)) continuation))
(define (rpcsession-preferred-thread session)
  (ninth session))
(define (set-rpcsession-preferred-thread! session thread)
  (set-car! (cddddr (cddddr session)) thread))
(define (rpcsession-read-port-reentrant-wind session)
  (tenth session))

(define (rpcsession-close session)
  ;(set-rpcsession-open session #f)
  (let ((target (rpcsession-async-command-target session)))
	(if (not (message-target-closed? target))
		(close-message-target target)))
  (ignore-errors ; Wierd errors can bubble out on a close of a broken socket
   (lambda () 
	 (close-port (rpcsession-socket session)))))


;;; Error handling helpers:

(define (error-with-filter fail-filter . args)
  (if (default-object? fail-filter) (set! fail-filter (lambda (cond) #f)))
  (bind-condition-handler (list condition-type:error) fail-filter
						  (lambda () (apply error args))))
(define (with-error-filter fail-filter thunk)
  (bind-condition-handler (list condition-type:error)						  
						  (lambda (condition)
							; Convenience filter for some occasional situations...
							(if (default-object? fail-filter)
								#f
								(fail-filter condition)))
						  thunk))

; If you want native conditions some of the time, it's hard to avoid making
; them all the time. (It's nice for the client side to bubble out ordinary
; exceptions on trouble, including restarts and stack traces.)
; However, I don't like pure exception handling (bind-condition-handler),
; because of the tendency to silently sweep up unexpected errors (though
; this is less of a concern with pure functional code because you know no
; state has been corrupted).
; So... Instead we use a form of continuation-based error handling. Errors
; are passed as conditions to a filter continuation. If the filter returns
; normally without bailing out, the condition is allowed to propagate as an
; exception, which should be fatal.

; Signal an internal consistency failure -- equivalent to an assertion
; failure.  Such errors should never be silently forwarded over the
; wire, because in theory they should be impossible to provoke except
; due to bugs in the RPC library.
(define (internal-error . args)
  ; Unfortunately, we don't seem to have a clean way to catch "all errors but these".
  ; So, we print a bunch of warning spam and then raise an ordinary error.
  (pp "Internal error: " *rpc-wallp-port*)
  (pp args)
  (call-with-current-continuation 
   (lambda (k)
	 (stack-trace k *rpc-wallp-port*)))
  (apply error args))


; Client-side
;;;;;;;;;;;;;

;;; Client structure definition:
(define (create-rpc-client)
  (list 'rpcclient #f #f))
(define (rpcclient-session client)
  (cadr client))
(define (set-rpcclient-session! client session)
  (set-car! (cdr client) session))
(define (rpcclient-session-owner client)
  (third client))
(define (set-rpcclient-session-owner! client owner)
  (set-car! (cddr client) owner))
; (A dedicated session owner field is necessary because looking at the session
;  itself to see if it is owned by another thread implies a race against
;  termination, when that thread relinquishes ownership as per current
;  semantics.)

(define (connect-rpc-client client portnum address)
  (disconnect-rpc-client client)
  (set-rpcclient-session! client 
						  (%make-rpcsession
						   (open-tcp-stream-socket address portnum) #f)
						   ; exposes network errors
						  ))

(define (disconnect-rpc-client client)
  (let ((oldsession (rpcclient-session client)))
	(if oldsession
		(if (rpcclient-session-owner client)
			(rpcsession-queue-shutdown oldsession) ; async cross-thread close
			(rpcsession-close oldsession)))) ; local close
  (set-rpcclient-session-owner! client #f)
  (set-rpcclient-session! client #f))

(define (bind-rpc-call client procname)
  (let ((procname-str (if (symbol? procname)
						  (symbol->string procname)
						  procname)))
	(lambda args
	  (proxy-invoke-rpc-call (rpcclient-session client) procname-str args))))

(define (bind-rpc-call-async client procname)
  ; Curried to accept an optional return thunk (pass #f if none desired)
  (let ((procname-str (if (symbol? procname)
						  (symbol->string procname)
						  procname)))
	(lambda (return-thunk) 
	  (lambda args
		(proxy-invoke-rpc-call-async (rpcclient-session client) procname-str
									 args return-thunk)))))

; It's kind of a screw that registering and binding RPC calls is
; completely different client vs. server. Maybe server should be
; replaced by "multi-threaded poly-session" and client by
; "single-threaded (restartable) mono-session", agnostic to how the
; sessions are created?

; Very simplistic polymorphic-to-host dispatch
; (does *not* consider session threading, yet)
(define (bind-robj-dispatch-call dispatch-arg-index procname local-handler)
  (let ((procname-str (if (symbol? procname)
                          (symbol->string procname)
                          procname)))
    (lambda args
      (if (<= (length args) dispatch-arg-index)
		  (error 'rpc-too-few-args-for-dispatch procname)
		  (let ((dispatch-obj (list-ref args dispatch-arg-index)))
			(if (remote-object? dispatch-obj)				
				(proxy-invoke-rpc-call (remote-object-session dispatch-obj)
									   procname-str args)
				(apply local-handler args)))))))


;;; Dual-use functions (prototypically for client-side):

;; Reentrant Wind:
;  Snarf an interruptable but non-reentrant call into a stream-like
;  coroutine, such that reentrant requests are shuttled back to
;  earlier, uncompleted calls, and calls stolen thusly are replaced
;  with new calls.  (Yes, this is an ugly kludge. Prepare your dynamic
;  winds for a workout.)

(define (make-reentrant-wind)
  (list 'reentrant-wind '() #f #f))
(define (reentrant-wind-target-continuations rw)
  (cadr rw))
(define (reentrant-wind-next-target-cont rw)
  (car (cadr rw)))
(define (set-reentrant-wind-target-continuations! rw k)
  (set-car! (cdr rw) k))
(define (push-reentrant-wind-target-cont! rw k)
  (set-reentrant-wind-target-continuations!
   rw (cons k (reentrant-wind-target-continuations rw)))
  unspecific)
(define (pop-reentrant-wind-target-cont! rw)
  (let ((next (reentrant-wind-next-target-cont rw)))
	(set-reentrant-wind-target-continuations!
	 rw (cdr (reentrant-wind-target-continuations rw)))
	next))
(define (reentrant-wind-pending-continuation rw)
  (third rw))
(define (set-reentrant-wind-pending-continuation! rw k)
  (set-car! (cddr rw) k))
(define (reentrant-wind-thread rw)
  (fourth rw))
(define (set-reentrant-wind-thread! rw thread)
  (set-car! (cdddr rw) thread))

; Helper fn to verify that nobody is abusing the thread-local
; requirement for reentrant winds.
(define (verify-reentrant-wind-thread rw thunk)
  (let ((rwthread (reentrant-wind-thread rw)))
	(dynamic-wind
		(lambda ()
		  (if rwthread
			  (if (not (eq? rwthread (get-current-thread)))
				  (error 'reentrant-wind-threading-violation
						 rw rwthread (get-current-thread)))
			  (set-reentrant-wind-thread! rw (get-current-thread))))
		thunk
		(lambda ()
		  (set-reentrant-wind-thread! rw rwthread)))))

; does this handle errors from thunk in any sane way??
; errors will bubble out at the outermost reentrant call, while simply
; winding over the inner ones with no condition visible.
; it might be worth capturing and forwarding errors in addition to
; ordinary return values...

; todo rw should probably encapsulate the message-target, because it's not
; safe to use more than one target with a rw -- you could pick up an 
; unfinished call handling messages from the wrong target.

(define (reentrant-messageable-wind rw message-target handler thunk)
  (verify-reentrant-wind-thread
   rw
   (lambda ()
	 (if (reentrant-wind-pending-continuation rw)
		 (begin
		   ;(pp '(reentering-rwind ,rw) *rpc-wallp-port*)
		   (let ((result
				  (call-with-current-continuation 
				   (lambda (return-k)
					 (push-reentrant-wind-target-cont! rw return-k)
					 ((reentrant-wind-pending-continuation rw) unspecific)))))
			 ;(pp '(re-exiting-rwind ,rw) *rpc-wallp-port*)
			 result))
	  
		 (call-with-current-continuation
		  (lambda (top-k)
			;(pp '(entering-rwind ,rw) *rpc-wallp-port*)
			(push-reentrant-wind-target-cont! rw top-k)		   
			; FIXME what if thunk winds out never to return? (e.g. with a condition)
			; Seems like we leak target stack entries. fix with dynamic-wind?
			; (though in practice, for what we use it for, it probably doesn't matter
			;  much, because once errors occur, errors always occur...)
			(let try-again ()
			  (let ((result
					 (with-messageable
					  message-target
					  (lambda (message)
						(if (reentrant-wind-pending-continuation rw)
							; We must've been invoked recursively -- something handler
							; did caused itself to be interrupted by another message.
							; Just call handler and return, the old pending continuation
							; is still fine.
							(begin
							  (if *rpc-threading-wallp*
								  (pp '(recursive-handler-in-reentrant-wind ,rw ,message) *rpc-wallp-port*))
							  (handler message))
							
							; Normal case -- snap a pending continuation.
							(begin
							  (call-with-current-continuation
							   (lambda (interruption-k)
								 (set-reentrant-wind-pending-continuation!
								  rw interruption-k)
								 
								 (handler message)
						   
								 ; If this call's return has already been used,
								 ; start a new one.
								 (if (not (reentrant-wind-pending-continuation rw))
									 (within-continuation top-k try-again))))
							  
							  ; We've been given the signal to run forward.
							  
							  ; If "we"'re not still pending, something's wrong.
							  (if (not (reentrant-wind-pending-continuation rw))
								  (internal-error 'confused-reentrant-wind-2 rw))
							  
							  ; Returning to call; no longer available as pending
							  ; (until we get interrupted with a new message).
							  (set-reentrant-wind-pending-continuation! rw #f))))
					  thunk)))
				(if (reentrant-wind-pending-continuation rw)
					(internal-error 'confused-reentrant-wind-3 rw))
				((pop-reentrant-wind-target-cont! rw) result)))))))))

; maybe I should junk this as "too clever by a half"? :P
; can I replace it with a buffering mechanism that attempts to parse
; after each successful block read, but otherwise just leaves bytes
; in a buffer? (might be tricky to avoid pathological quadratic overhead,
; though... at least without coroutines)
; or maybe a reader co-routine at top level? (would make stack traces
; ugly, though)
; additional possible complications in both cases: interrupt-triggered
; continuations carry most of the complications of threads in some
; implicit fashion or another. for a reader co-routine, we need
; read to be abortable, resumable, interruptable while aborted/unwound
; with writes, and (nonresumably at a minimum) interruptable while
; aborted/unwound with close. a block reader would separate parsing
; from port reading, and it's more likely parsing could be made
; amenable to these gyrations, but primitive, blocking block reads would
; still need to be either transactionally abortable to avoid lost data
; or would have to be similarly interruptable and resumable.

; a possible out might be to use non-blocking block reads and a pure
; functional blocking api, where blocking should thus be trivially
; transactional, and the stateful reads would never need to be abortable.
; parsing would still need to be based on coroutines or retries, though.

; another possibility, from gerry: this is roughly equivalent to a
; pclsr'ing problem. the right thing to do might be to wrap the port
; with a transactional wrapper port that can be snapshotted prior to
; issuing a read and rolled back if the read is aborted.


(define (rpcsession-read session failf)
  (if (not (rpcsession-open? session))
	  (error-with-filter failf 'rpcsession-closed session))
  (if (not (eq? (rpcsession-preferred-thread session) (get-current-thread)))
	  (error-with-filter failf 'bad-rpcsession-threading session))
  ; (...it seems like this check *ought* to apply to writes as well, but those
  ;  can happen outside of a handler loop, and so without a clearly associated
  ;  preferred thread...)
  (call-with-current-continuation
   (lambda (top-k)
	 ; Wrap read in a reentrant-wind coroutine so that interrupted reads
	 ; do not lose or duplicate data.
	 (reentrant-messageable-wind
	  (rpcsession-read-port-reentrant-wind session)
	  (rpcsession-async-command-target session)
	  (lambda (message)
		(if (pair? message)
			(let ((command (car message)))
			  (cond
			   ((eq? command 'drop)
				(let ((oid (cadr message)))
				  (rpcsession-issue-async-command session `(drop ,oid))))
			   ((eq? command 'exec) ; Same spec as wait-thunk-call

				; Unwind out of the messageable block, so that we're not holding
				; locks, then execute the thunk. We do not unwind all the way,
				; say, to the root, to avoid unnecessarily tripping user dynamic
				; winds. (Some of that is unavoidable if the thunk has user code
				; and itself calls into a read, which may wind back here, though.)

				; NOTE: Unwinding and rewinding like this can allow pending
				; messages to be processed, so the message handler make be
				; invoked quasi-recursively below.

				; (It might even be possible to revise with-alertable
				; to meaningfully allow reentrant invocation from an
				; alert handler &c; the obvious stumbling block is
				; that the lock is still held, and trying to drop it
				; might have unforseen consequences.)

				; TODO should we check on return for termination or other
				; issues that may have happened on a pseudo-recursive reentry?
				; Probably not critical, but may improve the sanity of reported
				; errors.
				(call-with-current-continuation
				 (lambda (k)
				   (within-continuation top-k
										;(rpcsession-root-continuation session)
										(lambda ()
										  ((cadr message))
										  (k unspecific))))))
			   ((eq? command 'terminate)
				(begin
				  (rpcsession-close session)
				  (error-with-filter failf 'rpcsession-terminated)))
			   (#t (internal-error 'rpcsession-bad-async-command command))))
			(internal-error 'rpcsession-bad-async-command-message message)))
	  (lambda ()
		(with-error-filter
		 (lambda (condition)
		   ; Punt the fail filter out of the interruptable extent
		   (call-with-current-continuation
			(lambda (error-k)
			  (within-continuation top-k
								   (lambda ()
									 (error-k (failf condition)))))))
		 (lambda ()
		   (read  ; sure hope this is security-safe! :P (do we care about memory exhaustion?)
			; ...is it safe for a read to be interrupted with a write on same port?
			(rpcsession-socket session)))))))))

(define (rpcsession-queue-shutdown session)
  (send-message (rpcsession-async-command-target session)
				'(terminate)))

(define (rpcsession-queue-drop session oid)
  (send-message (rpcsession-async-command-target session)
				`(drop ,oid)))				

(define (rpcsession-queue-call session thunk)
  (send-thunk-call-async (rpcsession-async-command-target session)
						 (let ((mobile-dynamic-context *mobile-dynamic-context*))
						   (lambda ()
							 (fluid-let ((*mobile-dynamic-context* mobile-dynamic-context))
							   (thunk))))))

; Specify explicit port, because RPC machinery can get called from
; weird places with output redirection.
(define *rpc-wallp-port* (trace-output-port))
; (this is a little hokey, maybe it should either be console-i/o-port, or every
;  wallp print should call trace-output-port...)

(define *rpc-client-side-wallp* #f)

(define (with-sane-unparser-config thunk)
  ; Fix unparser global state. Ugh!
  ; why isn't it port-associated??
  (let-fluids *unparser-list-breadth-limit* #f
              *unparser-list-depth-limit* #f
              *unparser-string-length-limit* #f
              *unparse-abbreviate-quotations?* #t
              *parser-canonicalize-symbols?* #t
        thunk))

(define (rpcsession-issue-async-command session command)
  ; Exposes errors directly for the client
  ; (perhaps a bit too much, such that server sweeps them up?)
  (if (not (rpcsession-open? session))
      (error 'rpcsession-closed session))
  (let ((socket (rpcsession-socket session)))
	(if *rpc-client-side-wallp* (begin
								  (display "Command: " *rpc-wallp-port*)
								  (display (get-current-thread) *rpc-wallp-port*) ;;;TMPTMP
								  (pp command *rpc-wallp-port*)))
	(with-sane-unparser-config
	 (lambda ()
	   (write command socket)
	   (newline socket)
	   ;(flush-output socket)
	   ; (so, when do flushes happen if you don't request them manually...?)
	   ))))

(define (rpcsession-flush-commands session)
  (flush-output (rpcsession-socket session)))
; fixme not sure it actually helps any to avoid unnecessary flushes... :P

; todo remove me eventually?
(define (rpcsession-issue-command-with-replyhandler session command replyhandler)
  ; Exposes errors directly for the client
  ; (perhaps a bit too much, such that server sweeps them up?)
  (rpcsession-issue-async-command session command)
  (rpcsession-flush-commands session)

  (call-with-current-continuation
   (lambda (k)
	 (handle-connection-loop session
							 (cons (replyhandler k) *default-rpc-command-handlers*)
							 (lambda (cond) #f))
	 (error 'rpc-unexpected-eof))))

(define (rpcsession-issue-command session command)
  ; Exposes errors directly for the client
  ; (perhaps a bit too much, such that server sweeps them up?)
  (rpcsession-issue-async-command session command)
  (rpcsession-flush-commands session)
  (rpcsession-wait-indefinitely session)
  (error 'rpc-unexpected-eof))

; It would be nice if multiple client threads could multiplex calls over the same socket,
; but the shared blocking dispatch mechanism and locking would be a pain. For now, if you
; want multiple threads calling and blocking on a server, you'll need one client connection
; per thread.

(define *mobile-dynamic-context* '())

(define (mobile-fluid-bind symbol value thunk)
  (let ((old-state *mobile-dynamic-context*))
	(dynamic-wind
		(lambda () (set! *mobile-dynamic-context*
						 (cons (cons symbol value)
							   (del-assq symbol *mobile-dynamic-context*))))
		thunk
		(lambda () (set! *mobile-dynamic-context* old-state)))))
; it *might* be more efficient to use in-place mutation -- it would produce less garbage
; but it requires a second scan at the end to remove the new and reinsert the old.

(define (mobile-fluid-query symbol #!optional default)
  (let ((pair (assq symbol *mobile-dynamic-context*)))
	(if pair
		(cdr pair)
		(if (default-object? default)
			; (of course, this means your default can't evaluate to #!default :P )
			(error 'mobile-fluid-unbound symbol)
			default))))

(define (augment-dynamic-roots root-continuation)
  (mobile-fluid-bind '%roots
					 (cons (cons *machine-instance-id*
								 ; FIXME? We use an OID, not a first-class
								 ; continuation that would be marshalled. This is
								 ; slightly dubious and probably should be removed.
								 ; As far as I can tell, the root should never be
								 ; usable if the return continuation becomes
								 ; unusable, so ATM this slightly saves on
								 ; marshalling overhead without introducing dangling
								 ; refs. But, if marshalling is fixed so redundant
								 ; objects are handled nonredundantly, then this
								 ; will be *less* efficient. :P
								 (get-object-id root-continuation))
						   (del-assoc *machine-instance-id*
									  (mobile-fluid-query '%roots '())))
					 (lambda ()
					   ; it would be more efficient to avoid the
					   ; dynamic wind and all, but that would make
					   ; this function messier and more redundant...
					   *mobile-dynamic-context*)))

; this whole thing would probably be more efficient if the callee
; rather than the caller did the dynamic root bookkeeping... though
; then the callee needs to know what the global machine instance names
; are. is that the "right" thing to do?

(define (get-dynamic-root dynamic-context)
  (let ((rootspair (assq '%roots dynamic-context)))
	(if rootspair
		(let ((localpair
			   (assoc *machine-instance-id* (cdr rootspair))))
		  (if localpair
			  (id-to-object (cdr localpair))
			  #f))
		#f)))

(define (make-cinvoke session seqno procname args root-continuation return-continuation)
  (list 'cinvoke seqno 
		(marshal-data (list procname args
							(if (not root-continuation)
								(if (continuation? return-continuation)
									(augment-dynamic-roots return-continuation)
									; If we're not using call-return semantics, assume
									; that nested dynamic extent is also not wanted.
									; (this may not play well with bizarre client-side tail
									;  calls, but whatever...)
									*mobile-dynamic-context*)
								(if (eq? root-continuation procname)
									#t ; optimization hack
									root-continuation))
							return-continuation)
					  session #f)))
; TODO FIXME restrict procname so that it can only be a prior known object, rather
; than a newly marshalled thing? (quasi-DoS issue, of sorts, since servers can be asked
; to call callbacks even with no functions exported. Less so if they could default to
; tail-calling it...) Might do this by passing just an oid rather than marshalling
; procname... though that's a bit of an end-run around marshalling.

(define (proxy-invoke-rpc-call session procname args)
  (proxy-invoke-rpc-call-internal
   session
   (lambda (seqno k)
	 (rpcsession-issue-command session
							   (make-cinvoke session seqno procname
											 args #f k)))))

(define (proxy-invoke-rpc-call-async session procname args callback)
  (proxy-invoke-rpc-call-internal
   session
   (lambda (seqno k)
	 (rpcsession-issue-async-command session
									 (make-cinvoke session seqno procname
												   args #f callback))
	 (rpcsession-flush-commands session))))

(define (proxy-invoke-rpc-call-internal session command-issuer)
  (if (not session)
	  (error 'rpc-error-session-not-connected))
  
  (define (proxythunk)
	(let ((seqno (rpcsession-next-seqno session)))
	  (set-rpcsession-next-seqno! session (+ seqno 1))
	  
	  (call-with-current-continuation
	   (lambda (k)	   
		 (command-issuer seqno k)))))

  ; Running on the wrong thread? Punt to the right one.

  ; NOTE: This does not absolve the caller from all synchronization
  ; concerns. In particular, client->session, object->session, and
  ; session->thread relationships are unsynchronized. In order to
  ; safely use this thread-punting facility, the caller must ensure
  ; that the session's thread is not dropping in and out of a handler
  ; loop (so that session->thread is stable) and must be willing to
  ; tolerate or synchronize against invocations on a slightly stale
  ; session due to a race over disconnect / reconnect.

  (let ((preferred-thread (rpcsession-preferred-thread session)))
	(if (or (not preferred-thread) (eq? (get-current-thread) preferred-thread))
		(proxythunk)
		(send-thunk-call (rpcsession-async-command-target session)
						 proxythunk))))

(define (stub-raise-condition remote-condition)
  ; todo is this clean enough or should a local condition be made?
  (error 'rpc-remote-error-raised remote-condition))

; TODO simplify this out? "method" arg no longer used
(define (invoke-remote-object session object method args)
  (proxy-invoke-rpc-call session object args))
;  (proxy-invoke-rpc-call session "" (list object method args)))
  
; FIXME this may not be meaningful in CPS protocol
(define (rpcclient-send-ping client)
  (define (((get-pong-command-handler timeseq) cont) session response failf)
	(if (and (eq? (car response) 'pong) (>= (length response) 2))
		(if (= (cadr response) timeseq)
			(cont timeseq)
			(error-with-filter failf 'rpc-error-misaligned-response))
		#f))
  
  (let* ((ticks-running (real-time-clock))
		 (session (rpcclient-session client))
		 (response (rpcsession-issue-command-with-replyhandler
					session
					(list 'ping ticks-running)
					(get-pong-command-handler ticks-running))))
	(- (real-time-clock) ticks-running)))
; (This bit of protocol may be superfluous as you can always make a
; function to handle ping echo...)


; Server-side
;;;;;;;;;;;;;

(load-relative "rpcthreading")

;;; Server structure definition:
(define (%make-rpcserver)
  (list 'rpcserver (make-message-target) '() (make-string-hash-table) #f #f (make-lock)))
(define (rpcserver-message-target server)
  (cadr server))
(define (rpcserver-connections server)
  (third server))
(define (set-rpcserver-connections! server connections)
  (set-car! (cddr server) connections))
(define (rpcserver-dispatch-table server)
  (fourth server))
(define (rpcserver-master-thread server)
  (fifth server))
(define (set-rpcserver-master-thread! server thread)
  (set-car! (cddddr server) thread))
(define (rpcserver-terminate-flag server)
  (sixth server))
(define (set-rpcserver-terminate-flag! server flag)
  (set-car! (cdr (cddddr server)) flag))
(define (with-rpcserver-lock server thunk)
  (with-lock (seventh server) thunk))


;(define-structure (rpcserver
;				   (constructor %make-rpcserver (server-socket)))
;  (server-socket #f read-only #t)
;  (connections '()))


; Simple global lock for all RPC dispatch tables; you don't expect much concurrency
; among multiple tables (i.e. multiple different servers). Ideally this would be a
; reader-writer lock. However, without SMP threads the distinction is a little silly.
(define *dispatch-tables-global-lock* (make-lock))

(define *rpc-server-side-wallp* #f) ;;;

(define (create-rpc-server)
  (%make-rpcserver))

(define (register-rpc-procedure server procname handler)
  (with-lock *dispatch-tables-global-lock*
			 (lambda ()
			   (hash-table/put! (rpcserver-dispatch-table server)
								procname handler))))

; TODO unregister function

(define (start-rpc-server server portnum address)
  (if (and (rpcserver-master-thread server)
		   (alive? (rpcserver-master-thread server)))
	  (error 'rpc-server-already-running))
	  ; todo locking? or just demand that only one thread start and stop?
  
  (let ((response-target (make-message-target)))
	(define (tcp-accept-thread-worker)
	  (call-with-current-continuation
	   (lambda (k)
		 (let ((server-socket 
				(with-error-filter
				 (lambda (condition)
				   (send-message response-target condition)
				   (k condition))
				 (lambda () 
				   (open-tcp-server-socket portnum address)))))
		   (send-message response-target 'ok)
		   (accept-rpc-connections server server-socket)
		   (close-tcp-server-socket server-socket)))))
	
	(set-rpcserver-master-thread! server
								  (spawn tcp-accept-thread-worker))
	(let ((status-message
		   (wait-message response-target)))
	  (if (condition? status-message)
		  (begin
			; can you *do* this, carting a condition across threads?
			(signal-condition status-message)
			(standard-error-handler status-message))))))

(define (start-rpc-server-free-port server range-start range-end address)
  (let retry ((attempts 1000))
	(call-with-current-continuation
	 (lambda (k)
	   (if (<= attempts 0) (error 'too-many-retries-finding-port))
	   (let ((port-attempt (+ (random (+ (- range-end range-start) 1)) range-start)))
		 (bind-condition-handler
		  (list condition-type:system-call-error)
		  (lambda (condition)
			(if (eq? (access-condition condition 'error-type)
					 'address-in-use)
				(within-continuation k (lambda () (retry (- attempts 1))))))
		  (lambda () 
			(start-rpc-server server port-attempt address)))
		 port-attempt)))))

; this goes in a thread
(define (accept-rpc-connections server server-socket)
  (let ((retval
		 (call-with-current-continuation 
		  (lambda (k)			
			(define (accept-connection address-out)
			  (with-messageable
			   (rpcserver-message-target server)
			   (lambda (message)
				 (if (eq? message 'shutdown)
					 (begin
					   (set-rpcserver-master-thread! server #f) 
					   ; the un-modularity of clearing this here is disgusting.
					   ; however, if it's not cleared until later, there is a race
					   ; if user tries to restart the server before thread quits.
					   (reply-message 'ok)
					   (k message))
					 (error 'unknown-message)))
			   (lambda ()
				 (tcp-server-connection-accept server-socket
											   #t address-out))))
			
			(let accept-loop ()
			  (let* ((client-address (allocate-host-address))
					 (socket (accept-connection client-address)))
				; Create session object and launch handler thread
				(server-spawn-new-session server socket)
				(accept-loop)))))))
	(if *rpc-threading-wallp* (pp "suspending..." *rpc-wallp-port*))
    (if *rpc-server-side-wallp* (pp retval *rpc-wallp-port*))
    retval))

(define (server-spawn-new-session server port)
  ; (Must be able to grant exclusive ownership of port to new thread!)
  (with-rpcserver-lock
   server
   (lambda () 				   
	 (let ((session
			(%make-rpcsession port
							  (rpcserver-dispatch-table server))))
	   
	   (set-rpcserver-connections!
		server
		(cons session (rpcserver-connections server)))
	   
	   (spawn (lambda () 
				(handle-rpc-connection session server)))
	   session))))

(define (server-connect-outbound server client portnum address)
  (disconnect-rpc-client client)
  (let* ((socket (open-tcp-stream-socket address portnum))
		 (session (server-spawn-new-session server socket)))
	(send-thunk-call (rpcsession-async-command-target session)
					 (lambda () 'ready))
	; (this is slightly screwey in how the client potentially has a dispatch
	;  table, which isn't used...)
	(set-rpcclient-session! client session)
	(set-rpcclient-session-owner! client server))
  client)


; Alas, we can't terminate just by closing the server socket --
; tcp-server-connection-accept dies horribly.
; Also, once closed, the server socket needs to be recreated from scratch.

(define (stop-rpc-server server)
  (with-rpcserver-lock 
   server
   (lambda ()
	 (let ((thread (rpcserver-master-thread server)))
	   (if (and thread (alive? thread))
		   (begin
			 (send-message-with-reply (rpcserver-message-target server) 'shutdown)

			 (for-each rpcsession-queue-shutdown (rpcserver-connections server))
			 ; This is asynchronous because cross-thread port close is dangerous.
			 ;(set-rpcserver-connections! server '())

			 (if *rpc-server-side-wallp* (display "Server closed.\n" *rpc-wallp-port*)))
		   'already-dead)))))

; FIXME this becomes rather awkward and screwey with outside attached 
; sessions. a non-"started" or stopped server can have live sessions,
; and moreover, stop will will fail unless you perfunctorily start it
; first.

(define *thread-associated-rpcsession* #f)
; (may not be the best way to convey this information...)

(define (handle-rpc-connection session server)
  (if *rpc-server-side-wallp* (display "Client connected.\n" *rpc-wallp-port*))
  
  (fluid-let ((*thread-associated-rpcsession* session))
	(call-with-current-continuation
	 (lambda (k)
	   (handle-connection-loop session 
							   *default-rpc-command-handlers*
							   (lambda (cond)
								 (if *rpc-server-side-wallp*
									 (pp `(shutting down session due to error ,cond) *rpc-wallp-port*))
								 (k cond))))))
   
  ; todo log/report errors?

  (with-rpcserver-lock
   server
   (lambda () 
     (set-rpcserver-connections! server
				 (delq session (rpcserver-connections server)))))
  
  (rpcsession-close session) ; should this be inside the lock? subtle, but unlikely to matter.
  (if *rpc-server-side-wallp* (display "Client disconnected.\n" *rpc-wallp-port*)))

  
;;; Common protocol handling
;;;;;;;;;;;;;;;;;;;;;;;;;;;;

(define (rpcsession-wait-indefinitely session)
  (handle-connection-loop session
                          *default-rpc-command-handlers*
						  ; Expose protocol & IO errors
                          (lambda (cond) #f)))

(define (handle-connection-loop session command-handlers failf)
  (let* ((protoerr-failf
		  (lambda (condition)
			(call-with-current-continuation
			 (lambda (k)
			   (issue-command-response 
				session
				`(proto-error (condition 
							   ,(condition/report-string condition)
							   ,(hash condition)))
				; Simplistic condition marshalling -- we don't want to depend
				; on the full-blown marshalling machinery, which may be the
				; source of the current error.											
				k) ; silently ignore errors reporting proto-errors -- we've done all we can do
			   ))
			(failf condition)))
		 (local-root (if (rpcsession-root-continuation session)
						 #f
						 (with-error-filter
						  protoerr-failf
						  (lambda ()
							(call-with-current-continuation
							 (lambda (k) k)))))))
	(dynamic-wind
		; Yes, a root only makes sense once you're listening for commands,
		; but this is kind of hokey and overly elaborate... :P
		(lambda ()
		  (if local-root
			  (begin
				; we're assuming for now root and thread always go together...
				(set-rpcsession-root-continuation! session local-root)
				(set-rpcsession-preferred-thread! session (get-current-thread)))))
		(lambda ()
		  (let command-loop ()
			(let ((expr (rpcsession-read session failf)))
			  (if (eof-object? expr)
				  'eof
				  (begin
					(handle-rpc-command session command-handlers expr protoerr-failf)
					(command-loop))))))
		(lambda ()
		  (if local-root
			  (begin
				(set-rpcsession-root-continuation! session #f)
				(set-rpcsession-preferred-thread! session #f)))))))
  
(define (issue-command-response session response fail-filter)
  (let ((socket (rpcsession-socket session)))
	(if *rpc-server-side-wallp*
		(begin (display "Response: " *rpc-wallp-port*)
			   (pp response *rpc-wallp-port*)))
	(with-sane-unparser-config
	 (lambda ()
	   ; fixme? it's a little weird calling the ordinary error filter on an IO error,
	   ; which may well attempt to write an error report to socket. of course, a smarter
	   ; error filter could check for IO errors and handle them differently, but really
	   ; it should check for IO errors *on the socket it would be using*. 
	   (with-error-filter fail-filter
						  (lambda ()
							(write response socket)
							(newline socket)
							(flush-output socket)))))))
       ; TODO shouldn't this be unified with rpcsession-issue-async-command?
       ; eror handling is a little different, though...

(define *rpc-inbound-command-wallp* #f)

(define (handle-rpc-command session command-handlers expr fail-filter)
  (if *rpc-inbound-command-wallp* (pp `(command-in
										,(get-current-thread) ;;;TMPTMP
										,expr) *rpc-wallp-port*))
  (if (pair? expr) 
	  ; (pattern dispatch would be nice here...)
	  (let try-handlers ((handlers command-handlers))
		(if (null? handlers)
			(error-with-filter fail-filter 'rpc-unrecognized-command)
			(or
			 ((car handlers) session expr fail-filter)
			 (try-handlers (cdr handlers)))))
	  (error-with-filter fail-filter 'rpc-unparseable-command)))

;;; Protocol command handlers:

(define (ping-command-handler session expr fail-filter)
  (if (eq? (car expr) 'ping)
	  (issue-command-response session (cons 'pong (cdr expr)) fail-filter)
	  #f))

#|
(define (invoke-command-handler session expr failf)
  (if (and (eq? (car expr) 'invoke)
		   (= (length expr) 4))
	  (let ((procname (third expr))
			(args (unmarshal-data (fourth expr) session failf)))
		(stub-invoke-rpc-procedure session procname args
								   (lambda (response)
									 (issue-command-response
									  session
									  (list 'return (cadr expr)
											(marshal-data response session #f failf))
									  failf))
								   (lambda (error-info)
									 (issue-command-response
									  session
									  (list 'error-raised (cadr expr)
											(marshal-data error-info session #f failf))
									  failf))))
										; (Errors scoped to a command are handled locally
										;  and sent only over the wire.)
	  ; TODO threading & cross-thread dispatch...		  
	  #f))
|#

(define (cinvoke-command-handler session expr failf)
  (if (and (eq? (car expr) 'cinvoke)
		   (= (length expr) 3))
	  (let ((seqno (second expr))
			(params (unmarshal-data (third expr) session failf)))
		(if (not (= (length params) 4))
			(error-with-filter failf 'rpc-bad-command-format))
		(let ((procname (first params))
			  (args (second params))
			  (dynamic-context (third params))
			  (return-continuation (fourth params)))
		  (if (eq? dynamic-context #t)
			  (set! dynamic-context procname)) ; optimization hack
		  (let ((root-continuation
				 (if (continuation? dynamic-context)
					 dynamic-context
					 (if (list? dynamic-context)
						 (get-dynamic-root dynamic-context)
						 (if (procedure? dynamic-context)
							 (begin
							   (pp "Warning: Micah still hasn't fixed dynamic contexts for asynchronous return thunks!"
								   *rpc-wallp-port*)
							   #f)
							 (error-with-filter failf 'rpc-bad-command-format dynamic-context))))))
			(call-with-current-continuation
			 (lambda (k)
			   ; (PROBLEM: within-continuation here can screw with user code's
			   ;  use of dynamic-wind. Most obvious case is a triangle cyclic 
			   ;  call among servers, such that the within-continuation is
			   ;  cross-thread and so has to unwind all the way to the thread
			   ;  root continuation and back, but there may be others...)
			   (within-continuation-with-workaround 
				(if root-continuation root-continuation
					(rpcsession-root-continuation session)) ; default case
				(lambda ()
				  (fluid-let ((*mobile-dynamic-context*
							   (if (list? dynamic-context) 
								   dynamic-context
								   ; No change; we're in some new context:
								   ; (Could be more efficient by avoiding the fluid-let
								   ;  in this case, but that's awkward-ish. Oh well.)
								   *mobile-dynamic-context*)))
					(stub-invoke-rpc-procedure
					 session procname args
					 (lambda (response)
					   (if return-continuation
						   (issue-command-response
							session
							(make-cinvoke session
										  seqno ; this is a really weird abuse of the seqno field
										  return-continuation
										  (list response)
										  return-continuation
										; ...maybe the dynamic context for a return should
										; actually be the mobile dynamic context?? fixme?
										  #f)
							failf)))
										; (Errors scoped to a command are handled locally
										;  and sent only over the wire.)
					 (lambda (condition)
					   (if return-continuation
						   (issue-command-response
							session
							(make-cinvoke session
										  seqno ; this is a really weird abuse of the seqno field
										  "%raise-condition" ; can this be abstracted?
										  (list condition)
										  return-continuation
										  #f)
							failf)
						   (begin
						     ; Orphaned error -- propagate up within root continuation
						     ; (it's either an ordinary error return or should cause a proto error)
							 (signal-condition condition)
							 (standard-error-handler condition)))))
					; is this the right thing to do, or should the root continuation somehow
					; bottom out into a command handler loop...?
					(k unspecific)))))))))
	  ; TODO threading & cross-thread dispatch...		  
	  #f))

(define (stub-invoke-rpc-procedure session procname args success fail)
  ; todo convert from CPS to fail-filter style?
  (call-with-current-continuation
   (lambda (k)
	 (if (or (not (or (string? procname) (procedure? procname)))
			 (not (list? args)))
		 (k (fail 'error-bad-procedure-call)))
	 (let ((handler
			(if (procedure? procname)
				procname
				(rpcsession-dispatch-lookup session procname
											(lambda allargs ; TODO proper error handling (??)
											  (k (fail 'error-procname-not-found)))))))
	   (success
		; TODO think about warning (not just condition-type:error) reporting some day
		(with-error-filter (lambda (condition)
							 (k (fail condition)))
						   (lambda ()
							 (apply handler args))))))))

(define (proto-error-command-handler session expr fail-filter)
  (if (eq? (car expr) 'proto-error)
	  ; FIXME? this will cause a proto error to be reported back to the sender :P
	  ; Aside from being weird, this introduces unnecessary dependencies on marshalling.
	  (error-with-filter fail-filter 'rpc-fatal-protocol-error (cadr expr))
	  ; (It's not a good idea to rely on high-level marshalling in a
	  ;  proto-error, even though it does make things more readable...)
	  #f))

(define (drop-command-handler session expr fail-filter)
  (if (and (eq? (car expr) 'drop)
		   (= (length expr) 2))
	  (let* ((oid (second expr))
			 (object (id-to-object oid)))
		(if object
			(remove-strong-reference object session)
			; todo error (or warning) for known oid but no session reference
			; todo make error non-fatal?
			(error-with-filter fail-filter 'drop-for-unknown-id oid)))	  
	  #f))

(define *default-rpc-command-handlers*
  (list cinvoke-command-handler
		ping-command-handler
		drop-command-handler
		proto-error-command-handler))


(define (rpc-object-invoke-stub object method args)
  (if (null? method)
	  (apply object args)
	  (error 'rpc-object-method-not-supported method)))


(load-relative "rpcmarshal")


; what the hell to do about continuations masquerading hiding inside lambdas?
; use unwind-protect to raise an exception because they make no sense?
; thread them all the way back and call them from the original, co-located caller?
; there are some situations where continuations make a good deal of sense
; and would not be hard to support, e.g. calling upward continuations to escape
; a remote procedure. other situations like reentrant continuations would
; take some real work, and would depend on knowing when a continuation had
; been captured, or on promiscuous preservation of client continuations until
; gc could demonstrate otherwise.

; why don't continuations "just work" given lambda support? well,
; upward, escaping continuations should almost work -- proxy back to
; originating host, invoke continuation that unwinds out of RPC
; protocol handling -- but it leaves orphaned loops on the caller side
; that will never return (and never be GC'ed). Similarly, if it passes
; through intermediate forwarding hosts, parallel slices of the loop
; will be left in a similar state (or just a one-sided slice, if the
; continuation was passed by a different route than the call sequence.
; is it necessary to have such things dangle? Well, it's a function of
; using strict procedure call semantics in the RPC system. It could
; probably be avoided by using continuation-based dispatch rather than
; recursive invocations of the command handling loop. Then, discarding
; unnecessary loops would be a function of reference dropping and GC.

; Actually, I think if you do CPS-transform on the protocol itself,
; everything just works. Instead of call/return, do you call +
; continuation / call-continuation. If you know that no continuations
; have been captured during the invocation, you could return with
; call-and-drop on the continuation.

; Is there a more efficient way to do it without all the object
; marshalling and likely gc invocation? Maybe. But I'm not sure it's
; worth working out.


; important to consider the hazards of letting someone else tell you
; what an object is, such as one you've forgotten. procedures, e.g.,
; if they could be remotely described, would be manifestly dangerous.

; also of concern: refereces are forgeable at the protocol level; no
; secure capabilities.



; A stupid: you will always invoke a lambda along the path you first learned about it,
; even if that path goes down or if you make direct contact with the owning server.
; I.e. P2P is not a robustly supported case.
; (this does make GC potentially easier, though)

; So... what to do about session reference cycles among lambdas and the like?




; it's a little borky that unsupported marshalling situations bubble out as 
; disconnecting protocol errors in many cases (e.g. return values)


; what happens to entities and other function-like invocable objects with
; local accessors? do they have all the same remote manipulator problems as
; other objects?



; do i need non-blocking calls? (e.g. for parallel operations and asynchronous messaging)
; is call with deferred return handler (i.e. promise) good enough?
; (session would be unavailable for making additional calls until return is processed)
; or is it better just to thread the handling of multiple sessions?
; (of course, exposed CPS might be a cleaner way of doing this... although the
;  single-threadedness of session handling acts as a constraint on what you can ask for.
;  but, you should naturally get multiplexing between what are effectively cooperative
;  threads for the different requests.)


; what's the best way to launch a gaggle of worker processes? fork does
; not seem to be available. need a way to find launch path and support library
; information to spawn children. maybe it's most expedient to push the problem
; back to the original launcher and make them bootstrap the interconnect?
; what's the best way to manage them? thread per child and synchronous calls?
; or get asynchronous subprocesses and deferred return handlers working?



; Major immediate TODOs:
; - pluggable connectivity attachments?
; - proxy objects
;
; Eventual feature todos:
; - lambda continuation escape trapping?
;   (or is it "screw around with your sesion at your own risk"?)
; - continuation-passing style protocol?
; - server passcodes
; - server logging
; - configurable threading policies
; - more debugging flags
; - secure OID hashes or grant checks
; - and more...



#|
things to do to support CPS protocol:

remove sequence numbers so that returns can be issued out of order or reentrantly


cook up some sort of handling loop so that client can optionally be
background threaded? preferably not too distinct from the server-side
loop? (although, to avoid multi-threaded invokes, it could possibly
only support drops while ... not within the extent of a
client-initiated request?)
or maybe add a HandleEvents nonblocking command that the client can
poll with if they so choose?

add invoke-with-return-continuation command
(return continuation optional :P )


optionally add invoke-and-drop command
(what happens when someone tries to return from an invoke-and-drop? automatic error? )



So, how the hell are network errors handled? Who do you give the
exception to? To both sides, if there's anyone still waiting? Does
that break network-based tail calling, because someone has to wait
around to check for an error? Or should the caller be able to specify
that they don't care? And if so, where does their continuation go to,
does it just return immediately so that if they tail-called all the
way through they return to start (possibly a toplevel message loop)?
Or do you have to keep track of what machine owns what thread,
e.g. sending keepalives, and is that enough information? Is it too
sloppy to wait and see if the continuation becomes available for GC
and only then error?

Tail calls, which can happen automatically in CPS, are the most
difficult case -- A calls B and B tail-calls to C, with the intent of
calling A's return continuation, but then C's network goes down. If
C's the only one who knows about the call besides A, nobody's
available to contact A about the problem. The simplest solution might
be to have B forward information to A that it should be waiting on C
for an answer as part of the tail-call procedure. (How do you know a
tail call is happening, in order to do this??) Alternatively, a cute,
implicit procedure but with unbounded delays would be to wait until
A's continuation is GC'ed and then raise an error, as a result of B
and whoever was responsible for holding onto C's copy of the
continuation (B or C or an intermediary) dropping their references
when C's connection dies.

Maybe it's not so automatic? How do you tell the automatic value
return code that it's a tail call and it shouldn't wait? Stack
walking? Or do you have to make the whole thing explicit in order to
inform the callee of the proper return continuation?

Gerry thinks there should be a "complaint department" you can
optionally provide and that this should be an optional mechanism or
layer, not baked deep into the protocol. 


Does recursively piling up calls that wait on the stack present any
problems? memory leaks, if they return out of order? or can you
be "condfident enough" that their continuations are chained so that
it's not worth worrying about? There are counterexamples. OTOH,
spaghettiing the stacks even in the common case could make debugging
really annoying.
(Actually, this breaks dynamic extents. See below.)

how is raising a remote condition implemented? Should there be a way
to call a continuation with an error? Is that a security hazard,
allowing continuations to be injected with arbitrary errors? Or is it
nothing more than you can do with within-continuation anyway?

(Should within-continuation be supported?)
does marshalling need to distinguish between lambdas and
continuations, apart from support for within-continuation?


should there be any distributed notion of extent and fluid-let? does
there need to be? (seems like the only differences from centralized
fluid-let and remote query/update operations are access latency and
failure semantics... though not necessarily something to sneeze
at. also avoids the problem of having to designate a location for such
stuff.)


With tail-call spaghetti among machines, are we more likely to run
into reference cycles that don't GC? Quite possibly. Might want to be
more aggressive about dropping unnecessary references. When an
originator of an object (such as a continuation) is provided with a
duplicate reference through someone else, there's no point in keeping
it -- they can drop it immediately. OTOH, if the reference is passed
back in a tighter cycle without making it all the way to the
originator (less likely to be common that the tail call triangle, I
think), how do you know your copy is better? Request a traceback?
Propagate hop counts and use some sort of path length rule?



Will thunk callbacks have weird issues with extents (consider the
with-foo port wrappers) in CPS-style protocol? Sure seems like
it. Should calls normally be made using within-continuation to avert
this? How do you know which continuation to use if you've been
bouncing across multiple machines? do you need to track per-machine
extent information with a call?? Seems like it, though only for the
trace of machines you've called through (excluding tail calls).

Might it be best to implement this on top of mobile dynamic state?

(This all probably means that within-continuation or the like needs to
be the the most primitive way of invoking a procedure...)

|#


#|
problem: always marshalling a copy of object contents fails to support
cyclic data structures

idea: assume peer has object if you're holding a reference (and
haven't heard drop), but support a request command to allow them to
grab it by id if you hit the narrow window between GC and drop
processing

use cases of object marshalling may not make this very useful,
however...

pure byval: not relevant because nothing is ever repeatedly referened
and no OIDs are employed

pure byref: usable, but not especially useful, because there's not
much information that needs to be marshalled (e.g. metadata like
lambda name and argument span), and sub-references, including cyclic
ones, are generally excluded from such data (record type of
record-type a possible exception!)

hybrid byval (transitive closure marshalled as a deep copy): not
relevant because within the span of a single expression, objects
shouldn't be GC'ed because they're still hanging around to be
returned. This is a case where always-marshal fails completely,
though. It's also a case where persistent OIDs give the wrong
semantics if the data is mutable, since copies can quickly become
non-equivalent, so across peers copies may diverge. Moreover, multiple
copies to a single host will cross-share references, undesired
convergence violating the purported byval copy semantics. Getting this
right seems to require an instance-specific temporary OID scheme.

(hybrid byval seems a little obnoxious, common as it is, because it's
going well above and beyond ordinary procedure call semantics... I
wonder if there's a way to package that up completely separate from
the marshalling core as an optional DIY bundler...)

immutable hybrid byref (transitive closure marshalled as a deep copy,
with eq semantics): more tractable than hybrid byval, as the OID
system should just work, as long as pre-existing (i.e. being held
strongly on peers behalf) objects bodies are omitted -- although such
strong holding is unnecessary, so we'd still prefer to use an
instance-specific object table or some other means of knowing what
we've given the peer already this time through -- though we still need
to use global OIDs here, to handle eq correctly. (If a means of
tracking peers with a prior weak copy of an object were added, it
might seem like the request command would make it useful for this
purpose, but here the peer isn't obligated to send drop messages, and
even if it did, there might be a window when both sides could GC the
object and then it would be gone!)

call-by-value-result hybrid byref: ???
(when do you do a sync-back? If your operations are too low-level and
you sync after each one, your performance may end up worse than just
remoting byref access to graph elements.)


What happens to context when you start mixing these schemes, e.g. by
type?? e.g. a pure-byval marshal of something that contains two refs
to an instance of a hybrid byval type

|#

#|
reasons i wanted objects settled before CPS...
to be able to name a session or machine-instance object
to invoke alternate methods on a continuation
to be able to create a call status object for tracking 
(and because in general it would be really nice to have full-featured conditions)


none of those are *absolutely* essential, although i need some
mechanism for error returns on a continuation
(object might not be the right way to do it regardless, if they end up being
fairly high-level, because that would produce a sort of architecutral
liability inversion.)


in the absence of polymorphic dispatch based on location (i.e. which
session, or local), object-associated methods are just another way of
looking up or naming lambdas... albeit with some package system
features implied (for grouping and importing together methods related
to a type). if so, why bother?

polymorphic dispatch sure would be nice, though... having to figure
out which session's lambda to use is kind of insane (and also which
thread, at that).

but how do you specify which lambdas to include in which polymorphic
ensemble? alternatively, how do you name the distribued polymorphic
ensemble to which you can bind and to which you can attached lambdas?
by method keyword and type keyword? by method keyword and type record
oid? or maybe just the method keyword should name the complex, and the
type record should be part of the dispatch criteria? That's sounding a
bit like general generic operators... although it would be a pain to
have to add new generic bindings for every session.

maybe polymorphic dispatch complexes should have global namespace
names, like URLs or UUIDs? How would the dispatch information get
assembled then, though? and how would you disambiguate or avoid
conflicting claims? Or should they do something trivial, like always
dispatch to the owner of the nth argument? Or maybe everything but the
owner of the argument chosen for dispatch (if any) should be
determined locally, by a commonly loaded package? Maybe that's the
least insane thing to do?

if so, what's the right way to name and bind to the dispatched slice
of the polymorphic complex? leave it up to the user, with
encouragement towards URIs and UUIDs? maybe even implement using a
globally-shared OID on a dispatch tree object or lambda?

(versioning and version fallback dispatch is something i haven't
thought about *at all* yet... stuff a minimum version number as an
extra implicit argument? or maybe it should be supplied interactively
during a bind?)

Maybe services like lambda invoke and generic dispatch should be
implemented as calls to reserved names, e.g. starting with a %? This
would require pre-initializing dictionaries with them, but it would
make them more easily overridable and less baked-in to the
protocol... and it would provide opportunity for adding other
miscellaneous bits of functionality, too high-level to be baked in.

(does this cause any layering inversions or other unpleasant
interactions with marshalling? not that i can forsee yet... except
that this ultimately won't work so well for things associated with
continuation calling and erroring, since those are needed for ordinary
call/return in a CPS protocol.)

(in CPS-style, does it make more sense for lambda invoke to be more
primitive than dispatch table? well, maybe. it's simpler, but it has
the disadvantage that you need to do a round-trip binding operation
before calling, so replacing old, defunct sessions is not so
straightforward, and also, online updating of an exported function
becomes more difficult because you need to update the behavior of an
existing lambda, you can't just re-register binding target.)

|#




#|

is it possible stack-trace is non-reentrant in a wierd, port & error-related way?

(I can still get the nonsensical werid error by ^C-ing the client during a stack trace print)



also to look into later: why does ^C sometimes leave the rpc session
in a borky, hung state?

(also, it sure would be nice if old invokable remote objects didn't go
mysteriously bork after a reconnect... (the shell type
re-instantiation issue?))



yuck. nearly 2x slower.
removing redundant marshals of root continuation shaved off a little time
removing unnecessary proc instance info shaved off a little more
suggestive that the bottleneck may be moving data back and forth, including parsing and unparsing sexprs
after much such flailing, it's only ~1.5x slower (over a slightly faster baseline)... but still slower

((lambda () (for-each (lambda (n) (WITH-TIMINGS (lambda () (for-each (lambda (i) (map-threads apply funs2)) (iota 500))) (lambda (run-time gc-time real-time) (pp (list run-time gc-time real-time))))) (iota 12)))) ; funs2 -- old protocol
(1100 30 1852)
(1160 30 2015)
(1410 40 2101)
(1380 60 2237)
(1230 30 2141)
(1270 20 2062)
(1400 30 2042)
(1350 70 2137)
(1300 20 2013)
(1340 40 2076)
(1360 60 2148)
(1160 30 2039)

((lambda () (for-each (lambda (n) (WITH-TIMINGS (lambda () (for-each (lambda (i) (map-threads apply funs2)) (iota 500))) (lambda (run-time gc-time real-time) (pp (list run-time gc-time real-time))))) (iota 12)))) ; funs2
(1570 40 2588)
(1360 100 2827)
(1610 130 2959)
(2140 90 3415)
(1430 90 2766)
(1400 140 2806)
(1850 100 3357)
(1550 80 2782)
(1570 130 2857)
(1930 150 3346)
(1250 50 2642)
(1520 110 2808)
|#


#|
What the hell? why is one-on-one RPC sometimes twice as fast as other times?
1 ]=> (for-each (lambda (n) (WITH-TIMINGS (lambda () (for-each fun (iota 1500))) (lambda (run-time gc-time real-time) (pp (list run-time gc-time real-time))))) (iota 12))                    (630 0 2053)
(640 30 2081)
(620 30 2007)
(570 0 2069)
(620 40 2096)
(580 40 2119)
(570 0 2070)
(600 50 2104)
(550 30 2064)
(610 0 2010)
(600 40 2116)
(640 40 2097)
;Unspecified return value

;next morning, without even rebuild or reload:
]=> (for-each (lambda (n) (WITH-TIMINGS (lambda () (for-each fun (iota 1500))) (lambda (run-time gc-time real-time) (pp (list run-time gc-time real-time))))) (iota 12))
(540 0 979)
(620 50 1171)
(520 0 945)
(610 60 1215)
(600 50 1181)
(500 0 913)
(570 60 1133)
(410 50 997)
(590 0 1066)
(560 50 1152)
(440 60 978)
(580 0 1127)

Also, why doesn't the stack sampler provide any useful info (laying
80% of the samples on block-on-io-descriptor, even though 50% of the
time is "run time") even on the single-threaded client? Is it really
burning all its time in the IO microcode hiding in there?

strace confirms it's not actually spending that time blocking on
syscalls. sure has a weird polling rubric... and lots of signal mask
twiddling.

cpuprofile claims it's spending 25% of its cpu time in highly assorted
microcode foo and the rest in a long tail of unnamed stuff, i guess
maybe compiled code. not sure the results are trustworthy (realtime is
especially suspicious) -- don't know how the itimers interact.

|#

#|
; test of dynamic extent and multiple reentrant continuations:

(define foobar 'xxx)
(define kiterdp (remote-eval '(lambda (k l rdp) (for-each (lambda (x) (pp `(hi ,x)) (rdp x) (call-with-current-continuation (lambda (j) (k (list j x)))) (pp `(ho ,x))) l))))

(fluid-let ((foobar 'baz)) (let ((thepair (call-with-current-continuation (lambda (k) (mobile-fluid-bind 'snoog 'boog (lambda () (fluid-let ((foobar 'fuzz)) (kiterdp k (iota 4) (lambda (x) (pp `(rdp ,x ,foobar ,(mobile-fluid-query 'snoog)))))))))))) (if (pair? thepair) (begin (pp `(got ,(cadr thepair) ,foobar ,(mobile-fluid-query 'snoog #f))) (fluid-let ((foobar 'bork)) (mobile-fluid-bind 'snoog 'choog (lambda () ((car thepair) 'dummy))))) 'done)))
#|
(rdp 0 fuzz boog)
(got 0 baz #f)
(rdp 1 fuzz boog)
(got 1 baz #f)
(rdp 2 fuzz boog)
(got 2 baz #f)
(rdp 3 fuzz boog)
(got 3 baz #f)
|#
|#


#|
monosession, threaded multisession:
register/unregister call
attach new connection thru port (monosession closes old, if connected)
close all (what about the acceptor?)


how should bind work vis a vis multisessions? is it monosession-only?
or monosession and plain session??
or should the only bind available for multisessions be arg dispatch bind?
(should that even be aware of multisession?)
or maybe anycast and everycast?
(should there be combinators for multi-session calls, like
anycast-from-set, everycast-from-set, everycast with parameters from
list, multiple concurrent attempts with first to return, etc.? what
would a basis set look like? is this an endless rat hole?)

should monosessions and multisessions simply expose a superset of the
session public interface?

what about lifetime management? should there be a transient
monosession that can be estabilshed on demand for direct connectivity
and then go away if not used or needed? or should that be done
directly at the session level? should there be a means to indicate
that a given session or session collection should have object lifetime
beyond session lifetime, possibly a lease up to some server-specified
maximum age?



should conditions be marshalled simply as remote objects with type &
display info, but when raised through the error stub be wrapped as
irritants in a locally created condition with local stack trace?
Making them first class remote objects makes it easier to grab the
full set of fields remotely, alhough this could also be accomplished
by marshalling them individually into a local structure. remote
condition objects are remarkably unhelpful if what you want is the
particular condition type and type's info fields without regard to
where the error was generated, though. It *might* be possible to
marshal them into an actual condition, but this seems unlikely to work
completely since the continuations and such will be remote objects
rather than true continuations.
really isn't a priority, now, though...



still need to do queue-to-session calls (ok, done) and async variants...



should there be generic function combinators for remote functions?
should they work without a round trip to do the combining, instead
producing some remotely executable description, or is that overkill?
|#

; can callback thunks be used instead of return continuations for
; async calls? if so, this basically unifies CPS-RPC and
; promise-RPC. how should errors be singalled, though? (and does this
; need special support to avoid tripping over within-continuation?
; don't *think* so... but what dynamic context -- mobile dynamic
; context or root continuation -- should it run the thunk in? callee
; cannot simply use the return thunk as normal, because it's not a
; dynamic context.)


; a thought: if you're handling requests, you want to issue one or a
; bunch of sub-requests and then continue processing when they
; complete: synchronous, fork-synchronous, or tail-synchronous. But,
; if you're managing a system, a UI, etc., you want to punt off the
; problem and keep managing your charge until results come back:
; asynchronous promises.



; security implications of passing continuation capabilities...
; intersting consequence that your calls can be forceably re-entered,
; if you ever make a synchronous call from them or even tack them on
; as a default dynamic context for callbacks. also, your continuation
; can be abandoned if you make a synchronous call. you must guard
; against these things if you do not sufficiently trust the remote
; host, e.g. by adding tripwires with dynamic-wind and hooking a
; timeout interrupt that stuffs a timeout error into your
; continuation.
; anything else of note?


#|

Updated to-do list:
- simplify code
- async calls and async lambda invoke
- marshalling back-references
- separate threading from tcp listening, pluggable connectivity attachments
- wire protocol for function combinators (for caller-configured tail call
  chains, etc.)? (or maybe they should be a suite of standard remote functions?)
- server passcodes, logging, etc.?
- marshalling options?
- on-demand direct connections?

|#