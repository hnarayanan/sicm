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
;;;;;;;;;;;;;;;;;;;;;;;;;;;;

;;; Author: Micah Brodsky
;;; Version: 0.2a, spring 2012


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

;;; Session structure definition:
(define (%make-rpcsession socket dispatch-table)
  (list 'rpcsession socket (make-strong-eq-hash-table) dispatch-table 0
		(make-message-target) #t))
; [Treat session as thread-local, with the exception of the dispatch table,
;  which has a global lock, and the async command target.]
(define (rpcsession-socket session)
  (cadr session))
(define (rpcsession-strong-object-table session)
  (third session))
(define (rpcsession-dispatch-lookup session procname default)
  (if (string=? procname "")
	  rpc-object-invoke-stub ; hackhack :P
	  (let ((dispatch-table (fourth session)))
		(if dispatch-table
			(with-lock *dispatch-tables-global-lock*
					   (lambda ()
						 (hash-table/get dispatch-table procname default)))
			default))))
(define (rpcsession-next-seqno session)
  (fifth session))
(define (set-rpcsession-next-seqno! session seqno)
  (set-car! (cddddr session) seqno))
(define (rpcsession-async-command-target session)
  (sixth session))
(define (rpcsession-open? session)
  (seventh session))
(define (set-rpcsession-open session flag)
  (set-car! (cddr (cddddr session)) flag))
; (this flag partly overlaps in functionality with session == #f on the client...)
; (note that this does not reflect socket errors, only whether the session has been
;  closed by the user)

(define (rpcsession-close session)
  (set-rpcsession-open session #f)
  (ignore-errors ; Wierd errors can bubble out on a close of a broken socket
   (lambda () 
	 (close-port (rpcsession-socket session)))))


;;; Error handling helpers:

(define (error-with-filter fail-filter . args)
  ;(pp `(error-with-filter ,fail-filter . ,args))
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


; Client-side
;;;;;;;;;;;;;

;;; Client structure definition:
(define (create-rpc-client)
  (list 'rpcclient #f))
(define (rpcclient-session client)
  (cadr client))
(define (set-rpcclient-session! client session)
  (set-car! (cdr client) session))

(define (connect-rpc-client client portnum address)
  (let ((oldsession (rpcclient-session client)))
	(if oldsession (rpcsession-close oldsession)))

  (set-rpcclient-session! client 
						  (%make-rpcsession
						   (open-tcp-stream-socket address portnum) #f)
						   ; exposes network errors
						  ))

(define (disconnect-rpc-client client)
  (rpcsession-close (rpcclient-session client))
  (set-rpcclient-session! client #f))

(define (bind-rpc-call client procname)
  (let ((procname-str (if (symbol? procname)
						  (symbol->string procname)
						  procname)))
	(lambda args
	  (proxy-invoke-rpc-call (rpcclient-session client) procname-str args))))

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

(define (rpcsession-read session)
  (if (not (rpcsession-open? session))
	  (error 'rpcsession-closed session))
  (with-messageable (rpcsession-async-command-target session)
					(lambda (message)
					  (if (pair? message)
						  (let ((command (car message)))
							(cond
							 ((eq? command 'drop)
							  (let ((oid (second message)))
								(rpcsession-issue-async-command session `(drop ,oid))))
							 ((eq? command 'terminate)
							  (begin
								(rpcsession-close session)
								(error 'rpcsession-terminated)))
							 ; todo add a sticky terminated flag, to produce better errors
							 ; on later attempts to read?
							 (#t (error 'rpcsession-bad-async-command command))))
						  (error 'rpcsession-bad-async-command-message message)))
						  ; these are internal rpc system errors; shouldn't really pass to filters...
					(lambda ()
					  (read  ; sure hope this is security-safe! :P (do we care about memory exhaustion?)
					   ; and... if a read is interrupted, is there any way you can lose data?
					   ; ...is it safe for a read to be interrupted with a write on same port?
					   (rpcsession-socket session)))))

(define (rpcsession-queue-shutdown session)
  (send-message (rpcsession-async-command-target session)
				'(terminate)))

(define (rpcsession-queue-drop session oid)
  (send-message (rpcsession-async-command-target session)
				`(drop ,oid)))				

; Specify explicit port, because RPC machinery can get called from
; weird places with output redirection.
(define *rpc-wallp-port* (trace-output-port))
; (this is a little hokey, maybe it should either be console-i/o-port, or every
;  wallp print should call trace-output-port...)

(define *rpc-client-side-wallp* #f)

(define (with-sane-unparser-config thunk)
  ; fix unparser global state ugh!
  ; why isn't it port-associated??
  (let-fluids *unparser-list-breadth-limit* #f
              *unparser-list-depth-limit* #f
              *unparser-string-length-limit* #f
              *unparse-abbreviate-quotations?* #t
      thunk))

(define (rpcsession-issue-async-command session command)
  ; Exposes errors directly for the client
  ; (perhaps a bit too much, such that server sweeps them up?)
  (if (not (rpcsession-open? session))
      (error 'rpcsession-closed session))
  (let ((socket (rpcsession-socket session)))
	(if *rpc-client-side-wallp* (begin
								  (display "Command: " *rpc-wallp-port*)
								  (pp command *rpc-wallp-port*)))
	(with-sane-unparser-config
	 (lambda ()
	   (write command socket)
	   (newline socket)
	   (flush-output socket)))))

(define (rpcsession-issue-command session command replyhandler)
  ; Exposes errors directly for the client
  ; (perhaps a bit too much, such that server sweeps them up?)
  (rpcsession-issue-async-command session command)

  (call-with-current-continuation
   (lambda (k)
	 (handle-connection-loop session
							 (cons (replyhandler k) *default-rpc-command-handlers*)
							 (lambda (cond) #f))
	 (error 'rpc-unexpected-eof))))

; It would be nice if multiple client threads could multiplex calls over the same socket,
; but the shared blocking dispatch mechanism and locking would be a pain. For now, if you
; want multiple threads calling and blocking on a server, you'll need one client connection
; per thread.

(define (((get-callreturn-command-handler seqno) cont) session response failf)
  (let ((cmd (car response)))
	(if (or (eq? cmd 'return)
			(eq? cmd 'error-raised))
		(if (and (list? response) (>= (length response) 2))
			(let ((seqno-received (cadr response)))
			  (if (= seqno-received seqno)
				  (if (eq? cmd 'return)
					  (cont (unmarshal-data (third response) session failf))
					  ; Pure user error, not proto error -- send directly without filters
					  (error 'rpc-remote-error-raised
										 (unmarshal-data (third response) session failf))) ; todo try demarshalling condition				 
				   ; TODO try ignoring old replies till realignment?
				   ; (this seems like it's itching for some sort of generic dispatch on commands and seqno...)
				  (error-with-filter failf 'rpc-error-misaligned-response)))
			(error-with-filter failf 'rpc-error-unintelligible-response))
		#f)))

(define (proxy-invoke-rpc-call session procname args)
  (if (not session)
	  (error 'rpc-error-session-not-connected))
  
  (let ((seqno (rpcsession-next-seqno session)))
	(set-rpcsession-next-seqno! session (+ seqno 1))
	
	; TODO sanity error re-wrapping for reader?
	(rpcsession-issue-command session
							  (list 'invoke seqno procname
									(marshal-data args session #f))
							  (get-callreturn-command-handler seqno))))

(define (invoke-remote-object session object method args)
  (proxy-invoke-rpc-call session "" (list object method args)))
  
(define (rpcclient-send-ping client)
  (define (((get-pong-command-handler timeseq) cont) session response failf)
	(if (and (eq? (car response) 'pong) (>= (length response) 2))
		(if (= (cadr response) timeseq)
			(cont timeseq)
			(error-with-filter failf 'rpc-error-misaligned-response))
		#f))
  
  (let* ((ticks-running (real-time-clock))
		 (session (rpcclient-session client))
		 (response (rpcsession-issue-command session
											 (list 'ping ticks-running)
											 (get-pong-command-handler ticks-running))))
	(- (real-time-clock) ticks-running)))
; (This bit of protocol may be superfluous as you can always make a
; function to handle ping echo...)


; Server-side
;;;;;;;;;;;;;

;;; Threading wrappers:
(define *rpc-threading-wallp* #f)

; Runtime package thread library wrappers:
(define (spawn thunk)
  (call-with-current-continuation
   (lambda (k)
	 (if *rpc-threading-wallp* (pp "spawning..." *rpc-wallp-port*))
	 (create-thread k thunk))))
 
(define (alive? thread)
  (not (thread-dead? thread)))
 
(define (make-lock)
  (make-thread-mutex))
 
(define (with-lock lock thunk)
  (with-thread-mutex-locked lock thunk))

(define (get-current-thread)
  (current-thread))

(define (sleep millis)
  (sleep-current-thread millis)) 

;; Conspiracy thread library wrappers:

;(load "conspire.scm")
;(define (spawn thunk)
;  (conspire:make-thread conspire:runnable thunk))
;(define (alive? thread)
;  (error 'conspiracy-feature-not-supported))
;(define (make-lock)
;  (conspire:make-lock))
;(define (with-lock lock thunk)
;  (if conspire:running? ; The client-side may call this without threads at all.
;      ; FIXME this really needs dynamic wind, which doesn't work!
;	  (let ((return-value))
;		(conspire:acquire-lock lock)
;		(set! return-value (thunk))
;		(conspire:unlock lock)
;		return-value)
;	  (thunk)))
;(define (get-current-thread)
;  *running-thread*)
; TODO: sleep, dynamic-wind, signal-thread-event

; (...dynamic-wind fires on conspiracy threads but not runtime package threads!)


;; Thread-safe asynchronous communication abstractions:

; FIXME none of this works with conspiracy threads, due to needing dynamic-wind and
; signal-thread-event

; Alert target structure definition:
(define (make-alert-target) (list 'alert-target #f #f (make-lock)))
(define (alert-target-pending-item target)
  (cadr target))
(define (set-alert-target-pending-item! target alert)
  (set-car! (cdr target) alert))
(define (alert-target-thread target)
  (third target))
(define (set-alert-target-thread! target thread)
  (set-car! (cddr target) thread))
(define (alert-target-lock target)
  (fourth target))

(define (with-alertable target thunk)
  (dynamic-wind
	  ; Provide semantics like fluid-let, but on a data structure, and
	  ; with a locking dance.
	  (lambda ()
		(with-lock
		 (alert-target-lock target)
		 (lambda ()					 
		   (let ((att (alert-target-thread target)))
			 (if att (error 'alert-target-already-entered target att)))
		   ;(if (alert-target-thread target) (error 'alert-target-already-entered target))
		   ; Probe for events that occured while we were outside the alertable
		   ; region. Probe repeatedly in case the event handler decides to
		   ; issue a new alert.
		   (let item-loop ()
			 (let ((pending-item (alert-target-pending-item target)))
			   (if pending-item
				   (begin
					 (set-alert-target-pending-item! target #f)
					 (pending-item)
					 ; is it really a good idea to call with the lock held??
					 (item-loop)))))
		   ; Now that the alert is guaranteed clear, with the lock held, we can
		   ; set ourselves alertable.
		   (set-alert-target-thread! target (get-current-thread)))))
	  thunk
	  ; To keep things simple, we want to ensure that the thread event
	  ; does not fire while target has the lock held, so we don't take
	  ; out the lock again until after thread field is cleared, which
	  ; prevents the event from following through.
	  (lambda () (set-alert-target-thread! target #f)
			  (with-lock (alert-target-lock target)
						 (lambda ()
						   ; This strange checkpoint is to make sure the thread does
						   ; not exit while the lock is still held, which could lead
						   ; to an error from signal-thread-event. An abnormal
						   ; termination could still cause an error, but that's okay.
						   #t)))))

(define (send-alert target alert-thunk)
  (with-lock
   (alert-target-lock target)
   (lambda ()
	 (let ((handler (lambda () ; called with lock held
					  (set-alert-target-pending-item! target #f)
					  (alert-thunk))))
	   
	   (if (alert-target-pending-item target)
		   (error 'alert-target-alert-already-pending target))
	   (set-alert-target-pending-item! target
									   handler)
	   (let ((thread (alert-target-thread target)))
		 (if thread
			 (signal-thread-event
			  thread
			  (lambda ()
				(with-lock (alert-target-lock target)
						   (lambda ()
							 ; Now in the context of the target thread, need to
							 ; check very carefully if it's safe to act (could be
							 ; less conservative if we had latency guarantees from
							 ; signal-thread-event, perhaps.)
							 (if (and (alert-target-thread target)
									  (eq? (alert-target-pending-item target) handler))
								 ; Still inside the alertable block, and the alert
								 ; hasn't fired yet. Go ahead.
								 (begin
								   (if *rpc-threading-wallp* (pp "triggering alert handler from async event" *rpc-wallp-port*))
								   (handler))
								 (if *rpc-threading-wallp* (pp "silening alert async event because no longer needed" *rpc-wallp-port*))
								 )))))
			 (if *rpc-threading-wallp* (pp "not sending alert async event" *rpc-wallp-port*))))))))

; Message target structure definition:
(define (make-message-target)
  (list 'message-target '() #f (make-alert-target)))
(define (message-target-queue target)
  (cadr target))
(define (set-message-target-queue! target newqueue)
  (set-car! (cdr target) newqueue))
(define (message-target-handler target)
  (third target))
(define (set-message-target-handler! target handler)
  (set-car! (cddr target) handler))
(define (message-target-alert-target target)
  (fourth target))

(define (with-messageable target handler thunk)
  ; Since message targets can be entered from only one thread context
  ; (at a time, at least; enforced by with-alertable), and not from an
  ; interrupt on said thread, we don't have to be too careful about
  ; handling the handler field -- no reentrancy or threading
  ; issues. OTOH, we do have to worry about bailing out via a
  ; continuation, so we still need dynamic wind. (Not that it is
  ; absolutely essential to clear the message target handler field,
  ; but it prevents confusion and potentially complicated memory
  ; leaks.)
  (dynamic-wind
	  (lambda () 
		(let ((queue-handler
			   (lambda () ; called with alert target lock held
				 (let item-loop ()
				   ; This loop is structured in its baroque form so
				   ; that handlers can safely call bailout
				   ; continuations and not lose queued messages.
				   (let ((queue (message-target-queue target)))
					 (if (pair? queue)
						 (begin
						   (set-message-target-queue! target
													  (cdr queue))
						   (handler (car queue))
						   (item-loop))))))))
		  (set-message-target-handler! target queue-handler)))		  
	  (lambda () 
		(with-alertable (message-target-alert-target target)
						thunk))
	  (lambda () 
		(set-message-target-handler! target #f))))

(define (wait-message target)
  (call-with-current-continuation
   (lambda (k)
	 (with-messageable target
					   (lambda (message)
						 (k message))
					   (lambda ()
						 (let loop ()
						   (sleep 5000)
						   (loop)))))))

(define *message-reply-target* #f) ; slightly ugly

(define (send-message target message #!optional reply-target)
  (let ((alert-target (message-target-alert-target target)))
	(with-lock
	 (alert-target-lock alert-target)
	 (lambda () 
	   ; This is not fast for giant queue lengths. So don't abuse it!
	   ; (Not to mention that flow control is caller's responsibility.)
	   (set-message-target-queue!
		target 
		(append! (message-target-queue target) (list message)))
	   (if (not (alert-target-pending-item alert-target))
		   (send-alert alert-target
					   ;(testtest-sendmessage-thunk target reply-target))))))) ; testtest
					   (lambda () ; called with alert target lock held
						 (fluid-let ((*message-reply-target* reply-target))
						   ((message-target-handler target))))))))))

;(define ((testtest-sendmessage-thunk target reply-target))
;  ; called with alert target lock held
;  (fluid-let ((*message-reply-target* reply-target))
;	((message-target-handler target))))


(define (reply-message reply)
  (if (not *message-reply-target*)
	  (error 'not-in-message-handler))
  (if (default-object? *message-reply-target*)
	  (error 'no-message-reply-expected))
  (send-message *message-reply-target* reply))

(define (delayed-message-replier)
  (call-with-current-continuation ; yuck :P
   (lambda (k)
	 (lambda (reply)
	   (within-continuation k
							(lambda ()
							  (reply-message reply)))))))
; (possibly i should cut this ugly function -- nobody's using it now)
							
(define (send-message-with-reply target message)
  (let ((reply-target (make-message-target)))
	(send-message target message reply-target)
	(wait-message reply-target)))


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
				(with-rpcserver-lock
				 server
				 (lambda () 				   
				   (let ((session
						  (%make-rpcsession socket
											(rpcserver-dispatch-table server))))
					 
					 (set-rpcserver-connections!
					  server
					  (cons session (rpcserver-connections server)))
						 
					 ;; launch handler thread
					 (spawn (lambda () (handle-rpc-connection session server))))))
				(accept-loop)))))))
	(if *rpc-threading-wallp* (pp "suspending..." *rpc-wallp-port*))
    (if *rpc-server-side-wallp* (pp retval *rpc-wallp-port*))
    retval))

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

(define (handle-rpc-connection session server)
  (if *rpc-server-side-wallp* (display "Client connected.\n" *rpc-wallp-port*))
  
  (call-with-current-continuation
   (lambda (k)
     (handle-connection-loop session 
			     *default-rpc-command-handlers*
			     (lambda (cond)
				   (if *rpc-server-side-wallp*
					   (pp `(shutting down session due to error ,cond)))
				   (k cond)))))
   
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

(define (handle-connection-loop session command-handlers failf)
  (let command-loop ()
	(let ((expr 
		   (with-error-filter failf
							  (lambda () (rpcsession-read session)))))
	  (if (eof-object? expr)
		  'eof
		  (begin
			(handle-rpc-command session command-handlers expr
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
											;(marshal-data condition session #f failf))
										    ;; fixme? it's a little screwey sending marshalled conditions for low-level protocol errors
									  k) ; silently ignore errors reporting proto-errors -- we've done all we can do
									 ))
								  (failf condition)))
			(command-loop))))))

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
       ; todo shouldn't this be unified with rpcsession-issue-async-command?

(define *rpc-inbound-command-wallp* #f)

(define (handle-rpc-command session command-handlers expr fail-filter)
  (if *rpc-inbound-command-wallp* (pp `(command ,expr) *rpc-wallp-port*))
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

(define (stub-invoke-rpc-procedure session procname args success fail)
  ; todo convert from CPS to fail-filter style?
  (call-with-current-continuation
   (lambda (k)
	 (if (or (not (string? procname))
			 (not (list? args)))
		 (k (fail 'error-bad-procedure-call)))
	 (let ((handler
			(rpcsession-dispatch-lookup session procname
										(lambda allargs ; TODO proper error handling (??)
										  (k (fail 'error-procname-not-found))))))
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
  (list invoke-command-handler
		ping-command-handler
		drop-command-handler
		proto-error-command-handler))


(define (rpc-object-invoke-stub object method args)
  (if (null? method)
	  (apply object args)
	  (error 'rpc-object-method-not-supported method)))


(load "rpcmarshal")


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