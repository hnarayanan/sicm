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

; Threading
;;;;;;;;;;;

;;; Low-level threading wrappers:
(define *rpc-threading-wallp* #f)

; Runtime package thread library wrappers:
(define *continuation-default-thread-for-workaround* (current-thread))

(define (spawn thunk)
  (call-with-current-continuation
   (lambda (k)
	 (if *rpc-threading-wallp* (pp "spawning..." *rpc-wallp-port*))
	 (create-thread
	  k
	  (lambda () 
		(fluid-let ((*continuation-default-thread-for-workaround* (current-thread)))
		  (thunk)))))))
 
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

(define (block-indefinitely)
  (yield-current-thread) ; improves performance
  (sleep 5000)
  (block-indefinitely))

(define (within-continuation-with-workaround k thunk)
  ; Workaround for MIT Scheme bug in within-continuation that causes
  ; a thread blocked on IO to hang if someone else calls within-continuation
  ; to a condition it used to inhabit. Wake it up and rewind its IO
  ; registration by signalling it an event after within-continuation does
  ; its wind. (Verified in Scheme 9.0.1 & 9.1.1.)
  (within-continuation
   k
   (lambda () 
	 (ignore-errors
	  (lambda ()
		(signal-thread-event ; *klonk* thread who previously owned this continuation on the head
		 *continuation-default-thread-for-workaround*
		 (lambda () #f))))
	 (thunk))))

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
; TODO: sleep, dynamic-wind, signal-thread-event, block-indefinitely

; (...dynamic-wind fires on conspiracy threads but not runtime package threads!)


;;; Thread-safe asynchronous communication abstractions:

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
		   ; Probe for events that occured while we were outside the alertable
		   ; region. Our handler will iterate if needed, in case the user event
		   ; handler decides to issue a new alert.
		   (let ((pending-item (alert-target-pending-item target)))
			 (if pending-item
				 (pending-item)))
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
	 (let ((handler
			(lambda () ; Called with lock held and alerts disabled

			  ; Note: There is one known circumstance where handler can
			  ; be invoked quasi-recursively, in spite of the lock and
			  ; alert mask -- if the user thunk captures a continuation
			  ; and unwinds, then later rewinds, pending alerts will be
			  ; processed during the rewind, before this extent here 
			  ; regains control.

			  ; Clear pending status before ceding control to user code
			  (set-alert-target-pending-item! target #f)

			  ; is it really a good idea to call with the lock held?
			  ; code elsewhere now relies on this behavior...
			  ; modularity, shmodularity? locks are fun. :P
			  (alert-thunk)
			  
			  ; Check again, in case the alert handler issued a new alert.
			  ; If so, handle it.
			  (let ((pending-item (alert-target-pending-item target)))
				(if pending-item
					(pending-item))))))

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
								   ; (fixme possible obscure bug? if thread leaves alert target
								   ;  and a different thread comes in and uses it, de-queueing 
								   ;  the existing item, then another item, eq? to the first
								   ;  one, is posted, this handler may run it on the wrong
								   ;  thread? (not that any threads even share targets like
								   ;  that anywhere AFAIK) is it even legal for an optimizer
								   ;  to render multiple such handler closures eq?)
								   (dynamic-wind
									   (lambda () 
										 ; Ensure that we are not alertable during event
										 ; processing.
										 (if (not (eq? (alert-target-thread target)
													   (get-current-thread)))
											 (error 'confused-alert-target target att))
										 (set-alert-target-thread! target #f))
									   handler
									   (lambda ()
										 ; Restore alertable state before returning
										 (if (alert-target-thread target)
											 (error 'confused-alert-target-2 target))
										 (set-alert-target-thread! target (get-current-thread)))))
								 (if *rpc-threading-wallp* (pp "silencing alert async event because no longer needed" *rpc-wallp-port*))
								 )))))
			 (if *rpc-threading-wallp* (pp "not sending alert async event" *rpc-wallp-port*))))))))

; Message target structure definition:
(define (make-message-target)
  (list 'message-target '() #f (make-alert-target) #f))
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
(define (message-target-closed? target)
  (fifth target))
(define (set-message-target-closed! target)
  (set-car! (cddddr target) #t))

(define (with-messageable target handler thunk)
  ; Since message targets can be entered from only one thread context
  ; (at a time, at least; enforced by with-alertable), and not from an
  ; interrupt on said thread, we don't have to be too careful about
  ; handling the handler field -- no reentrancy or threading
  ; issues. OTOH, we do have to worry about bailing out via a
  ; continuation, so we still need dynamic wind. (Not that it is not
  ; absolutely essential to clear the message target handler field,
  ; but it prevents confusion and potentially complicated memory
  ; leaks.)
  (if (message-target-closed? target)
	  (error 'message-target-closed target))
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
					   block-indefinitely))))

(define *message-reply-target* #f) ; slightly ugly
; (maybe this should just be changed to an extra parameter for the
;  message handler?)

(define (close-message-target target)
  ; NOTE: This can only be called from a thread context eligible to pull
  ; messages from target.
  (with-lock
   (alert-target-lock (message-target-alert-target target))
   (lambda () 
	 ; Drain the message queue (while holding the lock to prevent new messages)
	 (with-messageable target
					   (lambda (message)
						 (if (default-object? *message-reply-target*)
							 ; Drop message, nobody was waiting
							 unspecific 
							 ; Send rejection reply
							 (reply-message (list 'error-message-target-closed message))))
					   (lambda ()
						 'no-op))
	 ; Set closed flag and release the lock
	 (set-message-target-closed! target))))
					  

(define (send-message target message #!optional reply-target)
  (let ((alert-target (message-target-alert-target target)))
	(with-lock
	 (alert-target-lock alert-target)
	 (lambda () 
	   (if (message-target-closed? target)
		   ; Reject message if target is already closed (but don't raise an error
		   ; because that would make the particular choice of behavior determined
		   ; by a race with close.)
		   (if (not (default-object? reply-target))
			   (send-message reply-target (list 'error-message-target-closed message)))
		   (begin
			 ; This is not fast for giant queue lengths. So don't abuse it!
			 ; (Not to mention that flow control is caller's responsibility.)
			 (set-message-target-queue!
			  target 
			  (append! (message-target-queue target) (list message)))
			 (if (not (alert-target-pending-item alert-target))
				 (send-alert alert-target
							 (lambda () ; called with alert target lock held
							   (fluid-let ((*message-reply-target* reply-target))
								 ((message-target-handler target))))))))))))

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


; (...seems a little silly that we have queued messages built on unqueued
;  thunks, and then queued thunks built on queued messages. oh well? c.c )

(define (send-thunk-call-async target thunk)
  ; this does *not* do anything to carry along dynamic extent, fluids etc...
  ; should it??
  ; i kinda doubt it... (except mobile dynamic state, which the caller can deal with)
  ; (fwiw, i've had bad luck using within-continuation across threads so far...)
  (let ((reply-target (make-message-target)))
	(send-message
	 target 
	 (list 'exec
		   (lambda ()
			 (call-with-current-continuation
			  (lambda (k)		
				(send-message reply-target
							  (list 'ok
									(with-error-filter
									 (lambda (condition)
									   (send-message reply-target condition)
									   (k condition))
									 thunk)))))))
	 reply-target)
	; ..should this be a promise obj rather than an equivalent thunk?
	(lambda ()
	  (let ((reply (wait-message reply-target)))
		(set! reply-target #f) ; Should be unnecessary!!
							   ; But, helps prevent cryptic memory leaks thru interpreter.
		(if (and (pair? reply) (eq? (car reply) 'ok))
			(cadr reply)
			(if (and (pair? reply) (eq? (car reply) 'error-message-target-closed))
				(error 'message-target-closed target)
				(begin
				  (signal-condition reply)
				  (standard-error-handler reply))))))))

(define (send-thunk-call target thunk)
  ((send-thunk-call-async target thunk)))

(define (wait-thunk-call target)
  (let* ((message (wait-message target))
         (cmd (car message)))
    (if (eq? cmd 'exec)
        (begin
          ((cadr message))
          (wait-thunk-call target))
        (if (eq? cmd 'die)
            'terminated
            (error 'unrecognized-call-message message)))))


; Debugging routine to monitor for unintended reentry
; (does not check for temporal non-extent reentry achieved by
;  e.g. unwinding to a higher continuation before re-entering)
(define (ensure-nonreentrant-extents handler)
  (let ((entered #f))
	(lambda args
	  (dynamic-wind 
		  (lambda () (if entered
						 (error 'too-many-reentries! handler)
						 (set! entered #t)))						   
		  (lambda () (apply handler args))
		  (lambda () (set! entered #f))))))

; Debugging routine to monitor for unintended temporal reentry
; (includes entering from multiple threads or other extents
;  on the same thread)
(define (ensure-nonreentrant-temporally handler)
  (let ((entered #f))
	(lambda args
		  (if entered
			  (error 'too-many-reentries! handler))
		  (set! entered #t)
		  (apply handler args)
		  (set! entered #f))))



