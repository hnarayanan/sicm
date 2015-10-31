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

; Data Marshalling
;;;;;;;;;;;;;;;;;;

(define *marshal-gc-wallp* #f)

(define *global-object-table-lock* (make-lock))

(define *ids-by-object-table* (make-weak-eq-hash-table))
(define *objects-by-id-table* (make-string-hash-table)) ;(make-equal-hash-table))
; id -> weak pair<obj, source session set (weak hashtable)>

(define *next-object-id* 0)
;(define *global-id-suffix* (list (os/hostname) (get-universal-time)
;                                 (unix/current-pid) ; why isn't this implemented for Windows?
;                                 ))
(define *global-id-suffix*
  (md5-string (string-append (os/hostname)
                           " "
                           (number->string (get-universal-time))
                           " "
                           (number->string
                            (unix/current-pid)) ; why isn't this implemented for Windows?
                           )))

; ugly ugly quick and dirty function to marshal binary strings more compactly than sexprs do
; probably very similar to base64 but i was too busy to look that up :P
; no idea how much cpu time is wasted encoding
(define (three-bytes-to-four-ascii-chars str)
  (define (char-int offset)
    (if (>= offset (string-length str))
        0
        (char->ascii (string-ref str offset))))
  (let encode ((offset 0) (output ""))
    (let ((next3 (+ (char-int offset)
                    (* (char-int (+ offset 1)) 256)
                    (* (char-int (+ offset 2)) 65536))))
      (if (>= offset (string-length str))
          output
          (encode
           (+ offset 3)
           (string-append
            output
            (list->string
             (map (lambda (b) (ascii->char (+ b 48)))
                  (list (remainder next3 64)
                        (remainder (quotient next3 64) 64)
                        (remainder (quotient next3 4096) 64)
                        (remainder (quotient next3 262144) 64))))))))))

(define *machine-instance-id*
  (substring (three-bytes-to-four-ascii-chars *global-id-suffix*)
             0 14)) ; trimmed because we can :P 
      
; TODO *secure* hashing (not md5) with entropy so that references are unguessable...
(define (allocate-object-id)
  (with-lock 
   *global-object-table-lock*
   (lambda ()
     (let ((id *next-object-id*))
       (set! *next-object-id* (+ 1 id))
       (let ((newoid
              (substring ; encode func is too dumb to omit final zeros :P
               (three-bytes-to-four-ascii-chars
                (md5-string (string-append (number->string id) *global-id-suffix*)))
               0 22)))
         ; Sanity check for duplicates
         ; (Collisions shouldn't happen until around 10^19 live objects, but there could be a bug...)
         (hash-table/lookup *objects-by-id-table* newoid
                            (lambda (item)
                              (pp `(WARNING!!!!!! duplicate-id ,newoid) *rpc-wallp-port*)
                              ; try again (does not prevent remote collisions, but should get rid of locally known ones)
                              (allocate-object-id))
                            (lambda ()
                              newoid)))))))
       ;(cons id *global-id-suffix*)))))

(define (get-object-id object)
  (with-lock
   *global-object-table-lock*
   (lambda ()
     (let ((existing-id (hash-table/get *ids-by-object-table* object #f)))
       (if existing-id existing-id
           (let ((new-id (allocate-object-id)))
             (hash-table/put! *ids-by-object-table* object new-id)
             (hash-table/put! *objects-by-id-table* new-id
                              (weak-cons object (make-weak-eq-hash-table)))
             new-id))))))
; making so many weak hashtables might have perf implications...?
; could use weak lset instead, if needed. or create tables just for
; strongly source-held objects...

(define (attach-object-id object id)
  (if (literal-data? object)
      (error 'marshal-cannot-assign-id-to-literal object id))
  (with-lock
   *global-object-table-lock*
   (lambda ()
     (let ((existing-id (hash-table/get *ids-by-object-table* object #f))
           (existing-item (id-to-object id)))
       (if (and existing-id (not (equal? id existing-id)))
           (error 'marshal-object-already-has-id))
       (if (and existing-item (not (eq? object existing-item)))
           (error 'marshal-id-already-has-object))
       (hash-table/put! *ids-by-object-table* object id)
       (hash-table/put! *objects-by-id-table* id
                        (weak-cons object (make-weak-eq-hash-table)))))))

(define (marshal-has-special-id? object)
  ; todo maybe fast path this with its own compact table
  (with-lock
   *global-object-table-lock*
   (lambda ()
     (if (hash-table/get *ids-by-object-table* object #f)
         #t ; currently does not attempt to determine "specialness"
         #f))))

(define (attach-special-object-id object id)
  (attach-object-id object id)) ; maybe this will be a fast-path later...


(define (record-obj-source-session id session)
  (with-lock
   *global-object-table-lock*
   (lambda ()
     (let* ((obentry (hash-table/get *objects-by-id-table* id 'missing-obentry))
            (session-set (weak-cdr obentry)))
       (hash-table/put! session-set session #t)))))

(define (is-obj-source-session? id session)
  (with-lock
   *global-object-table-lock*
   (lambda ()
     (let* ((obentry (hash-table/get *objects-by-id-table* id 'missing-obentry))
            (session-set (weak-cdr obentry)))
       (hash-table/get session-set session #f)))))

(define (is-local-object? id)
  (with-lock
   *global-object-table-lock*
   (lambda ()
     (let ((obentry (hash-table/get *objects-by-id-table* id #f)))
       (if obentry
           (let ((session-set (weak-cdr obentry)))
             (null? (hash-table-keys session-set)))
           #f)))))

(define (id-to-object id)
  (with-lock
   *global-object-table-lock*
   (lambda ()
     (let ((entry (hash-table/get *objects-by-id-table* id #f)))
       (if entry (weak-car entry) #f))))) ; returns #f if lost or unknown

; ISSUE: objects from old, defunct sessions may still be around because
; they haven't been GC'ed yet. Must find a way to filter out objects
; pointing to dead sessions!
; But... how important is it that objects remain valid and eq? across
; session disconnects? If they stop working, they will not pick up
; again until re-marshalled... or unless some sort of lookup mechanism
; is used to translate (e.g.) id suffixes to live sessions. But if
; they're simply replaced with a newly marshalled instance, this
; violates the principle that all objects with same id are
; eq?... although if ids are not user- exposed, this may be
; tolerable... unless there are outstanding references, in which case
; the client could be still using them as eq-tokens. Barring that, it
; comes down to the question of what you do with old objects from a
; prior session... Do they all need to be relaced? With strong
; objects, you can't rely on being able to rehabilitate them, because
; they may have been GC'ed on the host (although you may be able to
; guarantee this doesn't happen for any particular application).

; Horrible hack for a temporary fix: pick an arbitrary source session
; not yet known to be closed.
(define (find-open-session-for-id id)
  (with-lock
   *global-object-table-lock*
   (lambda ()
     (let* ((obentry (hash-table/get *objects-by-id-table* id 'missing-obentry))
            (session-set (weak-cdr obentry))
            (sessions (hash-table-keys session-set)))
       (find rpcsession-open? sessions)))))

(define (gc-sweep-objects-by-id)
  (with-lock
   *global-object-table-lock*
   (lambda ()
     (if *marshal-gc-wallp* (pp 'gc-sweep-objects-by-id *rpc-wallp-port*))
     ;;(hash-table/clean! *ids-by-object-table*) ; fixme unnecessary voodoo??
     ; (This could almost be concurrent (were hash table ops thread-safe), except
     ;  hash-table/for-each disallows concurrent mutation aside from inline removes.)
     (hash-table/for-each *objects-by-id-table*
                          (lambda (key value)
                            (if (not (weak-pair/car? value))
                                (begin
                                  (if *marshal-gc-wallp* (pp `(gced ,key) *rpc-wallp-port*))
                                  (hash-table/remove! *objects-by-id-table* key)
                                  (for-each 
                                   (lambda (session)
                                     (rpcsession-queue-drop session key))
                                   ; FIXME an idle client may take arbitrarily long
                                   ; to send this...
                                   ; also, might this be too spammy for the inefficient
                                   ; de-queueing algorithm...?
                                   (hash-table-keys (weak-cdr value))))
                                ;;(hash-table/clean! (weak-cdr value)) ; fixme unnecessary voodoo??
                                ))))))

; A disadvantage with this scheme is that memory will not be reclaimed until
; peers deign to reclaim their own memory, with no means to ask them to do so.
; Also, it is entirely possible to have reference chains that zigzag back and
; forth across a session, requiring an arbitrary number of GC passes on
; alternating ends in order to deallocate.

(define *rpc-reaper*
  (spawn
   (lambda ()
     (let loop ()
       (sleep 5000)
       (loop)))))

; We run asynchronous gc sweeps from a reaper thread because locks are
; not allowed from interrupt context, or at least when interrupting
; blocking IO! This impacts both the object table lock and the locks
; needed by send-alert, used by rpcsession-queue-drop.

; (Even if locks were permitted, it would be dicey. The GOT lock need
; not necessarily prevent GC, because of reentrancy (except maybe the
; possible special case hash-table/for-each, in the case of a manual
; sweep interrupted by an automatic sweep (automatic daemon sweeps are
; not interrupted by daemon sweeps)), but this adds a special standard
; of care to the use of the lock -- session objects (or at least their
; message targets) must be consistent, and *objects-by-id-table* must
; not be in some horrible, unusable state.  Worse, though, is the case
; of the message targets' alert targets, ultimately needed by
; rpcsession-queue-drop -- those were never designed to be reentrant!)

(define *rpc-reaper-running* #f)
; (Prevent reaper from being recursively triggered)
; TODO allow reaper to be retriggerable if GC fires during sweep?
; OTOH, that may increase the risk of sustained GC looping...

(add-gc-daemon! 
 (lambda ()
   (if (not *rpc-reaper-running*)
       (begin
         (set! *rpc-reaper-running* #t)
         (signal-thread-event *rpc-reaper*
                              (lambda ()
                                (gc-sweep-objects-by-id)
                                (set! *rpc-reaper-running* #f)))))))

; (This might better belong in an initialize-package routine because of the 
;  possibility of GC running before the rest of the file is processed. As
;  is this should be fine, but it is a surprise hazard.)
; (Might this be better as a secondary-gc-daemon?)
; (Or... might a request to all peers to do a GC be good as a secondary?)
; (No, secondaries hardly get run at all.)
; (Perhaps this whole thing should be converted to using finalizers?)
; (OTOH, finalizers don't seem to be anything more sophisticated than an
;  ex-post-facto sweep over weak pairs, themselves.)

(define (attach-strong-reference object session)
  (if *marshal-gc-wallp* (pp `(attach-strong-reference ,object ,session) *rpc-wallp-port*))
  ; Needs no locking because runs only in context of session thread
  (let ((id (get-object-id object)))
    (hash-table/put! (rpcsession-strong-object-table session)
                     object id)
    id))

(define (remove-strong-reference object session)
  (if *marshal-gc-wallp* (pp `(remove-strong-reference ,object ,session) *rpc-wallp-port*))
  ; Needs no locking because runs only in context of session thread
  (hash-table/remove! (rpcsession-strong-object-table session)
                      object))

; Note: Permanent objects will accumulate ID information in the global tables,
; even if all peers drop references to them. This should not be catastrophic,
; but it may look like a memory leak to diagnostics.

;(define (weak-lset-member? wlset item)
;  (if (null? wlset) #f
;      (let ((wcar (weak-car lse)))
;        (or
;         (and (weak-pair/car? wlset)
;              (eq? wcar item))
;         (weak-lset-member? (weak-cdr wlset) item)))))
;
;(define (weak-lset-insert wlset item)
;  (if (weak-lset-member? wlset item)
;      wlset
;      (weak-cons item wlset)))
;
;(define (weak-lset-remove wlset item)
;  (if (null? wlset) '()
;      (let ((wcar (weak-car lse)))
;        (if (or (not (weak-pair/car? wlset)) ; implicit compaction
;                (eq? wcar item))
;            (weak-cdr wlset)
;            (weak-cons wcar
;                       (weak-lset-remove item (weak-cdr wlset)))))))
; Too ridiculous! Just use a weak hash table and deal with the overhead.


(define (marshalling-keyword? symbol)
  (or (eq? symbol 'unquote)
      (eq? symbol 'unquote-splicing)
      (eq? symbol 'quasiquote)))

(define (literal-data? x) ; is there a built-in way to do this? 
  (define (reserved? x)
    (or (memq x '(#!default #!aux #!key #!optional #!rest #!unspecific))
        (eof-object? x))) ; (very MIT-scheme-specific -- and yes, #!eof triggers eof on loading source :P )
  
  ; NOTE: these types are all readable as printed and eq?-up-to-read,
  ; *except* for fancy numbers. Only fixnums are guaranteed eq?. This
  ; code assumes the user doesn't care about eq-ness of fancy numbers.
  (or (interned-symbol? x) (number? x) (boolean? x) (null? x) (char? x)
      (string? x) (bit-string? x) (reserved? x)))


;(define (map-improper func list)
;  (cond
;   ((null? list) '())
;   ((pair? list) (cons (func (car list)) (map-improper func (cdr list))))
;   (#t (func list))))
; Assumes implicitly that (func '()) ==> '()...
; No, this is a bad idea, because it can't distinguish whether you mean to treat
; the a tail as a list in the cdr or as a list of items.

; ("context" presently means session... this may be revised)
(define (unmarshal-data item context #!optional ff)
  (define (length-two-list? pair) ; (does not verify pair is a pair!)
    (and (pair? (cdr pair)) (null? (cddr pair))))

  (define (unmarshal-data-internal subitem)
    (cond
     ((pair? subitem) (if (marshalling-keyword? (car subitem))
                          (if (length-two-list? subitem)
                              (if (eq? (car subitem) 'unquote) ; only unquote is supported
                                  (unmarshal-data (cadr subitem) context ff)
                                  (error-with-filter ff 'unmarshal-bad-format subitem))
                              (error-with-filter ff 'unmarshal-bad-format subitem))
                          (cons (unmarshal-data-internal (car subitem))
                                (unmarshal-data-internal (cdr subitem)))))
     ((vector? subitem) (vector-map unmarshal-data-internal subitem))
     (#t subitem)))

  (define (unmarshal-object subitem id)
    ; perhaps this needs to be folded into the cond below...
    ; or at least the fully marshallable cases that don't need object id
    (if (not (and (list? subitem) (>= (length subitem) 2)))
        (error-with-filter ff 'unmarshal-object-not-marshallable subitem)
        (let ((objtype (car subitem)))
          (cond
           ((eq? objtype 'uninterned) (string->uninterned-symbol (cadr subitem)))
           ((eq? objtype 'robj) (make-%remote-object #f (cadr subitem) context))
           ((eq? objtype 'cont) ; Handle continuations separately for efficiency
            (make-invokable-%remote-object '(continuation) "[*continuation*]" context))
           ((eq? objtype 'proc) 
            ; don't really know how to make use of arity ATM. oh well.
            (if (< (length subitem) 3) (error-with-filter ff 'unmarshal-object-bad-proc))
            (let ((procname (cadr subitem))
                  (arity (third subitem)))
              (make-invokable-%remote-object `(proc ,arity) procname context)))
           ; TODO in the absence of a capability secret, consider checking the strong
           ; object table to make sure this session was explicitly granted the lambda?
           ((eq? objtype 'port)
            (make-port    
             ; whee, redundant re-creation... hope it's not too costly
             (make-port-type (unmarshal-data (cadr subitem) context ff) #f)
             #f))
           (#t (let ((expr (unmarshal-data (cadr subitem) context ff)))
                 (attach-object-id expr id) ; should error out if double-id'ed or literal                 
                 expr))))))
           ;(#t (error-with-filter ff 'unmarshal-object-unknown-type subitem))))))
  
  (cond
   ((not (pair? item)) (error-with-filter ff 'unmarshal-bad-format item))
   ((eq? (car item) 'cons) (if (and (list? item) (= (length item) 3))
                               (cons
                                (unmarshal-data (cadr item) context ff)
                                (unmarshal-data (third item) context ff))
                               (error-with-filter ff 'unmarshal-bad-format item)))
   ((or (eq? (car item) 'oid) 
        (eq? (car item) 'soid))
    (if (not (= (length item) 3))
        (error-with-filter ff 'unmarshal-bad-format item)
        ; todo abstract this?
        (with-lock
         *global-object-table-lock*
         (lambda ()
           (let* ((id (cadr item))
                  (existing-item (id-to-object id)))
             (let ((obj
                    (if existing-item existing-item
                        ; TODO add check for session alive here, and replace
                        ; horrible hack of grabbing arbitrary open session
                        ; as replacement?
                        (let ((new-item (unmarshal-object (third item) id)))
                          (attach-object-id new-item id)
                          new-item))))
               (if (eq? (car item) 'soid)
                   (if (and existing-item (is-local-object? id))
                       (rpcsession-queue-drop context id) ; don't need them holding a ref
                       ; TODO test this!
                       ; ...Maybe this should be used even for objects that are not local,
                       ; but merely for which you have an existing source? That should
                       ; eliminate all same-object cycles (though not cycles among different,
                       ; co-referring objects -- which fortunately should never occur in
                       ; pure functional code), but it would seem to compromise robustness
                       ; to machine failures by eliminating path redundancy.
                       ; A possible compromise might be to add a TTL and only accept
                       ; redundant sources with newTTL <= ourTTL + 1 (relying on the
                       ; no-2-cycles rule to allow the +1 leniency)?
                       (record-obj-source-session id context)))
               obj))))))
   ((not (length-two-list? item)) (error-with-filter ff 'unmarshal-bad-format item))
   ((eq? (car item) 'quote) (cadr item))
   ((eq? (car item) 'quasiquote) (unmarshal-data-internal (cadr item)))
   ; TODO fixme condition sanity
   ((eq? (car item) 'condition) `#(REMOTE-CONDITION ,(caadr item) ,(cadadr item))) ;;;;;;;
   ((eq? (car item) 'unmarshal-inject-error) (error-with-filter ff 'unmarshal-debug))
   (#t (error-with-filter ff 'unmarshal-unrecognized-format item))))

; Special private debugging symbols for error injection
(define *marshal-debug-error-generator* (generate-uninterned-symbol))
(define *unmarshal-debug-error-generator* (generate-uninterned-symbol))

; ("context" presently means session... this may be revised)
(define (marshal-data item context mode #!optional ff)
  (define (procedure-name proc)
    (let ((proc-lambda (procedure-lambda proc)))
      (if proc-lambda
          (lambda-name proc-lambda)
          (generate-uninterned-symbol 'unnamed)))) ; pretty hacky

  (define (marshal-strong-object item instance-info)
    (let* ((id (get-object-id item))
           (refexpr (if (is-obj-source-session? id context)
                        ; no need to hold a reference for sessions holding a reference for us:
                        ; (this does not handle cycles of n>2, though)
                        ; and, no need to send instance info
                        `(oid ,id #f)
                        (begin
                          (attach-strong-reference item context)
                          `(soid ,id ,instance-info)))))
      (list 'unquote refexpr)))

  (define (marshal-default-remote-object item)
    (marshal-strong-object item
                           `(robj
                             ,(with-string-output-port
                               (lambda (port) ; (is this slow?)
                                 (display item port)))
                             ; (fixme? the remote hash numbers this produces is a little silly...
                             ;  and i hope they don't leak memory! but I can't figure out how to
                             ;  turn them off!)
                             ; TODO type dispatch key when i have cycles dealt with!
                             )))

  (define (marshal-add-special-ids subitem marshalled-subitem)
    (if (marshal-has-special-id? subitem)
        (list 'unquote `(oid ,(get-object-id subitem) ,marshalled-subitem))
        marshalled-subitem))

  (define (marshal-data-internal subitem)
    (cond
     ; (this is kind of begging for generic operator dispatch...)
     ; (also, it may make more sense to have ID attachment as a separate operation
     ;  after marshalling the core object rather than making it type-dependent)
     ((literal-data? subitem) subitem)
     ((pair? subitem) (marshal-add-special-ids
                       subitem
                       (if (marshalling-keyword? (car subitem))
                           (list 'unquote
                                 (list 'cons
                                       (marshal-data (car subitem) context mode ff)
                                       (marshal-data (cdr subitem) context mode ff)))
                           (cons (marshal-data-internal (car subitem))
                                 (marshal-data-internal (cdr subitem))))))
     ((vector? subitem) (marshal-add-special-ids
                         subitem
                         (vector-map marshal-data-internal subitem)))

     ; TODO less-dumb handling of conditions?
     ((condition? subitem) 
      (list 'unquote `(condition (,(condition/report-string subitem)
                                  ,(let ((port (open-output-string)))
                                     (stack-trace (condition/continuation subitem) port)
                                     (get-output-string port))))))

     ((procedure? subitem) (marshal-strong-object
                            subitem 
                            (if (continuation? subitem)
                                '(cont #f) ; Special-case continuations for efficiency
                                 ; (#f is hack to accommodate existing validation code :P )
                                `(proc ,(symbol->string (procedure-name subitem))
                                       ,(with-error-filter
                                         ff
                                         (lambda ()
                                           ; believe it or not, this can bomb out, on things like malformed entities
                                           (procedure-arity subitem)))))))

     ; Special case uninterned symbols for error injection:
     ((eq? subitem *marshal-debug-error-generator*)
      (error-with-filter ff 'marshal-debug))
     ((eq? subitem *unmarshal-debug-error-generator*)
      (list 'unquote `(unmarshal-inject-error #f)))

     ((uninterned-symbol? subitem)
      (list 'unquote `(oid ,(get-object-id subitem) (uninterned ,(symbol->string subitem)))))

     ; Hideously slow, quick & dirty port proxying
     ; (unnecessarily bulky, especially the initializer)
     ((port? subitem)
      (marshal-strong-object
       subitem
       `(port ,(marshal-data (port-type/operations (port/type subitem)) context mode ff))))

     ((record? subitem) (marshal-default-remote-object subitem))
     ; (todo some story for accessor/mutator methods?)
     ; (todo dynamic remote unparser?)

     ((or (environment? subitem) (cell? subitem) ;(port? subitem)
          (promise? subitem) (weak-pair? subitem))
      (marshal-default-remote-object subitem)) ; Minimal default handling

     ((undefined-value? subitem) unspecific)
     ; (in fact there are two flavors of undefined value in MIT
     ; Scheme, with constant codes 1 (normal #!unspecific) and 2
     ; (weird and non-parseable). I don't know what the significance
     ; is, but it seems hideously unspecified, so distinguishing them
     ; is not supported unless a good reason comes up!)

     (#t (error-with-filter ff 'marshal-unrecognized-data-type subitem))))

  (if (eq? mode 'literal)
      `',item
      (list 'quasiquote (marshal-data-internal item)))) ; (``,foo doesn't quote a quasiquote)


(define *disable-remote-unparse-calls* #f) ; not currently relevant...

(define (unparse-%remote-object-helper ob port)  
  (display " " port)
  (display
   (call-with-current-continuation
    (lambda (k)
      (with-error-filter
       ; Preferable not to leak errors, as this can be invoked from
       ; debugging procedures (though many seem to do their own error
       ; silencing).
       (lambda (condition) (k (string-append "<error unparsing "
                                             (number->string (hash condition))
                                             ">")))
       (lambda () 
         (let ((disp-info (remote-object-display-info ob)))
           (if (and (procedure? disp-info) (not *disable-remote-unparse-calls*))
               (disp-info)
               disp-info))))))
   port))

(define unparse-%remote-object
  (standard-unparser-method 'remote-object unparse-%remote-object-helper))

(define-structure (%remote-object
                   (print-procedure 
                    unparse-%remote-object))
  type-info
  display-info
  default-session)

; FIXME? ...should sessions be held weakly? We'd like the references
; to go away once a session is closed, so that old remote objects
; lying around don't keep a defunct session's strong object table from
; being collected. (Maybe it's just a bad idea to let collection of
; strong objects be entirely implicit, and the table should be wiped
; whenever a socket error is detected??)

(define-structure (%remote-lambda-shell
                   (type-descriptor %remote-lambda-shell-type))
  robj)

; To (safely) implement invokable remote objects, we wrap them in an
; entity which wraps a private shell type holding a
; %remote-object. This way, we can identify such remote objects by
; their being entities containing said private type, without risking
; confusing them with any user-made entities containing remote
; objects. (Of course, if the user wants to remote entity objects
; themselves, they'd better not try and use entity-extra remotely!)

(define *invokable-object-wallp* #f)

(define (make-invokable-%remote-object type-info display-info default-session)
  (make-entity 
   (lambda (entity-robj . args)    
     (if *invokable-object-wallp* 
         (fluid-let ((*invokable-object-wallp* #f)) ; don't know why this is necessary...!
                                        ; is there somehow a dummy call to default output
                                        ; (which may be proxied) made by pp, even when pp
                                        ; is outputting elsewhere?
           (pp `(invoke-remote ,display-info ,args) *rpc-wallp-port*)))
     (invoke-remote-object (remote-object-session entity-robj) entity-robj '() args))
   (make-%remote-lambda-shell
    (make-%remote-object type-info display-info default-session))))

(if (environment-bound? system-global-environment 'record-entity-unparser)
    ; only available since 9.1.1
    (add-generic-procedure-generator
     record-entity-unparser
     (let ((disptag (record-type-dispatch-tag %remote-lambda-shell-type))
           (unparser (standard-unparser-method 'invokable-remote-object 
                                               unparse-%remote-object-helper)))
       (lambda (genproc args)
         (if (eq? (car args) disptag)
             (lambda (extra) unparser)
             #f)))))

(define (invokable-remote-object? ob)
  (and (entity? ob) (%remote-lambda-shell? (entity-extra ob))))

(define (remote-object? ob)
  (or 
   (%remote-object? ob)
   (invokable-remote-object? ob)))

(define (remote-object-display-info ob)
  (%remote-object-display-info
   (if (invokable-remote-object? ob)
       (%remote-lambda-shell-robj (entity-extra ob))
       ob)))

(define (remote-object-session ob)
  (let ((raw-robj 
         ; Unwrap invokable objects
         (if (invokable-remote-object? ob)
             (%remote-lambda-shell-robj (entity-extra ob))
             ob)))

    (let ((session (%remote-object-default-session raw-robj)))
      (if (rpcsession-open? session)
          session
          ; hack for dealing with closed source session without
          ; violating eq?: pick an arbitrary alternate source
          ; that's still open.
          (let* ((oid (get-object-id ob)) ; (use wrapped, not unwrapped object)
                 (hacky-alt-session (find-open-session-for-id oid)))
            (if hacky-alt-session
                (begin
                  ; since old sessions are never resurrected, there's no point
                  ; in keeping them. update default session for stability and
                  ; efficiency.
                  (set-%remote-object-default-session! raw-robj hacky-alt-session)
                  hacky-alt-session)
                session ; no good options here
                ))))))
