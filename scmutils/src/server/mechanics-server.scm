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


(define *scmserver* (create-rpc-server))

; id symbol, salt, password hash, procedure

(define mechanics-port 9807)
(define mechanics-salt "NaCl")
(define mechanics-salted-key "\\d€y{;\211“¥\214[\030Ì\222u\v")

(define *scmserver-service-table*
  `(
    (new-env   ,mechanics-salt ,mechanics-salted-key
	       ,(lambda ()
		  (extend-top-level-environment
			  (access generic-environment
				  scmutils-base-environment))))
    (eval      ,mechanics-salt ,mechanics-salted-key
	       ,eval)
    ))


(define (scmserver-accessor requested-thing-id password)
  (let ((thing-record (assoc requested-thing-id *scmserver-service-table*)))
    (if (not thing-record)
	(error 'unknown-service-id requested-thing-id)
	(let* ((salt (cadr thing-record))
	       (salted-pw (string-append mechanics-salt password))
	       (salted-hash (md5-string salted-pw)))
	  (if (string=? salted-hash (caddr thing-record))
	      (cadddr thing-record)
	      (begin
		(sleep-current-thread 1000)
		(error 'unauthorized)))))))

(register-rpc-procedure *scmserver* "access-thing" scmserver-accessor)


(define (server-start #!optional address port)
  (let ((port (if (default-object? port) mechanics-port port))
	(address (if (default-object? address) (host-address-any) address)))
    (cd "/usr/local/scmutils/null")
    (write-line "About to start rpc server")
    (start-rpc-server *scmserver* port address)
    (write-line "Started rpc server")
    (block-indefinitely)
    (write-line "Unblocked")))

(define (server-stop)
  (stop-rpc-server *scmserver*))


#|
;;;;  Demo
;;; Assume that the password is "foo".

(define client (client-connect "127.0.0.1" mechanics-port))
;Value: client

(define reval
  (client-access-service client 'eval "foo"))
;Value: reval

(define env
  ((client-access-service client 'new-env "foo")))
;Value: env

(reval '|:G| env)
;Value: (*with-units* .0000000000667428 (*unit* si #(3 -1 -2 0 0 0 0) 1))

(reval '(define foo 3) env)
;Value: foo

(reval 'foo env)
;Value: 3

(define env1
  ((client-access-service client 'new-env "foo")))
;Value: env1

(reval 'foo env)
;Value: 3

(reval 'foo env1)
; rpc-remote-error-raised #(remote-condition "Unbound variable: foo" "Subproblem level: 0 (this is the lowest subproblem level)\nExpression (from stack):\n    foo\nEnvironment created by a LAMBDA special form\n\n applied to: ()\nThere is no execution history for this subproblem.\n\nSubproblem level: 1\nExpression (from stack):\n    (success ###)\n subproblem being executed (marked by ###):\n    (with-error-filter (lambda (condition) (k (fail condition)))\n                       (lambda () (apply handler args)))\nEnvironment created by a LET special form\n\n applied to: (#[compile...")
;Quit!

(reval '(define foo 4) env1)
;Value: foo

(reval 'foo env)
;Value: 3

(reval 'foo env1)
;Value: 4

(reval '(simplify '(/ 1 (+ (/ 1 r1) (/ 1 r2)))) env)
;Value: (/ (* r1 r2) (+ r1 r2))
|#