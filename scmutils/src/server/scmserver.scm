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

(define *scmserver-service-table*
  `((test "1234" "9†·≥\b'\212\215Â‡\aÕ\037yY " ,(lambda () 'test))
    (eval "1234" "9†·≥\b'\212\215Â‡\aÕ\037yY " ,(lambda (x) (eval x user-generic-environment)))))


(define (scmserver-accessor requested-thing-id password)
  (let ((thing-record (assoc requested-thing-id *scmserver-service-table*)))
    (if (not thing-record)
	(error 'unknown-service-id requested-thing-id)
	(let* ((salt (cadr thing-record))
	       (salted-pw (string-append salt password))
	       (salted-hash (md5-string salted-pw)))
	  (if (string=? salted-hash (caddr thing-record))
	      (cadddr thing-record)
	      (begin
		(sleep-current-thread 1000)
		(error 'unauthorized)))))))


(register-rpc-procedure *scmserver* "access-thing" scmserver-accessor)


(define (server-start #!optional port address)
  (let ((port (if (default-object? port) 6674 port))
	(address (if (default-object? address) (host-address-any) address)))
    (start-rpc-server *scmserver* port address)))


(define (server-stop)
  (stop-rpc-server *scmserver*))



;;;
;;; Client:

(define (client-connect port host)
  (let ((client (create-rpc-client)))
    (connect-rpc-client client port host)
    client))

(define (client-disconnect client)
  (disconnect-rpc-client client))

(define (client-access-service client service-id password)
  ((bind-rpc-call client "access-thing")
   service-id password))

#|
;;;;  Demo
;;; Assume that the password is "foo".

(define client (client-connect 6674 "maharal.csail.mit.edu"))
#| client |#

(define foosh
  (client-access-service client
			 'test
			 "foo"))
#| foosh |#

(foosh)
#| test |#


(define baz
  (client-access-service client
			 'eval

(baz 0)
#| 0 |#

(baz '(simplify '(/ 1 (+ (/ 1 r1) (/ 1 r2)))))
#|
(/ (* r1 r2) (+ r1 r2))
|#
|#