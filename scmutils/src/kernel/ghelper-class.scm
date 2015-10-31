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

;;;;           Most General Generic-Operator Dispatch

(declare (usual-integrations))

;;; Generic-operator dispatch is implemented here by a discrimination
;;; list, where the arguments passed to the operator are examined by
;;; predicates that are supplied at the point of attachment of a
;;; handler (by ASSIGN-OPERATION alias DEFHANDLER).

;;; To be the correct branch all arguments must be accepted by the
;;; branch predicates, so this makes it necessary to backtrack to find
;;; another branch where the first argument is accepted if the second
;;; argument is rejected.  Here backtracking is implemented using #f
;;; as a failure return, requiring further search.  A success is
;;; consummated by calling the WIN procedure.

;;; The discrimination list has the following structure: it is a
;;; possibly improper alist whose "keys" are the predicates that are
;;; applicable to the first argument.  If a predicate matches the
;;; first argument, the cdr of that alist entry is a discrimination
;;; list for handling the rest of the arguments.  If a discrimination
;;; list is improper, then the cdr at the end of the backbone of the
;;; alist is the default handler to apply (all remaining arguments are
;;; implicitly accepted).

(define (make-generic-operator arity #!optional name default-operation)
  (let ((record (make-operator-record arity)))
    (define (find-branch tree arg win)
      (let loop ((tree tree))
        (cond ((pair? tree)
               (or (and ((caar tree) arg)
                        (win (cdar tree)))
                   (loop (cdr tree))))
              ((null? tree) #f)
              (else tree))))
    (define (identity x) x)
    (define (find-handler arguments)
      (let loop ((tree (operator-record-tree record))
                 (args arguments))
        (find-branch tree (car args)
          (if (pair? (cdr args))
              (lambda (branch) (loop branch (cdr args)))
              identity))))
    (define (operator . arguments)
      (if (not (acceptable-arglist? arguments arity))
          (error:wrong-number-of-arguments
           (if (default-object? name) operator name) arity arguments))
      (apply (find-handler arguments) arguments))

    (set-operator-record! operator record)
    (if (not (default-object? name))
        (set-operator-record! name record))

    (set! default-operation
      (if (default-object? default-operation)
          (named-lambda (no-handler . arguments)
            (error "Generic operator inapplicable:"
                   (if (default-object? name) operator name)
                   arguments))
          default-operation))
    (assign-operation operator default-operation)

    operator))

(define *generic-operator-table*
  (make-eq-hash-table))

(define (get-operator-record operator)
  (hash-table/get *generic-operator-table* operator #f))

(define (set-operator-record! operator record)
  (hash-table/put! *generic-operator-table* operator record))

(define (make-operator-record arity) (cons arity '()))
(define (operator-record-arity record) (car record))
(define (operator-record-tree record) (cdr record))
(define (set-operator-record-tree! record tree) (set-cdr! record tree))

(define (acceptable-arglist? lst arity)
  (let ((len (length lst)))
    (and (fix:<= (procedure-arity-min arity) len)
         (or (not (procedure-arity-max arity))
             (fix:>= (procedure-arity-max arity) len)))))

(define (assign-operation operator handler . argument-predicates)
  (let ((record (get-operator-record operator))
        (arity (length argument-predicates)))
    (if record
        (begin
          (if (not (<= arity (procedure-arity-min
                              (operator-record-arity record))))
              (error "Incorrect operator arity:" operator))
          (bind-in-tree
           argument-predicates
           handler
           (operator-record-tree record)
           (lambda (new)
             (set-operator-record-tree! record new))))
        (error "Assigning a handler to an undefined generic operator"
               operator)))
  operator)

(define defhandler assign-operation)

(define (bind-in-tree keys handler tree replace!)
  (let loop ((keys keys) (tree tree) (replace! replace!))
    (if (pair? keys)
        ;; There are argument predicates left
        (let find-key ((tree* tree))
          (if (pair? tree*)
              (if (eq? (caar tree*) (car keys))
                  ;; There is already some discrimination list keyed
                  ;; by this predicate: adjust it according to the
                  ;; remaining keys
                  (loop (cdr keys)
                        (cdar tree*)
                        (lambda (new)
                          (set-cdr! (car tree*) new)))
                  (find-key (cdr tree*)))
              (let ((better-tree
                     (cons (cons (car keys) '()) tree)))
                ;; There was no entry for the key I was looking for.
                ;; Create it at the head of the alist and try again.
                (replace! better-tree)
                (loop keys better-tree replace!))))
        ;; Ran out of argument predicates
        (if (pair? tree)
            ;; There is more discrimination list here, because my
            ;; predicate list is a proper prefix of the predicate list
            ;; of some previous assign-operation.  Insert the handler
            ;; at the end, causing it to implicitly accept any
            ;; arguments that fail all available tests.
            (let ((p (last-pair tree)))
              (if (not (null? (cdr p)))
                  (warn "Replacing a default handler:" (cdr p) handler))
              (set-cdr! p handler))
            (begin
              ;; There is no discrimination list here, because my
              ;; predicate list is not the proper prefix of that of
              ;; any previous assign-operation.  This handler becomes
              ;; the discrimination list, accepting further arguments
              ;; if any.
              (if (not (null? tree))
                  (warn "Replacing a handler:" tree handler))
              (replace! handler))))))

#|
;;; Demonstration of handler tree structure.
;;; Note: symbols were used instead of procedures

(define foo (make-generic-operator 3 'foo 'foo-default))

(pp (get-operator-record foo))
(3 . foo-default)

(defhandler foo 'two-arg-a-b 'a 'b)
(pp (get-operator-record foo))
(3 (a (b . two-arg-a-b)) . foo-default)

(defhandler foo 'two-arg-a-c 'a 'c)
(pp (get-operator-record foo))
(3 (a (c . two-arg-a-c) (b . two-arg-a-b)) . foo-default)

(defhandler foo 'two-arg-b-c 'b 'c)
(pp (get-operator-record foo))
(3 (b (c . two-arg-b-c))
   (a (c . two-arg-a-c) (b . two-arg-a-b))
   . foo-default)

(defhandler foo 'one-arg-b 'b)
(pp (get-operator-record foo))
(3 (b (c . two-arg-b-c) . one-arg-b)
   (a (c . two-arg-a-c) (b . two-arg-a-b))
   . foo-default)

(defhandler foo 'one-arg-a 'a)
(pp (get-operator-record foo))
(3 (b (c . two-arg-b-c) . one-arg-b)
   (a (c . two-arg-a-c) (b . two-arg-a-b) . one-arg-a)
   .
   foo-default)

(defhandler foo 'one-arg-a-prime 'a)
;Warning: Replacing a default handler: one-arg-a one-arg-a-prime

(defhandler foo 'two-arg-a-b-prime 'a 'b)
;Warning: Replacing a handler: two-arg-a-b two-arg-a-b-prime

(defhandler foo 'three-arg-x-y-z 'x 'y 'z)
(pp (get-operator-record foo))
(3 (x (y (z . three-arg-x-y-z)))
   (b (c . two-arg-b-c) . one-arg-b)
   (a (c . two-arg-a-c) (b . two-arg-a-b-prime) . one-arg-a-prime)
   .
   foo-default)
|#
