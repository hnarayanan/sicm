;; Exercise 9.2

;; After computing the derivatives by hand in Exercise 9.1, here we
;; redo the same thing using the computer. You can open up notes from
;; that exercise alongside to compare notation.

;; As proposed in the question, we are going to represent the
;; functions we're interested in two different ways to see how each
;; feels.

;; Approach 1

(define (F x y)
  (* (square x) (cube y)))

(define (G x y)
  (up (F x y) y))

(define (H x y)
  (F (F x y) y))

;; 9.1 (a)

((component 0) ((D F) 'x 'y))
;; (* 2 x (expt y 3))

((component 1) ((D F) 'x 'y))
;; (* 3 (expt x 2) (expt y 2))

;; 9.1 (b)

((component 0) ((D H) 'x 'y))
;; (* 4 (expt x 3) (expt y 9))

((component 1) ((D H) 'x 'y))
;; (* 9 (expt x 4) (expt y 8))

;; 9.1 (c)

((component 0) ((D G) 'x 'y))
;; (up (* 2 x (expt y 3)) 0)

((component 1) ((D G) 'x 'y))
;; (up (* 3 (expt x 2) (expt y 2)) 1)

;; 9.1 (d)

((D F) 'a 'b)
;; (down (* 2 a (expt b 3)) (* 3 (expt a 2) (expt b 2)))

((D G) 3 5)
;; (down (up 750 0) (up 675 1))

((D H) (* 3 (expt 'a 2)) (* 5 (expt 'b 3)))
;; (down (* 210937500 (expt a 6) (expt b 27)) (* 284765625 (expt a 8) (expt b 24)))

;; Approach 2

;; As noted in the question, while the approach above was easy to
;; write, it is not easy to compose functions with arbitrary
;; structure. e.g. With F taking multiple arguments and G producing
;; tuples of arguments, we cannot just (define H (compose F G)).
;;
;; To allow for this to happen, we need to define procedures that
;; implement the above functions in ways that allow them to carefully
;; build and take apart the structure.

(define (F2 v)
  (let ((x (ref v 0))
        (y (ref v 1)))
    (* (square x) (cube y))))

(define (G2 v)
  (let ((x (ref v 0))
        (y (ref v 1)))
    (up (F2 v) y)))

(define H2 (compose F2 G2))

;; 9.1 (a)

((component 0) ((D F2) (up 'x 'y)))
;; (* 2 x (expt y 3))

((component 1) ((D F2) (up 'x 'y)))
;; (* 3 (expt x 2) (expt y 2))

;; 9.1 (b)

((component 0) ((D H2) (up 'x 'y)))
;; (* 4 (expt x 3) (expt y 9))

((component 1) ((D H2) (up 'x 'y)))
;; (* 9 (expt x 4) (expt y 8))

;; 9.1 (c)

((component 0) ((D G2) (up 'x 'y)))
;; (up (* 2 x (expt y 3)) 0)

((component 1) ((D G2) (up 'x 'y)))
;; (up (* 3 (expt x 2) (expt y 2)) 1)

;; 9.1 (d)

((D F2) (up 'a 'b))
;; (down (* 2 a (expt b 3)) (* 3 (expt a 2) (expt b 2)))

((D G2) (up 3 5))
;; (down (up 750 0) (up 675 1))

((D H2) (up (* 3 (expt 'a 2)) (* 5 (expt 'b 3))))
;; (down (* 210937500 (expt a 6) (expt b 27)) (* 284765625 (expt a 8) (expt b 24)))
