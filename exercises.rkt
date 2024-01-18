#lang sicp

(#%require rackunit) ;; check-equal?, check-within etc.
(#%require (only racket foldl sort))
(#%require racket/package)

;; -------------------------------------------
;; Exercise 1.2
;; -------------------------------------------
(define (exercise-1.2)
  (/ (+ 5 4 (- 2 (- 3 (+ 6 4/5))))
     (* 3 (- 6 2) (- 2 7))))

"Test Exercise 1.2"
(check-equal? (exercise-1.2) (/ (- 37) 150))

;; -------------------------------------------
;; Exercise 1.3
;; -------------------------------------------
(define (exercise-1.3.v1 x y z)
  (define (square x)
    (* x x))
  (- (+ (square x) (square y) (square z))
     (square (min x y z))))

;; there seems to be a conflict between the list defined in SICP and the sort procedure
;; (define (exercise-1.3.v2 x y z)
;;   (foldl + 0
;;          (map (lambda (x) (* x x))
;;               (cdr (sort (list x y z) <)))))

"Test Exercise 1.3"
(check-equal? (exercise-1.3.v1 9 5 7) 130)
;; (check-equal? (exercise-1.3.v2 9 5 7) 130)

;; -------------------------------------------
;; Exercise 1.4
;; -------------------------------------------
;; Solution:
;; (if (> b 0) + -) returns - if b <= 0 so the result is a + |b|

;; -------------------------------------------
;; Exercise 1.5
;; -------------------------------------------
;; Solution:
;; applicative-order evaluation: enter in an infinite recursion
;; normal-order evaluation: return 0

;; -------------------------------------------
;; Exercise 1.6
;; -------------------------------------------
(define tolerance 0.0001)

(define (sqrt-v1 x)
  (define (sqrt-recursive guess x)
    (define (improve guess x)
      (define (average a b)
        (/ (+ a b) 2.0))
      (average guess (/ x guess)))
    (define (good-enough? guess x)
      (define (square a)
        (* a a))
      (< (abs (- (square guess) x)) tolerance))
    (if (good-enough? guess x)
        guess
        (sqrt-recursive (improve guess x) x)))
  (sqrt-recursive 1.0 x))

"Test Exercise 1.6"
(check-within (* (sqrt-v1 2) (sqrt-v1 2)) 2 tolerance)

;; (define (new-if predicate then-clause else-clause)
;;   (cond (predicate then-clause)
;;         (else else-clause)))
;; Solution:
;; Note that new-if is a procedure and all of its arguments are evaluated
;; i.e., there is no short-circuit so even if the guess is good-enough, the
;; second argument would be evaluated leading to an infinite loop.

;; -------------------------------------------
;; Exercise 1.7
;; -------------------------------------------
(define (sqrt-v2 x)
  (define (sqrt-recursive old-guess guess x)
    (define (improve guess x)
      (define (average a b)
        (/ (+ a b) 2.0))
      (average guess (/ x guess)))
    (define (good-enough? old-guess guess)
      (< (abs (- old-guess guess)) tolerance))
    (if (good-enough? old-guess guess)
        guess
        (sqrt-recursive guess (improve guess x) x)))
  (sqrt-recursive 0.0 1.0 x))

"Test Exercise 1.7"
"  crazy, but the computation terminates"
(sqrt-v2 999999999999999999999999999999999999)
;; due to loss of numerical precision sqrt-v1 might diverge with the above number
"  very inaccurate"
(* (sqrt-v1 0.0001) (sqrt-v1 0.0001))
"  better accuracy"
(* (sqrt-v2 0.0001) (sqrt-v2 0.0001))

;; -------------------------------------------
;; Exercise 1.8
;; -------------------------------------------

(define (cube-root x)
  (define (cbrt-recursive old-guess guess x)
    (define (improve guess x)
      (define (square a)
        (* a a))
      (/ (+ (/ x (square guess)) (* 2 guess)) 3))
    (define (good-enough? old-guess guess)
      (< (abs (- old-guess guess)) tolerance))
    (if (good-enough? old-guess guess)
        guess
        (cbrt-recursive guess (improve guess x) x)))
  (cbrt-recursive 0.0 1.0 x))

(check-within (* (cube-root 9) (cube-root 9) (cube-root 9)) 9 tolerance)
