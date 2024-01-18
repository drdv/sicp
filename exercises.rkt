#lang sicp

(#%require rackunit) ;; check-equal?
(#%require (only racket foldl sort))

;; -------------------------------------------
;; Exercise 1.2 p. 27
;; -------------------------------------------
(define (exercise-1.2)
  (/ (+ 5 4 (- 2 (- 3 (+ 6 4/5))))
     (* 3 (- 6 2) (- 2 7))))

"Test Exercise 1.2"
(check-equal? (exercise-1.2) (/ (- 37) 150))

;; -------------------------------------------
;; Exercise 1.3 p. 27
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
;; (if (> b 0) + -) returns - if b <= 0 so the result is a + |b|

;; -------------------------------------------
;; Exercise 1.5
;; -------------------------------------------
;; applicative-order evaluation: enter in an infinite recursion
;; normal-order evaluation: return 0
