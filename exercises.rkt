#lang racket

;; #lang sicp
;; (#%require (only racket module))

;; -------------------------------------------
;; Exercise 1.2
;; -------------------------------------------
(module Exercise/1.2 sicp
  (#%require rackunit)
  (display "============= Exercise 1.2 =============\n")

  (define (exercise-1.2)
    (/ (+ 5 4 (- 2 (- 3 (+ 6 4/5))))
       (* 3 (- 6 2) (- 2 7))))

  (check-equal? (exercise-1.2) (/ (- 37) 150)))

;; -------------------------------------------
;; Exercise 1.3
;; -------------------------------------------
(module Exercise/1.3 sicp
  (#%require
   rackunit
   (only racket foldl sort module format))
  (display "============= Exercise 1.3 =============\n")

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

  (check-equal? (exercise-1.3.v1 9 5 7) 130)
  ;; (check-equal? (exercise-1.3.v2 9 5 7) 130)
  )

;; -------------------------------------------
;; Exercise 1.4
;; -------------------------------------------
(module Exercise/1.4 sicp
  (display "============= Exercise 1.4 =============\n")
  ;; Solution:
  ;; (if (> b 0) + -) returns - if b <= 0 so the result is a + |b|
  )

;; -------------------------------------------
;; Exercise 1.5
;; -------------------------------------------
(module Exercise/1.5 sicp
  (display "============= Exercise 1.5 =============\n")
  ;; Solution:
  ;; applicative-order evaluation: enter in an infinite recursion
  ;; normal-order evaluation: return 0
  )

;; -------------------------------------------
;; Exercise 1.6
;; -------------------------------------------
(module Exercise/1.6 sicp
  (#%require rackunit)
  (#%provide tolerance sqrt-v1)
  (display "============= Exercise 1.6 =============\n")

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

  (check-within (* (sqrt-v1 2) (sqrt-v1 2)) 2 tolerance)

  ;; (define (new-if predicate then-clause else-clause)
  ;;   (cond (predicate then-clause)
  ;;         (else else-clause)))
  ;; Solution:
  ;; Note that new-if is a procedure and all of its arguments are evaluated
  ;; i.e., there is no short-circuit so even if the guess is good-enough, the
  ;; second argument would be evaluated leading to an infinite loop.
  )

;; -------------------------------------------
;; Exercise 1.7
;; -------------------------------------------
(module Exercise/1.7 sicp
  (#%require
   rackunit
   (only racket format)
   (only (submod ".." Exercise/1.6) sqrt-v1 tolerance sqrt-v1))
  (#%provide sqrt-v2)
  (display "============= Exercise 1.7 =============\n")

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

  (display (format "computation terminates: ~a\n" (sqrt-v2 999999999999999999999999999999999999)))
  ;; due to loss of numerical precision sqrt-v1 might diverge with the above number
  (display (format "inaccurate: ~a\n" (* (sqrt-v1 0.0001) (sqrt-v1 0.0001))))
  (display (format "accurate  : ~a\n" (* (sqrt-v2 0.0001) (sqrt-v2 0.0001)))))

;; -------------------------------------------
;; Exercise 1.8
;; -------------------------------------------
(module Exercise/1.8 sicp
  (#%require rackunit)
  (#%require (only (submod ".." Exercise/1.6) tolerance))
  (display "============= Exercise 1.8 =============\n")

  (define (cube-root x)
    (define (cbrt-recursive old-guess guess)
      (define (improve guess)
        (define (square a)
          (* a a))
        (/ (+ (/ x (square guess)) (* 2 guess)) 3))
      (define (good-enough? old-guess guess)
        (< (abs (- old-guess guess)) tolerance))
      (if (good-enough? old-guess guess)
          guess
          (cbrt-recursive guess (improve guess))))
    (cbrt-recursive 0.0 1.0))
  (check-within (* (cube-root 9) (cube-root 9) (cube-root 9)) 9 tolerance))

;; -------------------------------------------
;; Section 1.2.1
;; -------------------------------------------
(module Section/1.2.1 sicp
  (#%require rackunit)
  (display "============= Section 1.2.1 =============\n")

  ;; linear recursive process
  ;; 1. the amount of information required to keep is proportional to n
  ;; 2. number of steps is linear to n
  ;; 3. actual implementation requires the use of an auxiliary date structure (a stack)
  (define (factorial-v1 n)
    (if (= n 1)
        1
        (* n (factorial-v1 (- n 1)))))

  (check-equal? (factorial-v1 5) 120)

  ;; linear iterative process
  ;; 1. a fixed number of state variables is required
  ;; 2. number of steps depend linearly on n
  ;; 3. can be implemented on a machine with a fixed set of registers (w/o auxiliary memory)
  (define (factorial-v2 n)
    (define (factorial-helper counter product)
      (if (> counter n)
          product
          (factorial-helper
           (+ counter 1)
           (* product counter))))
    (factorial-helper 1 1))

  (check-equal? (factorial-v2 5) 120)

  (define (factorial-v3 n)
    (define (factorial-helper acc n)
      (if (= n 1)
          acc
          (factorial-helper (* n acc) (- n 1))))
    (factorial-helper 1 n))

  (check-equal? (factorial-v3 5) 120)

  ;; note the difference between
  ;; iterative process vs. iterative procedure
  ;; recursive process vs. recursive procedure
  ;; the linear iterative process above is implemented with a recursive procedure

  ;; Some languages are able to implement in constant space an iterative process
  ;; even if it is defined in terms of a recursive procedure. An implementation with
  ;; this property is called tail-recursive. Many languages introduce iterative constructs
  ;; in order to model iterative processes because they don't exploit tail-recursion.
  ;; In languages that support tail recursion optimization, iterative constructs
  ;; can be considered as sintactic sugar.
  )

;; -------------------------------------------
;; Exercise 1.9
;; -------------------------------------------
(module Exercise/1.9 sicp
  (#%require rackunit)
  (display "============= Exercise 1.9 =============\n")

  (define (plus.rec a b)
    (if (= a 0)
        b
        (inc (plus.rec (dec a) b))))

  (define (plus.iter a b)
    (if (= a 0)
        b
        (plus.iter (dec a) (inc b))))

  (check-equal? (plus.rec 4 5) 9)
  ;; recursive process
  ;; (plus.rec 4 5)
  ;; (inc (plus.rec 3 5))
  ;; (inc (inc (plus.rec 2 5)))
  ;; (int (inc (inc (plus.rec 1 5))))
  ;; (int (int (inc (inc (plus.rec 0 5))))) ;; end expanding, start contracting
  ;; (int (int (inc (inc 5))))
  ;; (int (int (inc 6)))
  ;; (int (int 7))
  ;; (int 8)
  ;; 9

  (check-equal? (plus.iter 4 5) 9)
  ;; iterative process (implemented using a recursive procedure)
  ;; (plus.iter 4 5)
  ;; (plus.iter 3 6)
  ;; (plus.iter 2 7)
  ;; (plus.iter 1 8)
  ;; (plus.iter 0 9)
  ;; 9
  )

;; -------------------------------------------
;; Exercise 1.10
;; -------------------------------------------
(module Exercise/1.10 sicp
  (#%require
   rackunit
   (only racket format))
  (display "============= Exercise 1.10 =============\n")

  (define (A x y)
    (cond ((= y 0) 0)
          ((= x 0) (* 2 y))
          ((= y 1) 2)
          (else (A (- x 1)
                   (A x (- y 1))))))

  (display (format "(A 1 10): ~a\n" (A 1 10)))
  (display (format "(A 2 4): ~a\n" (A 2 4)))
  (display (format "(A 3 3): ~a\n" (A 3 3)))

  (define (f n) (A 0 n))
  (define (g n) (A 1 n))
  (define (h n) (A 2 n))

  ;; (A 0 n)
  ;; (* 2 n)
  ;; f(n) = 2*n
  (let* ([x 4]
         [y (f x)])
    (display (format "(f ~a): ~a\n" x y))
    (check-equal? y (* 2 x)))

  ;; (A 1 3)
  ;; (A 0 (A 1 2)) -> (f (g (- n 1)))
  ;; (A 0 (A 0 (A 1 1)))
  ;; (A 0 (A 0 2))
  ;; (A 0 (* 2 2))
  ;; (expt 2 n)
  ;; g(n) = 2^n
  (let* ([n 4]
         [y (g n)])
    (display (format "(g ~a): ~a\n" n y))
    (check-equal? y (expt 2 n))
    (check-equal? y (f (g (- n 1)))))

  ;; (A 2 4)
  ;; (A 1 (A 2 3)) -> (g (h (- n 1)))
  ;; (A 1 (A 1 (A 2 2)))
  ;; (A 1 (A 1 (A 1 (A 2 1))))
  ;; (A 1 (A 1 (A 1 2)))
  ;; (expt 2 (expt 2 (expt 2 2)))

  ;; 2^1
  ;; 2^2
  ;; 2^(2^2)
  ;; 2^(2^(2^2))
  ;; h(n) = 2^h(n-1)
  (define (expt-recursive n)
    (if (= n 1)
        2
        (expt 2 (expt-recursive (- n 1)))))

  (let* ([n 4] ;; don't use more than n = 4
         [y (h n)])
    (display (format "(h ~a): ~a" n y))
    (newline)
    (check-equal? y (expt-recursive n))
    (check-equal? y (g (h (- n 1)))))

  ;; we can generate the trace automatically
  ;; (#%require racket/trace)
  ;; (trace A)
  ;; (A 1 5)
  ;; (untrace A)
  ;; (A 1 5)
  )

(module Section/1.2.2 sicp
  (#%require rackunit)
  (display "============= Section 1.2.2 =============\n")

  ;; ----------------------------------------------------------
  ;; Fibonacci sequence (tree recursion)
  ;; ----------------------------------------------------------
  (define (fib.v1 n)
    (cond [(= n 0) 0]
          [(= n 1) 1]
          [else (+ (fib.v1 (- n 1))
                   (fib.v1 (- n 2)))]))

  (check-equal? (fib.v1 10) 55)

  ;; ----------------------------------------------------------
  ;; Fibonacci sequence (iterative process)
  ;; ----------------------------------------------------------
  ;; [n = 0] a1 = 0, a2 = 1
  ;; [n = 1] a1 = 1, a2 = 1
  ;; [n = 2] a1 = 1, a2 = 2
  ;; [n = 3] a1 = 2, a2 = 3
  ;; ...
  (define (fib.v2 n)
    (define (fib-iter a1 a2 n)
      (if (= n 0)
          a1
          (fib-iter a2 (+ a1 a2) (- n 1))))
    (fib-iter 0 1 n)
    )

  (check-equal? (fib.v2 10) 55)

  ;; ----------------------------------------------------------
  ;; Count change (tree recursion)
  ;; ----------------------------------------------------------
  (define (count-change amount coins)
    (cond [(= amount 0) 1]
          [(or (< amount 0)
               (= (length coins) 0)) 0]
          [else (+ (count-change amount (cdr coins))
                   (count-change (- amount (car coins)) coins))]))

  (check-equal? (count-change 100 '(50 25 10 5 1)) 292))

(module Exercise/1.11 sicp
  (#%require rackunit)
  (display "============= Exercise 1.11 =============\n")

  ;; ----------------------------------------------------------
  ;; recursive process
  ;; ----------------------------------------------------------
  (define (f.v1 n)
    (if (< n 3)
        n
        (+ (f.v1 (- n 1))
           (* 2 (f.v1 (- n 2)))
           (* 3 (f.v1 (- n 3))))))

  (check-equal? (f.v1 10) 1892)

  ;; ----------------------------------------------------------
  ;; iterative process
  ;; ----------------------------------------------------------
  (define (f.v2 n)
    (define (f.iter a0 a1 a2 n)
      (if (= n 0)
          a0
          ;; a2 <- 3*a0 + 2*a1 + a2
          ;; a1 <- a2
          ;; a0 <- a1
          (f.iter a1 a2 (+ (* 3 a0) (* 2 a1) a2) (- n 1))))
    (if (< n 3)
        n
        (f.iter 0 1 2 n)))

  (check-equal? (f.v2 10) 1892)
  (check-equal? (f.v2 -2) -2))

(module Exercise/1.12 sicp
  (#%require rackunit)
  (display "============= Exercise 1.12 =============\n")

  (define (pascal-triangle.v1 row-numb verbose)
    (define (pascal-triangle-next-row current-row row-counter)
      (define (sum-pairs row)
        (cond [(> (length row) 1) (cons (+ (car row) (car (cdr row)))
                                        (sum-pairs (cdr row)))]
              [else nil]))

      (cond [verbose (display current-row) (newline)])
      (let [(next-row (append (cons 1 (sum-pairs current-row))
                              (list 1)))]
        (cond [(= row-counter row-numb)
               next-row]
              [else (pascal-triangle-next-row next-row (+ row-counter 1))])))

    (cond [(< row-numb 1) nil]
          [(= row-numb 1) (list 1)]
          [else (pascal-triangle-next-row (list 1) 2)]))

  (check-equal? (pascal-triangle.v1 0 #f) '())
  (check-equal? (pascal-triangle.v1 1 #f) '(1))
  (check-equal? (pascal-triangle.v1 2 #f) '(1 1))
  (check-equal? (pascal-triangle.v1 3 #f) '(1 2 1))
  (check-equal? (pascal-triangle.v1 4 #f) '(1 3 3 1))
  (check-equal? (pascal-triangle.v1 5 #f) '(1 4 6 4 1))
  (check-equal? (pascal-triangle.v1 6 #f) '(1 5 10 10 5 1))

  (define (pascal-triangle.v2 row-numb col-numb)
    (if (or (= col-numb 1) (= col-numb row-numb))
        1
        (+ (pascal-triangle.v2 (- row-numb 1)
                               (- col-numb 1))
           (pascal-triangle.v2 (- row-numb 1)
                               col-numb)))
    )

  (check-equal? (list (pascal-triangle.v2 6 1)
                      (pascal-triangle.v2 6 2)
                      (pascal-triangle.v2 6 3)
                      (pascal-triangle.v2 6 4)
                      (pascal-triangle.v2 6 5)
                      (pascal-triangle.v2 6 6)) '(1 5 10 10 5 1))
  )

(#%require
 'Exercise/1.2
 'Exercise/1.3
 'Exercise/1.4
 'Exercise/1.5
 (only 'Exercise/1.6 sqrt-v1)
 (only 'Exercise/1.7 sqrt-v2)
 'Exercise/1.8
 'Section/1.2.1
 'Exercise/1.9
 'Exercise/1.10
 'Section/1.2.2
 'Exercise/1.11
 'Exercise/1.12)

;; (module mitko racket
;;   (provide f1)
;;   (define (f1)
;;     1)
;;   (module* test #f
;;     (provide f2)
;;     (define (f2)
;;     2)
;;     (f1)
;;     ))

;; (define mitko (make-hash))
;; (hash-set! mitko "key1" 11)
;; (hash-ref mitko "key2")



;; (#%require (only (submod "." mitko) f1))

;; (#%require (only 'mitko f1))

;; (#%require (only (submod 'mitko test) f2))  ;; WOW this actually works


;; (f2)
;; (only (submod "." mitko) f1)

;; Modules
;; https://stackoverflow.com/a/36460362
