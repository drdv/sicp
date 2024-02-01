#lang racket

;; #lang sicp
;; (#%require (only racket module))

(module common-utils sicp
  (#%provide run-n-times)

  (define (run-n-times n func args output)
    (cond [(= n 0) (apply + output)]
          [else
           (run-n-times (- n 1) func args (append output (list (apply func args))))]))
  )

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
  (#%provide count-change)
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
                      (pascal-triangle.v2 6 6)) '(1 5 10 10 5 1)))

(module Exercise/1.13 sicp
  (#%require rackunit)
  (display "============= Exercise 1.13 =============\n")
  ;; See latex note
  )

(module Exercise/1.14 sicp
  (#%require rackunit
             (only (submod ".." Section/1.2.2) count-change)
             (only racket format for in-range set!))
  (#%provide logb)
  (display "============= Exercise 1.14 =============\n")

  (check-equal? (count-change 11 '(50 25 10 5 1)) 4)

  (define (count-change-display amount coins verbose)
    (define (count-change-display-helper amount coins offset)
      (set! numb-calls (+ numb-calls 1))
      (if verbose
          (display
           (format "[~a]~a(cc ~a ~a)\n"
                   (if (= amount 0) "*" " ")
                   offset
                   amount
                   (length coins))))
      (define new-offset (string-append offset "  "))
      (cond [(= amount 0) 1]
            [(or (< amount 0)
                 (= (length coins) 0)) 0]
            [else
             (+ (count-change-display-helper amount (cdr coins) new-offset)
                (count-change-display-helper (- amount (car coins)) coins new-offset))]))

    (define numb-calls 0)
    (count-change-display-helper amount coins "")
    numb-calls)

  (define (logb b n)
    ;; log_b(n) = ln(n)/ln(b)
    (/ (log n) (log b)))

  (count-change-display 11 '(50 25 10 5 1) #t)
  (for ([coins '((1)
                 (1 1)
                 (1 1 1)
                 (1 1 1 1)
                 (1 1 1 1 1))])
    (display (format "========== ~a coin ==========\n" (length coins)))
    (for ([k '(5 10 50)])
      (let [(n (count-change-display k coins #f))]
        (display (format "~a: ~a (~a)\n" k n (logb k n)))))
    )

  ;; note: the number of iterations depends on the order of coins
  ;; e.g., '(1 5) vs. '(5 1)
  ;;
  ;; 1. The space is the number of stacks we have to keep, i.e., the depth of the
  ;; recursion. In the worst case we would have to express the amount in terms of the
  ;; smallest coin (which is 1) so the depth is proportional to the amount.
  ;;
  ;; 2. Intuitively we should have O(amount^5). It seems to me that we can compute an
  ;; upper bound for the number of steps by using '(1 1 1 1 1) as coins. Probably as we
  ;; increase the amount, the power would get closer to 5 but it takes a lot of time.
  )

(module Exercise/1.15 sicp
  (#%require rackunit
             (only racket format)
             (only (submod ".." Exercise/1.14) logb))
  (display "============= Exercise 1.15 =============\n")

  (define (cube x) (* x x x))
  (define (p x) (- (* 3 x) (* 4 (cube x))))
  (define (sine angle iter)
    (display (format "[~a] angle: ~a\n" iter angle))
    (if (not (> (abs angle) 0.1))
        angle
        (p (sine (/ angle 3.0) (+ iter 1)))))

  ;; At every call of sine, the angle is divided by 3 so the number of steps is
  ;; determined by the condition: angle / 3^k <= 0.1, from which we get
  ;; 3^k = angle / 0.1, and hence log_3(angle / 0.1)
  (sine 12.15 0)
  (format "[angle: 12.15] iter: ~a\n" (ceiling (logb 3 (/ 12.15 0.1))))
  ;; in terms of space, we need one stack per invocation (note that this is not tail
  ;; recursive due to the application of the function p)

  ;; test something else
  (sine 500.0 0)
  (format "[angle: 500.0] iter: ~a\n" (ceiling (logb 3 (/ 500.0 0.1))))
  )

(module Exercise/1.16 sicp
  (#%require rackunit
             (only racket format))
  (display "============= Exercise 1.16 =============\n")

  (define (show even-or-odd iter n b a)
    (display
     (format "(~a)[~a]: ~a = ~a*~a^~a\n"
             even-or-odd iter (* a (expt b n)) a b n)))

  (define (square x) (* x x))
  (define (fast-expt-recursive b n iter)
    (cond [(= n 0) 1]
          [(even? n)
           (show "E" iter n b 1)
           (square (fast-expt-recursive b (/ n 2) (+ iter 1)))]
          [else
           (show "O" iter n b 1)
           (* b (fast-expt-recursive b (- n 1) (+ iter 1)))]))

  ;; 1 * 2^16 = 1*(2^2)^8 = (2^2)^2 * (2^2)^(8-1)
  (define (fast-expt-iterative.v1 b n a iter)
    (cond [(= n 0) a]
          [(even? n)
           (show "E" iter n b a)
           (fast-expt-iterative.v1 (square b) (- (/ n 2) 1) (* a (square b)) (+ iter 1))]
          [else
           (show "O" iter n b a)
           (fast-expt-iterative.v1 b (- n 1) (* a b) (+ iter 1))]))

  (define (fast-expt-iterative.v2 b n a iter)
    (cond [(= n 0) a]
          [(even? n)
           (show "E" iter n b a)
           (fast-expt-iterative.v2 (square b) (/ n 2) a (+ iter 1))]
          [else
           (show "O" iter n b a)
           (fast-expt-iterative.v2 b (- n 1) (* a b) (+ iter 1))]))

  (check-equal? (fast-expt-recursive 5 21 1) 476837158203125)
  (check-equal? (fast-expt-iterative.v1 5 21 1 1) 476837158203125)
  (check-equal? (fast-expt-iterative.v2 5 21 1 1) 476837158203125))

(module Exercise/1.17 sicp
  (#%require rackunit
             (only racket format raise))
  (#%provide mult.v2)
  (display "============= Exercise 1.17 =============\n")

  (define (mult.v1 a b)
    (cond [(or (= a 0) (= b 0)) 0]
          [else (+ a (mult.v1 a (- b 1)))]))

  (check-equal? (mult.v1 5 7) 35)

  (define (show even-or-odd a b acc)
    (display
     (format "[~a]: ~a = ~a+~a*~a\n"
             even-or-odd (+ acc (* a b)) acc a b)))

  (define (mult.v2 a b acc verbose)
    (define (double x) (* 2 x))
    (define (halve x)
      (if (even? x)
          (/ x 2)
          (raise "Cannot halve an odd integer.")))
    (cond [(or (= a 0) (= b 0)) acc]
          [(even? b)
           (if verbose (show "E" a b acc))
           (mult.v2 (double a) (halve b) acc verbose)]
          [else
           (if verbose (show "O" a b acc))
           (mult.v2 a (- b 1) (+ acc a) verbose)]))

  (check-equal? (mult.v2 5 0 0 #f) 0)
  (check-equal? (mult.v2 5 1 0 #f) 5)
  ;; 5 * 9 = 5 + 5 * 8 = 5 + 10 * 4 = 5 + 20 * 2 = 5 + 40 * 1 = 45
  (check-equal? (mult.v2 5 9 0 #t) 45)
  (check-equal? (mult.v2 5 10 0 #f) 50))

(module Exercise/1.18 sicp
  (#%require rackunit
             (only (submod ".." Exercise/1.17) mult.v2))
  (display "============= Exercise 1.18 =============\n")
  ;; I implemented Exercise 1.17 taking into account the conditions of Exercise 1.18
  (check-equal? (mult.v2 5 9 0 #t) 45)
  )

(module Exercise/1.19 sicp
  (#%require rackunit)
  (display "============= Exercise 1.19 =============\n")

  (define (fib n)
    (fib-iter 1 0 0 1 n))

  (define (fib-iter a b p q count)
    (cond ((= count 0) b)
          ((even? count)
           (fib-iter a
                     b
                     (+ (* p p) (* q q)); compute p'
                     (+ (* q q) (* 2 p q) ) ; compute q'
                     (/ count 2)))
          (else (fib-iter (+ (* b q) (* a q) (* a p))
                          (+ (* b p) (* a q))
                          p
                          q
                          (- count 1)))))

  (check-equal? (fib 10) 55))

(module Exercise/1.20 sicp
  (#%require rackunit
             (only racket format))
  (display "============= Exercise 1.20 =============\n")

  (define (show a b)
    (display (format "[~a] a: ~a, b: ~a\n" (modulo a b) a b)))

  (define (gcd a b)
    (if (not (= b 0)) (show a b))
    (if (= b 0)
        a
        (gcd b (remainder a b))))

  (gcd 206 40)
  ;; applicative order: 4 evaluations (as there are 4 iterations)
  ;; normal order:
  ;; --------------------------------------------------------------
  ;; (gcd 206 40)
  ;; (if (= 40 0) ...)
  ;; --------------------------------------------------------------
  ;; (gcd 40 (remainder 206 40))
  ;; (if (= (remainder 206 40) 0) ...) [1] => (if (= 6 0) ...)
  ;; --------------------------------------------------------------
  ;; (gcd (remainder 206 40)
  ;;      (remainder 40 (remainder 206 40)))
  ;; (if (= (remainder 40 (remainder 206 40)) 0) ...) [2] => (if (= 4 0) ...)
  ;; --------------------------------------------------------------
  ;; (gcd (remainder 40 (remainder 206 40))
  ;;      (remainder (remainder 206 40) (remainder 40 (remainder 206 40))))
  ;; (if (= (remainder (remainder 206 40) (remainder 40 (remainder 206 40))) 0) ...) [4] => (if (= 2 0) ...)
  ;; --------------------------------------------------------------
  ;; (gcd (remainder (remainder 206 40) (remainder 40 (remainder 206 40)))
  ;;      (remainder (remainder 40 (remainder 206 40)) (remainder (remainder 206 40) (remainder 40 (remainder 206 40)))))
  ;; (if (= (remainder (remainder 40 (remainder 206 40)) (remainder (remainder 206 40) (remainder 40 (remainder 206 40)))) 0) ...) [7] => (if (= 0 0) ...)
  ;; --------------------------------------------------------------
  ;; (remainder (remainder 206 40) (remainder 40 (remainder 206 40))) [4] => DONE
  ;;
  ;; (+ 1 2 4 7 4) -> 18 evaluations of (remainder ...)
  )

(module Exercise/1.21 sicp
  (#%require rackunit)
  (#%provide smallest-divisor)
  (display "============= Exercise 1.21 =============\n")

  (define (smallest-divisor n)
    (define (find-divisor n test-divisor)
      (define (square n) (* n n))
      (define (divides? a b) (= (remainder b a) 0))
      (cond ((> (square test-divisor) n) n)
            ((divides? test-divisor n) test-divisor)
            (else (find-divisor n (+ test-divisor 1)))))

    (find-divisor n 2))

  (check-equal? (smallest-divisor 199) 199)
  (check-equal? (smallest-divisor 1999) 1999)
  (check-equal? (smallest-divisor 19999) 7))

(module Exercise/1.22 sicp
  (#%require rackunit
             (only (submod ".." Exercise/1.21) smallest-divisor))
  (#%provide timed-prime-test prime-numbers)
  (display "============= Exercise 1.22 =============\n")

  (define (timed-prime-test n)
    (define (start-prime-test n start-time)
      (define (report-prime elapsed-time)
        (display " *** ")
        (display elapsed-time)
        #t)
      (define (prime? n)
        (= n (smallest-divisor n)))
      (if (prime? n)
          (report-prime (- (runtime) start-time))
          #f))
    (newline)
    (display n)
    (start-prime-test n (runtime)))

  ;; I stop once the given number of primes have been found
  (define (serch-for-primes lb numb-primes)
    (let ((n (if (even? lb) (+ lb 1) lb)))
      (if (= numb-primes 0)
          (display "\n=== END ===\n")
          (if (timed-prime-test n)
              (serch-for-primes (+ n 2) (- numb-primes 1))
              (serch-for-primes (+ n 2) numb-primes)))))

  (serch-for-primes 1000 3)
  (serch-for-primes 10000 3)
  (serch-for-primes 100000 3)
  (serch-for-primes 1000000 3)
  (serch-for-primes 10000000 3)


  ;; ============================
  ;; the times I observe:
  ;; ============================
  ;;       1000: ~1
  ;;     10_000: ~3
  ;;    100_000: ~9
  ;;  1_000_000: ~28
  ;; 10_000_000: ~82
  ;; ============================
  ;; there is a sqrt(10) factor
  ;; ============================

  (define prime-numbers '(1009 1013 1019 10007 10009 10037 100003 100019 100043 1000003 1000033 1000037 10000019 10000079 10000103)))

(module Exercise/1.23 sicp
  (#%require rackunit
             (only racket format for)
             (only (submod ".." Exercise/1.21) smallest-divisor)
             (only (submod ".." Exercise/1.22) prime-numbers)
             (only (submod ".." common-utils) run-n-times))
  (display "============= Exercise 1.23 =============\n")

  (define (smallest-divisor-optimized n)
    (define (find-divisor n test-divisor)
      (define (next numb)
        (if (= numb 2) 3 (+ numb 2)))
      (define (square n) (* n n))
      (define (divides? a b) (= (remainder b a) 0))
      (cond ((> (square test-divisor) n) n)
            ((divides? test-divisor n) test-divisor)
            (else (find-divisor n (next test-divisor)))))
    (find-divisor n 2))

  (define (smallest-divisor-optimized-no-extra-function n)
    (define (find-divisor n test-divisor)
      (define (square n) (* n n))
      (define (divides? a b) (= (remainder b a) 0))
      (cond ((> (square test-divisor) n) n)
            ((divides? test-divisor n) test-divisor)
            (else (find-divisor n (if (= test-divisor 2) 3 (+ test-divisor 2))))))

    (find-divisor n 2))

  ;; ---------------------------------------------
  ;; Instead of displaying the time return it (here we test only prime numbers)
  ;; ---------------------------------------------
  (define (timed-prime-test n)
    (define (start-prime-test n start-time)
      (define (prime? n)
        (= n (smallest-divisor n)))
      (if (prime? n)
          (- (runtime) start-time)
          0))
    (start-prime-test n (runtime)))

  (define (timed-prime-test-optimized n)
    (define (start-prime-test n start-time)
      (define (prime? n)
        (= n (smallest-divisor-optimized n)))
      (if (prime? n)
          (- (runtime) start-time)
          0))
    (start-prime-test n (runtime)))

  (define (timed-prime-test-optimized-no-extra-function n)
    (define (start-prime-test n start-time)
      (define (prime? n)
        (= n (smallest-divisor-optimized-no-extra-function n)))
      (if (prime? n)
          (- (runtime) start-time)
          0))
    (start-prime-test n (runtime)))
  ;; ---------------------------------------------

  (let ([numb-evals 1000])
    (for ([prime-number prime-numbers])
      (display
       (format "~a: ~a" prime-number
               (/ (/ (run-n-times
                      numb-evals
                      timed-prime-test
                      (list prime-number)
                      '())
                     (/ numb-evals 1.0))
                  (/ (run-n-times
                      numb-evals
                      timed-prime-test-optimized
                      (list prime-number)
                      '())
                     (/ numb-evals 1.0)))))
      (newline)))

  ;; inlined version
  (let ([numb-evals 1000])
    (for ([prime-number prime-numbers])
      (display
       (format "~a: ~a" prime-number
               (/ (/ (run-n-times
                      numb-evals
                      timed-prime-test
                      (list prime-number)
                      '())
                     (/ numb-evals 1.0))
                  (/ (run-n-times
                      numb-evals
                      timed-prime-test-optimized-no-extra-function
                      (list prime-number)
                      '())
                     (/ numb-evals 1.0)))))
      (newline)))

  ;; So I observe a speedup but less than twice (close to 1.6 times). We halve the
  ;; number of iterations and I didn't expect that the extra function call would have
  ;; such a signifficant influence (especially because "next" is super simple). In the
  ;; second test, where "next" is inlined we see that the ratio is almost 2 for large
  ;; prime numbers. This means that the function call itself (with its additional stack)
  ;; is the reason. The additional if and equality comparison seem to influence mainly
  ;; the time for small numbers.
  )

(module Exercise/1.24 sicp
  (#%require (only racket format for in-range)
             (only (submod ".." Exercise/1.22) prime-numbers)
             (only (submod ".." common-utils) run-n-times))
  (display "============= Exercise 1.24 =============\n")

  ;; We could have used fast-expt-iterative.v2 to compute p = x^n and then find p%n.
  ;; In this way we would have an iterative procedure but we would be dealing with huge
  ;; numbers p. The idea behind the expmod procedure is that by using the following
  ;; transformations we don't need to deal with huge numbers (see latex note for proof):
  ;;   1. (x*y)%n = ((x%n)*(y%n))%n
  ;;   2. (x*y)%n = (x*(y%n))%n
  (define (expmod base exp m)
    (define (square x)
      (* x x))
    (cond ((= exp 0) 1)
          ((even? exp)
           (remainder
            (square (expmod base (/ exp 2) m))
            m))
          (else
           (remainder
            (* base (expmod base (- exp 1) m))
            m))))

  (define (fermat-test n)
    (define (try-it a)
      (= (expmod a n n) a))
    (try-it (+ 1 (random (- n 1)))))

  (define (fast-prime? n times)
    (cond ((= times 0) #t)
          ((fermat-test n) (fast-prime? n (- times 1)))
          (else #f)))

  (define numb-fermat-tests 10)
  (define (timed-prime-test-fermat n)
    (define (start-prime-test n start-time)
      (if (fast-prime? n numb-fermat-tests)
          (- (runtime) start-time)
          0))
    (start-prime-test n (runtime)))

  (let ([numb-evals 1000])
    (for ([prime-number prime-numbers])
      (display
       (format "~a: ~a" prime-number
               (/ (run-n-times
                   numb-evals
                   timed-prime-test-fermat
                   (list prime-number)
                   '())
                  (/ (* numb-fermat-tests numb-evals) 1.0))))
      (newline)))

  (for ([iter (in-range 1 21)])
    (let ([numb-evals 1000])
      (display
       (format "[~a] 1M / 1K: ~a"
               iter
               (/ (/ (run-n-times
                      numb-evals
                      timed-prime-test-fermat
                      (list 1000003)
                      '())
                     (/ (* numb-fermat-tests numb-evals) 1.0))
                  (/ (run-n-times
                      numb-evals
                      timed-prime-test-fermat
                      (list 1009)
                      '())
                     (/ (* numb-fermat-tests numb-evals) 1.0)))))
      (newline)))

  ;; Since the complexity is O(long(n)) I expect to see a linear increase in computation
  ;; time for n = [10^3, 10^4, ...] - which is indeed the case. But the slope of the
  ;; line is not as steep as I expected. In particular, I expected to have twice slower
  ;; computations for primes around 1M compared to 1K. However, I observe a factor of
  ;; ~1.6 instead of 2. I am not quite sure why.
  ;;
  ;; 1. Since we perform the (try-it ...) with a random number the same number of times
  ;; for 1K and 1M, in the former case this costs relatively more as it is distributed
  ;; accross less iterations. To test this, I run with (try-it 1) instead of a random
  ;; number (which of course makes no sense other than checking efficiency of
  ;; computations) and I get time ~1.8, so indeed this has an impact.
  ;;
  ;; 1. Another thing I noticed is that for 1K we have 6 iterations with odd powers, while
  ;; for 1M we have only 7, so it is possible that the time for 1K is relatively high
  ;; because of this:
  ;; 1K: (/ 6.0 16) => 0.38
  ;; 1M: (/ 7.0 27) => 0.26
  ;; This might explain the remaining difference.
  )

(module Exercise/1.25 sicp
  (#%require (only racket format))
  (display "============= Exercise 1.25 =============\n")

  (define (square x)
    (* x x))

  (define (expmod base exp m)
    (cond ((= exp 0) 1)
          ((even? exp)
           (let ([z (square (expmod base (/ exp 2) m))])
             (display (format "[E] ~a\n" z))
             (remainder z m)))
          (else
           (let ([z (expmod base (- exp 1) m)])
             (display (format "[O] ~a\n" z))
             (remainder (* base z) m)))))

  ;; same as fast-expt-iterative.v2 without the show
  (define (fast-expt b n a)
    (cond [(= n 0) a]
          [(even? n)
           (fast-expt (square b) (/ n 2) a)]
          [else
           (fast-expt b (- n 1) (* a b))]))

  (let ([x 2]
        [n 1000])
    (expmod x n n)
    (let ([z (fast-expt x n 1)])
      (display (format "~a\n" z))
      (remainder z n)))

  ;; the largest number we have to deal with in expmod e.g., for 2^1000 is 817216
  ;; while in fast-expt it is astronomical and has 302 digits -> (ceiling (* 1000 (logb 10 2)))
  ;; so for larger powers we would have to store huge numbers (note that not all languages can
  ;; handle such numbers like python and racket). See footnote 46 on page 68.
  )

(module Exercise/1.26 sicp
  (#%require rackunit
             (only racket format for set!))
  (display "============= Exercise 1.26 =============\n")

  ;; the version of Louis Reasoner has complexity O(log(2^n)) = O(n*log(2)) = O(n),
  ;; note that the linear recursion becomes tree recursion with two branches at each step.
  ;; The following test confirms this by counting number of iterations:

  (define (expmod-linear base exp m)
    (set! numb-calls (+ numb-calls 1))
    (define (square x)
      (* x x))
    (cond ((= exp 0) 1)
          ((even? exp)
           (let ([z (square (expmod-linear base (/ exp 2) m))])
             (remainder z m)))
          (else
           (let ([z (expmod-linear base (- exp 1) m)])
             (remainder (* base z) m)))))

  (define (expmod-tree base exp m)
    (set! numb-calls (+ numb-calls 1))
    (cond ((= exp 0) 1)
          ((even? exp)
           (let ([z (* (expmod-tree base (/ exp 2) m)
                       (expmod-tree base (/ exp 2) m))])
             (remainder z m)))
          (else
           (let ([z (expmod-tree base (- exp 1) m)])
             (remainder (* base z) m)))))

  (define numb-calls 0) ;; FIXME: there must be a better way to do this
  (define (test-f f n)
    (set! numb-calls 0)
    (f 2 n n)
    numb-calls)

  (define (logb b n)
    ;; log_b(n) = ln(n)/ln(b)
    (/ (log n) (log b)))

  (define n-to-test '(10 100 1000 10000 100000 1000000))
  (display "-------------------------------------\n")
  (display "expmod-linear\n")
  (display "-------------------------------------\n")
  (for ([n n-to-test])
    (let ([z (test-f expmod-linear n)])
      (display (format "[~a] ~a (~a)\n" n z (/ z (logb 2 n))))))
  (display "-------------------------------------\n")
  (display "expmod-tree\n")
  (display "-------------------------------------\n")
  (for ([n n-to-test])
    (let ([z (test-f expmod-tree n)])
      (display (format "[~a] ~a (~a)\n" n z (/ z (* n 1.0)))))))

;; FIXME: add a submodule for the tests so that they are not run when
;; I import something from a previous exercise.
