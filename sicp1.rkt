;; =====================================================================================
;; Exercises in Chapter 1
;; =====================================================================================
#lang racket/base

(module common-utils sicp
  (#%provide tolerance
             square
             cube
             average
             golden-ratio
             run-n-times)

  (define tolerance 1e-5)

  (define golden-ratio (/ (+ 1 (sqrt 5)) 2.0))

  (define (square x)
    (* x x))

  (define (cube x)
    (* x x x))

  (define (average a b)
    (/ (+ a b) 2.0))

  (define (run-n-times n func args output)
    (cond [(= n 0) (apply + output)]
          [else
           (run-n-times (- n 1)
                        func
                        args
                        (append output
                                (list (apply func args))))])))

(module conversion-utils racket/base
  (#%provide mcons->cons
             pict->file
             painter->png
             dc->png
             get-drawing-context)
  (#%require racket/class
             pict
             racket/draw
             racket/path
             file/convertible
             net/sendurl
             (only sicp-pict paint))

  #|
  The idea is to require a list defined in the sicp language and convert it to a list in
  the racket language because sometimes I need to use procedures that require immutable
  pairs.
  |#
  (define (mcons->cons mc)
    (cond [(null? mc) '()]
          [(number? mc) mc]
          [else (cons (mcons->cons (mcar mc))
                      (mcons->cons (mcdr mc)))]))

  ;; save a pict to file (docs.racket-lang.org/pict)
  (define (pict->file a-pict
                      #:file [filename "/tmp/_racket_tmp.svg"]
                      #:open [open-file #f])
    (define (pict->png)
      (send (pict->bitmap a-pict) save-file filename 'png))
    (define (pict->svg)
      (define out (open-output-file filename #:exists 'replace))
      (write-bytes (convert a-pict 'svg-bytes) out)
      ;; FIXME: can't we use a context manager as in python?
      (close-output-port out))

    (if (bytes=? (path-get-extension filename) #".svg")
        (pict->svg)
        (pict->png))
    (if open-file
        (send-url/file filename)
        (display (format "file ~a created\n" filename))))

  #|
  Towards the end of the "picture language" related exercises I use my own paiters and
  paiter transformers/combiners but in the initial exercises it is convenient to use the
  sicp-pict package to verify results. Visualizing painters from sicp-pict requires
  using the paint procedure (see with-frame at
  https://github.com/sicp-lang/sicp/blob/master/sicp-pict/main.rkt) and we cannot
  directly pass a frame - which I find "unfortunate". The save-painter procedure saves
  to a png the image associated with a painter.
  |#
  (define (painter->png painter
                        #:size [size 500]
                        #:file [filename "/tmp/_racket_tmp.png"]
                        #:open [open-file #f])
    (send (send (paint painter #:width size #:height size) get-bitmap)
          save-file filename 'png)
    (if open-file
        (send-url/file filename)
        (display (format "file ~a created\n" filename))))

  ;; save a drawing context as png
  (define (dc->png dc
                   #:file [filename "/tmp/_racket_tmp.png"]
                   #:open [open-file #f])
    (send (send dc get-bitmap) save-file filename 'png)
    (if open-file
        (send-url/file filename)
        (display (format "file ~a created\n" filename))))

  (define (get-drawing-context size)
    (let ([dc (new bitmap-dc% [bitmap (make-bitmap size size)])])
      (send dc set-pen "black" 5 'solid)
      (send dc set-brush "white" 'solid)
      (send dc draw-rectangle 0 0 size size)
      dc)))

(module Exercise/1.1 sicp
  (#%require (only racket/base module+))

  (define a 3)
  (define b (+ a 1))

  (module+ test
    (#%require rackunit)
    (display "--> Exercise/1.1\n")

    (check-equal? 10 10)
    (check-equal? (+ 5 3 4) 12)
    (check-equal? (- 9 1) 8)
    (check-equal? (/ 6 2) 3)
    (check-equal? (+ (* 2 4) (- 4 6)) 6)
    (check-equal? (+ a b (* a b)) 19)
    (check-false (= a b))
    (check-equal? (if (and (> b a) (< b (* a b))) b a) 4)
    (check-equal? (cond ((= a 4) 6)
                        ((= b 4) (+ 6 7 a))
                        (else 25)) 16)
    (check-equal? (+ 2 (if (> b a) b a)) 6)
    (check-equal? (* (cond ((> a b) a)
                           ((< a b) b)
                           (else -1))
                     (+ a 1)) 16)))

(module Exercise/1.2 sicp
  (#%require (only racket/base module+))

  (define (an-expression)
    (/ (+ 5 4 (- 2 (- 3 (+ 6 4/5))))
       (* 3 (- 6 2) (- 2 7))))

  (module+ test
    (#%require rackunit)
    (display "--> Exercise/1.2\n")

    (check-equal? (an-expression) (/ (- 37) 150))))

(module Exercise/1.3 sicp
  (#%require (only racket/base module+ module* λ)
             (only (submod ".." common-utils) square))

  (define (sum-squares-v1 x y z)
    (- (+ (square x) (square y) (square z))
       (square (min x y z))))

  (module+ test
    (#%require rackunit)
    (display "--> Exercise/1.3\n")

    (check-equal? (sum-squares-v1 9 5 7) 130))

  (module* test-in-racket racket/base ; sort requires immutable pairs
    (#%require rackunit
               (only (submod ".." ".." conversion-utils) mcons->cons))
    (display "--> Exercise/1.3 (test-in-racket)\n")

    (define (sum-squares-v2 x y z)
      (foldl + 0
             (map (λ (x) (* x x))
                  (cdr (sort (list x y z) <)))))

    (check-equal? (sum-squares-v2 9 5 7) 130)))

(module Exercise/1.4 sicp
  #| Solution:
  (if (> b 0) + -) returns - if b <= 0 so the result is a + |b|
  |#)

(module Exercise/1.5 sicp
  #| Solution:
  applicative-order evaluation: enter in an infinite recursion
  normal-order evaluation: return 0
  |#)

(module Section/1.1.7 sicp
  (#%provide sqrt-v1)
  (#%require (only racket/base module+ format)
             (only (submod ".." common-utils) square average tolerance))

  (define (sqrt-v1 x)
    (define (sqrt-recursive guess x)
      (define (improve guess x)
        (average guess (/ x guess)))
      (define (good-enough? guess x)
        (< (abs (- (square guess) x)) tolerance))
      (if (good-enough? guess x)
          guess
          (sqrt-recursive (improve guess x) x)))
    (sqrt-recursive 1.0 x))

  (module+ test
    (#%require rackunit)
    (display "--> Section/1.1.7\n")

    (check-within (* (sqrt-v1 2) (sqrt-v1 2)) 2 tolerance)))

(module Exercise/1.6 sicp
  (define (new-if predicate then-clause else-clause)
    (cond (predicate then-clause)
          (else else-clause)))
  #| Solution:
  Note that new-if is a procedure and all of its arguments are evaluated
  i.e., there is no short-circuit so even if the guess is good-enough, the
  second argument would be evaluated leading to an infinite loop.
  |#)

(module Exercise/1.7 sicp
  (#%require (only racket/base module+ format)
             (only (submod ".." common-utils) average tolerance)
             (only (submod ".." Section/1.1.7) sqrt-v1))

  (define (sqrt-v2 x)
    (define (sqrt-recursive old-guess guess x)
      (define (improve guess x)
        (average guess (/ x guess)))
      (define (good-enough? old-guess guess)
        (< (abs (- old-guess guess)) tolerance))
      (if (good-enough? old-guess guess)
          guess
          (sqrt-recursive guess (improve guess x) x)))
    (sqrt-recursive 0.0 1.0 x))

  (module+ test
    (display "--> Exercise/1.7\n")

    (display (format "computation terminates: ~a\n"
                     (sqrt-v2 999999999999999999999999999999999999)))

    ;; due to loss of numerical precision sqrt-v1 might diverge with the above number
    (display (format "inaccurate: ~a\n" (* (sqrt-v1 tolerance) (sqrt-v1 tolerance))))
    (display (format "accurate  : ~a\n" (* (sqrt-v2 tolerance) (sqrt-v2 tolerance))))))

(module Exercise/1.8 sicp
  (#%require (only racket/base module+)
             (only (submod ".." common-utils) square tolerance))

  (define (cube-root x)
    (define (cbrt-recursive old-guess guess)
      (define (improve guess)
        (/ (+ (/ x (square guess)) (* 2 guess)) 3))
      (define (good-enough? old-guess guess)
        (< (abs (- old-guess guess)) tolerance))
      (if (good-enough? old-guess guess)
          guess
          (cbrt-recursive guess (improve guess))))
    (cbrt-recursive 0.0 1.0))

  (module+ test
    (#%require rackunit)
    (display "--> Exercise/1.8\n")

    (check-within (* (cube-root 9) (cube-root 9) (cube-root 9)) 9 tolerance)))

(module Section/1.2.1 sicp
  (#%provide factorial-v2)
  (#%require (only racket/base module+))

  #| linear recursive process
  1. the amount of information required to keep is proportional to n
  2. number of steps is linear in n
  3. actual implementation requires the use of an auxiliary date structure (a stack)
  |#
  (define (factorial-v1 n)
    (if (= n 1)
        1
        (* n (factorial-v1 (- n 1)))))

  (module+ test
    (#%require rackunit)
    (display "--> Section/1.2.1\n")

    (check-equal? (factorial-v1 5) 120))

  #| linear iterative process
  1. a fixed number of state variables is required
  2. number of steps depend linearly on n
  3. can be implemented on a machine with a fixed set of registers (w/o auxiliary memory)
  |#
  (define (factorial-v2 n)
    (define (factorial-helper counter product)
      (if (> counter n)
          product
          (factorial-helper
           (+ counter 1)
           (* product counter))))
    (factorial-helper 1 1))

  (module+ test
    (check-equal? (factorial-v2 5) 120))

  (define (factorial-v3 n)
    (define (factorial-helper acc n)
      (if (= n 1)
          acc
          (factorial-helper (* n acc) (- n 1))))
    (factorial-helper 1 n))

  (module+ test
    (check-equal? (factorial-v3 5) 120))

  #|
  Note the difference between:
  1. iterative process vs. iterative procedure
  2. recursive process vs. recursive procedure
  the linear iterative process above is implemented with a recursive procedure

  Some languages are able to implement in constant space an iterative process
  even if it is defined in terms of a recursive procedure. An implementation with
  this property is called tail-recursive. Many languages introduce iterative constructs
  in order to model iterative processes because they don't exploit tail-recursion.
  In languages that support tail recursion optimization, iterative constructs
  can be considered as sintactic sugar.
  |#)

(module Exercise/1.9 sicp
  (#%require (only racket/base module+))

  ;; O(a) in time
  ;; O(a) in space
  ;; linear recursion (because it is proportional to a in both time and space)
  (define (plus.rec a b)
    (if (= a 0)
        b
        (inc (plus.rec (dec a)
                       b))))

  (module+ test
    (#%require rackunit)
    (display "--> Exercise/1.9\n")

    (check-equal? (plus.rec 4 5) 9)
    #| recursive process
    (plus.rec 4 5)
    (inc (plus.rec 3 5))
    (inc (inc (plus.rec 2 5)))
    (int (inc (inc (plus.rec 1 5))))
    (int (int (inc (inc (plus.rec 0 5))))) ;; end expanding, start contracting
    (int (int (inc (inc 5))))
    (int (int (inc 6)))
    (int (int 7))
    (int 8)
    9
    |#)

  ;; O(a) in time
  ;; O(1) in space
  ;; linear iteration
  (define (plus.iter a b)
    (if (= a 0)
        b
        (plus.iter (dec a)
                   (inc b))))

  (module+ test
    (check-equal? (plus.iter 4 5) 9)
    #| iterative process (implemented using a recursive procedure)
    (plus.iter 4 5)
    (plus.iter 3 6)
    (plus.iter 2 7)
    (plus.iter 1 8)
    (plus.iter 0 9)
    9
    |#))

(module Exercise/1.10 sicp
  (#%require racket/trace
             (only racket/base module+ format))

  (module+ test
    (#%require rackunit)
    (display "--> Exercise/1.10\n")

    ;; I have to define A in the test module because I want to use (trace A) later
    (define (A x y)
      (cond ((= y 0) 0)
            ((= x 0) (* 2 y))
            ((= y 1) 2)
            (else (A (- x 1)
                     (A x (- y 1))))))

    (define (f n) (A 0 n))
    (define (g n) (A 1 n))
    (define (h n) (A 2 n))

    (display (format "(A 1 10): ~a\n" (A 1 10)))
    (display (format "(A 2 4): ~a\n" (A 2 4)))
    (display (format "(A 3 3): ~a\n" (A 3 3))))

  (module+ test
    #|
    (A 0 n)
    (* 2 n)
    f(n) = 2*n
    |#
    (let* ([x 4]
           [y (f x)])
      (display (format "(f ~a): ~a\n" x y))
      (check-equal? y (* 2 x))))

  (module+ test
    #|
    (A 1 3)
    (A 0 (A 1 2)) -> (f (g (- n 1)))
    (A 0 (A 0 (A 1 1)))
    (A 0 (A 0 2))
    (A 0 (* 2 2))
    (expt 2 n)
    g(n) = 2^n
    |#
    (let* ([n 4]
           [y (g n)])
      (display (format "(g ~a): ~a\n" n y))
      (check-equal? y (expt 2 n))
      (check-equal? y (f (g (- n 1))))))

  (module+ test
    #|
    (A 2 4)
    (A 1 (A 2 3)) -> (g (h (- n 1)))
    (A 1 (A 1 (A 2 2)))
    (A 1 (A 1 (A 1 (A 2 1))))
    (A 1 (A 1 (A 1 2)))
    (expt 2 (expt 2 (expt 2 2)))

    2^1
    2^2
    2^(2^2)
    2^(2^(2^2))
    h(n) = 2^h(n-1)
    |#
    (define (expt-recursive n)
      (if (= n 1)
          2
          (expt 2 (expt-recursive (- n 1)))))

    (let* ([n 4] ;; don't use more than n = 4
           [y (h n)])
      (display (format "(h ~a): ~a" n y))
      (newline)
      (check-equal? y (expt-recursive n))
      (check-equal? y (g (h (- n 1))))))

  (module+ test
    ;; we can generate the trace automatically
    (trace A)
    (A 1 5)
    (untrace A)
    (A 1 5)))

(module Section/1.2.2 sicp
  (#%provide count-change)
  (#%require (only racket/base module+))

  ;; ----------------------------------------------------------
  ;; Fibonacci sequence (tree recursion)
  ;; ----------------------------------------------------------
  (define (fib.v1 n)
    (cond [(= n 0) 0]
          [(= n 1) 1]
          [else (+ (fib.v1 (- n 1))
                   (fib.v1 (- n 2)))]))

  (module+ test
    (#%require rackunit)
    (display "--> Section/1.2.2\n")

    (check-equal? (fib.v1 10) 55))

  ;; ----------------------------------------------------------
  ;; Fibonacci sequence (iterative process)
  ;; ----------------------------------------------------------
  #|
  [n = 0] a1 = 0, a2 = 1
  [n = 1] a1 = 1, a2 = 1
  [n = 2] a1 = 1, a2 = 2
  [n = 3] a1 = 2, a2 = 3
  ...
  |#
  (define (fib.v2 n)
    (define (fib-iter a1 a2 n)
      (if (= n 0)
          a1
          (fib-iter a2 (+ a1 a2) (- n 1))))
    (fib-iter 0 1 n))

  (module+ test
    (check-equal? (fib.v2 10) 55))

  ;; ----------------------------------------------------------
  ;; Count change (tree recursion)
  ;; ----------------------------------------------------------
  (define (count-change amount coins)
    (cond [(= amount 0) 1]
          [(or (< amount 0)
               (= (length coins) 0)) 0]
          [else (+ (count-change amount (cdr coins))
                   (count-change (- amount (car coins)) coins))]))

  (module+ test
    (check-equal? (count-change 100 '(50 25 10 5 1)) 292)))

(module Exercise/1.11 sicp
  (#%require (only racket/base module+))

  ;; ----------------------------------------------------------
  ;; recursive process
  ;; ----------------------------------------------------------
  (define (f.v1 n)
    (if (< n 3)
        n
        (+ (f.v1 (- n 1))
           (* 2 (f.v1 (- n 2)))
           (* 3 (f.v1 (- n 3))))))

  (module+ test
    (#%require rackunit)
    (display "--> Exercise/1.11\n")

    (check-equal? (f.v1 10) 1892))

  ;; ----------------------------------------------------------
  ;; iterative process
  ;; ----------------------------------------------------------
  (define (f.v2 n)
    (define (f.iter a0 a1 a2 n)
      (if (= n 0)
          a0
          #|
          a2 <- 3*a0 + 2*a1 + a2
          a1 <- a2
          a0 <- a1
          |#
          (f.iter a1 a2 (+ (* 3 a0) (* 2 a1) a2) (- n 1))))
    (if (< n 3)
        n
        (f.iter 0 1 2 n)))

  (module+ test
    (check-equal? (f.v2 10) 1892)
    (check-equal? (f.v2 -2) -2)))

(module Exercise/1.12 sicp
  (#%require (only racket/base module+))

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

  (module+ test
    (#%require rackunit)
    (display "--> Exercise/1.12\n")

    (check-equal? (pascal-triangle.v1 0 #f) '())
    (check-equal? (pascal-triangle.v1 1 #f) '(1))
    (check-equal? (pascal-triangle.v1 2 #f) '(1 1))
    (check-equal? (pascal-triangle.v1 3 #f) '(1 2 1))
    (check-equal? (pascal-triangle.v1 4 #f) '(1 3 3 1))
    (check-equal? (pascal-triangle.v1 5 #f) '(1 4 6 4 1))
    (check-equal? (pascal-triangle.v1 6 #f) '(1 5 10 10 5 1)))

  (define (pascal-triangle.v2 row-numb col-numb)
    (if (or (= col-numb 1) (= col-numb row-numb))
        1
        (+ (pascal-triangle.v2 (- row-numb 1)
                               (- col-numb 1))
           (pascal-triangle.v2 (- row-numb 1)
                               col-numb))))

  (module+ test
    (check-equal? (list (pascal-triangle.v2 6 1)
                        (pascal-triangle.v2 6 2)
                        (pascal-triangle.v2 6 3)
                        (pascal-triangle.v2 6 4)
                        (pascal-triangle.v2 6 5)
                        (pascal-triangle.v2 6 6))
                  '(1 5 10 10 5 1))))

(module Exercise/1.13 sicp
  #| Solution:
  See latex note.
  |#)

(module Exercise/1.14 sicp
  (#%require (only (submod ".." Section/1.2.2) count-change)
             (only racket/base module+ format for in-range set!))

  (module+ test
    (#%require rackunit)
    (display "--> Exercise/1.14\n")

    (check-equal? (count-change 11 '(50 25 10 5 1)) 4))

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
             (+ (count-change-display-helper amount
                                             (cdr coins)
                                             new-offset)
                (count-change-display-helper (- amount (car coins))
                                             coins
                                             new-offset))]))

    (define numb-calls 0)
    (count-change-display-helper amount coins "")
    numb-calls)

  ;; (log n b) can be computed using log_b(n) = ln(n)/ln(b)
  ;; (define (logb n b) (/ (log n) (log b)))

  (module+ test
    (count-change-display 11 '(50 25 10 5 1) #t)
    (for ([coins '((1)
                   (1 1)
                   (1 1 1)
                   (1 1 1 1)
                   (1 1 1 1 1))])
      (display (format "========== ~a coin ==========\n" (length coins)))
      (for ([k '(5 10 50)])
        (let [(n (count-change-display k coins #f))]
          (display (format "~a: ~a (~a)\n" k n (log n k))))))

    #|
    Note: the number of iterations depends on the order of coins
    e.g., '(1 5) vs. '(5 1)

    1. The space is the number of stacks we have to keep, i.e., the depth of the
    recursion. In the worst case we would have to express the amount in terms of the
    smallest coin (which is 1) so the depth is proportional to the amount.

    2. Intuitively we should have O(amount^5). It seems to me that we can compute an
    upper bound for the number of steps by using '(1 1 1 1 1) as coins. Probably as we
    increase the amount, the power would get closer to 5 but it takes a lot of time.
    |#))

(module Exercise/1.15 sicp
  (#%require (only racket/base module+ format)
             (only (submod ".." common-utils) cube))

  (define (p x) (- (* 3 x) (* 4 (cube x))))
  (define (sine angle iter)
    (display (format "[~a] angle: ~a\n" iter angle))
    (if (not (> (abs angle) 0.1))
        angle
        (p (sine (/ angle 3.0) (+ iter 1)))))

  (module+ test
    (display "--> Exercise/1.15\n")

    #|
    At every call of sine, the angle is divided by 3 so the number of steps is
    determined by the condition: angle / 3^k <= 0.1, from which we get
    3^k = angle / 0.1, and hence log_3(angle / 0.1)
    |#
    (sine 12.15 0)
    (format "[angle: 12.15] iter: ~a\n" (ceiling (log (/ 12.15 0.1) 3)))
    #|
    in terms of space, we need one stack per invocation (note that this is not tail
    recursive due to the application of the function p)
    |#

    ;; test something else
    (sine 500.0 0)
    (format "[angle: 500.0] iter: ~a\n" (ceiling (log (/ 500.0 0.1) 3)))))

(module Exercise/1.16 sicp
  (#%require (only racket/base module+ format)
             (only (submod ".." common-utils) square))

  (define (show even-or-odd iter n b a)
    (display
     (format "(~a)[~a]: ~a = ~a*~a^~a\n"
             even-or-odd iter (* a (expt b n)) a b n)))

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
           (fast-expt-iterative.v1 (square b)
                                   (- (/ n 2) 1)
                                   (* a (square b))
                                   (+ iter 1))]
          [else
           (show "O" iter n b a)
           (fast-expt-iterative.v1 b
                                   (- n 1)
                                   (* a b)
                                   (+ iter 1))]))

  (define (fast-expt-iterative.v2 b n a iter)
    (cond [(= n 0) a]
          [(even? n)
           (show "E" iter n b a)
           (fast-expt-iterative.v2 (square b)
                                   (/ n 2)
                                   a
                                   (+ iter 1))]
          [else
           (show "O" iter n b a)
           (fast-expt-iterative.v2 b
                                   (- n 1)
                                   (* a b)
                                   (+ iter 1))]))

  (module+ test
    (#%require rackunit)
    (display "--> Exercise/1.16\n")

    (check-equal? (fast-expt-recursive 5 21 1) 476837158203125)
    (check-equal? (fast-expt-iterative.v1 5 21 1 1) 476837158203125)
    (check-equal? (fast-expt-iterative.v2 5 21 1 1) 476837158203125)))

(module Exercise/1.17 sicp
  (#%provide mult.v2)
  (#%require (only racket/base module+ format))

  (define (mult.v1 a b)
    (cond [(or (= a 0) (= b 0)) 0]
          [else (+ a (mult.v1 a (- b 1)))]))

  (module+ test
    (#%require rackunit)
    (display "--> Exercise/1.17\n")

    (check-equal? (mult.v1 5 7) 35))

  (define (show even-or-odd a b acc)
    (display
     (format "[~a]: ~a = ~a+~a*~a\n"
             even-or-odd (+ acc (* a b)) acc a b)))

  (define (mult.v2 a b acc verbose)
    (define (double x) (* 2 x))
    (define (halve x)
      (if (even? x)
          (/ x 2)
          (error "Cannot halve an odd integer.")))
    (cond [(or (= a 0) (= b 0)) acc]
          [(even? b)
           (if verbose (show "E" a b acc))
           (mult.v2 (double a) (halve b) acc verbose)]
          [else
           (if verbose (show "O" a b acc))
           (mult.v2 a (- b 1) (+ acc a) verbose)]))

  (module+ test
    (check-equal? (mult.v2 5 0 0 #f) 0)
    (check-equal? (mult.v2 5 1 0 #f) 5)
    ;; 5 * 9 = 5 + 5 * 8 = 5 + 10 * 4 = 5 + 20 * 2 = 5 + 40 * 1 = 45
    (check-equal? (mult.v2 5 9 0 #t) 45)
    (check-equal? (mult.v2 5 10 0 #f) 50)))

(module Exercise/1.18 sicp
  (#%require (only racket/base module+)
             (only (submod ".." Exercise/1.17) mult.v2))

  (module+ test
    (#%require rackunit)
    (display "--> Exercise/1.18\n")

    ;; I implemented Exercise 1.17 taking into account the conditions of Exercise 1.18
    (check-equal? (mult.v2 5 9 0 #t) 45)))

(module Exercise/1.19 sicp
  (#%provide fib)
  (#%require (only racket/base module+))

  (define (fib n)
    (fib-iter 1 0 0 1 n))

  (define (fib-iter a b p q count)
    (cond ((= count 0) b)
          ((even? count)
           (fib-iter a
                     b
                     (+ (* p p) (* q q)) ; compute p'
                     (+ (* q q) (* 2 p q)) ; compute q'
                     (/ count 2)))
          (else (fib-iter (+ (* b q) (* a q) (* a p))
                          (+ (* b p) (* a q))
                          p
                          q
                          (- count 1)))))

  (module+ test
    (#%require rackunit)
    (display "--> Exercise/1.19\n")

    (check-equal? (fib 10) 55)))

(module Exercise/1.20 sicp
  (#%require (only racket/base module+ format))

  (define (show a b)
    (display (format "[~a] a: ~a, b: ~a\n" (modulo a b) a b)))

  (define (gcd a b)
    (if (not (= b 0)) (show a b))
    (if (= b 0)
        a
        (gcd b (remainder a b))))

  (module+ test
    (#%require rackunit)
    (display "--> Exercise/1.20\n")

    (check-equal? (gcd 206 40) 2)
    #|
    applicative order: 4 evaluations (as there are 4 iterations)
    normal order:
    --------------------------------------------------------------
    (gcd 206 40)
    (if (= 40 0) ...)
    --------------------------------------------------------------
    (gcd 40 (remainder 206 40))
    (if (= (remainder 206 40) 0) ...)

    [1] => (if (= 6 0) ...)
    --------------------------------------------------------------
    (gcd (remainder 206 40)
         (remainder 40 (remainder 206 40)))
    (if (= (remainder 40 (remainder 206 40)) 0) ...)

    [2] => (if (= 4 0) ...)
    --------------------------------------------------------------
    (gcd (remainder 40 (remainder 206 40))
         (remainder (remainder 206 40)
                    (remainder 40 (remainder 206 40))))
    (if (= (remainder (remainder 206 40)
                      (remainder 40 (remainder 206 40))) 0) ...)

    [4] => (if (= 2 0) ...)
    --------------------------------------------------------------
    (gcd (remainder (remainder 206 40)
                    (remainder 40 (remainder 206 40)))
         (remainder (remainder 40 (remainder 206 40))
                    (remainder (remainder 206 40)
                               (remainder 40 (remainder 206 40)))))
    (if (= (remainder (remainder 40 (remainder 206 40))
                      (remainder (remainder 206 40)
                                 (remainder 40 (remainder 206 40)))) 0) ...)

    [7] => (if (= 0 0) ...)
    --------------------------------------------------------------
    (remainder (remainder 206 40)
               (remainder 40 (remainder 206 40))) [4] => DONE

    (+ 1 2 4 7 4) -> 18 evaluations of (remainder ...)
    |#))

(module Exercise/1.21 sicp
  (#%provide smallest-divisor)
  (#%require (only racket/base module+)
             (only (submod ".." common-utils) square))

  (define (smallest-divisor n)
    (define (find-divisor n test-divisor)
      (define (divides? a b) (= (remainder b a) 0))
      (cond ((> (square test-divisor) n) n)
            ((divides? test-divisor n) test-divisor)
            (else (find-divisor n (+ test-divisor 1)))))

    (find-divisor n 2))

  (module+ test
    (#%require rackunit)
    (display "--> Exercise/1.21\n")

    (check-equal? (smallest-divisor 199) 199)
    (check-equal? (smallest-divisor 1999) 1999)
    (check-equal? (smallest-divisor 19999) 7)))

(module Exercise/1.22 sicp
  (#%provide timed-prime-test
             prime-numbers
             prime?)
  (#%require (only racket/base module+ for)
             (only (submod ".." Exercise/1.21) smallest-divisor))

  ;; This detects 1 as a prime number (I choose to ignore this detail)
  (define (prime? n)
    (= n (smallest-divisor n)))

  (define (timed-prime-test n)
    (define (start-prime-test n start-time)
      (define (report-prime elapsed-time)
        (display " *** ")
        (display elapsed-time)
        #t)
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

  (module+ test
    (display "--> Exercise/1.22\n")

    (for ([n '(1e3 1e4 1e5 1e6 1e7)])
      (serch-for-primes n 3))

    #|
    ============================
    the times I observe:
    ============================
          1000: ~1
        10_000: ~3
       100_000: ~9
     1_000_000: ~28
    10_000_000: ~82
    ============================
    the factor is sqrt(10)
    ============================
    |#)

  ;; I have extracted them manually from the above results
  (define prime-numbers '(1009
                          1013
                          1019
                          10007
                          10009
                          10037
                          100003
                          100019
                          100043
                          1000003
                          1000033
                          1000037
                          10000019
                          10000079
                          10000103)))

(module Exercise/1.23 sicp
  (#%require (only racket/base module+ format for)
             (only (submod ".." common-utils) square)
             (only (submod ".." Exercise/1.22) prime-numbers prime?)
             (only (submod ".." common-utils) run-n-times))

  (define (smallest-divisor-optimized n)
    (define (find-divisor n test-divisor)
      (define (next numb)
        (if (= numb 2) 3 (+ numb 2)))
      (define (divides? a b) (= (remainder b a) 0))
      (cond ((> (square test-divisor) n) n)
            ((divides? test-divisor n) test-divisor)
            (else (find-divisor n (next test-divisor)))))
    (find-divisor n 2))

  (define (smallest-divisor-optimized-no-extra-function n)
    (define (find-divisor n test-divisor)
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

  (module+ test
    (display "--> Exercise/1.23\n")

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
        (newline))))

  ;; inlined version
  (module+ test
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
        (newline))))

  #|
  So I observe a speedup but less than twice (close to 1.6 times). We halve the
  number of iterations and I didn't expect that the extra function call would have
  such a signifficant influence (especially because "next" is super simple). In the
  second test, where "next" is inlined we see that the ratio is almost 2 for large
  prime numbers. This means that the function call itself (with its additional stack)
  is the reason. The additional if and equality comparison seem to influence mainly
  the time for small numbers.
  |#)

(module Exercise/1.24 sicp
  (#%provide expmod)
  (#%require (only racket/base module+ format for in-range)
             (only (submod ".." common-utils) square)
             (only (submod ".." Exercise/1.22) prime-numbers)
             (only (submod ".." common-utils) run-n-times))

  #|
  We could have used fast-expt-iterative.v2 to compute p = x^n and then find p%n.
  In this way we would have an iterative procedure but we would be dealing with huge
  numbers p. In the latex note I included a proof for the following two transformations
  as initially I thought that we need them:
    1. (x*y)%n = ((x%n)*(y%n))%n
    2. (x*y)%n = (x*(y%n))%n

  The reduction step for an even exponent e is:
  (x^e)%n = (x^{e/2} * x^{e/2})%n = ((x^{e/2})^2)%n

  The reduction step for an odd exponent e is:
  (x^e)%n = (x*x^{e-1})%n, where x^{e-1} is even ...
  |#
  (define (expmod base exp m)
    (cond ((= exp 0) 1)
          ((even? exp)
           (remainder
            (square (expmod base (/ exp 2) m))
            m))
          (else
           (remainder
            (* base (expmod base (- exp 1) m))
            m))))

  ;; implement as an iterative process
  (define (expmod-iter base exp m)
    (define (expmod-iter-helper base exp acc)
      (cond ((= exp 0) acc)
            ((even? exp)
             (expmod-iter-helper (remainder (square base) m) (/ exp 2) acc))
            (else
             (expmod-iter-helper base (- exp 1) (remainder (* acc base) m)))))
    (expmod-iter-helper base exp 1))

  ;; here n > 1 is assumed: (random 0) raises a contract violation
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

  (module+ test
    (#%require rackunit)
    (display "--> Exercise/1.24\n")

    (for ([x '(2 5 9 19 7 6 3 123 5 17)]
          [p '(3 3 7 4 6 4 7 456 100 5)]
          [m '(5 13 17 7 11 13 10 1234 9 19)])
      (let ([res1 (expmod x p m)]
            [res2 (expmod-iter x p m)])
        (display (format "~a, ~a, ~a: ~a\n" x p m res1))
        (check-equal? res1 res2)))

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

    #|
    Since the complexity is O(long(n)) I expect to see a linear increase in computation
    time for n = [10^3, 10^4, ...] - which is indeed the case. But the slope of the
    line is not as steep as I expected. In particular, I expected to have twice slower
    computations for primes around 1M compared to 1K. However, I observe a factor of
    ~1.6 instead of 2. I am not quite sure why.

    1. Since we perform the (try-it ...) with a random number the same number of times
    for 1K and 1M, in the former case this costs relatively more as it is distributed
    accross less iterations. To test this, I run with (try-it 1) instead of a random
    number (which of course makes no sense other than checking efficiency of
    computations) and I get time ~1.8, so indeed this has an impact.

    2. Another thing I noticed is that for 1K we have 6 iterations with odd powers,
    while for 1M we have only 7, so it is possible that the time for 1K is relatively
    high because of this:
      1K: (/ 6.0 16) => 0.38
      1M: (/ 7.0 27) => 0.26
    This might explain the remaining difference.
    |#))

(module Exercise/1.25 sicp
  (#%provide fast-expt)
  (#%require (only racket/base module+ format)
             (only (submod ".." common-utils) square))

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

  (module+ test
    (display "--> Exercise/1.25\n")

    (let ([x 2]
          [n 1000])
      (expmod x n n)
      (let ([z (fast-expt x n 1)])
        (display (format "~a\n" z))
        (remainder z n)))

    (define numb-digits (ceiling (* 1000 (log 2 10))))
    #|
    The largest number we have to deal with in expmod e.g., for 2^1000 is 817216
    while in fast-expt it is astronomical and has 302 digits
    so for larger powers we would have to store huge numbers (note that not all
    languages can handle such numbers like python and racket).
    See footnote 46 on page 68.
    |#))

(module Exercise/1.26 sicp
  (#%require (only racket/base module+ format for set!)
             (only (submod ".." common-utils) square))
  #|
  The version of Louis Reasoner has complexity O(log(2^n)) = O(n*log(2)) = O(n),
  note that the linear recursion becomes tree recursion with two branches at each step.
  The following test confirms this by counting number of iterations:
  |#
  (define (expmod-linear base exp m)
    (set! numb-calls (+ numb-calls 1))
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

  (module+ test
    (display "--> Exercise/1.26\n")

    (define n-to-test '(1e1 1e2 1e3 1e4 1e5 1e6))
    (display "-------------------------------------\n")
    (display "expmod-linear\n")
    (display "-------------------------------------\n")
    (for ([n n-to-test])
      (let ([z (test-f expmod-linear n)])
        (display (format "[~a] ~a (~a)\n" n z (/ z (log n 2))))))
    (display "-------------------------------------\n")
    (display "expmod-tree\n")
    (display "-------------------------------------\n")
    (for ([n n-to-test])
      (let ([z (test-f expmod-tree n)])
        (display (format "[~a] ~a (~a)\n" n z (/ z (* n 1.0))))))))

(module Exercise/1.27 sicp
  (#%provide carmichael-numbers)
  (#%require (only racket/base module+ format for)
             (only (submod ".." common-utils) square)
             (only (submod ".." Exercise/1.22) prime?)
             (only (submod ".." Exercise/1.24) expmod))

  (define carmichael-numbers '(561 1105 1729 2465 2821 6601))

  (define (fermat-test-exhaustive n)
    (define (try-it a)
      (cond [(= a n) #t]
            [(not (= (expmod a n n) a)) #f]
            [else (try-it (+ a 1))]))
    (try-it 2))

  (define (test-carmichael-numbers func numbers)
    (cond [(= (length numbers) 0) #t]
          [(not (func (car numbers))) #f]
          [else (test-carmichael-numbers func (cdr numbers))]))

  (module+ test
    (#%require rackunit)
    (display "--> Exercise/1.27\n")

    (check-true (test-carmichael-numbers fermat-test-exhaustive carmichael-numbers))
    (check-false (test-carmichael-numbers fermat-test-exhaustive
                                          (append carmichael-numbers (list 9))))

    ;; of course a real test gives false for each carmichael number
    (for ([n carmichael-numbers])
      (check-false (test-carmichael-numbers prime? (list n))))))


(module Exercise/1.28/byte-utils racket/base
  (#%provide poetry-prime
             friend-prime)

  ;; See my discord discussion:
  ;; discord.com/channels/571040468092321801/1018246159208288366/1317083898114605107

  ;; big-endian assumed
  (define (bytes-be->int xs)
    (for/fold ([acc 0])
              ([x (in-bytes xs)])
      (+ x (* acc 256))))

  (define (bytes-convert-encoding
           bytes-seq
           #:in-encoding [in-encoding "UTF-8"]
           #:out-encoding [out-encoding "UTF-16be"])
    (let-values ([(encoded-bytes _ status)
                  (bytes-convert
                   (bytes-open-converter in-encoding out-encoding)
                   bytes-seq)])
      (unless (eq? status 'complete)
        (raise "Conversion not complete."))
      encoded-bytes))

  (define (str->int text #:out-encoding [out-encoding "UTF-8"])
    (let ([text-as-bytes (string->bytes/utf-8 text)]
          [no-conversion (equal? out-encoding "UTF-8")])
      (bytes-be->int
       (if no-conversion
           text-as-bytes
           (bytes-convert-encoding text-as-bytes #:out-encoding out-encoding)))))

  ;; A part of the poem "Хайдути" (Христо Ботев) https://chitanka.info/text/3232I
  ;; The integer corresponding to its utf-16-be encoding is a prime number
  (define poetry-prime-text "Кой не знай Чавдар войвода,
кой не е слушал за него?
Чорбаджия ли изедник,
или турските сердари?
Овчар ли по планината,
или пък клети сюрмаси!")

  ;; In python we can use `int.from_bytes(our_string.encode("utf-16-be"))`
  (define poetry-prime (str->int poetry-prime-text #:out-encoding "UTF-16be"))
  (define friend-prime (str->int "Приятел" #:out-encoding "UTF-16be"))

  (module+ test
    (#%require rackunit)
    (display "--> Exercise/1.28/byte-utils\n")

    (check-equal? (str->int "Genius lasts longer than beauty" #:out-encoding "UTF-8")
                  126146194680531155813303888168799649264643087982681371548542387524568708217)
    (check-equal? (str->int "Приятел" #:out-encoding "UTF-8")
                  4231403313494534579515183733264571)))

(module Exercise/1.28 sicp
  (#%require (only racket/base module+ for)
             ;; I can use random-integer to generate huge random numbers
             ;; while I could use random to generate numbers only in [0, 4294967087]
             (only math random-integer)
             (only (submod ".." common-utils) square)
             (only (submod ".." Exercise/1.22) prime-numbers)
             (only (submod ".." Exercise/1.27) carmichael-numbers)
             (only (submod ".." Exercise/1.28/byte-utils) poetry-prime friend-prime))

  ;; see latex note for details
  (define (expmod-miller-rabin base exp m)
    (cond ((= exp 0) 1)
          ((even? exp)
           (let* ([u (expmod-miller-rabin base (/ exp 2) m)]
                  [v (remainder (square u) m)])
             ;; a nontrivial square root of 1 modulo m is a number that is
             ;; not equal to 1 or m - 1 whose square root is equal to 1 modulo m
             ;; (if we find such a nontrivial square root then m is surely not prime)
             (cond [(and
                     (not (= u 1))
                     (not (= u (- m 1)))
                     (= v 1)) 0]
                   [else v])))
          (else
           (remainder
            (* base (expmod-miller-rabin base (- exp 1) m))
            m))))

  (define (miller-rabin-test n)
    (define (try-it a)
      (= (expmod-miller-rabin a (- n 1) n) 1))
    (try-it (random-integer 2 (- n 2))))

  (define (fast-prime? n times)
    (cond ((= times 0) #t)
          ((miller-rabin-test n) (fast-prime? n (- times 1)))
          (else #f)))

  (module+ test
    (#%require rackunit)
    (display "--> Exercise/1.28\n")

    (for ([n carmichael-numbers])
      (check-false (fast-prime? n 100)))

    (for ([n prime-numbers])
      (check-true (fast-prime? n 100)))

    ;; I simply needed a nice prime number to test with ...
    (check-true (fast-prime? poetry-prime 100))
    (check-true (fast-prime? friend-prime 100))))

(module Lecture/1B sicp
  (#%require (only racket/base module+ format hash-set! hash-ref make-hash))

  ;; Hanoi towers problem
  (define (move-v1 n src dest extra)
    (cond [(= n 0) "DONE"]
          [else
           ;; 1. imagive we can move n-1 blocks from SRC to EXTRA
           (move-v1 (- n 1) src extra dest)
           ;; 2. then we move the one block that is left from SRC to DEST
           (display (format "~a -> ~a\n" src dest))
           ;; 3. and then we move all n-1 blocks from EXTRA to DEST
           (move-v1 (- n 1) extra dest src)]))

  #| writing it like this is more clear to me
                                S D E
  (move 3 S D E)     [0] STATE: 3 0 0
    (move 2 S E D)
      (move 1 S D E) [1] STATE: 2 1 0  S -> D
      (move 1 S E D) [2] STATE: 1 1 1  S -> E
      (move 1 D E S) [3] STATE: 1 0 2  D -> E
    (move 1 S D E)   [4] STATE: 0 1 2  S -> D
    (move 2 E D S)
      (move 1 E S D) [5] STATE: 1 1 1  E -> S
      (move 1 E D S) [6] STATE: 1 2 0  E -> D
      (move 1 S D E) [7] STATE: 0 3 0  S -> D
  |#
  (define (move-v2 n src dest extra)
    (cond [
           (= n 1) (display (format "~a -> ~a\n" src dest)) ; define an actual move
           ]
          [else
           (move-v2 (- n 1) src extra dest) ; move n-1 blocks from SRC to EXTRA
           (move-v2 1 src dest extra)       ; move remaining block from SRC to DEST
           (move-v2 (- n 1) extra dest src) ; move n-1 blocks from EXTRA to DEST
           ]))

  ;; adding offset for better visualization
  (define (move-v2-offset n src dest extra offset)
    (define (indent+ offset) (string-append offset "  "))
    (display (format
              (string-append "~a~a~a~a~a" (if (= n 1) " " "\n"))
              offset n src dest extra))
    (cond [(= n 1) (display (format "(~a -> ~a)\n" src dest))]
          [else (move-v2-offset (- n 1) src extra dest (indent+ offset))
                (move-v2-offset 1 src dest extra (indent+ offset))
                (move-v2-offset (- n 1) extra dest src (indent+ offset))]))

  ;; display count of blocks on each tower
  (define (move-v2-counts n src dest extra counts)
    (define (update-counts counts)
      (hash-set! counts src (- (hash-ref counts src) 1))
      (hash-set! counts dest (+ (hash-ref counts dest) 1)))
    (cond [(= n 1)
           (update-counts counts)
           (display (format "~a -> ~a (~a)\n" src dest counts))]
          [else (move-v2-counts (- n 1) src extra dest counts)
                (move-v2-counts 1 src dest extra counts)
                (move-v2-counts (- n 1) extra dest src counts)]))

  (module+ test
    (display "--> Lecture/1B\n")

    ;; (move-v1 4 "S" "D" "E")
    ;; (move-v2 4 "S" "D" "E")
    (move-v2-offset 4 "S" "D" "E" "")

    ;; For some reason I cannot use the (make-hash alists) form when I use SICP
    ;; Just like I had problems with sort in Exercise/1.3
    ;; (define counts (make-hash (list (cons "S" 4) (cons "D" 0) (cons "E" 0))))
    (define counts (make-hash))
    (hash-set! counts "S" 4) ; so I populate the dict manually
    (hash-set! counts "D" 0)
    (hash-set! counts "E" 0)
    (move-v2-counts 4 "S" "D" "E" counts)))

(module Exercise/1.29 sicp
  (#%require (only racket/base module+ λ format)
             (only (submod ".." common-utils) cube))

  (define (sum f next a b)
    (if (> a b)
        0
        (+ (f a)
           (sum f next (next a) b))))

  (define (integral f a b dx)
    (define (add-dx x)
      (+ x dx))
    (* (sum f add-dx (+ a (/ dx 2.0)) b)
       dx))

  ;; here I use a modified version of sum with a counter
  (define (simpson.v1 f a b n)
    (define (sum-k f next k a b)
      (if (> a b)
          0
          (+ ((f k) a)
             (sum-k f next (+ k 1) (next a) b))))
    (define h (/ (- b a) n))
    (define (g k)
      (cond [(or (= k 0)
                 (= k n)) f]
            [(even? k) (λ (x) (* 2.0 (f x)))]
            [else (λ (x) (* 4.0 (f x)))]))
    (* (sum-k g (λ (x) (+ x h)) 0 a b) (/ h 3.0)))

  (define (simpson.v2 f a b n)
    (define h (/ (- b a) n))
    ;; note that g is a function of k
    (define (g k)
      (cond [(or (= k 0)
                 (= k n)) (f (+ a (* k h)))]
            [(even? k) (* 2.0 (f (+ a (* k h))))]
            [else (* 4.0 (f (+ a (* k h))))]))
    (* (sum g (λ (k) (+ k 1)) 0 n) (/ h 3.0)))

  ;; grouping terms
  ;; y_0 + 4(y_1 + y_3 + ... + y_{n-1}) + 2(y_2 + y_4 + ... + y_{n-2}) + y_n
  ;; argument at k: a + k*h
  (define (simpson.v3 f a b n)
    (define h (/ (- b a) n))
    (define (+kh a k) (+ a (* k h))) ; how to use default values in sicp?
    (define (+2h a) (+kh a 2))
    (* (+ (f a)                                         ; 0
          (* 4.0 (sum f +2h (+kh a 1) (+kh a (- n 1)))) ; odd
          (* 2.0 (sum f +2h (+kh a 2) (+kh a (- n 2)))) ; even
          (f (+ a (+kh a n))))                          ; n (i.e. b)
       (/ h 3.0)))

  (module+ test
    (#%require rackunit)
    (display "--> Exercise/1.29\n")

    (check-equal? (sum cube (λ (x) (+ x 1)) 1 10) 3025)
    (check-equal? (sum (λ (x) x) (λ (x) (+ x 1)) 1 10) 55)
    (check-within (* 8 (sum (λ (x) (/ 1.0 (* x (+ x 2))))
                            (λ (x) (+ x 4))
                            1
                            1000)) 3.1396 1e-4)

    (display (format "[integral(0.01)  ] ~a\n" (integral cube 0 1 0.01)))
    (display (format "[integral(0.001) ] ~a\n" (integral cube 0 1 0.001)))
    (display (format "[simpson.v1(100) ] ~a\n" (simpson.v1 cube 0 1 100)))
    (display (format "[simpson.v1(1000)] ~a\n" (simpson.v1 cube 0 1 1000)))
    (display (format "[simpson.v2(100) ] ~a\n" (simpson.v2 cube 0 1 100)))
    (display (format "[simpson.v2(1000)] ~a\n" (simpson.v2 cube 0 1 1000)))
    (display (format "[simpson.v3(100) ] ~a\n" (simpson.v3 cube 0 1 100)))
    (display (format "[simpson.v3(1000)] ~a\n" (simpson.v3 cube 0 1 1000)))))

(module Exercise/1.30 sicp
  (#%require (only racket/base module+ λ)
             (only (submod ".." common-utils) cube))

  (define (sum f next a b)
    (define (iter a result)
      (if (> a b)
          result
          (iter (next a) (+ result (f a)))))
    (iter a 0))

  (module+ test
    (#%require rackunit)
    (display "--> Exercise/1.30\n")

    (check-equal? (sum cube (λ (x) (+ x 1)) 1 10) 3025)
    (check-equal? (sum (λ (x) x) (λ (x) (+ x 1)) 1 10) 55)
    (check-within (* 8 (sum (λ (x) (/ 1.0 (* x (+ x 2))))
                            (λ (x) (+ x 4))
                            1
                            1000)) 3.1396 1e-4)))

(module Exercise/1.31 sicp
  (#%require (only racket/base module+ λ)
             (only (submod ".." common-utils) square)
             (only (submod ".." Section/1.2.1) factorial-v2))

  (define (product-recursion f next a b)
    (if (> a b)
        1
        (* (f a)
           (product-recursion f next (next a) b))))

  (define (product-iter f next a b)
    (define (iter a result)
      (if (> a b)
          result
          (iter (next a) (* result (f a)))))
    (iter a 1))

  (module+ test
    (#%require rackunit)
    (display "--> Exercise/1.31\n")

    (define (I x) x)
    (define (incr x) (+ x 1))
    (let* ([n 10]
           [z (factorial-v2 n)])
      (check-equal? (product-recursion I incr 1 n) z)
      (check-equal? (product-iter I incr 1 n) z))

    (define (ratio-square x) (square (/ x (- x 1))))
    (define (final-term x) (/ x (- x 1) (- x 1)))
    (define (pi-approx n)
      (define (approx m) ; m has to be even (verified below)
        (* (* 2.0 4.0 (final-term (+ m 2)))
           (product-iter ratio-square (λ (x) (+ x 2)) 4.0 m)))
      (approx (if (even? n)
                  n
                  (+ n 1))))

    ;; for n = 8: pi/4 = 2*((4/3)^2 * (6/5)^2 * (8/7)^2) * 10/9^2
    (define ref-result (* 4.0
                          2.0
                          (final-term 10.0)
                          (ratio-square 4.0)
                          (ratio-square 6.0)
                          (ratio-square 8.0)))
    (check-within (pi-approx 8) ref-result 1e-15)
    (check-within (pi-approx 1e7) 3.14159 1e-4)))

(module Exercise/1.32 sicp
  (#%require (only racket/base module+ λ)
             (only (submod ".." common-utils) cube)
             (only (submod ".." Section/1.2.1) factorial-v2))

  (define (accumulate-recursion combiner null-value f next a b)
    (if (> a b)
        null-value
        (combiner (f a)
                  (accumulate-recursion combiner
                                        null-value
                                        f
                                        next
                                        (next a)
                                        b))))

  (define (accumulate-iter combiner null-value f next a b)
    (define (iter a result)
      (if (> a b)
          result
          (iter (next a) (combiner result (f a)))))
    (iter a null-value))

  (module+ test
    (#%require rackunit)
    (display "--> Exercise/1.32\n")

    (check-equal? (accumulate-recursion + 0 cube (λ (x) (+ x 1)) 1 10) 3025)
    (check-equal? (accumulate-iter + 0 cube (λ (x) (+ x 1)) 1 10) 3025)
    (check-equal? (accumulate-recursion + 0 (λ (x) x) (λ (x) (+ x 1)) 1 10) 55)
    (check-equal? (accumulate-iter + 0 (λ (x) x) (λ (x) (+ x 1)) 1 10) 55)

    (define (I x) x)
    (define (incr x) (+ x 1))
    (let* ([n 10]
           [z (factorial-v2 n)])
      (check-equal? (accumulate-recursion * 1 I incr 1 n) z)
      (check-equal? (accumulate-iter * 1 I incr 1 n) z))))

(module Exercise/1.33 racket/base ; I want to use foldl
  (#%require (only (submod ".." common-utils) square)
             (only (submod ".." Exercise/1.22) prime?))

  (define (filter-accumulate predicate combiner null-value f next a b)
    (if (> a b)
        null-value
        (combiner (if (predicate a) (f a) null-value)
                  (filter-accumulate predicate
                                     combiner
                                     null-value
                                     f
                                     next
                                     (next a)
                                     b))))

  (module+ test
    (#%require rackunit
               (only racket/list range))
    (display "--> Exercise/1.33\n")

    ;; Note: (prime? 1) -> #t
    (check-equal?
     (filter-accumulate prime? + 0 square (λ (x) (+ x 1)) 1 20)
     (foldl + 0 (map square '(1 2 3 5 7 11 13 17 19))))

    (define (solve-assignment-b n)
      (define (relatively-prime? i)
        ;; gcd is from racket/base
        (= (gcd i n) 1))
      (filter-accumulate relatively-prime? * 1
                         (λ (x) x)
                         (λ (x) (+ x 1)) 1 (- n 1)))

    (let ([n 16])
      (check-equal? (solve-assignment-b n)
                    (foldl * 1 (filter (λ (i) (= (gcd i n) 1))
                                       (range 1 n)))))))

(module Exercise/1.34 sicp
  (#%require (only racket/base module+ λ exn:fail?))

  ;; (f f) -> (f 2) -> (2 2) ERROR: 2 is not a procedure
  (define (f g) (g 2))

  (module+ test
    (#%require rackunit)
    (display "--> Exercise/1.34\n")

    ;; expect to raise
    (check-exn
     exn:fail?
     (λ () (f f)))

    ;; we could match the error message
    (check-exn
     #rx"application: not a procedure.*given: 2"
     (λ () (f f)))))

(module Section/1.3.3 sicp
  (#%provide fixed-point fixed-point-iter)
  (#%require (only racket/base module+ λ format)
             (only (submod ".." common-utils) average tolerance))

  (define (close-enough? x y) (< (abs (- x y)) 0.001))

  (define (search f neg-point pos-point)
    (let ((midpoint (average neg-point pos-point)))
      (if (close-enough? neg-point pos-point)
          midpoint
          (let ((test-value (f midpoint)))
            (cond ((positive? test-value)
                   (search f neg-point midpoint))
                  ((negative? test-value)
                   (search f midpoint pos-point))
                  (else midpoint))))))

  (define (half-interval-method f a b)
    (let ((a-value (f a))
          (b-value (f b)))
      (cond ((and (negative? a-value) (positive? b-value))
             (search f a b))
            ((and (negative? b-value) (positive? a-value))
             (search f b a))
            (else
             (error "Values are not of opposite sign" a b)))))

  (module+ test
    (#%require rackunit)
    (display "--> Section/1.3.3\n")

    (check-within (half-interval-method sin 2.0 4.0)
                  3.14111328125
                  1e-4)
    (check-within (half-interval-method (λ (x) (- (* x x x) (* 2 x) 3)) 1.0 2.0)
                  1.89306640625
                  1e-4))

  (define (fixed-point-iter f first-guess max-iter verbose)
    (define (close-enough? v1 v2)
      (< (abs (- v1 v2))
         tolerance))
    (define (try guess iter)
      (if verbose (display (format "[~a] ~a\n" iter guess)))
      (let ((next (f guess)))
        (if (or (close-enough? guess next) (= iter max-iter))
            (list next iter)
            (try next (+ iter 1)))))
    (try first-guess 0))

  (define (fixed-point f first-guess max-iter)
    (list-ref (fixed-point-iter f first-guess max-iter #t) 0))

  (module+ test
    (check-within (fixed-point cos 1.0 100)
                  0.7390822985224023
                  1e-4)
    (check-within (fixed-point (λ (y) (+ (sin y) (cos y))) 1.0 100)
                  1.2587315962971173
                  1e-4)


    (let ([x 4])
      (fixed-point (λ (y) (/ x y)) 1.0 5) ;; verify infinite oscilation
      ;; y -> x / y
      ;; y + y -> y + x / y
      ;; 0.5*(y + y) -> 0.5*(y + x / y)
      ;; y -> average(y, x / y)
      (check-within (fixed-point (λ (y) (average y (/ x y))) 1.0 100) 2 1e-4))))

(module Exercise/1.35 sicp
  (#%require (only racket/base module+ λ)
             (only (submod ".." common-utils) golden-ratio)
             (only (submod ".." Section/1.3.3) fixed-point))

  (module+ test
    (#%require rackunit)
    (display "--> Exercise/1.35\n")

    #| Solution
    Using the characteritic equation x^2 - x -1 = 0 (see latex note Exercise 1.13),
    we get x = 1 + 1/x
    |#

    (check-within (fixed-point (λ (x) (+ 1 (/ 1.0 x))) 1.0 100) golden-ratio 1e-4)))

(module Exercise/1.36 sicp
  (#%require (only racket/base module+ λ)
             (only (submod ".." common-utils) average)
             ;; I already defined fixed-point to display its iterates
             (only (submod ".." Section/1.3.3) fixed-point))

  (module+ test
    (#%require rackunit)
    (display "--> Exercise/1.36\n")

    ;; original fixed-point problem: 33 iterations
    ;; fixed-point problem with damping: 8 iterations
    (let* ([y 1000]
           [x1 (fixed-point (λ (x) (log y x)) 2.0 100)]
           [x2 (fixed-point (λ (x) (average x (log y x))) 2.0 100)])
      (check-within x1 (log y x1) 1e-4)
      (check-within x2 (log y x2) 1e-4)
      (check-within x1 x2 1e-4))))

(module Exercise/1.37 sicp
  (#%provide cont-frac-rec)
  (#%require (only racket/base module+ λ)
             (only (submod ".." common-utils) golden-ratio))

  (define (cont-frac-rec n d k)
    (define (helper-recursion i)
      (if (= i k)
          0
          (/ (n i)
             (+ (d i) (helper-recursion (+ i 1))))))
    (helper-recursion 0))

  (define (cont-frac-iter n d k)
    (define (helper-iteration i acc)
      (if (= i 0)
          acc
          (helper-iteration (- i 1) (/ (n i) (+ (d i) acc)))))
    (helper-iteration (- k 1) (/ (n k) (d k))))

  (module+ test
    (#%require rackunit)
    (display "--> Exercise/1.37\n")

    (let ([inverse-golden-ratio (/ 1 golden-ratio)]
          [tolerance 1e-4])
      (check-within (cont-frac-rec (λ (i) 1.0)
                                   (λ (i) 1.0)
                                   10)
                    inverse-golden-ratio
                    tolerance)

      (check-within (cont-frac-iter (λ (i) 1.0)
                                    (λ (i) 1.0)
                                    10)
                    inverse-golden-ratio
                    tolerance))))

(module Exercise/1.38 sicp
  (#%require (only racket/base module+ λ)
             (only (submod ".." Exercise/1.37) cont-frac-rec)
             (only (submod ".." common-utils) tolerance))

  (module+ test
    (#%require rackunit)
    (display "--> Exercise/1.38\n")

    #|
    [0] 0 1 2
    [1] 3 4 5
    [2] 6 7 8
    [3] 9 ...

    k-th column: i%3 == k
    |#
    (check-within (cont-frac-rec
                   (λ (i) 1.0)
                   (λ (i)
                     (cond [(or (= (remainder i 3) 0)
                                (= (remainder i 3) 2)) 1]
                           [(= (remainder i 3) 1)
                            (let ([iteration (/ (- i 1) 3)])
                              (+ 2 (* 2 iteration)))]
                           [else (error "Shouldn't be here.")]))
                   100)
                  (- (exp 1) 2)
                  tolerance)))

(module Exercise/1.39 sicp
  (#%require (only racket/base module+ λ)
             (only (submod ".." common-utils) square tolerance)
             (only (submod ".." Exercise/1.37) cont-frac-rec))

  (define (tan-cf x k)
    (cont-frac-rec
     (λ (i) (if (= i 0) x (- (square x))))
     (λ (i) (+ (* 2 i) 1))
     k))

  (module+ test
    (#%require rackunit)
    (display "--> Exercise/1.39\n")

    (let ([x 0.2])
      (check-within (tan-cf x 100) (tan x) tolerance))))

(module Section/1.3.4 sicp
  (#%provide average-damp newtons-method)
  (#%require (only racket/base module+ λ)
             (only (submod ".." common-utils) average square cube tolerance)
             (only (submod ".." Section/1.3.3) fixed-point))

  (define (average-damp f)
    (λ (x) (average x (f x))))

  (define (sqrt-v1 x)
    (fixed-point (average-damp (λ (y) (/ x y)))
                 1.0 100))

  (define (cube-root x)
    (fixed-point (average-damp (λ (y) (/ x (square y))))
                 1.0 100))

  (module+ test
    (#%require rackunit)
    (display "--> Section/1.3.4\n")

    (check-equal? ((average-damp square) 10) 55.0)
    (check-within (sqrt-v1 4) 2.0 tolerance)
    (check-within (cube-root 8) 2.0 tolerance)
    (check-within ((deriv cube) 5) 75.0 1e-3))

  (define (deriv g)
    (define dx tolerance)
    (λ (x) (/ (- (g (+ x dx)) (g x)) dx)))

  (define (newton-transform g)
    (λ (x) (- x (/ (g x) ((deriv g) x)))))

  (define (newtons-method g guess max-iter)
    (fixed-point (newton-transform g) guess max-iter))

  (define (sqrt-v2 x)
    (newtons-method
     (λ (y) (- (square y) x)) 1.0 100))

  (module+ test
    (check-within (sqrt-v2 4) 2.0 tolerance))

  (define (fixed-point-of-transform g transform guess)
    (fixed-point (transform g) guess 100))

  (define (sqrt-v3 x)
    (fixed-point-of-transform
     (λ (y) (/ x y)) average-damp 1.0))

  (define (sqrt-v4 x)
    (fixed-point-of-transform
     (λ (y) (- (square y) x)) newton-transform 1.0))

  (module+ test
    (check-within (sqrt-v3 4) 2.0 tolerance)
    (check-within (sqrt-v4 4) 2.0 tolerance)))

(module Exercise/1.40 sicp
  (#%require (only racket/base module+ λ)
             (only (submod ".." common-utils) square cube tolerance)
             (only (submod ".." Section/1.3.4) newtons-method))

  (define (cubic a b c)
    (λ (x)
      (+ (cube x)
         (* a (square x))
         (* b x)
         c)))

  (module+ test
    (#%require rackunit)
    (display "--> Exercise/1.40\n")

    (let ([a 1]
          [b 2]
          [c 3])
      (check-within ((cubic a b c)
                     (newtons-method (cubic a b c) 1 100))
                    0
                    tolerance))))

(module Exercise/1.41 sicp
  (#%require (only racket/base module+ λ))

  (define (double g)
    (λ (x) (g (g x))))

  (module+ test
    (#%require rackunit)
    (display "--> Exercise/1.41\n")

    (check-equal? (((double (double double)) inc) 5) 21)))

(module Exercise/1.42 sicp
  (#%provide compose)
  (#%require (only racket/base module+ λ)
             (only (submod ".." common-utils) square))

  (define (compose f g)
    (λ (x) (f (g x))))

  (module+ test
    (#%require rackunit)
    (display "--> Exercise/1.42\n")

    (check-equal? ((compose square inc) 6) 49)))

(module Exercise/1.43 sicp
  (#%provide repeated)
  (#%require (only racket/base module+)
             (only (submod ".." common-utils) square)
             (only (submod ".." Exercise/1.42) compose))

  (define (repeated f n)
    (if (= n 1)
        f
        (compose f (repeated f (- n 1)))))

  (module+ test
    (#%require rackunit)
    (display "--> Exercise/1.43\n")

    (check-equal? ((repeated square 1) 5) 25)
    (check-equal? ((repeated square 2) 5) 625)))

(module Exercise/1.44 sicp
  (#%require (only racket/base module+ λ)
             (only (submod ".." common-utils) square tolerance)
             (only (submod ".." Exercise/1.43) repeated))

  (define (smooth f dx)
    (λ (x)
      (+ (f (- x dx))
         (f x)
         (f (+ x dx)))))

  (define (smooth-n n f dx)
    (repeated (smooth f dx) n))

  (module+ test
    (#%require rackunit)
    (display "--> Exercise/1.44\n")

    (check-within ((smooth square 0.1) 2) 12.02 tolerance)
    (check-within ((smooth-n 2 square 0.1) 2) 433.4612 tolerance)))

(module Exercise/1.45 sicp
  (#%require (only racket/base module+ λ format for in-range)
             (only (submod ".." Exercise/1.25) fast-expt)
             (only (submod ".." Section/1.3.3) fixed-point-iter)
             (only (submod ".." Section/1.3.4) average-damp)
             (only (submod ".." Exercise/1.43) repeated))

  (define (pow b n)
    (fast-expt b n 1))

  (define (nth-root-fixed-point-damp x n numb-damp max-iter)
    (let ([res (fixed-point-iter ((repeated average-damp numb-damp)
                                  (λ (y) (/ x (pow y (- n 1)))))
                                 1.0
                                 max-iter
                                 #f)])
      (display (format "Computing ~a-th root (damping: ~a) of ~a: ~a\n"
                       n
                       numb-damp
                       x
                       res))
      res))

  (module+ test
    (#%require rackunit)
    (display "--> Exercise/1.45\n")

    #|
    When using (floor (log n 2)) damping steps, the computation does not cycle
    (at least for n <= 64). But note that, in general, this is not the number of damping
    steps that minimizes the number of iterations taken by the procedure. The number of
    iterations depends of course on the x and x0 we use.
    |#
    (for ([n (in-range 2 65)])
      (let* ([x 2]
             [numb-damp (floor (log n 2))]
             [max-iter 300])
        (check-true
         (< (list-ref (nth-root-fixed-point-damp 16 n numb-damp max-iter) 1)
            max-iter))))))

(module Exercise/1.46 sicp
  (#%require (only racket/base module+ λ)
             (only (submod ".." common-utils) square average tolerance))

  (define (iterative-improve good-enough? improve)
    (define (iter guess)
      (let ([next-guess (improve guess)])
        (if (good-enough? guess next-guess)
            next-guess
            (iter next-guess))))
    iter)

  (define (sqrt-iterative-improve-v1 x)
    (iterative-improve (λ (guess next-guess)
                         ;; next-guess is not used intentionally
                         (< (abs (- (square guess) x)) tolerance))
                       (λ (guess)
                         (average guess (/ x guess)))))

  (define (sqrt-iterative-improve-v2 x)
    (iterative-improve (λ (guess next-guess)
                         (< (abs (- guess next-guess))
                            tolerance))
                       (λ (guess)
                         (average guess (/ x guess)))))

  (define (fixed-point-iterative-improve f)
    (iterative-improve (λ (guess next-guess)
                         (< (abs (- guess next-guess))
                            tolerance))
                       (λ (guess)
                         (f guess))))

  (module+ test
    (#%require rackunit)
    (display "--> Exercise/1.46\n")

    (let ([x 4]
          [x0 1]
          [solution 2])
      (check-within ((sqrt-iterative-improve-v1 x) x0) solution tolerance)
      (check-within ((sqrt-iterative-improve-v2 x) x0) solution tolerance)
      ;; implement sqrt in terms of fixed-point-iterative-improve
      (check-within ((fixed-point-iterative-improve (λ (y) (average y (/ x y))))
                     x0)
                    solution tolerance))))

(module+ test
  (require (submod ".." Exercise/1.1 test)
           (submod ".." Exercise/1.2 test)
           (submod ".." Exercise/1.3 test)
           (submod ".." Exercise/1.3 test-in-racket)
           ;; 1.4: no tests
           ;; 1.5: no tests
           (submod ".." Section/1.1.7 test)
           ;; 1.6: no tests
           (submod ".." Exercise/1.7 test)
           (submod ".." Exercise/1.8 test)
           (submod ".." Exercise/1.9 test)
           (submod ".." Exercise/1.10 test)
           (submod ".." Exercise/1.11 test)
           (submod ".." Exercise/1.12 test)
           ;; 1.13: no tests
           (submod ".." Exercise/1.14 test)
           (submod ".." Exercise/1.15 test)
           (submod ".." Exercise/1.16 test)
           (submod ".." Exercise/1.17 test)
           (submod ".." Exercise/1.18 test)
           (submod ".." Exercise/1.19 test)
           (submod ".." Exercise/1.20 test)
           (submod ".." Exercise/1.21 test)
           (submod ".." Exercise/1.22 test)
           (submod ".." Exercise/1.23 test)
           (submod ".." Exercise/1.24 test)
           (submod ".." Exercise/1.25 test)
           (submod ".." Exercise/1.26 test)
           (submod ".." Exercise/1.27 test)
           (submod ".." Exercise/1.28 test)
           (submod ".." Exercise/1.28/byte-utils test)
           (submod ".." Lecture/1B test)
           (submod ".." Exercise/1.29 test)
           (submod ".." Exercise/1.30 test)
           (submod ".." Exercise/1.31 test)
           (submod ".." Exercise/1.32 test)
           (submod ".." Exercise/1.33 test)
           (submod ".." Exercise/1.34 test)
           (submod ".." Exercise/1.35 test)
           (submod ".." Exercise/1.36 test)
           (submod ".." Exercise/1.37 test)
           (submod ".." Exercise/1.38 test)
           (submod ".." Exercise/1.39 test)
           (submod ".." Section/1.3.4 test)
           (submod ".." Exercise/1.40 test)
           (submod ".." Exercise/1.41 test)
           (submod ".." Exercise/1.42 test)
           (submod ".." Exercise/1.43 test)
           (submod ".." Exercise/1.44 test)
           (submod ".." Exercise/1.45 test)
           (submod ".." Exercise/1.46 test)))
