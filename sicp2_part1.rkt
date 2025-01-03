;; =====================================================================================
;; Exercises in Chapter 2 (part 1)
;; =====================================================================================
#lang racket/base

(module Exercise/2.1 sicp
  (#%provide make-rat
             numer
             denom
             add-rat
             sub-rat
             mul-rat
             div-rat
             equal-rat?)
  (#%require (only racket/base module+ λ format let-values exn:fail?))

  ;; there are simpler solutions but I wanted to learn how to use let-values
  (define (make-rat n d)
    (cond [(= d 0) (error "Not a number")]
          [(= n 0) (cons n d)]
          [else (let-values ([(n-normalized d-normalized)
                              (cond [(or (and (> n 0) (> d 0))
                                         (and (< n 0) (> d 0)))
                                     (values n d)]
                                    [else
                                     (values (- n) (- d))])])
                  (let ((g (gcd (abs n) (abs d))))
                    (cons (/ n-normalized g)
                          (/ d-normalized g))))]))

  (define (numer x) (car x))
  (define (denom x) (cdr x))

  (define (add-rat x y)
    (make-rat (+ (* (numer x) (denom y))
                 (* (numer y) (denom x)))
              (* (denom x) (denom y))))

  (define (sub-rat x y)
    (make-rat (- (* (numer x) (denom y))
                 (* (numer y) (denom x)))
              (* (denom x) (denom y))))

  (define (mul-rat x y)
    (make-rat (* (numer x) (numer y))
              (* (denom x) (denom y))))

  (define (div-rat x y)
    (make-rat (* (numer x) (denom y))
              (* (denom x) (numer y))))

  (define (equal-rat? x y)
    (= (* (numer x) (denom y))
       (* (numer y) (denom x))))

  (define (print-rat x)
    (display (format "~a/~a\n" (numer x) (denom x))))

  (module+ test
    (#%require rackunit)
    (display "--> Exercise/2.1\n")

    (let* ([one-third (make-rat 1 3)]
           [res (add-rat one-third one-third)])
      (check-true (equal-rat? res (make-rat 2 3)))
      (print-rat res))

    (let* ([neg-neg (make-rat -1 -3)]
           [neg-pos (make-rat -1  3)]
           [pos-neg (make-rat  1 -3)]
           [neg-neg (make-rat -1 -3)]
           [zero-something (make-rat 0 1)])
      (check-equal? (numer neg-neg)  1)
      (check-equal? (denom neg-neg)  3)
      (check-equal? (numer neg-pos) -1)
      (check-equal? (denom neg-pos)  3)
      (check-equal? (numer pos-neg) -1)
      (check-equal? (denom pos-neg)  3)
      (check-equal? (numer neg-neg)  1)
      (check-equal? (denom neg-neg)  3)
      (check-equal? (numer zero-something) 0)
      (check-equal? (denom zero-something) 1))

    (check-exn
     exn:fail?
     (λ () (make-rat 1 0)))))

(module Exercise/2.2 sicp
  (#%provide make-point
             x-point
             y-point
             make-segment
             start-segment
             end-segment)
  (#%require (only racket/base module+ format)
             (only (submod "sicp1.rkt" common-utils) average tolerance))

  (define (make-segment point-a point-b) (cons point-a point-b))
  (define (start-segment segment) (car segment))
  (define (end-segment segment) (cdr segment))

  (define (make-point x y) (cons x y))
  (define (x-point point) (car point))
  (define (y-point point) (cdr point))

  (define (midpoint-segment segment)
    (let ([point-a (start-segment segment)]
          [point-b (end-segment segment)])
      (make-point
       (average (x-point point-a) (x-point point-b))
       (average (y-point point-a) (y-point point-b)))))

  (define (print-point point)
    (display (format "(~a, ~a)\n" (x-point point) (y-point point))))

  (module+ test
    (#%require rackunit)
    (display "--> Exercise/2.2\n")

    (let ([point (midpoint-segment (make-segment (make-point 1 2)
                                                 (make-point 3 4)))])
      (check-equal? (x-point point) 2.0)
      (check-equal? (y-point point) 3.0)
      (print-point point))))

(module Exercise/2.3 sicp
  (#%require (only racket/base module+ format)
             (only racket/math pi)
             (only (submod "sicp1.rkt" common-utils) square tolerance)
             (only (submod ".." Exercise/2.2)
                   make-point x-point y-point
                   make-segment start-segment end-segment))

  (define (rot-point point angle-rad)
    (let ([x (x-point point)]
          [y (y-point point)]
          [s (sin angle-rad)]
          [c (cos angle-rad)])
      (make-point (- (* c x)
                     (* s y))
                  (+ (* s x)
                     (* c y)))))

  (define (add-points point-a point-b)
    (make-point (+ (x-point point-a) (x-point point-b))
                (+ (y-point point-a) (y-point point-b))))

  (define (sub-points point-a point-b)
    (make-point (- (x-point point-a) (x-point point-b))
                (- (y-point point-a) (y-point point-b))))

  (define (segment-lenght segment)
    (let ([vect (sub-points (start-segment segment)
                            (end-segment segment))])
      (sqrt (+ (square (x-point vect))
               (square (y-point vect))))))

  #| NOTE:
  rectangle-area and rectangle-perimeter take procedures rectangle-width and
  rectangle-height as arguments because I want to use the same procedures for computing
  the area and perimeter with the different rectangle representations. An alternative
  would be to use parameters: https://docs.racket-lang.org/guide/parameterize.html
  Of course, all this is related to how to organize the tests and not to the exercise.
  |#
  (define (rectangle-area rectangle rectangle-width rectangle-height)
    (let ([w (rectangle-width rectangle)]
          [h (rectangle-height rectangle)])
      (* w h)))

  (define (rectangle-perimeter rectangle rectangle-width rectangle-height)
    (let ([w (rectangle-width rectangle)]
          [h (rectangle-height rectangle)])
      (* 2 (+ w h))))

  (module+ test
    (#%require rackunit)
    (display "--> Exercise/2.3\n")

    (let* ([a (make-point 1 0)]
           [b (add-points a (rot-point a (/ pi 2)))]
           [segment (make-segment a b)])
      (check-within (x-point b) 1.0 tolerance)
      (check-within (y-point b) 1.0 tolerance)
      (check-within (segment-lenght segment) 1.0 tolerance)))

  (define width 2)
  (define height 3)
  (define angle-rad (/ pi 4))
  (define offset (make-point 1 4))
  (define area (* width height))
  (define perimeter (* 2 (+ width height)))

  (module+ test-representation-1
    (#%require rackunit)
    (display "--> Exercise/2.3 (representation-1)\n")

    ;; --------------------------------------
    ;; representation using two segments
    ;; --------------------------------------
    (define (make-rectangle width height point angle-rad)
      (let ([point-lower-left (rot-point (make-point 0 0) angle-rad)]
            [point-lower-right (rot-point (make-point width 0) angle-rad)]
            [point-upper-left (rot-point (make-point 0 height) angle-rad)])
        (cons (make-segment (add-points point point-lower-left)
                            (add-points point point-lower-right))
              (make-segment (add-points point point-lower-left)
                            (add-points point point-upper-left)))))

    (define (rectangle-width rectangle)
      (segment-lenght (car rectangle)))

    (define (rectangle-height rectangle)
      (segment-lenght (cdr rectangle)))

    (let ([rectangle (make-rectangle width height offset angle-rad)])
      (check-within (rectangle-width rectangle) width tolerance)
      (check-within (rectangle-height rectangle) height tolerance)
      (check-within (rectangle-area rectangle rectangle-width rectangle-height)
                    area tolerance)
      (check-within (rectangle-perimeter rectangle rectangle-width rectangle-height)
                    perimeter tolerance)))

  (module+ test-representation-2
    (#%require rackunit)
    (display "--> Exercise/2.3 (representation-2)\n")

    ;; --------------------------------------
    ;; representation using three points
    ;; --------------------------------------
    ;; the point of the exercise is for the second representation to require different
    ;; selectors (and this is what I achieve by storing the points differently -
    ;; internally a list is implemented using cons)
    (define (make-rectangle width height point angle-rad)
      (list (rot-point (make-point 0 0) angle-rad)
            (rot-point (make-point width 0) angle-rad)
            (rot-point (make-point 0 height) angle-rad)))

    (define (rectangle-width rectangle)
      (segment-lenght (make-segment (car rectangle)
                                    (car (cdr rectangle)))))

    (define (rectangle-height rectangle)
      (segment-lenght (make-segment (car rectangle)
                                    (car (cdr (cdr rectangle))))))

    (let ([rectangle (make-rectangle width height offset angle-rad)])
      (check-within (rectangle-width rectangle) width tolerance)
      (check-within (rectangle-height rectangle) height tolerance)
      (check-within (rectangle-area rectangle rectangle-width rectangle-height)
                    area tolerance)
      (check-within (rectangle-perimeter rectangle rectangle-width rectangle-height)
                    perimeter tolerance))))

(module Exercise/2.4 sicp
  (#%require (only racket/base module+ λ))

  (define (cons x y)
    (λ (m) (m x y)))

  (define (car z)
    (z (λ (p q) p)))

  (define (cdr z)
    (z (λ (p q) q)))

  (module+ test
    (#%require rackunit)
    (display "--> Exercise/2.4\n")

    #|
    (cons x y) -> (λ (m) (m x y)) where x and y are stored in the closure
    (car (λ (m) (m x y))) -> ((λ (m) (m x y)) (λ (p q) p))
                          -> ((λ (p q) p) x y) -> x
    |#
    (let ([c (cons 1 2)])
      (check-equal? (car c) 1)
      (check-equal? (cdr c) 2))))

(module Exercise/2.5 sicp
  (#%require (only racket/base module+))

  ;; works because:
  ;;  1. no power of 2 is divisible by 3
  ;;  2. no power of 3 is divisible by 2
  (define (cons a b)
    (* (expt 2 a)
       (expt 3 b)))

  (define (reduction z counter base)
    (cond [(= (remainder z base) 0) (reduction (/ z base)
                                               (+ counter 1)
                                               base)]
          [else counter]))

  (define (car z)
    (reduction z 0 2))

  (define (cdr z)
    (reduction z 0 3))

  (module+ test
    (#%require rackunit)
    (display "--> Exercise/2.5\n")

    (let* ([a 5]
           [b 7]
           [c (cons a b)])
         (check-equal? (car c) a)
         (check-equal? (cdr c) b))))

(module Exercise/2.6 sicp
  (#%require (only racket/base module+ λ))

  (define zero (λ (f) (λ (x) x)))
  (define (add-1 n)
    (λ (f) (λ (x) (f ((n f) x)))))

  (module+ test
    (#%require rackunit)
    (display "--> Exercise/2.6\n")

    #|
    (λ (f) (λ (x) (f ((zero f) x)))) ->
    (λ (f) (λ (x) (f ((λ (x) x) x)))) ->
    (λ (f) (λ (x) (f x)))
    |#
    (define one (λ (f) (λ (x) (f x))))

    #|
    (λ (f) (λ (x) (f ((one f) x)))) ->
    (λ (f) (λ (x) (f (((λ (f) (λ (x) (f x))) f) x)))) ->
    (λ (f) (λ (x) (f (f x))))
    |#
    (define two (λ (f) (λ (x) (f (f x)))))

    (define three (λ (f) (λ (x) (f (f (f x))))))
    (define four (λ (f) (λ (x) (f (f (f (f x)))))))

    (define (church-add m n)
      (λ (f) (λ (x) ((m f) ((n f) x)))))

    (let ([f inc]
          [n0 0])
      (check-equal? ((zero f) n0) 0)
      (check-equal? ((one f) n0) 1)
      (check-equal? ((two f) n0) 2)
      (check-equal? ((three f) n0) 3)
      (check-equal? ((four f) n0) 4)
      (check-equal? (((add-1 four) f) n0) 5)
      (check-equal? (((church-add four two) f) n0) 6)
      (check-equal? (((church-add four three) f) n0) 7))))

(module Exercise/2.7 sicp
  (#%provide make-interval
             lower-bound
             upper-bound
             add-interval
             mul-interval
             div-interval)
  (#%require (only racket/base module+ λ format exn:fail?))

  (define (make-interval a b)
    (if (> a b)
        (error (format "lower bound ~a > upper bound ~a" a b))
        (cons a b)))
  (define (lower-bound interval) (car interval))
  (define (upper-bound interval) (cdr interval))

  (define (add-interval x y)
    (make-interval (+ (lower-bound x) (lower-bound y))
                   (+ (upper-bound x) (upper-bound y))))

  (define (mul-interval x y)
    (let ((p1 (* (lower-bound x) (lower-bound y)))
          (p2 (* (lower-bound x) (upper-bound y)))
          (p3 (* (upper-bound x) (lower-bound y)))
          (p4 (* (upper-bound x) (upper-bound y))))
      (make-interval (min p1 p2 p3 p4)
                     (max p1 p2 p3 p4))))

  (define (div-interval x y)
    (mul-interval
     x
     (make-interval (/ 1.0 (upper-bound y))
                    (/ 1.0 (lower-bound y)))))

  (module+ test
    (#%require rackunit)
    (display "--> Exercise/2.7\n")

    (let* ([x (make-interval -1 2)]
           [res-add (add-interval x x)]
           [res-mul (mul-interval x x)])
      (check-equal? (lower-bound res-add) -2)
      (check-equal? (upper-bound res-add)  4)
      (check-equal? (lower-bound res-mul) -2)
      (check-equal? (upper-bound res-mul)  4))

    ;; NOTE: the reciprocal interval in div-interval doesn't make sense
    (check-exn exn:fail? (λ ()
                           (let ([y (make-interval -1 2)])
                             (make-interval (/ 1.0 (upper-bound y))
                                            (/ 1.0 (lower-bound y))))))))

(module Exercise/2.8 sicp
  (#%provide sub-interval)
  (#%require (only racket/base module+)
             (only (submod ".." Exercise/2.7) make-interval lower-bound upper-bound))

  #|
  ub(y) >= lb(y) -> -ub(y) <= -lb(y) -> lb(x) - ub(y) <= lb(x) - lb(y)
                                     -> ub(x) - ub(y) <= ub(x) - lb(y)
  hence, the interval is [lb(x) - ub(y), ub(x) - lb(y)]
  clearly, lb(x) - ub(y) <= ub(x) - lb(y) since lb(y) - ub(y) <= ub(x) - lb(x) because
  ub(x) - lb(x) >= 0
  lb(y) - ub(y) <= 0
  |#
  (define (sub-interval x y)
    (make-interval (- (lower-bound x) (upper-bound y))
                   (- (upper-bound x) (lower-bound y))))

  ;; or we could simply refactor mul-interval
  (define (sub-interval-check x y)
    (let ((p1 (- (lower-bound x) (lower-bound y)))
          (p2 (- (lower-bound x) (upper-bound y)))
          (p3 (- (upper-bound x) (lower-bound y)))
          (p4 (- (upper-bound x) (upper-bound y))))
      (make-interval (min p1 p2 p3 p4)
                     (max p1 p2 p3 p4))))

  (module+ test
    (#%require rackunit)
    (display "--> Exercise/2.8\n")

    (let* ([x (make-interval -1 2)]
           [res-sub (sub-interval x x)]
           [res-sub-check (sub-interval-check x x)])
      (check-equal? (lower-bound res-sub) (lower-bound res-sub-check))
      (check-equal? (upper-bound res-sub) (upper-bound res-sub-check)))))

(module Exercise/2.9 sicp
  (#%provide width-interval)
  (#%require (only racket/base module+)
             (only (submod ".." Exercise/2.7)
                   make-interval
                   lower-bound
                   upper-bound
                   add-interval
                   mul-interval
                   div-interval)
             (only (submod ".." Exercise/2.8) sub-interval))

  (define (width-interval interval)
    (/ (- (upper-bound interval)
          (lower-bound interval)) 2))

  (define (offset-interval interval offset)
    (make-interval (+ (lower-bound interval) offset)
                   (+ (upper-bound interval) offset)))

  #| add-interval
  (width-interval (add-interval x y)) = (width-interval x) + (width-interval y) because
  0.5 * (ub(x) + ub(y) - (lb(x) + lb(y))) = 0.5 * (ub(x) - lb(x)) + 0.5*(ub(y) - lb(y))
  |#

  #| sub-interval
  (width-interval (sub-interval x y)) = (width-interval x) + (width-interval y) because
  0.5 * (ub(x) - lb(y) - (lb(x) - ub(y))) = 0.5 * (ub(x) - lb(x)) + 0.5*(ub(y) - lb(y))
  |#

  #| mul-interval and div-interval
  The examples below demonstrate that the width of the interval resulting from the
  application of mul-interval or div-interval does not depend only on the widths of the
  input intervals but on the lower and upper bounds as well. Note that
  (width-interval (offset-interval interval offset)) = (width-interval interval) for any
  offset
  |#

  (module+ test
    (#%require rackunit)
    (display "--> Exercise/2.9\n")

    (let* ([x (make-interval -1 2)]
           [w (width-interval x)]
           [offset 1])
      (check-equal? (width-interval (add-interval x x)) (* 2 w))
      (check-equal? (width-interval (sub-interval x x)) (* 2 w))
      (check-equal? (width-interval (sub-interval x x))
                    (width-interval (offset-interval (sub-interval x x) offset)))
      (check-not-equal? (width-interval (mul-interval x x))
                        (width-interval (mul-interval (offset-interval x offset)
                                                      (offset-interval x offset))))
      (check-not-equal? (width-interval (mul-interval x x))
                        (width-interval (mul-interval (offset-interval x offset)
                                                      (offset-interval x offset)))))))

(module Exercise/2.10 sicp
  (#%provide div-interval)
  (#%require (only racket/base module+ λ format exn:fail?)
             (only (submod ".." Exercise/2.7)
                   make-interval
                   lower-bound
                   upper-bound
                   mul-interval))

  (define (div-interval x y)
    (cond [(and (<= (lower-bound y) 0)
                (>= (upper-bound y) 0))
           (error (format "Denominator interval ~a spans 0." y))]
          [else (mul-interval
                 x
                 (make-interval (/ 1.0 (upper-bound y))
                                (/ 1.0 (lower-bound y))))]))

  (module+ test
    (#%require rackunit)
    (display "--> Exercise/2.10\n")

    (let* ([x (make-interval -1 2)]
           [res-div (div-interval x (make-interval -2 -1))])
      (check-exn exn:fail? (λ () (div-interval x (make-interval -1 1))))
      (check-exn exn:fail? (λ () (div-interval x (make-interval  0 1))))
      (check-exn exn:fail? (λ () (div-interval x (make-interval -1 0))))
      (check-equal? (lower-bound res-div) -2.0)
      (check-equal? (upper-bound res-div)  1.0))))

(module Exercise/2.11-data racket/base
  (#%provide test-intervals-pairs)
  (#%require (only racket/list combinations)
             (only (submod ".." Exercise/2.7) make-interval))

  #|
  Here I generate data used in Exercise/2.11. Note that the combinations procedure
  doesn't work with language sicp so I have to use racket/base with mcons because the
  data would be used in sicp (this is a hack which I normally try to avoid).
  |#
  (define test-intervals-pairs
    (map
     (λ (x) (mcons (car x) (cadr x))) ; the sicp language is based on mutable pairs
     (combinations ; construct all pairs of intervals
      (map (λ (x) (make-interval (car x) (cadr x)))
           ;; NOTE: order is preserved
           (combinations '(-5 -4 -3 -2 -1 0 1 2 3 4 5) 2)) 2)))

  (display (format "generated ~a random pairs of intervals\n"
                   (length test-intervals-pairs))))

(module Exercise/2.11 sicp
  (#%require (only racket/base module+ for)
             (only (submod ".." Exercise/2.7)
                   make-interval
                   lower-bound
                   upper-bound
                   mul-interval)
             (only (submod ".." Exercise/2.11-data) test-intervals-pairs))

  #|
  1. 0 [x1 x2] [y1 y2]: [x1*y1, x2*y2]
  2. [x1 0 x2] [y1 y2]: [x1*y2, x2*y2]
  3. [x1 x2] 0 [y1 y2]: [x1*y2, x2*y1]
  4. [x1 x2] [y1 0 y2]: [x1*y2, x1*y1]
  5. [x1 x2] [y1 y2] 0: [x1*y1, x2*y2]
  6. [y1 0 y2] [x1 x2]: [y1*x2, y2*x2]
  7. [y1 y2] 0 [x1 x2]: [y1*x2, y2*x1]
  8. [y1 y2] [x1 0 x2]: [y1*x2, y1*x1]
  9. [x1 0 x2] [y1 0 y2]: [min(x1*y2, y1*x2), max(x1*y1, x2*y2)]
  |#
  (define (mul-interval-cases x y)
    (let ([x1 (lower-bound x)]
          [x2 (upper-bound x)]
          [y1 (lower-bound y)]
          [y2 (upper-bound y)])
      ;; the conditions could be grouped (e.g., by y1 <= 0)
      (cond
        [(and (>= x1 0) (>= x2 0) (>= y1 0) (>= y2 0)) ;; case 1
         (make-interval (* x1 y1) (* x2 y2))]
        [(and (<= x1 0) (>= x2 0) (>= y1 0) (>= y2 0)) ;; case 2
         (make-interval (* x1 y2) (* x2 y2))]
        [(and (<= x1 0) (<= x2 0) (>= y1 0) (>= y2 0)) ;; case 3
         (make-interval (* x1 y2) (* x2 y1))]
        [(and (<= x1 0) (<= x2 0) (<= y1 0) (>= y2 0)) ;; case 4
         (make-interval (* x1 y2) (* x1 y1))]
        [(and (<= x1 0) (<= x2 0) (<= y1 0) (<= y2 0)) ;; case 5
         (make-interval (* x2 y2) (* x1 y1))]
        [(and (<= y1 0) (>= y2 0) (>= x1 0) (>= x2 0)) ;; case 6
         (make-interval (* y1 x2) (* y2 x2))]
        [(and (<= y1 0) (<= y2 0) (>= x1 0) (>= x2 0)) ;; case 7
         (make-interval (* y1 x2) (* y2 x1))]
        [(and (<= y1 0) (<= y2 0) (<= x1 0) (>= x2 0)) ;; case 8
         (make-interval (* y1 x2) (* y1 x1))]
        [else                                          ;; case 9
         (make-interval (min (* x1 y2) (* y1 x2))
                        (max (* x1 y1) (* x2 y2)))])))

  (module+ test
    (#%require rackunit)
    (display "--> Exercise/2.11\n")

    ;; case 1
    (check-equal? (mul-interval       (make-interval 1 2) (make-interval 3 4))
                  (mul-interval-cases (make-interval 1 2) (make-interval 3 4)))
    ;; case 2
    (check-equal? (mul-interval       (make-interval -1 2) (make-interval 3 4))
                  (mul-interval-cases (make-interval -1 2) (make-interval 3 4)))
    ;; case 3
    (check-equal? (mul-interval       (make-interval -2 -1) (make-interval 3 4))
                  (mul-interval-cases (make-interval -2 -1) (make-interval 3 4)))
    ;; case 4
    (check-equal? (mul-interval       (make-interval -3 -2) (make-interval -1 4))
                  (mul-interval-cases (make-interval -3 -2) (make-interval -1 4)))
    ;; case 5
    (check-equal? (mul-interval       (make-interval -4 -3) (make-interval -2 -1))
                  (mul-interval-cases (make-interval -4 -3) (make-interval -2 -1)))
    ;; case 6
    (check-equal? (mul-interval       (make-interval 2 3) (make-interval -1 1))
                  (mul-interval-cases (make-interval 2 3) (make-interval -1 1)))
    ;; case 7
    (check-equal? (mul-interval       (make-interval 1 2) (make-interval -2 -1))
                  (mul-interval-cases (make-interval 1 2) (make-interval -2 -1)))
    ;; case 8
    (check-equal? (mul-interval       (make-interval -1 1) (make-interval -3 -2))
                  (mul-interval-cases (make-interval -1 1) (make-interval -3 -2)))
    ;; case 9
    (check-equal? (mul-interval       (make-interval -3 2) (make-interval -3 1))
                  (mul-interval-cases (make-interval -3 2) (make-interval -3 1)))
    ;; case 9
    (check-equal? (mul-interval       (make-interval -3 2) (make-interval -3 5))
                  (mul-interval-cases (make-interval -3 2) (make-interval -3 5))))

  ;; test on automatically generated pairs of intervals
  (module+ test
    (for ([pair test-intervals-pairs])
      (check-equal? (mul-interval       (car pair) (cdr pair))
                    (mul-interval-cases (car pair) (cdr pair))))))

(module Exercise/2.12 sicp
  (#%provide make-center-percent
             center
             percent)
  (#%require (only racket/base module+)
             (only (submod ".." Exercise/2.7)
                   make-interval
                   lower-bound
                   upper-bound)
             (only (submod ".." Exercise/2.9) width-interval))

  (define (make-center-percent center percent)
    (let ([width (abs (/ (* center percent) 100.0))])
      (make-interval (- center width) (+ center width))))

  (define (center interval)
    (/ (+ (lower-bound interval)
          (upper-bound interval))
       2))

  (define (percent interval)
    (let ([w (width-interval interval)]
          ;; when the center is 0, the division below results in "+inf.0"
          [c (center interval)])
      ;; I want to get a positive result
      (abs (* 100.0 (/ w c)))))

  (module+ test
    (#%require rackunit)
    (display "--> Exercise/2.12\n")

    (let* ([c 5.0]
           [p 10.0]
           [interval (make-center-percent c p)])
      (check-equal? (center interval) c)
      (check-equal? (percent interval) p))))

(module Exercise/2.13 sicp
  (#%require (only racket/base module+)
             (only (submod ".." Exercise/2.7) mul-interval)
             (only (submod ".." Exercise/2.12)
                   make-center-percent
                   percent))

  ;; see latex note Exercise 2.13
  (define (mul-percent-approx-case1 x y)
    (+ (percent x)
       (percent y)))

  (module+ test
    (#%require rackunit)
    (display "--> Exercise/2.13\n")

    (let* ([p 1]
           [x (make-center-percent 5 p)]
           [y (make-center-percent 10 p)]
           [tolerance 1e-3])
      (check-within (percent (mul-interval x y))
                    (mul-percent-approx-case1 x y)
                    tolerance))))

(module Exercise/2.14 sicp
  (#%provide show-center-percent
             show-center-width
             show-bounds)
  (#%require (only racket/base module+ format)
             (only (submod ".." Exercise/2.7)
                   make-interval
                   add-interval
                   mul-interval)
             (only (submod ".." Exercise/2.8) sub-interval)
             (only (submod ".." Exercise/2.9) width-interval)
             (only (submod ".." Exercise/2.10) div-interval)
             (only (submod ".." Exercise/2.12)
                   make-center-percent
                   center
                   percent))

  (define (par1 r1 r2)
    (div-interval (mul-interval r1 r2)
                  (add-interval r1 r2)))

  (define (par2 r1 r2)
    (let ((one (make-interval 1 1)))
      (div-interval
       one (add-interval (div-interval one r1)
                         (div-interval one r2)))))

  (define (show-center-percent interval)
    (display (format "center-%: ~a\n" (cons (center interval)
                                            (percent interval)))))

  (define (show-center-width interval)
    (display (format "center-w: ~a\n" (cons (center interval)
                                            (width-interval interval)))))

  (define (show-bounds interval)
    (display (format "lb-ub   : ~a\n" interval)))

  (module+ test
    (#%require rackunit)
    (display "--> Exercise/2.14\n")

    ;; conclusion: Lem is right
    (let* ([R1 (make-center-percent 6.8 10)]
           [R2 (make-center-percent 4.7 5)]
           [res1 (par1 R1 R2)]
           [res2 (par2 R1 R2)])
      (show-center-percent res1)
      (show-bounds res1)
      ;; this is the original example from Section 2.1.4 with interval (2.58 . 2.97)
      (show-center-percent res2)
      (show-bounds res2)
      (check-within res2 (make-interval 2.58 2.97) 1e-2))

    ;; verify that the two expressions are equivalent in the deterministic case
    (let* ([R1 (make-center-percent 6.8 0)]
           [R2 (make-center-percent 4.7 0)]
           [res1 (par1 R1 R2)]
           [res2 (par2 R1 R2)])
      (show-center-percent res1)
      (show-bounds res1)
      (show-center-percent res2)
      (show-bounds res2)))

  ;; ---------------------
  ;; play with the system
  ;; ---------------------
  (module+ test
    (define one (make-center-percent 1 0))
    (define two (make-center-percent 2 0))
    (define A (make-center-percent 10 1))
    (define B (make-center-percent 20 1))

    ;; center changes but uncertainty remains the same
    (show-center-percent (div-interval one A))

    ;; division is multiplication with reciprocal interval (whose uncertainty is the
    ;; same), hence we expect for the uncertainty to approximately double
    ;; (see Exercise/2.13) and the center to be nearly 1 (which is the case)
    (show-center-percent (div-interval A A))

    ;; we have two different (identical) measurements whose uncertainty propagates
    ;; (according to the given expression)
    (show-center-percent (mul-interval A A))
    (let ([interval (sub-interval A A)])
      (show-center-width interval)
      (show-center-percent interval))

    ;; uncertainty of the result is 1 (see latex note for add-interval)
    (show-center-percent (add-interval A B))

    ;; in both cases the uncertainty of the result is approximately the sum of the
    ;; uncertainties
    (show-center-percent (mul-interval A B))
    (show-center-percent (div-interval A B))

    ;; see latex note for sub-interval (percent is undefined)
    (let ([c 5])
      (show-center-percent (sub-interval (make-center-percent c 20)
                                         (make-center-percent c 40))))

    ;; ----------------------------------------------
    ;; try other algebraically equivalent expressions
    ;; ----------------------------------------------
    ;; the uncertainty of A*A/(A*A) is approximately double that of A/A
    (show-center-percent (div-interval (mul-interval A A)
                                       (mul-interval A A)))

    (show-center-percent (sub-interval (add-interval A A) A))

    (check-true (> (percent (sub-interval (add-interval A B) A))
                   (percent B)))
    (check-true (> (percent (sub-interval (add-interval A B) B))
                   (percent A)))
    (check-true (> (percent (div-interval (mul-interval A B) A))
                   (percent B)))
    (check-true (> (percent (div-interval (mul-interval A B) B))
                   (percent A)))

    ;; A + A + A = 3*A
    ;; this hinges on two facts:
    ;; 1. uncertainty(A) = uncertainty(A + A)
    ;; 2. uncertainty(two * A) = uncertainty(A) because uncertainty(two) = 0
    (show-center-percent (add-interval (add-interval A A) A))
    (show-center-percent (mul-interval (add-interval two one) A))))

(module Exercise/2.15 sicp
  #|
  Eva's statement regarding tighter error bounds (in general) is discussed in the latex
  note. Regarding whether par2 a “better” program for parallel resistances than par1,
  the answer is yes (see Exercise/2.14 above).
  |#)

(module Exercise/2.16 sicp
  #|
  The answer is no (see the latex note for details).
  |#)

(module Section/2.2.1 sicp
  (#%provide for-each-custom)
  (#%require (only racket/base module+ λ format))

  (define (list-ref items n)
    (if (= n 0)
        (car items)
        (list-ref (cdr items) (- n 1))))

  (module+ test
    (#%require rackunit)
    (display "--> Section/2.2.1\n")

    (check-equal? (list-ref (list 1 4 9 16 25) 3) 16))

  (define (length-custom items)
    (if (null? items)
        0
        (+ 1 (length-custom (cdr items)))))

  (module+ test
    (check-equal? (length-custom '(1 2 3 4 5 6)) 6))

  (define (append-custom l1 l2)
    (cond [(null? l1) l2]
          [else (cons (car l1)
                      (append-custom (cdr l1) l2))]))

  (module+ test
    (check-equal? (append-custom (list 1 2 3) (list 4 5)) (list 1 2 3 4 5)))

  (define (scale-list-recur s lst)
    (cond [(null? lst) nil]
          [else (cons (* s (car lst))
                      (scale-list-recur s
                                        (cdr lst)))]))

  (define (scale-list-iter s lst acc)
    (cond [(null? lst) (reverse acc)]
          [else (scale-list-iter s
                                 (cdr lst)
                                 (cons (* s (car lst))
                                       acc))]))

  ;; not a good idea to use append in this case
  (define (scale-list-iter-append s lst acc)
    (cond [(null? lst) acc]
          [else (scale-list-iter-append s
                                        (cdr lst)
                                        (append acc (list (* s (car lst)))))]))

  (define (map proc items)
    (if (null? items)
        nil
        (cons (proc (car items))
              (map proc (cdr items)))))

  (define (scale-list-map factor items)
    (map (λ (x) (* x factor))
         items))

  (module+ test
    (check-equal? (scale-list-recur 2 (list 1 2 3)) (list 2 4 6))
    (check-equal? (scale-list-iter 2 (list 1 2 3) '()) (list 2 4 6))
    (check-equal? (scale-list-iter-append 2 (list 1 2 3) '()) (list 2 4 6))
    (check-equal? (scale-list-map 2 (list 1 2 3)) (list 2 4 6))
    (check-equal? (map abs (list -10 2.5 -11.6 17)) (list 10 2.5 11.6 17)))

  (define (for-each-custom f lst)
    (cond [(null? lst) (newline)]
          [else
           (f (car lst))
           (for-each-custom f (cdr lst))]))

  (module+ test
    (let ([lst '(1 2 3)]
          [scale 4]
          [show (λ (x) (display (format "~a " x)))])
      (for-each-custom show (scale-list-recur scale lst))
      (for-each-custom show (scale-list-iter scale lst '()))
      (for-each-custom show (scale-list-iter-append scale lst '())))))

(module Exercise/2.17 sicp
  (#%require (only racket/base module+))

  (define (last-pair lst)
    (let ([tail (cdr lst)])
      (cond [(null? tail) lst]
            [else (last-pair tail)])))

  (module+ test
    (#%require rackunit)
    (display "--> Exercise/2.17\n")

    (check-equal? (last-pair (list 23 72 149 34)) (cons 34 nil))))

(module Exercise/2.18 sicp
  (#%provide reverse)
  (#%require (only racket/base module+))

  ;; using an iterative process
  (define (reverse lst)
    (define (iter l acc)
      (cond [(null? l) acc]
            [else (iter (cdr l)
                        (cons (car l) acc))]))
    (iter lst '()))

  ;; using a recursive process (not the best of ideas)
  (define (reverse-recur-append lst)
    (cond [(null? lst) lst]
          [else (append (reverse-recur-append (cdr lst))
                        (list (car lst)))]))

  (module+ test
    (#%require rackunit)
    (display "--> Exercise/2.18\n")

    (let ([lst (list 1 4 9 16 25)]
          [res (list 25 16 9 4 1)])
      (check-equal? (reverse-recur-append lst) res)
      (check-equal? (reverse lst) res))))

(module Exercise/2.19 sicp
  (#%require (only racket/base module+)
             (only (submod "sicp1.rkt" Section/1.2.2) count-change))

  ;; this is nearly how I had defined it in Section/1.2.2 in sicp1.rkt
  (define no-more? null?)
  (define except-first-denomination cdr)
  (define first-denomination car)

  (define (cc amount coin-values)
    (cond ((= amount 0) 1)
          ((or (< amount 0) (no-more? coin-values)) 0)
          (else
           (+ (cc amount
                  (except-first-denomination
                   coin-values))
              (cc (- amount
                     (first-denomination
                      coin-values))
                  coin-values)))))

  (module+ test
    (#%require rackunit)
    (display "--> Exercise/2.19\n")

    ;; The order of coins has no impact (our tree recursion is an exostive search).
    (let ([us-coins (list 50 25 10 5 1)]
          [us-coins-reordered (list 1 5 10 25 50)]
          [uk-coins (list 100 50 20 10 5 2 1 0.5)]
          [amount 100])
      (check-equal? (cc amount us-coins) (count-change amount us-coins))
      (check-equal? (cc amount us-coins-reordered) (cc amount us-coins))
      (check-equal? (cc amount uk-coins) (count-change amount uk-coins)))))

(module Exercise/2.20 sicp
  (#%require (only racket/base module+ λ format))

  #|
  This exercise introduces a way to define a procedure with variable number of arguments
  using the "dotted-tail notation" (i.e. a variadic procedure). The Racket Guide has a
  nice section on this and more (docs.racket-lang.org/guide/lambda.html) with a very
  clear summary of all the options:

  (lambda gen-formals
    body ...+)

    gen-formals	= (arg ...)
                | rest-id
                | (arg ...+ . rest-id)

            arg	= arg-id
                | [arg-id default-expr]
                | arg-keyword arg-id
                | arg-keyword [arg-id default-expr]

  I found the Curried Function Shorthand quite nice as well and it plays nicely with
  kwargs: docs.racket-lang.org/guide/define.html#(part._.Curried_.Function_.Shorthand)

  (define (head args) body ...+)

           head = id
                | (head args)

           args = arg ...
                | arg ... . rest-id
  |#
  (define (f1 . x) (display (format "~a\n" x)))
  (define f2 (λ x (display (format "~a\n" x)))) ; rather peculiar!
  (define (f3 x . y) (display (format "~a\n~a\n" x y)))
  (define f4 (λ (x . y) (display (format "~a\n~a\n" x y))))

  (define (check-even-odd-parity a b)
    (= (abs (remainder a 2))
       (abs (remainder b 2))))

  (define (same-parity-iter x . y)
    (define (iter ys acc)
      (cond [(null? ys) acc]
            [(check-even-odd-parity x (car ys)) (iter (cdr ys) (cons (car ys) acc))]
            [else (iter (cdr ys) acc)]))
    (reverse (iter y (list x))))

  (define (same-parity-recur x . y)
    (define (recur ys)
      (cond [(null? ys) nil]
            [(check-even-odd-parity x (car ys)) (cons (car ys)
                                                      (recur (cdr ys)))]
            [else (recur (cdr ys))]))
    (cons x (recur y)))

  (module+ test
    (#%require rackunit)
    (display "--> Exercise/2.20\n")

    (f1 1 2 3 4)
    (f2 1 2 3 4)
    (f3 1 2 3 4)
    (f4 1 2 3 4)

    (check-true (check-even-odd-parity -1 3))
    (check-true (check-even-odd-parity -2 4))
    (check-true (check-even-odd-parity 1 3))
    (check-true (check-even-odd-parity 2 4))
    (check-false (check-even-odd-parity -3 6))

    (check-equal? (same-parity-iter 1 2 3 4 5 6 7) (list 1 3 5 7))
    (check-equal? (same-parity-iter 0 3 4 5 6 7) (list 0 4 6))
    (check-equal? (same-parity-iter 2 3 4 5 6 7) (list 2 4 6))
    (check-equal? (same-parity-recur 1 2 3 4 5 6 7) (list 1 3 5 7))
    (check-equal? (same-parity-recur 0 3 4 5 6 7) (list 0 4 6))
    (check-equal? (same-parity-recur 2 3 4 5 6 7) (list 2 4 6))))

(module Exercise/2.21 sicp
  (#%require (only racket/base module+ format)
             (only (submod "sicp1.rkt" common-utils) square))

  (define (square-list items)
    (if (null? items)
        nil
        (cons (square (car items))
              (square-list (cdr items)))))

  (define (square-list-map items)
    (map square items))

  (module+ test
    (#%require rackunit)
    (display "--> Exercise/2.21\n")

    (let ([lst (list 1 2 3 4)]
          [res (list 1 4 9 16)])
      (check-equal? (square-list lst) res)
      (check-equal? (square-list-map lst) res))))

(module Exercise/2.22 sicp
  (#%require (only racket/base module+)
             (only (submod "sicp1.rkt" common-utils) square))

  #|
  The order is reversed because we take the elements in turn and prepend them to dest:
  src: (1 2 3), dest: ()
  src:   (2 3), dest: (cost 1 ()) -> (1)
  src:     (3), dest: (cost 2 (1)) -> (2 1)
  src:      (), dest: (cost 3 (2 1)) -> (3 2 1)
  |#
  (define (square-list-1 items)
    (define (iter things answer)
      (if (null? things)
          answer
          (iter (cdr things)
                (cons (square (car things))
                      answer))))
    (iter items nil))

  #|
  We are simply forming a different data-structure here (not a list):
  src: (1 2 3), dest: ()
  src:   (2 3), dest: (cost () 1) -> (() . 1)
  src:     (3), dest: (cost (() . 1) 2) -> ((() . 1) . 4)
  src:      (), dest: (cost ((() . 1) . 4) 3) -> (((() . 1) . 4) . 9)
  |#
  (define (square-list-2 items)
    (define (iter things answer)
      (if (null? things)
          answer
          (iter (cdr things)
                (cons answer
                      (square (car things))))))
    (iter items nil))

  (module+ test
    (#%require rackunit)
    (display "--> Exercise/2.22\n")

    (check-equal? (square-list-1 (list 1 2 3)) (list 9 4 1))
    (check-equal? (square-list-2 (list 1 2 3)) (cons (cons (cons '() 1) 4) 9))))

(module Exercise/2.23 sicp
  (#%require (only racket/base module+ λ)
             (only (submod ".." Section/2.2.1) for-each-custom))

  (module+ test
    (#%require rackunit)
    (display "--> Exercise/2.23\n")

    (let ([lst (list 57 321 88)]
          [f (λ (x) (newline) (display x))])
      (for-each        f lst)
      (for-each-custom f lst))))

(module Exercise/2.24 sicp
  (#%provide count-leaves
             test-list-2)
  (#%require (only racket/base module+ module* format))

  (define (count-leaves x)
    (cond ((null? x) 0)
          ((not (pair? x)) 1)
          (else (+ (count-leaves (car x))
                   (count-leaves (cdr x))))))

  (define test-list-1 (cons (list 1 2) (list 3 4)))
  (define test-list-2 (list 1 (list 2 (list 3 4))))

  (module+ test
    (#%require rackunit)
    (display "--> Exercise/2.24\n")

    (count-leaves test-list-1)
    (count-leaves (list test-list-1 test-list-1))

    (display (format "test-list-y: ~a\n" test-list-2))
    (count-leaves test-list-2)

    #|
    t0 -> (1 . (t1 . nil))
    t1 -> (2 . (t2 . nil))
    t2 -> (3 . ( 4 . nil))
            t0
           /  \
          1   t1
             /  \
            2    t2
                /  \
               3    4
    |#
    (check-equal? test-list-2
                  (cons 1
                        (cons (cons 2
                                    (cons (cons 3
                                                (cons 4 '()))
                                          '()))
                              '()))))

  #|
  To force sdraw to draw a box-and-pointer diagram we need to temporarily change the
  language of this exercise to racket/base because sdraw assumes immutable pairs (as
  used by default in racket/base) while scip is based on mutable pairs. Note that
  cons in sicp seems to be equivalent to mcons in racket/base.
  |#
  (module* test-box-and-pointer racket/base
    (#%require (only (submod "sicp1.rkt" conversion-utils) mcons->cons)
               (only (submod "..") test-list-2)
               sdraw)
    (display "--> Exercise/2.24 (test-box-and-pointer)\n")

    (sdraw (mcons->cons test-list-2) #:null-style '/)))

(module Exercise/2.25 sicp
  (#%require (only racket/base module+))

  (define x (list 1 3 (list 5 7) 9))
  (define y (list (list 7)))
  (define z (list 1 (list 2 (list 3 (list 4 (list 5 (list 6 7)))))))

  (module+ test
    (#%require rackunit)
    (display "--> Exercise/2.25\n")

    (check-equal? (car (cdaddr x)) 7)
    (check-equal? (car (cdr (car (cdr (cdr x))))) 7)

    (check-equal? (caar y) 7)
    (check-equal? (car (car y)) 7)

    (check-equal? (cadadr (cadadr (cadadr z))) 7)
    (check-equal? (car (cdr (car (cdr (car (cdr (car (cdr (car (cdr
                                                                (car
                                                                 (cdr z)))))))))))) 7)))

(module Exercise/2.26 sicp
  (#%require (only racket/base module+ format))

  (define x (list 1 2 3))
  (define y (list 4 5 6))

  (module+ test
    (#%require rackunit)
    (display "--> Exercise/2.26\n")

    (display (format "x: ~a\n" x))
    (display (format "y: ~a\n" y))
    (display (format "(append x y): ~a\n" (append x y)))
    (display (format "(cons x y)  : ~a\n" (cons x y)))
    (display (format "(list x y)  : ~a\n" (list x y)))))

(module Exercise/2.27 sicp
  (#%require (only racket/base module+)
             (only (submod ".." Exercise/2.18) reverse))

  (define (deep-reverse-v1 lst-or-number)
    (define (iter l acc)
      (cond [(null? l) acc]
            [(not (pair? l)) l]
            [else (iter (cdr l)
                        (cons (deep-reverse-v1 (car l)) acc))]))
    (iter lst-or-number '()))

  (define (deep-reverse-v2 lst)
    (define (iter l acc)
      (cond [(null? l) acc]
            [else (let ([head (car l)]
                        [tail (cdr l)])
                    (iter tail
                          (cons (if (pair? head)
                                    (deep-reverse-v1 head)
                                    head)
                                acc)))]))
    (iter lst '()))

  (define (deep-reverse-v3 lst-or-number)
    (display lst-or-number)
    (newline)
    (cond [(pair? lst-or-number)
           (map deep-reverse-v3 (reverse lst-or-number))]
          [else lst-or-number]))

  (module+ test
    (#%require rackunit)
    (display "--> Exercise/2.27\n")

    (define x (list (list 1 2) (list 3 4)))
    (check-equal? (deep-reverse-v1 x) '((4 3) (2 1)))

    ;; lst: (list (list 1 2 (list 11 (list 12 13)) 3 '()) 4 (list 5 6))
    (let ([lst '((1 2 (11 (12 13)) 3 ()) 4 (5 6))]
          [res-rev '((5 6) 4 (1 2 (11 (12 13)) 3 ()))]
          [res-deep-rev '((6 5) 4 (() 3 ((13 12) 11) 2 1))])
      (check-equal? (reverse lst) res-rev)
      (check-equal? (deep-reverse-v1 lst) res-deep-rev)
      (check-equal? (deep-reverse-v2 lst) res-deep-rev)
      (check-equal? (deep-reverse-v3 lst) res-deep-rev))))

(module Exercise/2.28 sicp
  (#%provide fringe)
  (#%require (only racket/base module+))

  (define (fringe lst)
    (cond [(null? lst) lst]
          [(not (pair? lst)) (list lst)]
          [else (let ([head (fringe (car lst))]
                      [tail (fringe (cdr lst))])
                  (if (null? head)
                      ;; otherwise append would drop an empty head
                      (cons head tail)
                      (append head tail)))]))

  (module+ test
    (#%require rackunit)
    (display "--> Exercise/2.28\n")

    (define x (list (list 1 2) (list 3 4)))
    (check-equal? (fringe x) '(1 2 3 4))
    (check-equal? (fringe (list x x)) '(1 2 3 4 1 2 3 4))

    (check-equal? (fringe '(1 ())) '(1 ()))
    (check-equal? (fringe '(() 1)) '(() 1))
    (check-equal? (fringe '((1 2 (11 (12 13)) 3 ()) 4 (5 6)))
                  '(1 2 11 12 13 3 () 4 5 6))
    (check-equal? (append '() (list 2 3)) (list 2 3))
    (check-equal? (append (list 1 '()) (list 2 3)) (list 1 '() 2 3))))

(module Exercise/2.29 sicp
  (#%require (only racket/base module+ λ))

  (define (make-mobile left right)
    (list left right))

  ;; a better name for "structure" would probably be "mobile-or-weight"
  (define (make-branch length structure)
    (list length structure))

  ;; ==========================================
  ;; A
  ;; ==========================================
  (define (left-branch mobile)
    (car mobile))

  (define (right-branch mobile)
    (cadr mobile))

  (define (branch-length branch)
    (car branch))

  (define (branch-structure branch)
    (cadr branch))

  ;; ==========================================
  ;; B
  ;; ==========================================
  (define (total-weight mobile-or-weight)
    (define (branch-weight branch)
      (let ([mobile-or-weight (branch-structure branch)])
        (if (pair? mobile-or-weight)
            (total-weight mobile-or-weight)
            mobile-or-weight)))
    (cond [(pair? mobile-or-weight) (+ (branch-weight (left-branch mobile-or-weight))
                                       (branch-weight (right-branch mobile-or-weight)))]
          ;; directly return the weight
          [else mobile-or-weight]))

  ;; ==========================================
  ;; C
  ;; ==========================================
  (define (balanced? mobile)
    (define (torque branch mobile-or-weight)
      (* (branch-length branch)
         (total-weight mobile-or-weight)))

    (cond [(not (pair? mobile)) #t]
          [else (let* ([l-branch (left-branch mobile)]
                       [r-branch (right-branch mobile)]
                       [l-mobile-or-weight (branch-structure l-branch)]
                       [r-mobile-or-weight (branch-structure r-branch)])
                  (and (= (torque l-branch l-mobile-or-weight)
                          (torque r-branch r-mobile-or-weight))
                       (balanced? l-mobile-or-weight)
                       (balanced? r-mobile-or-weight)))]))

  ;; ==========================================
  ;; D
  ;; ==========================================
  #|
  The alternative representation (based on cons instead of list) requires to change
  right-branch and branch-structure to use cdr instead of cadr.
  |#

  (module+ test
    (#%require rackunit)
    (display "--> Exercise/2.29\n")

    (define bm-1 (make-mobile
                  (make-branch 1
                               (make-mobile
                                (make-branch 2 (make-mobile
                                                (make-branch 2 15)
                                                (make-branch 1 25)))
                                (make-branch 1 20)))
                  (make-branch 2
                               (make-mobile
                                (make-branch 2 3)
                                (make-branch 1 (make-mobile
                                                (make-branch 2 17)
                                                (make-branch 1 20)))))))

    (check-equal? (total-weight bm-1) 100)
    (check-false (balanced? bm-1))

    ;; same weight as bm-1 but with adjusted lengths to get a balanced binary mobile
    (define bm-2
      (let*
          ([balanced-weight (λ (l1 w1 w2) (/ (* l1 w1) w2))]
           [t1 (make-mobile (make-branch 2 15)
                            (make-branch (balanced-weight 2 15 25) 25))]
           [t2 (make-mobile (make-branch 2 t1)
                            (make-branch (balanced-weight 2 (total-weight t1) 20) 20))]
           [t3 (make-mobile (make-branch 2 17)
                            (make-branch (balanced-weight 2 17 20) 20))]
           [t4 (make-mobile (make-branch 2 3)
                            (make-branch (balanced-weight 2 3 (total-weight t3)) t3))])
        (make-mobile (make-branch 1 t2)
                     (make-branch (balanced-weight 1
                                                   (total-weight t2)
                                                   (total-weight t4)) t4))))

    (check-equal? (total-weight bm-2) 100)
    (check-true (balanced? bm-2))))

(module Section/2.2.2/mapping-over-trees sicp
  (#%require (only racket/base module+ λ))

  #|
  The pattern is:
  1. check if done
  2. check if dealing with a leaf
  3. break cons into car and cdr and handle them recursively
  |#
  (define (scale-tree-seq tree factor)
    (cond ((null? tree) nil)
          ((not (pair? tree)) (* tree factor))
          (else (cons (scale-tree-seq (car tree) factor)
                      (scale-tree-seq (cdr tree) factor)))))

  #|
  The pattern is:
  1. process a tree as a sequence of elements using map
  2. each element could be a tree or a leaf (treat accordingly)

  I used it in deep-reverse-v3 in Exercise/2.27
  |#
  (define (scale-tree-map tree factor)
    (map (λ (sub-tree)
           (if (pair? sub-tree)
               (scale-tree-map sub-tree factor)
               (* sub-tree factor)))
         tree))

  (module+ test
    (#%require rackunit)
    (display "--> Section/2.2.2/mapping-over-trees\n")

    (let ([tree (list 1 (list 2 (list 3 4) 5) (list 6 7))]
          [scaled-tree (list 10 (list 20 (list 30 40) 50) (list 60 70))])
      (check-equal? (scale-tree-seq tree 10) scaled-tree)
      (check-equal? (scale-tree-map tree 10) scaled-tree))))

(module Exercise/2.30 sicp
  (#%require (only racket/base module+ λ)
             (only (submod "sicp1.rkt" common-utils) square))

  (define (square-tree-seq tree)
    (cond ((null? tree) nil)
          ((not (pair? tree)) (square tree))
          (else (cons (square-tree-seq (car tree))
                      (square-tree-seq (cdr tree))))))

  (define (square-tree-map tree)
    (map (λ (sub-tree)
           (if (pair? sub-tree)
               (square-tree-map sub-tree)
               (square sub-tree)))
         tree))

  (module+ test
    (#%require rackunit)
    (display "--> Exercise/2.30\n")

    (let ([tree (list 1 (list 2 (list 3 4) 5) (list 6 7))]
          [res '(1 (4 (9 16) 25) (36 49))])
      (check-equal? (square-tree-seq tree) res)
      (check-equal? (square-tree-map tree) res))))

(module Exercise/2.31 sicp
  (#%require (only racket/base module+ λ)
             (only (submod "sicp1.rkt" common-utils) square))

  (define (tree-map f tree)
    (map (λ (sub-tree)
           (if (pair? sub-tree)
               (tree-map f sub-tree)
               (f sub-tree)))
         tree))

  (define (square-tree tree) (tree-map square tree))

  (module+ test
    (#%require rackunit)
    (display "--> Exercise/2.31\n")

    (let ([tree (list 1 (list 2 (list 3 4) 5) (list 6 7))]
          [res '(1 (4 (9 16) 25) (36 49))])
      (check-equal? (square-tree tree) res))))

(module Exercise/2.32 sicp
  (#%require (only racket/base module+ λ))

  #|
  We construct rest using wishful thinking and then cons the car of the input set with
  each of its elements. We hit the base case when the cdr of the input set is nil and
  return (list '()). Note that (append '(()) '((1))) -> '(() (1)).
  |#
  (define (subsets s)
    (if (null? s)
        (list nil)
        (let ([rest (subsets (cdr s))])
          (append rest (map (λ (x)
                              (cons (car s) x)) rest)))))

  (module+ test
    (#%require rackunit)
    (display "--> Exercise/2.32\n")

    (let ([s (list 1 2 3)]
          [powerset '(() (3) (2) (2 3) (1) (1 3) (1 2) (1 2 3))])
      (check-equal? (subsets s) powerset))))

(module Section/2.2.3 sicp
  (#%provide accumulate
             filter
             enumerate-tree
             enumerate-interval)
  (#%require (only racket/base module+)
             (only (submod "sicp1.rkt" common-utils) square)
             (only (submod "sicp1.rkt" Exercise/1.19) fib)
             (only (submod ".." Exercise/2.28) fringe))

  (define (sum-odd-squares tree)
    (cond [(null? tree) 0]
          [(not (pair? tree)) (if (odd? tree) (square tree) 0)]
          (else (+ (sum-odd-squares (car tree))
                   (sum-odd-squares (cdr tree))))))

  (define (even-fibs n)
    (define (next k)
      (if (> k n)
          nil
          (let ((f (fib k)))
            (if (even? f)
                (cons f (next (+ k 1)))
                (next (+ k 1))))))
    (next 0))

  (module+ test
    (#%require rackunit)
    (display "--> Section/2.2.3\n")

    (check-equal? (sum-odd-squares '(1 2 (3 4 5) (6))) 35)
    (check-equal? (even-fibs 20) '(0 2 8 34 144 610 2584)))

  (define (filter predicate sequence)
    (cond [(null? sequence) nil]
          [(predicate (car sequence))
           (cons (car sequence)
                 (filter predicate (cdr sequence)))]
          [else (filter predicate (cdr sequence))]))

  ;; a slightly reorganized version
  (define (filter-v2 predicate sequence)
    (if (null? sequence)
        nil
        (let ([head (car sequence)]
              [tail (cdr sequence)])
          (if (predicate head)
              (cons head (filter-v2 predicate tail))
              (filter-v2 predicate tail)))))

  (define (accumulate op initial sequence)
    (if (null? sequence)
        initial
        (op (car sequence)
            (accumulate op initial (cdr sequence)))))

  (define (enumerate-interval low high)
    (if (> low high)
        nil
        (cons low (enumerate-interval (+ low 1) high))))

  (define (enumerate-tree tree)
    (cond ((null? tree) nil)
          ((not (pair? tree)) (list tree))
          (else (append (enumerate-tree (car tree))
                        (enumerate-tree (cdr tree))))))

  (module+ test
    (let ([1-to-5 '(1 2 3 4 5)])
      (check-equal? (filter odd? 1-to-5) '(1 3 5))
      (check-equal? (filter-v2 odd? 1-to-5) '(1 3 5))
      (check-equal? (accumulate + 0 1-to-5) 15)
      (check-equal? (accumulate * 1 1-to-5) 120)
      (check-equal? (accumulate cons nil 1-to-5) 1-to-5)
      (check-equal? (enumerate-tree (list 1 (list 2 (list 3 4)) 5)) 1-to-5))
    (check-equal? (enumerate-interval 2 7) '(2 3 4 5 6 7))

    ;; In my implementation of fringe, I intentionaly keep track of nil leaves
    ;; this is not the case in enumerate-tree (this could be a design choice)
    (let ([tree '(1 2 (3 () 4) (5 6))])
      (check-not-equal? (fringe tree) (enumerate-tree tree))))

  (define (sum-odd-squares-signal tree)
    (accumulate + 0 (map square (filter odd? (enumerate-tree tree)))))

  (define (even-fibs-signal n)
    (accumulate cons nil (filter even? (map fib (enumerate-interval 0 n)))))

  (define (list-fib-squares n)
    (accumulate cons nil (map square (map fib (enumerate-interval 0 n)))))

  (define (product-of-squares-of-odd-elements sequence)
    (accumulate * 1 (map square (filter odd? sequence))))

  (module+ test
    (check-equal? (even-fibs-signal 20) (even-fibs 20))
    (let ([tree '(1 2 (3 4 5) (6))])
      (check-equal? (sum-odd-squares-signal tree) (sum-odd-squares tree)))
    (check-equal? (list-fib-squares 10) '(0 1 1 4 9 25 64 169 441 1156 3025))
    (check-equal?(product-of-squares-of-odd-elements (list 1 2 3 4 5)) 225))

  ;; here it would probably be better to use a dict
  (define (make-record name type salary)
    (list name type salary))

  (define (salary record)
    (caddr record))

  (define (programmer? record)
    (equal? (cadr record) "programmer"))

  (define (salary-of-highest-paid-programmer records)
    (accumulate max 0 (map salary (filter programmer? records))))

  (module+ test
    (define records (list (make-record "Dimitar" "programmer" 1)
                          (make-record "Chika" "CEO" 2)
                          (make-record "Elena" "PO" 3)
                          (make-record "Marina" "HR" 4)
                          (make-record "Other" "programmer" 5)))

    (check-equal? (salary-of-highest-paid-programmer records) 5)))

(module Exercise/2.33 sicp
  (#%require (only racket/base module+ λ)
             (only (submod "sicp1.rkt" common-utils) square)
             (only (submod ".." Section/2.2.3) accumulate))

  (define (map p sequence)
    (accumulate (λ (x y) (cons (p x) y)) nil sequence))

  (define (append seq1 seq2)
    (accumulate cons seq2 seq1))

  (define (length sequence)
    (accumulate (λ (x length-tail) (+ 1 length-tail)) 0 sequence))

  (module+ test
    (#%require rackunit)
    (display "--> Exercise/2.33\n")

    (let ([1-to-5 '(1 2 3 4 5)])
      (check-equal? (map square 1-to-5) '(1 4 9 16 25))
      (check-equal? (append '(1 2 3) '(4 5)) 1-to-5)
      (check-equal? (length 1-to-5) 5))))

(module Exercise/2.34 sicp
  (#%require (only racket/base module+ λ)
             (only (submod ".." Section/2.2.3) accumulate))

  (define (horner-eval x coefficient-sequence)
    (accumulate (λ (this-coeff higher-terms)
                  (+ this-coeff
                     (* higher-terms
                        x)))
                0
                coefficient-sequence))

  (module+ test
    (#%require rackunit)
    (display "--> Exercise/2.34\n")

    (check-equal? (horner-eval 2 (list 1 3 0 5 0 1)) 79)))

(module Exercise/2.35 sicp
  (#%require (only racket/base module+ λ)
             (only (submod ".." Section/2.2.3) accumulate enumerate-tree)
             (only (submod ".." Exercise/2.24) count-leaves))

  ;; see scale-tree-map in Section/2.2.2/mapping-over-trees
  (define (count-leaves-signal-v1 t)
    (accumulate + 0 (map (λ (sub-tree)
                           (if (pair? sub-tree)
                               (count-leaves-signal-v1 sub-tree)
                               1))
                         t)))

  (define (count-leaves-signal-v2 t)
    (accumulate + 0 (map (λ (x) 1) (enumerate-tree t))))

  (module+ test
    (#%require rackunit)
    (display "--> Exercise/2.35\n")

    (let* ([x '((1 2 (3)) 4 (5 6))]
           [xx (list x x)]
           [res (count-leaves xx)])
      (check-equal? (count-leaves-signal-v1 xx) res)
      (check-equal? (count-leaves-signal-v2 xx) res))))

(module Exercise/2.36 sicp
  (#%provide accumulate-n)
  (#%require (only racket/base module+)
             (only (submod ".." Section/2.2.3) accumulate))

  (define (accumulate-n op init seqs)
    (if (null? (car seqs))
        nil
        (cons (accumulate op init (map car seqs))
              (accumulate-n op init (map cdr seqs)))))

  (module+ test
    (#%require rackunit)
    (display "--> Exercise/2.36\n")

    (let ([seqs '((1 2 3) (4 5 6) (7 8 9) (10 11 12))]
          [res '(22 26 30)])
      (check-equal? (accumulate-n + 0 seqs) res))))

(module Exercise/2.37 sicp
  (#%require (only racket/base module+ λ exn:fail?)
             (only racket/format ~a)
             (only racket/math exact-round)
             (only (submod ".." Section/2.2.3) accumulate)
             (only (submod ".." Exercise/2.28) fringe)
             (only (submod ".." Exercise/2.36) accumulate-n))

  (define (show-mat mat)
    (define (max-element mat) (accumulate max -inf.0 (fringe mat)))
    ;; assume integer coefficients
    (define (count-digits n) (exact-round (+ 1 (floor (log n 10)))))
    (for-each (λ (row)
                (display "|")
                (for-each (λ (x)
                            (display
                             (~a x
                                 #:min-width (inc (count-digits (max-element mat)))
                                 #:align 'right
                                 #:left-pad-string " ")))
                          row)
                (display " |")
                (newline))
              mat))

  (define (dot-product v w)
    (accumulate + 0 (map * v w)))

  (define (matrix-*-vector m v)
    (map (λ (w) (dot-product v w)) m))

  (define (transpose mat)
    (accumulate-n cons nil mat))

  (define (matrix-*-matrix m n)
    (let ([n.T (transpose n)])
      (map (λ (x) (matrix-*-vector n.T x)) m)))

  (module+ test
    (#%require rackunit)
    (display "--> Exercise/2.37\n")

    (define A '((1 2 3 4) (4 5 6 6) (6 7 8 9)))
    (define A.T '((1 4 6) (2 5 7) (3 6 8) (4 6 9)))
    (define I3 '((1 0 0) (0 1 0) (0 0 1)))
    (define I4 '((1 0 0 0) (0 1 0 0) (0 0 1 0) (0 0 0 1)))
    (show-mat I4)
    (show-mat A)
    (show-mat (transpose A))
    (show-mat (matrix-*-matrix A A.T))

    (let ([x '(1 2 3 4)])
      (check-equal? (dot-product x x) 30)
      (check-equal? (matrix-*-vector A x)
                    (list (dot-product (car A) x)
                          (dot-product (cadr A) x)
                          (dot-product (caddr A) x)))
      (check-equal? (transpose A) A.T))
    (check-equal? (matrix-*-matrix A A.T) '((30 56 80)
                                            (56 113 161)
                                            (80 161 230)))
    (check-equal? (matrix-*-matrix A.T I3) A.T)
    (check-equal? (matrix-*-matrix I4 A.T) A.T)
    (check-equal? (matrix-*-matrix A I4) A)
    (check-equal? (matrix-*-matrix I3 A) A)
    (check-exn
     exn:fail?
     (λ () (matrix-*-matrix A A)))))

(module Exercise/2.38 sicp
  (#%provide fold-right
             fold-left)
  (#%require (only racket/base module+)
             ;; (rename raw-module-path local-id exported-id)
             (rename (submod ".." Section/2.2.3) fold-right accumulate))

  (define (fold-left op initial sequence)
    (define (iter result rest)
      (if (null? rest)
          result
          (iter (op result (car rest))
                (cdr rest))))
    (iter initial sequence))

  (module+ test
    (#%require rackunit)
    (display "--> Exercise/2.38\n")

    (check-equal? (fold-right / 1 (list 1 2 3)) (/ 1 (/ 2 3))) ; 1/(2/(3/1)) -> 1/(2/3) -> 3/2
    (check-equal? (fold-left / 1 (list 1 2 3)) (/ (/ 1 2) 3)) ; ((1/1)/2)/3 -> (1/2)/3
    (check-equal? (fold-right list nil (list 1 2 3)) '(1 (2 (3 ()))))
    (check-equal? (fold-left list nil (list 1 2 3)) '(((() 1) 2) 3)))

  ;; fold-right and fold-left would produce the same result if (op x y) = (op y x)
  (module+ test
    (let ([x (list 1 2 3)])
      (check-equal? (fold-right * 1 x)
                    (fold-left * 1 x))
      (check-equal? (fold-right + 1 x)
                    (fold-left + 1 x)))))

(module Exercise/2.39 sicp
  (#%require (only racket/base module+ λ)
             (only (submod ".." Exercise/2.38) fold-right fold-left))

  (define (reverse-fold-right sequence)
    (fold-right (λ (x y) (append y (list x))) nil sequence))

  (define (reverse-fold-left sequence)
    (fold-left (λ (x y) (cons y x)) nil sequence))

  (module+ test
    (#%require rackunit)
    (display "--> Exercise/2.39\n")

    (let ([1-to-5 '(1 2 3 4 5)]
          [5-to-1 '(5 4 3 2 1)])
      (check-equal? (reverse-fold-right 1-to-5) 5-to-1)
      (check-equal? (reverse-fold-left 1-to-5) 5-to-1))))

(module Section/2.2.3/nested-mapings sicp
  (#%provide flatmap
             prime-sum?
             make-pair-sum
             prime-sum-pairs)
  (#%require (only racket/base module+ λ)
             (only (submod "sicp1.rkt" Exercise/1.22) prime?)
             (only (submod ".." Section/2.2.3) accumulate filter enumerate-interval))

  (define (flatmap proc seq)
    (accumulate append nil (map proc seq)))

  (define (prime-sum? pair)
    (prime? (+ (car pair)
               (cadr pair))))

  (define (make-pair-sum pair)
    (let ([first (car pair)]
          [last (cadr pair)])
      (list first
            last
            (+ first last))))

  (define (prime-sum-pairs n)
    (map make-pair-sum
         (filter prime-sum? (flatmap
                             (λ (i)
                               (map (λ (j) (list i j))
                                    (enumerate-interval 1 (- i 1))))
                             (enumerate-interval 1 n)))))

  (module+ test
    (#%require rackunit)
    (display "--> Section/2.2.3/nested-mapings\n")

    (define (show x) (display x) (newline))

    ;; two for loops using nested maps
    (for-each show
              (map (λ (i)
                     (map (λ (j) (cons i j))
                          '(4 5)))
                   '(1 2 3)))

    (let ([res '((2 1 3) (3 2 5) (4 1 5) (4 3 7) (5 2 7) (6 1 7) (6 5 11))])
      (check-equal? (prime-sum-pairs 6) res)
      (for-each show res)))

  (define (remove item sequence)
    (filter (λ (x) (not (= x item)))
            sequence))

  (define (permutations s)
    (if (null? s)
        (list nil)
        (flatmap (λ (x)
                   (map (λ (other-permutations)
                          (cons x other-permutations))
                        (permutations (remove x s))))
                 s)))

  (module+ test
    (check-equal? (remove 3 '(1 2 3 4 5)) '(1 2 4 5))
    (let ([res '((1 2 3) (1 3 2) (2 1 3) (2 3 1) (3 1 2) (3 2 1))])
      (check-equal? (permutations '(1 2 3)) res)
      (for-each show res))))

(module Exercise/2.40 sicp
  (#%require (only racket/base module+ λ)
             (only (submod ".." Section/2.2.3) filter enumerate-interval)
             (only (submod ".." Section/2.2.3/nested-mapings)
                   flatmap
                   prime-sum?
                   make-pair-sum
                   prime-sum-pairs))

  (define (unique-pairs n)
    (flatmap
     (λ (i)
       (map (λ (j) (list i j))
            (enumerate-interval 1 (- i 1))))
     (enumerate-interval 1 n)))

  (define (prime-sum-pairs-updated n)
    (map make-pair-sum (filter prime-sum? (unique-pairs n))))

  (module+ test
    (#%require rackunit)
    (display "--> Exercise/2.40\n")

    (let ([n 6])
      (check-equal? (prime-sum-pairs-updated n) (prime-sum-pairs n)))))

(module Exercise/2.41 sicp
  (#%require (only racket/base module+ λ)
             (only (submod ".." Section/2.2.3) filter accumulate enumerate-interval)
             (only (submod ".." Section/2.2.3/nested-mapings) flatmap))

  (define (ordered-triplets n)
    (flatmap (λ (i)
               (flatmap (λ (j)
                          (map (λ (k)
                                 (list k j i))
                               (enumerate-interval 1 (- j 1))))
                        (enumerate-interval 1 (- i 1))))
             (enumerate-interval 1 n)))

  (define (filter-sum-to s sequence-of-triplets)
    (filter (λ (triplet) (= (accumulate + 0 triplet) s))
            sequence-of-triplets))

  (define (ordered-triplets-sum-to-s n s)
    (filter-sum-to s (ordered-triplets n)))

  (module+ test
    (#%require rackunit)
    (display "--> Exercise/2.41\n")

    ;; show all triplets with their sum (to verify results)
    (for-each (λ (item) (display item) (newline))
              (map
               (λ (x) (cons (accumulate + 0 x) x))
               (ordered-triplets 6)))

    (let* ([n 6]
           [s (* 2 n)]
           [res '((3 4 5) (2 4 6) (1 5 6))])
      (check-equal? (ordered-triplets-sum-to-s n s) res))))

(module Exercise/2.42 sicp
  (#%provide empty-board
             safe?
             adjoin-position
             queens)
  (#%require (only racket/base module+ λ)
             (only (submod ".." Section/2.2.3) filter accumulate enumerate-interval)
             (only (submod ".." Section/2.2.3/nested-mapings) flatmap))

  (define empty-board '())
  ;; column and row indexes are in [1 board-size]
  (define (make-cell row col) (cons row col))
  (define (cell-row cell) (car cell))
  (define (cell-col cell) (cdr cell))

  ;; return true if the two cells share a row, a column or are on the same diagonal
  (define (visible-cells cell-a cell-b)
    (let ([re (cell-row cell-a)]
          [rn (cell-row cell-b)]
          [ce (cell-col cell-a)]
          [cn (cell-col cell-b)])
      (or (= rn re)
          (= cn ce)
          (= (+ rn cn)
             (+ re ce))
          (= (- rn cn)
             (- re ce)))))

  (define (adjoin-position new-row k rest-of-queens)
    (cons (make-cell new-row k)
          rest-of-queens))

  ;; I don't use the column index k (but keep it in the function signature)
  (define (safe? k board)
    (let ([head-cell (car board)]
          [tail-cells (cdr board)])
      (accumulate (λ (x y) (and x y)) #t
                  (map (λ (cell)
                         (not (visible-cells cell head-cell)))
                       tail-cells))))

  (define (queens board-size)
    (define (queen-cols k)
      (if (= k 0)
          (list empty-board)
          (filter
           (λ (positions)
             (safe? k positions))
           (flatmap
            (λ (rest-of-queens)
              (map (λ (new-row)
                     (adjoin-position
                      new-row k rest-of-queens))
                   (enumerate-interval 1 board-size)))
            (queen-cols (- k 1))))))
    (queen-cols board-size))

  (define (all seq)
    (accumulate
     (λ (x y) (and x y))
     #t
     seq))

  (define queens-dimensions (enumerate-interval 1 10))
  ;; https://en.wikipedia.org/wiki/Eight_queens_puzzle#Exact_enumeration
  (define queens-numb-solutions '(1 0 0 2 10 4 40 92 352 724))
  (define queens-6-solutions
    '(((5 . 6) (3 . 5) (1 . 4) (6 . 3) (4 . 2) (2 . 1))
      ((4 . 6) (1 . 5) (5 . 4) (2 . 3) (6 . 2) (3 . 1))
      ((3 . 6) (6 . 5) (2 . 4) (5 . 3) (1 . 2) (4 . 1))
      ((2 . 6) (4 . 5) (6 . 4) (1 . 3) (3 . 2) (5 . 1))))

  (module+ test
    (#%require rackunit)
    (display "--> Exercise/2.42\n")

    (check-true (visible-cells (make-cell 1 5) (make-cell 1 3)))
    (check-true (visible-cells (make-cell 1 5) (make-cell 3 5)))
    (check-true (visible-cells (make-cell 1 1) (make-cell 5 5)))
    (check-true (visible-cells (make-cell 1 3) (make-cell 3 1)))
    (check-false (visible-cells (make-cell 1 3) (make-cell 2 5)))

    (check-equal? (queens 6) queens-6-solutions)
    (check-true
     (all
      (map (λ (n m)
             (= (length (queens n)) m))
           queens-dimensions
           queens-numb-solutions))))

  #|
  test-my-version contains my implementation before seeing the code provided with the
  exercise (I have factored out some common parts post factum)
  |#
  (module+ test-my-version
    (#%require rackunit)
    (display "--> Exercise/2.42 (my version)\n")

    (define (make-empty-board n)
      ;; (cons size list-of-cells-with-queens)
      (cons n empty-board))
    (define (board-size board) (car board))
    (define (board-cells board) (cdr board))
    (define (add-cell board cell) (cons (board-size board)
                                        (cons cell (board-cells board))))

    ;# return #t if the new-cell threatens any of the existing queens on the board
    (define (threatens? new-cell board)
      #|
      I have to pass (λ (x y) (or x y)) because or/and are "syntactic forms", see:
      https://docs.racket-lang.org/guide/conditionals.html#(part._and+or)
      https://stackoverflow.com/a/5859945
      A prodecure is basically a value that can be used in any way we want but a
      syntactic form cannot be passed as an argument to a higher-order function. That is
      why we cannot do (map and ...).
      |#
      (accumulate (λ (x y) (or x y)) #f
                  (map (λ (cell)
                         (visible-cells cell new-cell))
                       (board-cells board))))

    (define (feasible-board? board)
      (not (null? (car (board-cells board)))))

    (define (add-new-col boards col)
      (flatmap (λ (board)
                 (filter feasible-board?
                         (map (λ (row)
                                (let ([new-cell (make-cell row col)])
                                  (add-cell board
                                            (if (threatens? new-cell board)
                                                nil
                                                new-cell))))
                              (enumerate-interval 1 (board-size board)))))
               boards))

    (define (queens n)
      (define (queens-helper col)
        (cond [(= col 1) (add-new-col (list (make-empty-board n)) 1)]
              [else (add-new-col (queens-helper (- col 1)) col)]))
      (queens-helper n))

    (check-equal? (map (λ (board) (board-cells board))
                       (queens 6))
                  queens-6-solutions)
    (check-true
     (all
      (map (λ (n m)
             (= (length (queens n)) m))
           queens-dimensions
           queens-numb-solutions)))))

(module Exercise/2.43 sicp
  (#%require (only racket/base module+ λ format)
             (only (submod ".." Section/2.2.3) filter enumerate-interval)
             (only (submod ".." Section/2.2.3/nested-mapings) flatmap)
             (only (submod ".." Exercise/2.18) reverse)
             (only (submod ".." Exercise/2.42)
                   empty-board
                   safe?
                   adjoin-position
                   queens))

  #|
  In queens-inverted, queen-cols is recursively evaluated for every row starting from
  the current column. This is inefficient.

  For the original implementation the time to execute (queen-cols k) is approximately
  given by T[k] = T[k-1] + N*H[k], where N = 8 is the board size, k ranges from 1 to N
  and H[k] is a term depending on the number of solutions maintained up to column k (see
  queen-cols-original-8 and H below). Summing up over k gives:
  T[8] = N*H[1] + ... + N*H[8] assuming that T[0] = 0 (see effort-original).

  For the inverted procedure the time to execute (queen-cols k) is approximately given
  by I[k] = N*I[k-1] + H[k] - N, where subtracting N models the computations we save by
  not having to call enumerate-interval for each maintained solution. Summing up over k
  gives: I[8] = N^7*(H[1] - N) + N^6*(H[2] - N) + ... + N*(H[7] - N) + (H[8] - N), where
  I[0] = T[0] = 0 is assumed (see effort-inverted).
  |#
  (define (queens-inverted board-size)
    (define (queen-cols k)
      (if (= k 0)
          (list empty-board)
          (filter
           (λ (positions)
             (safe? k positions))
           (flatmap
            (λ (new-row)
              (map (λ (rest-of-queens)
                     (adjoin-position new-row k rest-of-queens))
                   (queen-cols (- k 1))))
            (enumerate-interval 1 board-size)))))
    (queen-cols board-size))

  (define board-size 8)
  ;; extract the original implementation of queen-cols for an 8x8 board
  (define (queen-cols-original-8 k)
    (if (= k 0)
        (list empty-board)
        (filter
         (λ (positions)
           (safe? k positions))
         (flatmap
          (λ (rest-of-queens)
            (map (λ (new-row)
                   (adjoin-position
                    new-row k rest-of-queens))
                 (enumerate-interval 1 board-size)))
          (queen-cols-original-8 (- k 1))))))

  ;; number of maintained solutions (see note above)
  (define H (map (λ (n)
                   (length (queen-cols-original-8 n)))
                 (enumerate-interval 1 8)))

  (define (effort-original n H)
    (if (= n 0)
        0
        (+ (* board-size (car H))
           (effort-original (- n 1)
                            (cdr H)))))

  (define (effort-inverted n H)
    (if (= n 0)
        0
        (- (+ (car H)
              (* board-size (effort-inverted (- n 1)
                                             (cdr H))))
           board-size)))

  (module+ test
    (#%require rackunit)
    (display "--> Exercise/2.43\n")

    ;; show solutions we maintain at a given column for an 8x8 board
    (for-each (λ (k h)
                (display (format "[column ~a] ~a\n" k h)))
              (enumerate-interval 1 board-size)
              H)

    (define (get-time queens-procedure n)
      (let ([start-time (runtime)])
        (queens-procedure n)
        (* (- (runtime) start-time)
           1e-6)))

    (let ([n 6])
      (check-equal?
       (queens n)
       (reverse (queens-inverted n))))

    (display
     (format "The inverted version is expected to be ~a times slower.\n"
             (round (/ (effort-inverted board-size (reverse H))
                       (effort-original board-size (reverse H))))))

    ;; for n = 8, empirically queens-inverted is ~1000 times slower than queens
    (let ([n 6]) ; I keep n = 6 here because I don't want to wait too much in the tests
      (display (format "[n: ~a] empirical check: ~a\n" n
                       (/ (get-time queens-inverted n)
                          (get-time queens n)))))))

(module+ test
  (require (submod ".." Exercise/2.1 test)
           (submod ".." Exercise/2.2 test)
           (submod ".." Exercise/2.3 test)
           (submod ".." Exercise/2.3 test-representation-1)
           (submod ".." Exercise/2.3 test-representation-2)
           (submod ".." Exercise/2.4 test)
           (submod ".." Exercise/2.5 test)
           (submod ".." Exercise/2.6 test)
           (submod ".." Exercise/2.7 test)
           (submod ".." Exercise/2.8 test)
           (submod ".." Exercise/2.9 test)
           (submod ".." Exercise/2.10 test)
           (submod ".." Exercise/2.11 test)
           (submod ".." Exercise/2.12 test)
           (submod ".." Exercise/2.13 test)
           (submod ".." Exercise/2.14 test)
           ;; 2.15: no tests
           ;; 2.16: no tests
           (submod ".." Section/2.2.1 test)
           (submod ".." Exercise/2.17 test)
           (submod ".." Exercise/2.18 test)
           (submod ".." Exercise/2.19 test)
           (submod ".." Exercise/2.20 test)
           (submod ".." Exercise/2.21 test)
           (submod ".." Exercise/2.22 test)
           (submod ".." Exercise/2.23 test)
           (submod ".." Exercise/2.24 test)
           (submod ".." Exercise/2.24 test-box-and-pointer)
           (submod ".." Exercise/2.25 test)
           (submod ".." Exercise/2.26 test)
           (submod ".." Exercise/2.27 test)
           (submod ".." Exercise/2.28 test)
           (submod ".." Exercise/2.29 test)
           (submod ".." Section/2.2.2/mapping-over-trees test)
           (submod ".." Exercise/2.30 test)
           (submod ".." Exercise/2.31 test)
           (submod ".." Exercise/2.32 test)
           (submod ".." Section/2.2.3 test)
           (submod ".." Exercise/2.33 test)
           (submod ".." Exercise/2.34 test)
           (submod ".." Exercise/2.35 test)
           (submod ".." Exercise/2.36 test)
           (submod ".." Exercise/2.37 test)
           (submod ".." Exercise/2.38 test)
           (submod ".." Exercise/2.39 test)
           (submod ".." Section/2.2.3/nested-mapings test)
           (submod ".." Exercise/2.40 test)
           (submod ".." Exercise/2.41 test)
           (submod ".." Exercise/2.42 test)
           (submod ".." Exercise/2.42 test-my-version)
           (submod ".." Exercise/2.43 test)))
