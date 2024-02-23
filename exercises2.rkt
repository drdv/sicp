;; =====================================================================================
;; Exercises in Chapter 2
;; =====================================================================================
#lang racket/base

(module Exercise/2.1 sicp
  (#%require (only racket/base module+ format let-values exn:fail?))

  ;; there are simpler solutions but I wanted to learn how to use let-values
  (define (make-rat n d)
    (cond [(= d 0) (error "Not a number")]
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
    (display "==================== Exercise/2.1 ====================\n")

    (let* ([one-third (make-rat 1 3)]
           [res (add-rat one-third one-third)])
      (check-true (equal-rat? res (make-rat 2 3)))
      (print-rat res))

    (let* ([neg-neg (make-rat -1 -3)]
           [neg-pos (make-rat -1  3)]
           [pos-neg (make-rat  1 -3)]
           [neg-neg (make-rat -1 -3)])
      (check-equal? (car neg-neg)  1)
      (check-equal? (cdr neg-neg)  3)
      (check-equal? (car neg-pos) -1)
      (check-equal? (cdr neg-pos)  3)
      (check-equal? (car pos-neg) -1)
      (check-equal? (cdr pos-neg)  3)
      (check-equal? (car neg-neg)  1)
      (check-equal? (cdr neg-neg)  3))

    (check-exn
     exn:fail?
     (lambda () (make-rat 1 0)))))

(module Exercise/2.2 sicp
  (#%provide make-point
             x-point
             y-point
             make-segment
             start-segment
             end-segment)
  (#%require (only racket/base module+ format)
             (only (submod "exercises1.rkt" common-utils) average tolerance))

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
    (display "==================== Exercise/2.2 ====================\n")

    (let ([point (midpoint-segment (make-segment (make-point 1 2)
                                                 (make-point 3 4)))])
      (check-equal? (x-point point) 2.0)
      (check-equal? (y-point point) 3.0)
      (print-point point))))

(module Exercise/2.3 sicp
  (#%require (only racket/base module+ format)
             (only racket/math pi)
             (only (submod "exercises1.rkt" common-utils) square tolerance)
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
    (display "==================== Exercise/2.3 ====================\n")

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
    (display "-------------------> Exercise/2.3 (representation-1) \n")

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
    (display "-------------------> Exercise/2.3 (representation-2) \n")

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
  (#%require (only racket/base module+))

  (define (cons x y)
    (lambda (m) (m x y)))

  (define (car z)
    (z (lambda (p q) p)))

  (define (cdr z)
    (z (lambda (p q) q)))

  (module+ test
    (#%require rackunit)
    (display "==================== Exercise/2.4 ====================\n")

    #|
    (cons x y) -> (lambda (m) (m x y)) where x and y are stored in the closure
    (car (lambda (m) (m x y))) -> ((lambda (m) (m x y)) (lambda (p q) p))
                               -> ((lambda (p q) p) x y) -> x
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
    (display "==================== Exercise/2.5 ====================\n")

    (let* ([a 5]
           [b 7]
           [c (cons a b)])
         (check-equal? (car c) a)
         (check-equal? (cdr c) b))))

(module Exercise/2.6 sicp
  (#%require (only racket/base module+))

  (define zero (lambda (f) (lambda (x) x)))
  (define (add-1 n)
    (lambda (f) (lambda (x) (f ((n f) x)))))

  (module+ test
    (#%require rackunit)
    (display "==================== Exercise/2.6 ====================\n")

    #|
    (lambda (f) (lambda (x) (f ((zero f) x)))) ->
    (lambda (f) (lambda (x) (f ((lambda (x) x) x)))) ->
    (lambda (f) (lambda (x) (f x)))
    |#
    (define one (lambda (f) (lambda (x) (f x))))

    #|
    (lambda (f) (lambda (x) (f ((one f) x)))) ->
    (lambda (f) (lambda (x) (f (((lambda (f) (lambda (x) (f x))) f) x)))) ->
    (lambda (f) (lambda (x) (f (f x))))
    |#
    (define two (lambda (f) (lambda (x) (f (f x)))))

    (define three (lambda (f) (lambda (x) (f (f (f x))))))
    (define four (lambda (f) (lambda (x) (f (f (f (f x)))))))

    (define (church-add m n)
      (lambda (f) (lambda (x) ((m f) ((n f) x)))))

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

(module+ test
  (require (submod ".." Exercise/2.1 test))
  (require (submod ".." Exercise/2.2 test))
  (require (submod ".." Exercise/2.3 test)
           (submod ".." Exercise/2.3 test-representation-1)
           (submod ".." Exercise/2.3 test-representation-2))
  (require (submod ".." Exercise/2.4 test))
  (require (submod ".." Exercise/2.5 test))
  (require (submod ".." Exercise/2.6 test)))
