#lang racket
(require rackunit)

;; Three approaches for implementing the same recursion, see
;; docs.racket-lang.org/guide/let.html#(part._.Named_let)
(let ([input-lst '("a" "b" "c")]
      [index 1])

  ;; essentially, here we save one line (the function call at the end)
  (define (duplicate-named-let pos lst)
    (let dup ([k 0]
              [l lst])
      (cond
        [(= k pos) (cons (car l) l)]
        [else (cons (car l) (dup (add1 k) (cdr l)))])))

  (define (duplicate-letrec pos lst)
    (letrec ([dup
              (lambda ([k 0]
                       [l  lst])
                (cond
                  [(= k pos) (cons (car l) l)]
                  [else (cons (car l) (dup (add1 k) (cdr l)))]))])
      (dup)))

  (define (duplicate-define pos lst)
    (define (dup [k 0]
                 [l lst])
      (cond
        [(= k pos) (cons (car l) l)]
        [else (cons (car l) (dup (add1 k) (cdr l)))]))
    (dup))

  (let ([res-named-let (duplicate-named-let index input-lst)]
        [res-letrec (duplicate-letrec index input-lst)]
        [res-define (duplicate-define index input-lst)])
    (display res-named-let)
    (check-equal? res-named-let res-letrec)
    (check-equal? res-named-let res-define)))
