#lang racket/base

;; ===============================================================
(for/fold ([counter 0])
          ([element '(3 5 3 4 3 2 6 3)])
  (if (= element 3)
      (add1 counter)
      counter))

;; ===============================================================
(define dict (make-hash (list (cons 1 "a") (cons 2 "b"))))
(displayln dict)

(hash-set! dict 3 "c")
(displayln dict)

(hash-remove! dict 2)
(displayln dict)

(hash-ref dict 1)
;; ===============================================================
(let ([x 3]
      [y 11])
  (let-values ([(a b) (values x y)])
    (display (format "a: ~a, b: ~a" a b))))
;; ===============================================================
;; raco pkg install sdraw
;; (#%require sdraw)
;; (sdraw (list 1) #:null-style '/)
;; ===============================================================
