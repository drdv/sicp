#lang racket/base

(require racket/class)

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
(#%require sdraw)
(sdraw (list 1) #:null-style '/)
;; sdraw cannot seem to draw mutable cons (and I cannot use with with SICP)
(sdraw (mcons 1 2) #:null-style '/)

(define c (mcons 1 2))
(define (mcons->cons mc)
  (cond [(null? mc) '()]
        [(number? mc) mc]
        [else (cons (mcons->cons (mcar mc))
                    (mcons->cons (mcdr mc)))]))

(mcons 1 (mcons 2 (mcons 3 '())))
(mcons->cons (mcons 1 (mcons 2 (mcons 3 '()))))

(sdraw (mcons->cons c) #:null-style '/)
(sdraw (mcons->cons (mcons 1 (mcons 2 (mcons 3 '())))) #:null-style '/)
;; ===============================================================
;; https://stackoverflow.com/a/36915357
(define (welcome #:first [first-name "Anonymous"]
                 #:last [last-name "Person"])
  (display (string-append "Welcome, " first-name " " last-name "!")))

(welcome #:last "Doe")
;; ===============================================================
  (define book-class%
    (class object%
      (field (pages 5))
      (define/public (letters)
        (* pages 500))
      (define/public (set-pages n)
        (set! pages n))
      (super-new)))

  (define b
    (new book-class%))
  (send b letters)
  (send b set-pages 6)
  (send b letters)
  (get-field pages b)
  (set-field! pages b 7)
  (send b letters)
;; ===============================================================
(module M1 racket/base
  (provide data1)
  (define data1 1)

  (module M2 racket/base
    (provide data2)
    (define data2 2))

  ;; this needs to be module* to require from the enclosing module
  (module* M3 racket/base
    (#%require (only (submod ".." M2) data2))
    (#%require (only (submod "..") data1))

    (display data1)
    (display data2)))
