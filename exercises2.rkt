;; =====================================================================================
;; Exercises in Chapter 2
;; =====================================================================================
#lang racket/base

;; (#%require (only (submod "exercises1.rkt" Exercise/1.20) gcd))

(module Exercise/2.1 sicp
  (#%require (only racket/base module+ format let-values exn:fail?))

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

;; (module Exercise/2.2 sicp
;;   (#%require (only racket/base module+))
;;   (module+ test
;;     (#%require rackunit)
;;     (display "==================== Exercise/2.2 ====================\n")

;;     ))

(module+ test
  (require (submod ".." Exercise/2.1 test)))
