;; Example: set! modifies a variable through the reference in its closure.
;; (docs.racket-lang.org/guide/set_.html#(part._using-set!))
#lang racket
(require rackunit)

#|
We have a closure that contains a reference to the local variable defined in
(let ([n 0]) ...). When the let goes out of scope, this reference keeps the variable n
alive (here I am not sure about the exact mechanics, because n is on the stack and the
stack will be dropped, so probably the variable is copied elsewhere and then a reference
to it is created ... I don't know). Anyway, set! keeps incrementing the variable through
its reference.
|#
(define next-number!
  (let ([n 0])
    (lambda ()
      (set! n (add1 n))
      n)))

(define numb-increments 5)
(define (show-results next [state 'undefined])
  (display
   (format "~a: state: ~a, eq?: ~a\n"
           next
           state
           (eq? next state))))

;; Every execution of the lambda associated with next-number! increments our counter
(display "--------------------------\n")
(for ([_ (in-range numb-increments)])
  (let ([next (next-number!)])
    (show-results next)))

#|
Below is a modified version of next-number! that returns as well the state variable.
Note that I need to have it in a box otherwise a copy is returned by (values ...).
|#
(define-values (next-number-box! STATE)
  (let ([boxed-n (box 0)])
    (values (lambda ()
              (let ([n (unbox boxed-n)])
                (set-box! boxed-n (add1 n))
                boxed-n))
            boxed-n)))

(display "--------------------------\n")
(for ([_ (in-range numb-increments)])
  (let ([next (next-number-box!)])
    (show-results next STATE)))

;; Another modification: using a global variable to store the state
(define GLOBAL-STATE 0)
(define next-number-global!
  (lambda ()
    (set! GLOBAL-STATE (add1 GLOBAL-STATE))
    GLOBAL-STATE))

(display "--------------------------\n")
(for ([_ (in-range numb-increments)])
  (let ([next (next-number-global!)])
    (show-results next GLOBAL-STATE)))
