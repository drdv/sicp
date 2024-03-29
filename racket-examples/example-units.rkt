;; Simple example of using Units https://docs.racket-lang.org/guide/units.html
#lang racket

(module my-units racket
  (provide procedure-parameters^
           ;; -----------------------------------------------------------
           ;; two implementations of the procedure-parameters^ signature
           ;; -----------------------------------------------------------
           procedure-parameters-v1@
           procedure-parameters-v2@
           ;; -----------------------------------------------------------
           ;; a library using the procedure-parameters^ signature
           ;; -----------------------------------------------------------
           parameterized-library^
           parameterized-library@)

  (define-signature procedure-parameters^
    (f g))

  (define-unit procedure-parameters-v1@
    (import)
    (export procedure-parameters^)

    (define (f x) (* 2 x))
    (define (g x) (* 3 x)))

  (define-unit procedure-parameters-v2@
    (import)
    (export procedure-parameters^)

    (define (f x) (* 3 x))
    (define (g x) (* 4 x)))

  (define-signature parameterized-library^
    (h))

  (define-unit parameterized-library@
    (import procedure-parameters^)
    (export parameterized-library^)

    (define (h x) (+ (f x)
                     (g x)))))

(module m1 racket
  (provide f g h p)
  (require (only-in (submod ".." my-units)
                    procedure-parameters-v1@
                    parameterized-library@))
  (define-values/invoke-unit/infer procedure-parameters-v1@)
  (define-values/invoke-unit/infer parameterized-library@)
  (define (p x)
    (+ (h x) 1)))

(module m2 racket
  (provide f g h p)
  (require (only-in (submod ".." my-units)
                    procedure-parameters-v2@
                    parameterized-library@))
  (define-values/invoke-unit/infer procedure-parameters-v2@)
  (define-values/invoke-unit/infer parameterized-library@)
  (define (p x)
    (+ (h x) 1)))

(require (rename-in (submod "." m1)
                    [p p1]
                    [f f1]
                    [g g1]
                    [h h1])
         (rename-in (submod "." m2)
                    [p p2]
                    [f f2]
                    [g g2]
                    [h h2]))

(f1 3)
(g1 3)
(h1 3)
(p1 3)

(f2 3)
(g2 3)
(h2 3)
(p2 3)
