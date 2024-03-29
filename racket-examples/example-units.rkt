#|
Simple example of using Units https://docs.racket-lang.org/guide/units.html
|#
#lang racket/base

(module my-units sicp
  (#%provide procedure-parameters^
           ;; -----------------------------------------------------------
           ;; two implementations of the procedure-parameters^ signature
           ;; -----------------------------------------------------------
           procedure-parameters-v1@
           procedure-parameters-v2@
           ;; -----------------------------------------------------------
           ;; a library using the procedure-parameters^ signature
           ;; -----------------------------------------------------------
           parameterized-library^
           parameterized-library@
           ;; -----------------------------------------------------------
           ;; another library
           ;; -----------------------------------------------------------
           other-library@)
  (#%require (only racket/unit define-signature define-unit import export))

  (define-signature procedure-parameters^ (f g))
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

  (define-signature parameterized-library^ (h))
  (define-unit parameterized-library@
    (import procedure-parameters^)
    (export parameterized-library^)

    (define (h x) (+ (f x)
                     (g x))))

  (define-signature other-library-parameter^ (parameter-procedure))
  (define-signature other-library^ (f))

  (define-unit other-library@
    (import other-library-parameter^)
    (export other-library^)

    (define (f x) (parameter-procedure x))))

(module m1 racket
  (#%provide f g h p)
  (#%require (only racket/unit define-values/invoke-unit/infer)
             (only (submod ".." my-units)
                   procedure-parameters-v1@
                   parameterized-library@))
  (define-values/invoke-unit/infer procedure-parameters-v1@)
  (define-values/invoke-unit/infer parameterized-library@)
  (define (p x)
    (+ (h x) 1)))

(module m2 sicp
  (#%provide f g h p)
  (#%require (only racket/unit define-values/invoke-unit/infer)
             (only (submod ".." my-units)
                   procedure-parameters-v2@
                   parameterized-library@))
  (define-values/invoke-unit/infer procedure-parameters-v2@)
  (define-values/invoke-unit/infer parameterized-library@)
  (define (p x)
    (+ (h x) 1)))

(module m3 sicp
  (#%provide f)
  (#%require (only racket/unit define-values/invoke-unit/infer)
             (only (submod ".." my-units) other-library@))

  ;; we can define the specification manually
  (define (parameter-procedure x) (+ x 1))
  (define-values/invoke-unit/infer other-library@))

(require (rename-in (submod "." m1)
                    [p p1]
                    [f f1]
                    [g g1]
                    [h h1])
         (rename-in (submod "." m2)
                    [p p2]
                    [f f2]
                    [g g2]
                    [h h2])
         (rename-in (submod "." m3)
                    [f f3]))

(f1 3)
(g1 3)
(h1 3)
(p1 3)

(f2 3)
(g2 3)
(h2 3)
(p2 3)

(f3 3)
