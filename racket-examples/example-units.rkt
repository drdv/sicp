#|
Simple example of using Units https://docs.racket-lang.org/guide/units.html
|#
#lang racket/base

(module my-units sicp
  (#%require (only racket/base λ))

  (#%provide
   ;; -----------------------------------------------------------
   ;; a library using the parameters^ signature as inputs
   ;; -----------------------------------------------------------
   parameters^
   parameterized-library-output^
   parameterized-library@
   ;; -----------------------------------------------------------
   ;; two implementations of the parameters^ signature
   ;; -----------------------------------------------------------
   parameters-v1@
   parameters-v2@
   ;; -----------------------------------------------------------
   ;; another library and its input/output signatures
   ;; -----------------------------------------------------------
   other-parameters^
   other-parameterized-library-output^
   other-parameterized-library@)
  (#%require (only racket/unit define-signature define-unit import export))

  ;; input parameters for the parameterized-library@ unit
  (define-signature parameters^ (f g))
  (define-unit parameters-v1@
    (import)
    (export parameters^)

    (define (f x) (* 2 x))
    (define (g x) (* 3 x)))

  (define-unit parameters-v2@
    (import)
    (export parameters^)

    (define (f x) (* 3 x))
    (define (g x) (* 4 x)))

  (define-signature parameterized-library-output^ (h))
  (define-unit parameterized-library@
    (import parameters^)
    (export parameterized-library-output^)

    (define h (λ (x) (+ (f x)
                        (g x)))))

  (define-signature other-parameters^ (w))
  (define-signature other-parameterized-library-output^ (q))

  (define-unit other-parameterized-library@
    (import other-parameters^)
    (export other-parameterized-library-output^)

    (define (q x) (w x))))

(module m1 racket
  (#%provide f g h p)
  (#%require (only racket/unit define-values/invoke-unit/infer)
             (only (submod ".." my-units)
                   parameters-v1@
                   parameterized-library@))
  (define-values/invoke-unit/infer parameters-v1@)
  (define-values/invoke-unit/infer parameterized-library@)
  (define (p x)
    (+ (h x) 1)))

(module m2 sicp
  (#%provide f g h p)
  (#%require (only racket/unit define-values/invoke-unit/infer)
             (only (submod ".." my-units)
                   parameters-v2@
                   parameterized-library@))
  (define-values/invoke-unit/infer parameters-v2@)
  (define-values/invoke-unit/infer parameterized-library@)
  (define (p x)
    (+ (h x) 1)))

(module m3 sicp
  (#%provide q)
  (#%require (only racket/unit define-values/invoke-unit/infer)
             (only (submod ".." my-units) other-parameterized-library@))

  ;; we can define the specification manually
  (define (w x) (+ x 1))
  (define-values/invoke-unit/infer other-parameterized-library@))

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
         (only-in (submod "." m3) q))

(f1 3)
(g1 3)
(h1 3)
(p1 3)

(f2 3)
(g2 3)
(h2 3)
(p2 3)

(q 3)
