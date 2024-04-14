;; =====================================================================================
;; Exercises in Chapter 2 (part 4)
;; =====================================================================================
#lang racket/base

(module Exercise/2.87 sicp
  (#%provide make-polynomial
             make-poly
             make-term
             variable
             term-list
             the-empty-termlist
             empty-termlist?
             order
             coeff
             adjoin-term
             first-term
             rest-terms
             filter-terms
             map-terms
             add-terms
             same-variable?
             equ?
             ;; ------------------
             install-polynomial-package
             install-generic-arithmetic-package-polynomial-zero
             install-tower-of-types-raise-complex
             install-generic-arithmetic-package-equality-polynomials
             install-tower-of-types-drop-polynomial)
  (#%require (only racket/base module+ λ exn:fail?)
             (only (submod "sicp1.rkt" common-utils) tolerance)
             (only (submod "sicp2_part2.rkt" Example/symbolic-differentiation)
                   variable?)
             (only (submod "sicp2_part3.rkt" Exercise/2.83) raise)
             (only (submod "sicp2_part3.rkt" Exercise/2.84) find-arg-with-highest-type)
             (only (submod "sicp2_part3.rkt" Exercise/2.85) drop project)
             (only (submod "sicp2_part3.rkt" Exercise/2.86)
                   attach-tag
                   put
                   get
                   contents
                   clear-op-type-table
                   ;; --------------
                   add
                   mul
                   make-rational
                   make-complex-from-real-imag
                   make-complex-from-mag-ang
                   ;; --------------
                   real-part
                   imag-part
                   ;; --------------
                   equ?
                   approx-equ?
                   =zero?
                   ;; --------------
                   install-generic-arithmetic-package
                   install-generic-arithmetic-package-equality
                   install-generic-arithmetic-package-zero
                   install-racket-integers-package
                   install-tower-of-types-raise
                   install-tower-of-types-drop
                   install-functions-of-racket-number))

  #|
  I cannot reuse the same-variable? procedure from Example/symbolic-differentiation
  because I want to coerce a number to a polynomial so that I can e.g., add y^2 + 1 and
  2. When I coerce the number 2 to the polynomial 2 I have to have a "special" variable
  that doesn't collide with y (in this example).
  |#
  (define (same-variable? v1 v2)
    (and (variable? v1) (variable? v2)
         (or (eq? v1 v2)
             (eq? v1 'no-variable)
             (eq? v2 'no-variable))))

  (define (make-poly variable term-list) (cons variable term-list))
  (define (variable p) (car p))
  (define (term-list p) (cdr p))

  (define (make-term order coeff) (list order coeff))
  (define (order term) (car term))
  (define (coeff term) (cadr term))

  (define (the-empty-termlist) '())
  (define (empty-termlist? term-list) (null? term-list))
  (define (first-term term-list) (car term-list))
  (define (rest-terms term-list) (cdr term-list))
  (define (adjoin-term term term-list)
    (let ([next-order (if (empty-termlist? term-list)
                          0
                          (+ (order (first-term term-list)) 1))])
      (if (< (order term) next-order) ; verify assumption see footnote 59 (page 282)
          (error "Order to new term should be at least" next-order)
          (if (=zero? (coeff term))
              term-list
              (cons term term-list)))))

  ;; ---------------------------------------------------------------------------------

  #|
  I shouldn't use directly filter and map when working with polynomial terms because
  they use cons, car, cdr, null?, while I have access to the terms only through the data
  abstraction: first-term, rest-terms, empty-termlist?, adjoin-term.
  |#

  ;; see filter in Section/2.2.3
  (define (filter-terms predicate terms)
    (cond [(empty-termlist? terms) (the-empty-termlist)]
          [(predicate (first-term terms))
           (adjoin-term (first-term terms)
                        (filter-terms predicate (rest-terms terms)))]
          [else (filter-terms predicate (rest-terms terms))]))

  #|
  See map in Section/2.2.1. I assume that the `proc` argument is such that the orders of
  terms are not modified (or at least are kept in decreasing order). An example of a
  "valid" `proc` is (λ (x) (make-term (order x) (negate (coeff x)))) - it simply
  negates the coefficient of each term but doesn't modify its order (i.e., power).
  |#
  (define (map-terms proc terms)
    (if (empty-termlist? terms)
        (the-empty-termlist)
        (adjoin-term (proc (first-term terms))
                     (map-terms proc (rest-terms terms)))))

  ;; ---------------------------------------------------------------------------------

  (define (add-terms L1 L2)
    (cond [(empty-termlist? L1) L2]
          [(empty-termlist? L2) L1]
          [else (let ([t1 (first-term L1)]
                      [t2 (first-term L2)])
                  (cond [(> (order t1) (order t2))
                         (adjoin-term t1
                                      (add-terms (rest-terms L1) L2))]
                        [(< (order t1) (order t2))
                         (adjoin-term t2
                                      (add-terms L1 (rest-terms L2)))]
                        [else
                         (adjoin-term (make-term (order t1)
                                                 (add (coeff t1) (coeff t2)))
                                      (add-terms (rest-terms L1)
                                                 (rest-terms L2)))]))]))

  ;; ---------------------------------------------------------------------------------

  (define (mul-terms L1 L2)
    (if (empty-termlist? L1)
        (the-empty-termlist)
        (add-terms (mul-term-by-all-terms (first-term L1) L2)
                   (mul-terms (rest-terms L1) L2))))

  (define (mul-term-by-all-terms t1 L)
    (if (empty-termlist? L)
        (the-empty-termlist)
        (let ([t2 (first-term L)])
          (adjoin-term
           (make-term (+ (order t1) (order t2))
                      (mul (coeff t1) (coeff t2)))
           (mul-term-by-all-terms t1 (rest-terms L))))))

  ;; ---------------------------------------------------------------------------------

  (define (install-polynomial-package)
    (define (add-poly p1 p2)
      (if (same-variable? (variable p1) (variable p2))
          (make-poly (variable p1)
                     (add-terms (term-list p1) (term-list p2)))
          (error "Polys not in same var: ADD-POLY" (list p1 p2))))

    ;; ---------------------------------------------------------------------------------

    (define (mul-poly p1 p2)
      (if (same-variable? (variable p1) (variable p2))
          (make-poly (variable p1)
                     (mul-terms (term-list p1) (term-list p2)))
          (error "Polys not in same var: MUL-POLY" (list p1 p2))))

    ;; ---------------------------------------------------------------------------------

    (define (tag p) (attach-tag 'polynomial p))
    (put 'add '(polynomial polynomial)
         (lambda (p1 p2) (tag (add-poly p1 p2))))
    (put 'mul '(polynomial polynomial)
         (lambda (p1 p2) (tag (mul-poly p1 p2))))
    (put 'make 'polynomial
         (lambda (var terms) (tag (make-poly var terms))))
    'polynomial-package-installed)

  (define (make-polynomial var terms)
    ((get 'make 'polynomial) var terms))

  (define (change-polynomial-variable poly new-variable)
    (make-polynomial new-variable (term-list (contents poly))))

  ;; ---------------------------------------------------------------------------------

  ;; tower: racket-integer -> rational -> racket-number -> complex -> polynomial
  (define (install-tower-of-types-raise-complex)
    (put 'raise '(complex)
         (λ (z)
           (make-polynomial 'no-variable (adjoin-term
                                          (make-term 0 (drop (attach-tag 'complex z)))
                                          (the-empty-termlist))))))

  (define (install-generic-arithmetic-package-polynomial-zero)
    (define (zero-polynomial poly)
      (define (terms-handler terms)
        (if (empty-termlist? terms)
            #t
            (and (=zero? (coeff (first-term terms)))
                 (terms-handler (rest-terms terms)))))
      (terms-handler (term-list poly)))

    (put '=zero? '(polynomial) zero-polynomial)
    'generic-arithmetic-package-=zero-polynomial-installed)

  ;; ---------------------------------------------------------------------------------

  #|
  I implemented the following procedures because I wanted to use `drop`. Later, I
  realized that `drop` was implemented in Exercise/2.85 where we still didn't have the
  notion of nested types (e.g., a complex number with rational coefficients) whick was
  implemented in Exercise/2.86 (so the `drop` functionality is a bit limited - anyway
  this is not a part of the exercise).
  |#
  (define (install-generic-arithmetic-package-equality-polynomials)
    (define (polynomial-equality p1 p2)
      (define (compare-terms terms1 terms2)
        (cond [(not (= (length terms1) (length terms2))) #f]
              [(empty-termlist? terms1) #t]
              [else (let ([h1 (first-term terms1)]
                          [h2 (first-term terms2)])
                      (and (and (= (order h1) (order h2))
                                (equ? (coeff h1) (coeff h2)))
                           (compare-terms (rest-terms terms1) (rest-terms terms2))))]))
      (if (not (same-variable? (variable p1) (variable p2)))
          #f
          (let ([non-zero-terms (λ (term) (not (=zero? (coeff term))))])
            (compare-terms (filter-terms non-zero-terms (term-list p1))
                           (filter-terms non-zero-terms (term-list p2))))))

    (put 'equ? '(polynomial polynomial) polynomial-equality)
    'generic-arithmetic-package-equality-polynomials-installed)

  (define (install-tower-of-types-drop-polynomial)
    (define (polynomial->complex poly)
      (define (iter terms)
        (cond [(empty-termlist? terms) (make-complex-from-real-imag 0 0)]
              [(= (order (first-term terms)) 0)
               (make-complex-from-real-imag (coeff (first-term terms)) 0)]
              [else (iter (rest-terms terms))]))
      (iter (term-list poly)))
    (put 'project '(polynomial) (λ (p) (polynomial->complex p))))

  (module+ test
    (#%require rackunit
               (only (submod "sicp1.rkt" Exercise/1.43) repeated))
    (display "--> Exercise/2.87\n")

    (clear-op-type-table)
    (install-generic-arithmetic-package)
    (install-generic-arithmetic-package-equality)
    (install-generic-arithmetic-package-zero)
    (install-generic-arithmetic-package-polynomial-zero)
    (install-racket-integers-package)
    (install-tower-of-types-raise)
    (install-tower-of-types-raise-complex)
    (install-tower-of-types-drop)
    (install-functions-of-racket-number)
    (install-polynomial-package)
    (install-generic-arithmetic-package-equality-polynomials)
    (install-tower-of-types-drop-polynomial)

    ;; --------------------------------------------------------------------------------

    (define p0 (make-polynomial 'x (list (make-term 2 3)
                                         (make-term 1 2)
                                         (make-term 0 7))))
    (check-equal? (find-arg-with-highest-type (list p0)) (list 4 'polynomial p0))
    (check-false (=zero? p0))

    ;; --------------------------------------------------------------------------------

    (let* ([i 0]
           [r (raise i)]
           [n (raise r)]
           [c (raise n)]
           [p (raise c)])
      (check-true (=zero? i))
      (check-true (=zero? r))
      (check-true (=zero? n))
      (check-true (=zero? c))
      (check-true (=zero? p)))

    (let* ([i 1]
           [r (raise i)]
           [n (raise r)]
           [c (raise n)]
           [p (raise c)])
      (check-false (=zero? i))
      (check-false (=zero? r))
      (check-false (=zero? n))
      (check-false (=zero? c))
      (check-false (=zero? p)))

    (check-true (=zero? (make-polynomial 'x (list (make-term 2 0)
                                                  (make-term 1 0)
                                                  (make-term 0 0)))))

    ;; --------------------------------------------------------------------------------
    ;; See footnote 57.
    ;; --------------------------------------------------------------------------------

    (define p1
      (make-polynomial
       'x
       (list (make-term 2 1)
             (make-term 1 (make-polynomial 'y (list (make-term 1 1)
                                                    (make-term 0 1))))
             (make-term 0 5))))

    (define p2
      (make-polynomial
       'x
       (list (make-term 2 1)
             (make-term 1 2)
             (make-term 0 1))))

    (define p1+p2
      (make-polynomial
       'x
       (list (make-term 2 2)
             (make-term 1 (make-polynomial
                           'y
                           (list (make-term 1 1)
                                 (make-term 0 3))))
             (make-term 0 6))))

    (check-equal? (add p1 p2) p1+p2)
    (check-exn exn:fail? (λ () (add p1 (change-polynomial-variable p1 'z))))

    ;; --------------------------------------------------------------------------------
    ;; See page 281
    ;; --------------------------------------------------------------------------------

    (define p3
      (make-polynomial
       'x
       (list (make-term 5 1)
             (make-term 4 2)
             (make-term 2 3)
             (make-term 1 -2)
             (make-term 0 -5))))

    (define p4
      (make-polynomial
       'x
       (list (make-term 100 1)
             (make-term 2 2)
             (make-term 0 1))))

    (define p3+p4
      (make-polynomial
       'x
       (list (make-term 100 1)
             (make-term 5 1)
             (make-term 4 2)
             (make-term 2 5)
             (make-term 1 -2)
             (make-term 0 -4))))

    (check-equal? (add p3 p4) p3+p4)

    ;; --------------------------------------------------------------------------------
    ;; See page 280
    ;; --------------------------------------------------------------------------------

    (define p5
      (make-polynomial
       'x
       (list (make-term 2 (make-polynomial 'y (list (make-term 1 1)
                                                    (make-term 0 1))))
             (make-term 1 (make-polynomial 'y (list (make-term 2 1)
                                                    (make-term 0 1))))
             (make-term 0 (make-polynomial 'y (list (make-term 1 1)
                                                    (make-term 0 -1)))))))

    (define p6
      (make-polynomial
       'x
       (list (make-term 1 (make-polynomial 'y (list (make-term 1 1)
                                                    (make-term 0 -2))))
             (make-term 0 (make-polynomial 'y (list (make-term 3 1)
                                                    (make-term 0 7)))))))

    #|
    (y^2 - y - 2)*x^3 +
    (y^4 + 2*y^3 - 2*y^2 + 8*y + 5)*x^2 +
    (y^5 + y^3 + 8*y^2 - 3*y + 9)*x +
    y^4 - y^3 + 7*y - 7
    |#
    (define p5*p6
      (make-polynomial
       'x
       (list (make-term 3 (make-polynomial 'y (list (make-term 2  1)
                                                    (make-term 1 -1)
                                                    (make-term 0 -2))))
             (make-term 2 (make-polynomial 'y (list (make-term 4  1)
                                                    (make-term 3  2)
                                                    (make-term 2 -2)
                                                    (make-term 1  8)
                                                    (make-term 0  5))))
             (make-term 1 (make-polynomial 'y (list (make-term 5  1)
                                                    (make-term 3  1)
                                                    (make-term 2  8)
                                                    (make-term 1 -3)
                                                    (make-term 0  9))))
             (make-term 0 (make-polynomial 'y (list (make-term 4  1)
                                                    (make-term 3 -1)
                                                    (make-term 1  7)
                                                    (make-term 0 -7)))))))

    (check-equal? (mul p5 p6) p5*p6)

    ;; --------------------------------------------------------------------------------
    ;; See page 280 (rational and complex coefficients)
    ;; --------------------------------------------------------------------------------

    (define p7
      (make-polynomial
       'x
       (list (make-term 2 3)
             (make-term 1 (make-complex-from-real-imag 2 3))
             (make-term 0 7))))

    (define p8
      (make-polynomial
       'x
       (list (make-term 4 1)
             (make-term 2 (make-rational 2 3))
             (make-term 0 (make-complex-from-real-imag 5 3)))))

    (define result:p7*p8 (mul p7 p8))
    (define terms (term-list (contents result:p7*p8)))
    (check-equal? (order (first-term terms)) 6)
    (check-equal? (order (first-term ((repeated rest-terms 1) terms))) 5)
    (check-equal? (order (first-term ((repeated rest-terms 2) terms))) 4)
    (check-equal? (order (first-term ((repeated rest-terms 3) terms))) 3)
    (check-equal? (order (first-term ((repeated rest-terms 4) terms))) 2)
    (check-equal? (order (first-term ((repeated rest-terms 5) terms))) 1)
    (check-equal? (order (first-term ((repeated rest-terms 6) terms))) 0)

    (check-equal? (coeff (first-term terms)) 3)
    (check-true (approx-equ? (coeff (first-term ((repeated rest-terms 1) terms)))
                             (make-complex-from-mag-ang 3.605551 0.982793)))
    (check-equal? (coeff (first-term ((repeated rest-terms 2) terms)))
                  (make-rational 9 1))
    (check-true (approx-equ? (coeff (first-term ((repeated rest-terms 3) terms)))
                             (make-complex-from-mag-ang 2.403700 0.982793)))
    (check-true (approx-equ? (coeff (first-term ((repeated rest-terms 4) terms)))
                             (make-complex-from-real-imag 19.666666 9.0)))
    (check-true (approx-equ? (coeff (first-term ((repeated rest-terms 5) terms)))
                             (make-complex-from-mag-ang 21.023796 1.523213)))
    (check-true (approx-equ? (coeff (first-term ((repeated rest-terms 6) terms)))
                             (make-complex-from-mag-ang 40.816663 0.5404195)))

    ;; --------------------------------------------------------------------------------
    ;; equ? / drop
    ;; --------------------------------------------------------------------------------

    (check-false (equ? p1 p2))
    (check-true (equ? p1 p1))

    (check-false (equ? (make-polynomial 'x (list (make-term 3 0)
                                                 (make-term 2 1)
                                                 (make-term 0 3)))
                       (make-polynomial 'x (list (make-term 0 3)))))

    (check-true (equ? (make-polynomial 'x (list (make-term 3 0)
                                                (make-term 2 0)
                                                (make-term 0 3)))
                      (make-polynomial 'x (list (make-term 0 3)))))

    (check-equal? (drop (make-polynomial 'x (list (make-term 3 0)
                                                  (make-term 2 0)
                                                  (make-term 0 3))))
                  3)

    ;; --------------------------------------------------------------------------------
    ;; filter-terms / map-terms
    ;; --------------------------------------------------------------------------------

    (define poly-coeff (make-polynomial 'y (list (make-term 1 0) (make-term 0 0))))
    (define poly
      (make-polynomial
       'x
       (list (make-term 2 1)
             (make-term 1 poly-coeff)
             (make-term 0 5))))

    (check-equal?
     (filter-terms (λ (term) (not (=zero? (coeff term))))
                   (term-list (contents poly)))
     (list (make-term 2 1) (make-term 0 5)))

    (let ([a 10])
      (check-equal?
       (map-terms (λ (term) (make-term (order term) (add (coeff term) a)))
                  (term-list (contents poly)))
       (list (make-term 2 (+ 1 a))
             (make-term 1 (add poly-coeff a))
             (make-term 0 (+ 5 a)))))))

(module Exercise/2.88 sicp
  (#%provide negate
             install-generic-arithmetic-package-negation
             install-generic-arithmetic-package-sub-polynomial)
  (#%require (only racket/base module+ λ)
             (only (submod "sicp1.rkt" common-utils) tolerance)
             (only (submod "sicp2_part3.rkt" Exercise/2.83) type-tag)
             (only (submod "sicp2_part3.rkt" Exercise/2.86)
                   attach-tag
                   put
                   apply-generic
                   clear-op-type-table
                   ;; --------------
                   add
                   sub
                   make-rational
                   make-complex-from-real-imag
                   make-complex-from-mag-ang
                   ;; --------------
                   real-part
                   imag-part
                   magnitude
                   angle
                   numer
                   denom
                   ;; --------------
                   install-generic-arithmetic-package
                   install-generic-arithmetic-package-equality
                   install-generic-arithmetic-package-zero
                   install-racket-integers-package
                   install-tower-of-types-raise
                   install-tower-of-types-drop
                   install-functions-of-racket-number)
             (only (submod ".." Exercise/2.87)
                   make-polynomial
                   make-term
                   ;; --------------
                   variable
                   term-list
                   the-empty-termlist
                   order
                   coeff
                   map-terms
                   ;; --------------
                   install-polynomial-package
                   install-generic-arithmetic-package-polynomial-zero
                   install-tower-of-types-raise-complex
                   install-generic-arithmetic-package-equality-polynomials
                   install-tower-of-types-drop-polynomial))

  (define (negate x) (apply-generic 'negate x))
  (define (install-generic-arithmetic-package-negation)
    (put 'negate '(racket-integer) (λ (x) (- x)))
    (put 'negate '(rational)
         (λ (x) (let ([x-tag (attach-tag 'rational x)])
                  (make-rational (- (numer x-tag)) (denom x-tag)))))
    (put 'negate '(racket-number) (λ (x) (- x)))
    #|
    A more consistent way to implement negation of complex numbers would be to dispatch
    (put 'negate '(complex) (λ (x) (apply-generic 'negate x)))
    and implement (put 'negate '(rectangular) ...) and (put 'negate '(polar) ...).
    This requires some boilerplate in the rectangular and polar packages which I avoid
    here (as I am being sloppy).
    |#
    (put 'negate '(complex)
         (λ (x) (let ([x-tag (attach-tag 'complex x)])
                  (let ([rect (make-complex-from-real-imag
                               (negate (real-part x-tag))
                               (negate (imag-part x-tag)))])
                    (cond [(eq? (type-tag x) 'rectangular) rect]
                          [(eq? (type-tag x) 'polar) (make-complex-from-mag-ang
                                                      (magnitude rect)
                                                      (angle rect))]
                          [else (error "Unknown complex representation:" x)])))))

    (put 'negate '(polynomial)
         (λ (poly)
           (make-polynomial
            (variable poly)
            (map-terms (λ (x) (make-term (order x) (negate (coeff x))))
                       (term-list poly)))))
    'generic-arithmetic-package-negation-installed)

  (define (install-generic-arithmetic-package-sub-polynomial)
    (put 'sub '(polynomial polynomial)
         (λ (p1 p2) (let ([p1-tag (attach-tag 'polynomial p1)]
                          [p2-tag (attach-tag 'polynomial p2)])
                      (add p1-tag (negate p2-tag)))))
    'generic-arithmetic-package-sub-polynomial-installed)

  (module+ test
    (#%require rackunit)
    (display "--> Exercise/2.88\n")

    (clear-op-type-table)
    (install-generic-arithmetic-package)
    (install-generic-arithmetic-package-equality)
    (install-generic-arithmetic-package-zero)
    (install-generic-arithmetic-package-polynomial-zero)
    (install-racket-integers-package)
    (install-tower-of-types-raise)
    (install-tower-of-types-raise-complex)
    (install-tower-of-types-drop)
    (install-functions-of-racket-number)
    (install-polynomial-package)
    (install-generic-arithmetic-package-equality-polynomials)
    (install-tower-of-types-drop-polynomial)
    (install-generic-arithmetic-package-negation)
    (install-generic-arithmetic-package-sub-polynomial)

    (check-equal? (negate 1) -1)
    (check-equal? (negate (make-rational 1 2)) (make-rational -1 2))
    (check-equal? (negate 1.0) -1.0)
    (check-equal? (negate (make-complex-from-real-imag 1 2))
                  (make-complex-from-real-imag -1 -2))

    (let ([c (negate (make-complex-from-mag-ang 1 2))])
      (check-equal? (magnitude c) 1.0)
      (check-within (angle c) -1.141592 tolerance))

    (check-equal? (negate (make-complex-from-real-imag (make-rational 1 2) 3))
                  (make-complex-from-real-imag (make-rational -1 2) -3))

    (check-equal?
     (negate
      (make-polynomial
       'x
       (list (make-term 2 3)
             (make-term 1 (make-polynomial 'y (list (make-term 2 1)
                                                    (make-term 1 (make-rational 2 3))
                                                    (make-term 0 3))))
             (make-term 0 7))))
     (make-polynomial
      'x
      (list (make-term 2 -3)
            (make-term 1 (make-polynomial 'y (list (make-term 2 -1)
                                                   (make-term 1 (make-rational -2 3))
                                                   (make-term 0 -3))))
            (make-term 0 -7))))

    (let ([c1 (make-complex-from-real-imag 2 7)]
          [c2 (make-complex-from-real-imag 3 5)]
          [result (make-complex-from-real-imag -1 2)])
      (check-equal? (add c1 (negate c2)) result)
      (check-equal? (sub c1 c2) result))))

(module Exercise/2.89 sicp
  (#%provide first-term
             adjoin-term
             add-terms)
  (#%require (only racket/base module+ λ exn:fail?)
             (only (submod "sicp2_part3.rkt" Exercise/2.86)
                   add
                   =zero?
                   ;; --------------
                   clear-op-type-table
                   ;; --------------
                   install-generic-arithmetic-package
                   install-generic-arithmetic-package-equality
                   install-generic-arithmetic-package-zero
                   install-racket-integers-package
                   install-tower-of-types-raise
                   install-tower-of-types-drop
                   install-functions-of-racket-number)
             (only (submod ".." Exercise/2.87)
                   make-term
                   ;; --------------
                   the-empty-termlist
                   order
                   coeff
                   empty-termlist?
                   rest-terms
                   ;; --------------
                   install-polynomial-package
                   install-generic-arithmetic-package-polynomial-zero
                   install-tower-of-types-raise-complex
                   install-generic-arithmetic-package-equality-polynomials
                   install-tower-of-types-drop-polynomial)
             (only (submod ".." Exercise/2.88)
                   install-generic-arithmetic-package-negation
                   install-generic-arithmetic-package-sub-polynomial))

  #|
  My modelling is as follows:
  1. In the dense representation, terms are stored as a list of coefficients and the
  position in the list determines the order of each term. But a term by itself (i.e.,
  without the context of how it is stored) should explicitly contain its order and
  coefficient, so I preserve as is the existing make-term, order and coeff procedures.
  2. We have two selectors: first-term and rest-terms. The former returns an actual term
  (i.e., an object with an order and a coefficient), while the latter returns our
  representation (dense or sparse) of the remaining terms.
  3. For the adjoin-term we make the same assumption as in footnote 59 (page 282) i.e.,
  it is allways called with a term of higher order than the existing terms.
  4. In this way, the existing implementation of add-terms and mul-terms can be reused
  as is (but I have to copy/paste them here).
  |#

  (define (first-term term-list)
    (list (- (length term-list) 1)
          (car term-list)))

  (define (adjoin-term term term-list)
    (if (=zero? (coeff term))
        term-list
        (let ([next-order (if (empty-termlist? term-list)
                              0
                              (+ (order (first-term term-list)) 1))])
          (cond [(= (order term) next-order) (cons (coeff term) term-list)]
                [(> (order term) next-order) (adjoin-term term (cons 0 term-list))]
                [else (error "Order to new term should be at least" next-order)]))))

  (define (add-terms L1 L2)
    (cond [(empty-termlist? L1) L2]
          [(empty-termlist? L2) L1]
          [else (let ([t1 (first-term L1)]
                      [t2 (first-term L2)])
                  (cond [(> (order t1) (order t2))
                         (adjoin-term t1
                                      (add-terms (rest-terms L1) L2))]
                        [(< (order t1) (order t2))
                         (adjoin-term t2
                                      (add-terms L1 (rest-terms L2)))]
                        [else
                         (adjoin-term (make-term (order t1)
                                                 (add (coeff t1) (coeff t2)))
                                      (add-terms (rest-terms L1)
                                                 (rest-terms L2)))]))]))

  (module+ test
    (#%require rackunit)
    (display "--> Exercise/2.89\n")

    ;; not all packages need to be installed
    (clear-op-type-table)
    (install-generic-arithmetic-package)
    (install-generic-arithmetic-package-equality)
    (install-generic-arithmetic-package-zero)
    (install-generic-arithmetic-package-polynomial-zero)
    (install-racket-integers-package)
    (install-tower-of-types-raise)
    (install-tower-of-types-raise-complex)
    (install-tower-of-types-drop)
    (install-functions-of-racket-number)
    (install-polynomial-package)
    (install-generic-arithmetic-package-equality-polynomials)
    (install-tower-of-types-drop-polynomial)
    (install-generic-arithmetic-package-negation)
    (install-generic-arithmetic-package-sub-polynomial)

    (define t0 (the-empty-termlist))
    (check-true (empty-termlist? t0))

    (define t1 (adjoin-term (make-term 0 1) t0))
    (check-equal? t1 '(1))

    (define t2 (adjoin-term (make-term 5 2) t1))
    (check-equal? t2 '(2 0 0 0 0 1))
    (check-equal? (adjoin-term (make-term 7 0) t2) t2)

    (check-exn exn:fail? (λ () (adjoin-term (make-term 3 1) t2)))

    (check-equal? (first-term t2) '(5 2))
    (check-equal? (rest-terms t2) '(0 0 0 0 1))

    (check-equal? (add-terms t2 t2) '(4 0 0 0 0 2))))

(module Exercise/2.90 sicp
  (#%provide install-sparse-terms-representation
             install-dense-terms-representation
             install-polynomial-package
             install-generic-arithmetic-package-equality-polynomials
             install-generic-arithmetic-package-polynomial-zero
             install-tower-of-types-raise-complex
             install-tower-of-types-drop-polynomial
             install-generic-arithmetic-package-negation
             install-generic-arithmetic-package-sub-polynomial
             ;;------------------
             add-terms
             mul-term-by-all-terms
             ;;------------------
             the-empty-termlist
             sparse-empty-termlist
             dense-empty-termlist
             adjoin-term
             first-term
             rest-terms
             empty-termlist?
             convert-terms
             convert-poly
             ;;------------------
             make-typed-polynomial)
  (#%require (only racket/base module+ λ local-require submod only-in)
             (only (submod "sicp2_part3.rkt" Exercise/2.83) type-tag)
             (only (submod "sicp2_part3.rkt" Exercise/2.85) drop)
             (only (submod "sicp2_part3.rkt" Exercise/2.86)
                   =zero?
                   equ?
                   ;; --------------
                   make-complex-from-real-imag
                   make-complex-from-mag-ang
                   make-rational
                   ;; --------------
                   add
                   sub
                   mul
                   ;; --------------
                   real-part
                   imag-part
                   magnitude
                   angle
                   numer
                   denom
                   ;; --------------
                   clear-op-type-table
                   get
                   put
                   apply-generic
                   contents
                   attach-tag
                   ;; --------------
                   install-generic-arithmetic-package
                   install-generic-arithmetic-package-equality
                   install-generic-arithmetic-package-zero
                   install-racket-integers-package
                   install-tower-of-types-raise
                   install-tower-of-types-drop
                   install-functions-of-racket-number)
             (only (submod ".." Exercise/2.87)
                   variable
                   same-variable?
                   make-term
                   make-polynomial
                   make-poly
                   ;; --------------
                   term-list
                   order
                   coeff)
             (rename (submod ".." Exercise/2.87)
                     the-empty-termlist-no-tag the-empty-termlist)
             (only (submod ".." Exercise/2.88) negate))

  #|
  Design notes:
  1. adjoin-term takes two arguments. The second one is a list of terms and it would
  have a tag depending on the (dense or sparse) representation. The first one, however,
  is a single term to adjoin and I don't want to tag it (there is no point as no
  dispatch would be done on it). To achieve this, the adjoin-term interface would return
  a procedure.
  2. I want for arithmetic operations (add, sub, mul) to be able to handle both sparse
  and dense polynomials transparently. To achieve this I define them in terms of generic
  versions of adjoin-term, first-term, rest-terms and empty-termlist?. In this way we
  can e.g., add two polynomials with different representation.
  3. When adding two polynomials with different representation, the type of the output
  depends not only on the type of the operands but on the particular polynomial terms.
  I have added a procedure that converts between polynomial types (for fun). When
  multiplying two polynomials with different representation the type of the second
  operand determines the type of the output.
  4. Apart from the arithmetic operations, I reimplemented equ? and =zero? in term of
  the generic versions of adjoin-term, first-term, rest-terms and empty-termlist?.
  Unfortunately, in the process, I had to copy/paste quite some code from previous
  exercises (it seems like an overkill to use racket's units as I did in Section/2.5.1).
  |#

  (define (install-sparse-terms-representation)
    (local-require (only-in (submod ".." Exercise/2.87)
                            first-term
                            rest-terms
                            adjoin-term
                            empty-termlist?
                            add-terms))
    (define (tag term) (attach-tag 'sparse-terms term))
    (put 'adjoin-term '(sparse-terms) (λ (terms)
                                        (λ (term)
                                          (tag (adjoin-term term terms)))))
    (put 'first-term '(sparse-terms) first-term)
    (put 'rest-terms '(sparse-terms) (λ (terms) (tag (rest-terms terms))))
    (put 'empty-termlist? '(sparse-terms) empty-termlist?))

  (define (install-dense-terms-representation)
    (local-require (only-in (submod ".." Exercise/2.87) rest-terms empty-termlist?))
    (local-require (only-in (submod ".." Exercise/2.89)
                            first-term
                            adjoin-term
                            add-terms))
    (define (tag term) (attach-tag 'dense-terms term))
    (put 'adjoin-term '(dense-terms) (λ (terms)
                                       (λ (term)
                                         (tag (adjoin-term term terms)))))
    (put 'first-term '(dense-terms) first-term)
    (put 'rest-terms '(dense-terms) (λ (terms) (tag (rest-terms terms))))
    (put 'empty-termlist? '(dense-terms) empty-termlist?))

  (define (the-empty-termlist) (sparse-empty-termlist))
  (define (sparse-empty-termlist)
    (attach-tag 'sparse-terms (the-empty-termlist-no-tag)))
  (define (dense-empty-termlist) (attach-tag 'dense-terms (the-empty-termlist-no-tag)))
  (define (adjoin-term term terms) ((apply-generic 'adjoin-term terms) term))
  (define (first-term terms) (apply-generic 'first-term terms))
  (define (rest-terms terms) (apply-generic 'rest-terms terms))
  (define (empty-termlist? terms) (apply-generic 'empty-termlist? terms))

  (define (convert-terms terms terms-type)
    (if (empty-termlist? terms)
        (attach-tag terms-type (the-empty-termlist-no-tag))
        (adjoin-term (first-term terms) (convert-terms (rest-terms terms)
                                                       terms-type))))

  (define (convert-poly poly terms-type)
    (make-polynomial (variable (contents poly))
                     (convert-terms (term-list (contents poly)) terms-type)))

  ;; below I mostly copy/paste stuff to take the new functionality into account
  (define (add-terms L1 L2)
    (cond [(empty-termlist? L1) L2]
          [(empty-termlist? L2) L1]
          [else (let ([t1 (first-term L1)]
                      [t2 (first-term L2)])
                  (cond [(> (order t1) (order t2))
                         (adjoin-term t1
                                      (add-terms (rest-terms L1) L2))]
                        [(< (order t1) (order t2))
                         (adjoin-term t2
                                      (add-terms L1 (rest-terms L2)))]
                        [else
                         (adjoin-term (make-term (order t1)
                                                 (add (coeff t1) (coeff t2)))
                                      (add-terms (rest-terms L1)
                                                 (rest-terms L2)))]))]))

  (define (mul-term-by-all-terms t1 L)
    (if (empty-termlist? L)
        ;; makes sense to preserve the term-list type
        (convert-terms (the-empty-termlist) (type-tag L))
        (let ([t2 (first-term L)])
          (adjoin-term
           (make-term (+ (order t1) (order t2))
                      (mul (coeff t1) (coeff t2)))
           (mul-term-by-all-terms t1 (rest-terms L))))))

  (define (install-polynomial-package)
    (define (add-poly p1 p2)
      (if (same-variable? (variable p1) (variable p2))
          (make-polynomial (variable p1)
                           (add-terms (term-list p1)
                                      (term-list p2)))
          (error "Polys not in same var: ADD-POLY" (list p1 p2))))

    (define (mul-terms L1 L2)
      (if (empty-termlist? L1)
          (convert-terms (the-empty-termlist) (type-tag L2))
          (add-terms (mul-term-by-all-terms (first-term L1) L2)
                     (mul-terms (rest-terms L1) L2))))

    (define (mul-poly p1 p2)
      (if (same-variable? (variable p1) (variable p2))
          (make-polynomial (variable p1)
                           (mul-terms (term-list p1) (term-list p2)))
          (error "Polys not in same var: MUL-POLY" (list p1 p2))))
    ;; --------------------------------------------------------------------------------

    (define (tag poly) (attach-tag 'polynomial poly))
    (put 'add '(polynomial polynomial) add-poly)
    (put 'mul '(polynomial polynomial) mul-poly)
    (put 'make 'polynomial (lambda (var terms) (tag (make-poly var terms)))))

  ;; ----------------------------------------------------------------------------------
  ;; equ? / =zero?
  ;; ----------------------------------------------------------------------------------
  (define (install-generic-arithmetic-package-equality-polynomials)
    (define (filter-terms predicate terms)
      (cond [(empty-termlist? terms) (the-empty-termlist)]
            [(predicate (first-term terms))
             (adjoin-term (first-term terms)
                          (filter-terms predicate (rest-terms terms)))]
            [else (filter-terms predicate (rest-terms terms))]))

    (define (polynomial-equality p1 p2)
      (define (compare-terms terms1 terms2)
        (cond [(not (= (length terms1) (length terms2))) #f]
              [(empty-termlist? terms1) #t]
              [else (let ([h1 (first-term terms1)]
                          [h2 (first-term terms2)])
                      (and (and (= (order h1) (order h2))
                                (equ? (coeff h1) (coeff h2)))
                           (compare-terms (rest-terms terms1) (rest-terms terms2))))]))
      (if (not (same-variable? (variable p1) (variable p2)))
          #f
          (let ([non-zero-terms (λ (term) (not (=zero? (coeff term))))])
            (compare-terms (filter-terms non-zero-terms (term-list p1))
                           (filter-terms non-zero-terms (term-list p2))))))

    (put 'equ? '(polynomial polynomial) polynomial-equality)
    'generic-arithmetic-package-equality-polynomials-installed)

  (define (install-generic-arithmetic-package-polynomial-zero)
    (define (zero-polynomial poly)
      (define (terms-handler terms)
        (if (empty-termlist? terms)
            #t
            (and (=zero? (coeff (first-term terms)))
                 (terms-handler (rest-terms terms)))))
      (terms-handler (term-list poly)))

    (put '=zero? '(polynomial) zero-polynomial)
    'generic-arithmetic-package-=zero-polynomial-installed)

  ;; -----------------------------------------------------------------------------------
  ;; to be able to (add 1 some-polynomial)
  ;; -----------------------------------------------------------------------------------

  (define (install-tower-of-types-raise-complex)
    (put 'raise '(complex)
         (λ (z)
           (make-polynomial 'no-variable (adjoin-term
                                          (make-term 0 (drop (attach-tag 'complex z)))
                                          (the-empty-termlist))))))

  ;; -----------------------------------------------------------------------------------
  ;; drop
  ;; -----------------------------------------------------------------------------------
  (define (install-tower-of-types-drop-polynomial)
    (define (polynomial->complex poly)
      (define (iter terms)
        (cond [(empty-termlist? terms) (make-complex-from-real-imag 0 0)]
              [(= (order (first-term terms)) 0)
               (make-complex-from-real-imag (coeff (first-term terms)) 0)]
              [else (iter (rest-terms terms))]))
      (iter (term-list poly)))
    (put 'project '(polynomial) (λ (p) (polynomial->complex p))))

  ;; -----------------------------------------------------------------------------------
  ;; sub (I added as well a negation of a sparse/dense term list)
  ;; -----------------------------------------------------------------------------------
  (define (install-generic-arithmetic-package-negation)
    (define (map-terms proc terms)
      (if (empty-termlist? terms)
          (convert-terms (the-empty-termlist) (type-tag terms))
          (adjoin-term (proc (first-term terms))
                       (map-terms proc (rest-terms terms)))))

    (put 'negate '(racket-integer) (λ (x) (- x)))
    (put 'negate '(rational)
         (λ (x) (let ([x-tag (attach-tag 'rational x)])
                  (make-rational (- (numer x-tag)) (denom x-tag)))))
    (put 'negate '(racket-number) (λ (x) (- x)))
    (put 'negate '(complex)
         (λ (x) (let ([x-tag (attach-tag 'complex x)])
                  (let ([rect (make-complex-from-real-imag
                               (negate (real-part x-tag))
                               (negate (imag-part x-tag)))])
                    (cond [(eq? (type-tag x) 'rectangular) rect]
                          [(eq? (type-tag x) 'polar) (make-complex-from-mag-ang
                                                      (magnitude rect)
                                                      (angle rect))]
                          [else (error "Unknown complex representation:" x)])))))

    (define (negate-terms terms)
      (map-terms (λ (x) (make-term (order x) (negate (coeff x)))) terms))

    (put 'negate '(polynomial)
         (λ (poly) (make-polynomial (variable poly)
                                    (negate (term-list poly)))))
    (put 'negate '(dense-terms)
         (λ (untyped-terms)
           (negate-terms (attach-tag 'dense-terms untyped-terms))))
    (put 'negate '(sparse-terms)
         (λ (untyped-terms)
           (negate-terms (attach-tag 'sparse-terms untyped-terms))))
    'generic-arithmetic-package-negation-installed)

  (define (install-generic-arithmetic-package-sub-polynomial)
    (put 'sub '(polynomial polynomial)
         (λ (p1 p2) (let ([p1-tag (attach-tag 'polynomial p1)]
                          [p2-tag (attach-tag 'polynomial p2)])
                      (add p1-tag (negate p2-tag)))))
    'generic-arithmetic-package-sub-polynomial-installed)
  ;; -----------------------------------------------------------------------------------

  (define (make-typed-polynomial var type terms)
    (make-polynomial var (attach-tag type terms)))

  (module+ test
    (#%require rackunit)
    (display "--> Exercise/2.90\n")

    (clear-op-type-table)
    (install-generic-arithmetic-package)
    (install-generic-arithmetic-package-equality)
    (install-generic-arithmetic-package-zero)
    (install-racket-integers-package)
    (install-tower-of-types-raise)
    (install-tower-of-types-drop)
    (install-functions-of-racket-number)

    (install-sparse-terms-representation)
    (install-dense-terms-representation)
    (install-polynomial-package)
    (install-generic-arithmetic-package-equality-polynomials)
    (install-generic-arithmetic-package-polynomial-zero)
    (install-tower-of-types-raise-complex)
    (install-tower-of-types-drop-polynomial)
    (install-generic-arithmetic-package-negation)
    (install-generic-arithmetic-package-sub-polynomial)

    (define s1 (make-typed-polynomial 'x 'sparse-terms '((3 13) (2 12) (0 10))))
    (define d1 (make-typed-polynomial 'x 'dense-terms '(13 12 0 10)))

    (define s2 (make-typed-polynomial 'x 'sparse-terms '((7 17) (5 15) (2 12) (0 10))))
    (define d2 (make-typed-polynomial 'x 'dense-terms '(17 0 15 0 0 12 0 10)))

    (define d1+d2 (make-typed-polynomial 'x 'dense-terms '(17 0 15 0 13 24 0 20)))
    (define d1*d2 (make-typed-polynomial 'x 'dense-terms
                                         '(221 204 195 350 0 306 144 130 240 0 100)))

    (check-true (equ? (add s1 d2) d1+d2))
    (check-true (equ? (add d1 s2) d1+d2))

    (check-true (equ? (mul s1 d2) d1*d2))
    (check-true (equ? (mul d1 s2) d1*d2))

    ;; ---------------------------------------------------------------------------------
    ;; output polynomial depends on the actual terms and not only on arguments types
    (check-equal? (add (make-typed-polynomial 'x 'dense-terms '(1))
                       (make-typed-polynomial 'x 'sparse-terms '()))
                  (make-typed-polynomial 'x 'dense-terms '(1)))

    (check-equal? (add (make-typed-polynomial 'x 'dense-terms '(1))
                       (make-typed-polynomial 'x 'sparse-terms '((0 1))))
                  (make-typed-polynomial 'x 'sparse-terms '((0 2))))
    ;; ---------------------------------------------------------------------------------

    (let ([sparse-poly
           (make-typed-polynomial 'x 'sparse-terms (the-empty-termlist-no-tag))]
          [dense-poly
           (make-typed-polynomial 'x 'dense-terms (the-empty-termlist-no-tag))])
      (check-equal? (convert-poly sparse-poly 'dense-terms) dense-poly)
      (check-equal? (convert-poly dense-poly 'sparse-terms) sparse-poly))

    (let ([sparse-poly s2]
          [dense-poly d2])
      (check-equal? sparse-poly (convert-poly dense-poly 'sparse-terms))
      (check-equal? dense-poly (convert-poly sparse-poly 'dense-terms))
      (check-equal? sparse-poly (convert-poly sparse-poly 'sparse-terms))
      (check-equal? dense-poly (convert-poly dense-poly 'dense-terms)))

    (check-true (equ? s1 s1))
    (check-true (equ? s1 d1))
    (check-false (equ? s1 s2))
    (check-false (equ? s1 d2))

    (check-true (=zero? (make-typed-polynomial 'x 'sparse-terms '())))
    (check-true (=zero? (make-typed-polynomial 'x 'dense-terms '())))
    (check-false (=zero? s1))
    (check-false (=zero? d1))

    (check-equal? (add 1 d1)
                  (make-typed-polynomial 'no-variable 'dense-terms '(13 12 0 11)))

    #|
    The `drop` procedure doesn't support nested types. See the note above
    install-generic-arithmetic-package-equality-polynomials in Exercise/2.87.
    |#
    (check-equal? (drop (make-typed-polynomial 'x 'dense-terms (list 1.5))) 1.5)

    (check-true (equ? (sub s1 s1) 0))
    (check-true (equ? (sub d1 d1) 0))
    (check-false (equ? (sub d2 d1) 0))
    (check-false (equ? (sub s1 s2) 0))
    (check-true (=zero? (add s1 (negate s1))))

    ;; preserve the termlist type when negating
    (check-equal? (negate (term-list (contents s1)))
                  (attach-tag 'sparse-terms '((3 -13) (2 -12) (0 -10))))
    (check-equal? (negate (term-list (contents d1)))
                  (attach-tag 'dense-terms '(-13 -12 0 -10)))))

(module Exercise/2.91 sicp
  (#%provide install-polynomial-division)
  (#%require (only racket/base module+ λ)
             (only (submod "sicp2_part3.rkt" Exercise/2.83) type-tag)
             (only (submod "sicp2_part3.rkt" Exercise/2.86)
                   install-generic-arithmetic-package
                   install-generic-arithmetic-package-equality
                   install-generic-arithmetic-package-zero
                   install-racket-integers-package
                   install-tower-of-types-raise
                   install-tower-of-types-drop
                   install-functions-of-racket-number
                   clear-op-type-table
                   ;;------------------
                   attach-tag
                   contents
                   put
                   ;;------------------
                   add
                   mul
                   sub
                   div)
             (only (submod ".." Exercise/2.87)
                   variable
                   same-variable?
                   make-term
                   make-polynomial
                   make-poly
                   term-list
                   order
                   coeff)
             (only (submod ".." Exercise/2.88) negate)
             (only (submod ".." Exercise/2.90)
                   install-sparse-terms-representation
                   install-dense-terms-representation
                   install-polynomial-package
                   install-generic-arithmetic-package-equality-polynomials
                   install-generic-arithmetic-package-polynomial-zero
                   install-tower-of-types-raise-complex
                   install-tower-of-types-drop-polynomial
                   install-generic-arithmetic-package-negation
                   install-generic-arithmetic-package-sub-polynomial
                   ;;------------------
                   the-empty-termlist
                   sparse-empty-termlist
                   dense-empty-termlist
                   adjoin-term
                   first-term
                   rest-terms
                   empty-termlist?
                   convert-poly
                   ;;------------------
                   add-terms
                   mul-term-by-all-terms
                   convert-terms
                   convert-poly
                   ;;------------------
                   make-typed-polynomial))

  (define (install-polynomial-division)
    #|
    L1: terms of numerator
    L2: terms of denominator
    |#
    (define (div-terms L1 L2)
      (if (empty-termlist? L1)
          (list (convert-terms (the-empty-termlist) (type-tag L1))
                (convert-terms (the-empty-termlist) (type-tag L1)))
          (let ([t1 (first-term L1)]
                [t2 (first-term L2)])
            (if (> (order t2) (order t1))
                (list (convert-terms (the-empty-termlist) (type-tag L1)) L1)
                (let ([new-coeff (div (coeff t1) (coeff t2))]
                      [new-order (- (order t1) (order t2))])
                  (let* ([new-term (make-term new-order new-coeff)]
                         [rest-of-result
                          (div-terms
                           (add-terms
                            (rest-terms L1)
                            (negate (mul-term-by-all-terms new-term (rest-terms L2))))
                           L2)])
                    (list
                     (adjoin-term new-term (car rest-of-result))
                     (cadr rest-of-result))))))))

    (define (div-poly p1 p2)
      (if (same-variable? (variable p1) (variable p2))
          (let ([quotient-and-remainder (div-terms (term-list p1) (term-list p2))])
            (list (make-poly (variable p1) (car quotient-and-remainder))
                  (make-poly (variable p1) (cadr quotient-and-remainder))))
          (error "Polys not in same var: DIV-POLY" (list p1 p2))))

    (put 'div '(polynomial polynomial)
         (λ (p1 p2)
           (let ([quotient-and-remainder (div-poly p1 p2)])
             (list (attach-tag 'polynomial (car quotient-and-remainder))
                   (attach-tag 'polynomial (cadr quotient-and-remainder))))))
    'polynomial-division-installed)

  (module+ test
    (#%require rackunit)
    (display "--> Exercise/2.91\n")

    (clear-op-type-table)
    (install-generic-arithmetic-package)
    (install-generic-arithmetic-package-equality)
    (install-generic-arithmetic-package-zero)
    (install-racket-integers-package)
    (install-tower-of-types-raise)
    (install-tower-of-types-drop)
    (install-functions-of-racket-number)

    (install-sparse-terms-representation)
    (install-dense-terms-representation)
    (install-polynomial-package)
    (install-generic-arithmetic-package-equality-polynomials)
    (install-generic-arithmetic-package-polynomial-zero)
    (install-tower-of-types-raise-complex)
    (install-tower-of-types-drop-polynomial)
    (install-generic-arithmetic-package-negation)
    (install-generic-arithmetic-package-sub-polynomial)

    (install-polynomial-division)

    (define numer-sparse (make-typed-polynomial 'x 'sparse-terms '((5 1) (0 -1))))
    (define denom-sparse (make-typed-polynomial 'x 'sparse-terms '((2 1) (0 -1))))
    (define numer-dense (make-typed-polynomial 'x 'dense-terms '(1 0 0 0 0 -1)))
    (define denom-dense (make-typed-polynomial 'x 'dense-terms '(1 0 -1)))

    ;; example in the exercise
    (let ([quotient-and-remainder (div numer-sparse denom-sparse)])
      (check-equal? (car quotient-and-remainder)
                    (make-typed-polynomial 'x 'sparse-terms '((3 1) (1 1))))
      (check-equal? (cadr quotient-and-remainder)
                    (make-typed-polynomial 'x 'sparse-terms '((1 1) (0 -1)))))

    (let ([quotient-and-remainder (div numer-dense denom-dense)])
      (check-equal? (car quotient-and-remainder)
                    (make-typed-polynomial 'x 'dense-terms '(1 0 1 0)))
      (check-equal? (cadr quotient-and-remainder)
                    (make-typed-polynomial 'x 'dense-terms '(1 -1))))

    (let ([quotient-and-remainder (div numer-sparse denom-dense)])
      (check-equal? (car quotient-and-remainder)
                    (make-typed-polynomial 'x 'dense-terms '(1 0 1 0)))
      (check-equal? (cadr quotient-and-remainder)
                    (make-typed-polynomial 'x 'dense-terms '(1 -1))))

    (let ([quotient-and-remainder (div numer-dense denom-sparse)])
      (check-equal? (car quotient-and-remainder)
                    (make-typed-polynomial 'x 'dense-terms '(1 0 1 0)))
      (check-equal? (cadr quotient-and-remainder)
                    (make-typed-polynomial 'x 'dense-terms '(1 -1))))

    ;; https://en.wikipedia.org/wiki/Polynomial_long_division#Example_2
    (let ([quotient-and-remainder
           (div (make-typed-polynomial 'x 'dense-terms '(1 -12 0 -42))
                (make-typed-polynomial 'x 'dense-terms '(1 -2 1)))])
      (check-equal? (car quotient-and-remainder)
                    (make-typed-polynomial 'x 'dense-terms '(1 -10)))
      (check-equal? (cadr quotient-and-remainder)
                    (make-typed-polynomial 'x 'dense-terms '(-21 -32))))))

(module+ test
  (require (submod ".." Exercise/2.87 test)
           (submod ".." Exercise/2.88 test)
           (submod ".." Exercise/2.89 test)
           (submod ".." Exercise/2.90 test)
           (submod ".." Exercise/2.91 test)))
