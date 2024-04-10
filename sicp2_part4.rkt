;; =====================================================================================
;; Exercises in Chapter 2 (part 4)
;; =====================================================================================
#lang racket/base

(module Exercise/2.87 sicp
  (#%provide make-polynomial
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
             ;; ------------------
             install-polynomial-package
             install-generic-arithmetic-package-polynomial-zero
             install-tower-of-types-raise-complex
             install-generic-arithmetic-package-equality-polynomials
             install-tower-of-types-drop-polynomial)
  (#%require (only racket/base module+ λ)
             (only (submod "sicp1.rkt" common-utils) tolerance)
             (only (submod "sicp2_part1.rkt" Section/2.2.3) filter)
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

  (define (the-empty-termlist) '())
  (define (first-term term-list) (car term-list))
  (define (rest-terms term-list) (cdr term-list))
  (define (empty-termlist? term-list) (null? term-list))
  (define (make-term order coeff) (list order coeff))
  (define (order term) (car term))
  (define (coeff term) (cadr term))
  (define (adjoin-term term term-list)
    (if (=zero? (coeff term))
        term-list
        (cons term term-list)))

  (define (install-polynomial-package)
    (define (add-poly p1 p2)
      (if (same-variable? (variable p1) (variable p2))
          (make-poly (variable p1)
                     (add-terms (term-list p1) (term-list p2)))
          (error "Polys not in same var: ADD-POLY" (list p1 p2))))

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

    (define (mul-poly p1 p2)
      (if (same-variable? (variable p1) (variable p2))
          (make-poly (variable p1)
                     (mul-terms (term-list p1) (term-list p2)))
          (error "Polys not in same var: MUL-POLY" (list p1 p2))))

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

  ;; ---------------------------------------------------------------------------------

  ;; tower: racket-integer -> rational -> racket-number -> complex -> polynomial
  (define (install-tower-of-types-raise-complex)
    (put 'raise '(complex)
         (λ (z)
           (attach-tag 'polynomial
                       (make-poly 'no-variable
                                  (adjoin-term
                                   (make-term 0 (drop (attach-tag 'complex z)))
                                   (the-empty-termlist)))))))

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
  I implemented the following procedures because I wanted to use `drop` but then I
  realized that `drop` was implemented in Exercise/2.85 where we still didn't have the
  notion of nested types (e.g., a complex number with rational coefficients) - this was
  implemented in Exercise/2.86 (so the `drop` functionality is a bit limited - anyway
  this is not a part of the exercise).
  |#
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
          ;; FIXME: shoudn't use filter (can only use first-term and rest-terms)
          (compare-terms (filter non-zero-terms (term-list p1))
                         (filter non-zero-terms (term-list p2))))))

  (define (install-generic-arithmetic-package-equality-polynomials)
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

    (define p2-y
      (make-polynomial
       'y
       (list (make-term 2 1)
             (make-term 1 2)
             (make-term 0 1))))

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
    ;; equ?/drop
    ;; --------------------------------------------------------------------------------

    (check-equal? (add p1 p2) p1+p2)
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
                  3)))

(module+ test
  (require (submod ".." Exercise/2.87 test)))