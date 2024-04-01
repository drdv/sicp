;; =====================================================================================
;; Exercises in Chapter 2 (part 3)
;; =====================================================================================
#lang racket/base

(module Section/2.4.1 sicp
  (#%require (only racket/base module))

  (module rectangular-package sicp ; by Ben Bitdiddle
    (#%provide real-part
               imag-part
               magnitude
               angle
               make-from-real-imag
               make-from-mag-ang)
    (#%require (only racket/base module+)
               (only (submod "sicp1.rkt" common-utils) square tolerance))

    (define (real-part z) (car z))
    (define (imag-part z) (cdr z))
    (define (magnitude z)
      (sqrt (+ (square (real-part z))
               (square (imag-part z)))))
    (define (angle z)
      (atan (imag-part z) (real-part z)))

    (define (make-from-real-imag x y) (cons x y))
    (define (make-from-mag-ang r a)
      (cons (* r (cos a)) (* r (sin a))))

    (module+ test
      (#%require rackunit)
      (display "--> Section/2.4.1 (rectangular-package)\n")

      (let ([z (make-from-real-imag 3 4)])
        (check-equal? (real-part z) 3)
        (check-equal? (imag-part z) 4)
        (check-within (magnitude z) 5 tolerance)
        (check-within (angle z) 0.927295 tolerance))

      (let ([z (make-from-mag-ang 5 0.927295)])
        (check-within (real-part z) 3 tolerance)
        (check-within (imag-part z) 4 tolerance)
        (check-within (magnitude z) 5 tolerance)
        (check-within (angle z) 0.927295 tolerance))))

  (module polar-package sicp ; by Alyssa P. Hacker
    (#%provide real-part
               imag-part
               magnitude
               angle
               make-from-real-imag
               make-from-mag-ang)
    (#%require (only racket/base module+)
               (only (submod "sicp1.rkt" common-utils) square tolerance))

    (define (real-part z) (* (magnitude z) (cos (angle z))))
    (define (imag-part z) (* (magnitude z) (sin (angle z))))
    (define (magnitude z) (car z))
    (define (angle z) (cdr z))

    (define (make-from-real-imag x y)
      (cons (sqrt (+ (square x) (square y)))
            (atan y x)))
    (define (make-from-mag-ang r a) (cons r a))

    (module+ test
      (#%require rackunit)
      (display "--> Section/2.4.1 (polar-package)\n")

      (let ([z (make-from-real-imag 3 4)])
        (check-within (real-part z) 3 tolerance)
        (check-within (imag-part z) 4 tolerance)
        (check-within (magnitude z) 5 tolerance)
        (check-within (angle z) 0.927295 tolerance))

      (let ([z (make-from-mag-ang 5 0.927295)])
        (check-within (real-part z) 3 tolerance)
        (check-within (imag-part z) 4 tolerance)
        (check-equal? (magnitude z) 5)
        (check-equal? (angle z) 0.927295)))))

(module Section/2.4.2 sicp
  (#%provide attach-tag
             type-tag
             contents)
  (#%require (only racket/base module+)
             (only (submod "sicp1.rkt" common-utils) square tolerance))

  ;; --------------------------------------------------
  ;; helper procedures
  ;; --------------------------------------------------
  (define (attach-tag type-tag contents)
    (cons type-tag contents))

  (define (type-tag datum)
    (if (pair? datum)
        (car datum)
        (error "Bad tagged datum: TYPE-TAG" datum)))
  (define (contents datum)
    (if (pair? datum)
        (cdr datum)
        (error "Bad tagged datum: CONTENTS" datum)))

  (define (rectangular? z)
    (eq? (type-tag z) 'rectangular))
  (define (polar? z)
    (eq? (type-tag z) 'polar))

  ;; --------------------------------------------------
  ;; rectangular representation
  ;; --------------------------------------------------
  (define (real-part-rectangular z) (car z))
  (define (imag-part-rectangular z) (cdr z))
  (define (magnitude-rectangular z)
    (sqrt (+ (square (real-part-rectangular z))
             (square (imag-part-rectangular z)))))
  (define (angle-rectangular z)
    (atan (imag-part-rectangular z)
          (real-part-rectangular z)))

  (define (make-from-real-imag-rectangular x y)
    (attach-tag 'rectangular (cons x y)))
  (define (make-from-mag-ang-rectangular r a)
    (attach-tag 'rectangular
                (cons (* r (cos a)) (* r (sin a)))))

  ;; --------------------------------------------------
  ;; polar representation
  ;; --------------------------------------------------
  (define (real-part-polar z)
    (* (magnitude-polar z) (cos (angle-polar z))))
  (define (imag-part-polar z)
    (* (magnitude-polar z) (sin (angle-polar z))))
  (define (magnitude-polar z) (car z))
  (define (angle-polar z) (cdr z))

  (define (make-from-real-imag-polar x y)
    (attach-tag 'polar
                (cons (sqrt (+ (square x) (square y)))
                      (atan y x))))
  (define (make-from-mag-ang-polar r a)
    (attach-tag 'polar (cons r a)))

  ;; --------------------------------------------------
  ;; generic representation
  ;; --------------------------------------------------
  (define (real-part z)
    (cond [(rectangular? z) (real-part-rectangular (contents z))]
          [(polar? z) (real-part-polar (contents z))]
          [else (error "Unknown type: REAL-PART" z)]))

  (define (imag-part z)
    (cond [(rectangular? z) (imag-part-rectangular (contents z))]
          [(polar? z) (imag-part-polar (contents z))]
          [else (error "Unknown type: IMAG-PART" z)]))

  (define (magnitude z)
    (cond [(rectangular? z) (magnitude-rectangular (contents z))]
          [(polar? z) (magnitude-polar (contents z))]
          [else (error "Unknown type: MAGNITUDE" z)]))

  (define (angle z)
    (cond [(rectangular? z) (angle-rectangular (contents z))]
          [(polar? z) (angle-polar (contents z))]
          [else (error "Unknown type: ANGLE" z)]))

  (define (make-from-real-imag x y)
    (make-from-real-imag-rectangular x y))

  (define (make-from-mag-ang r a)
    (make-from-mag-ang-polar r a))
  ;; --------------------------------------------------

  (module+ test
    (#%require rackunit)
    (display "--> Section/2.4.2\n")

    (let ([z (make-from-real-imag 3 4)])
      (check-equal? (real-part z) 3)
      (check-equal? (imag-part z) 4)
      (check-within (magnitude z) 5 tolerance)
      (check-within (angle z) 0.927295 tolerance))

    (let ([z (make-from-mag-ang 5 0.927295)])
      (check-within (real-part z) 3 tolerance)
      (check-within (imag-part z) 4 tolerance)
      (check-equal? (magnitude z) 5)
      (check-equal? (angle z) 0.927295))))

(module Section/2.4.3 sicp
  (#%provide real-part
             imag-part
             magnitude
             angle
             make-from-real-imag
             make-from-mag-ang
             add-complex
             sub-complex
             mul-complex
             div-complex
             install-rectangular-package
             install-polar-package
             get
             put
             get-op-type-table
             clear-op-type-table
             apply-generic)
  (#%require (only racket/base module+ λ hash-set! hash-ref make-hash hash-clear!)
             (only racket/base local-require submod only-in)
             (only (submod "sicp1.rkt" common-utils) square tolerance)
             (only (submod ".." Section/2.4.2) attach-tag type-tag contents))

  #|
  Implementations of put and get are not given so in order to test the code I use a
  built-in hash-table to implement double dispatch with (op . type) as a key. Actually,
  we model multiple dispatch because, in the book, the second element in the cons (i.e.,
  the type) is (sometimes) a list.
  |#
  (define OP-TYPE-TABLE (make-hash))
  ;; see page 224
  (define (put op type item)
    (hash-set! OP-TYPE-TABLE (cons op type) item))
  (define (get op type)
    ;; #f is returned as a failure result
    (hash-ref OP-TYPE-TABLE (cons op type) #f))

  (define (clear-op-type-table)
    (hash-clear! OP-TYPE-TABLE))
  ;; this is useful (to examine the table) when I use put and get in another module
  (define (get-op-type-table)
    OP-TYPE-TABLE)

  (define (install-rectangular-package)
    (local-require (only-in (submod ".." Section/2.4.1 rectangular-package)
                            real-part
                            imag-part
                            magnitude
                            angle
                            make-from-mag-ang
                            make-from-real-imag))

    ;; interface to the rest of the system
    (define (tag x) (attach-tag 'rectangular x))
    (put 'real-part '(rectangular) real-part)
    (put 'imag-part '(rectangular) imag-part)
    (put 'magnitude '(rectangular) magnitude)
    (put 'angle '(rectangular) angle)
    (put 'make-from-real-imag 'rectangular (λ (x y) (tag (make-from-real-imag x y))))
    (put 'make-from-mag-ang 'rectangular (λ (r a) (tag (make-from-mag-ang r a))))
    'rectangular-package-installed)

  (define (install-polar-package)
    (local-require (only-in (submod ".." Section/2.4.1 polar-package)
                            real-part
                            imag-part
                            magnitude
                            angle
                            make-from-mag-ang
                            make-from-real-imag))

    (define (tag x) (attach-tag 'polar x))
    (put 'real-part '(polar) real-part)
    (put 'imag-part '(polar) imag-part)
    (put 'magnitude '(polar) magnitude)
    (put 'angle '(polar) angle)
    (put 'make-from-real-imag 'polar (λ (x y) (tag (make-from-real-imag x y))))
    (put 'make-from-mag-ang 'polar (λ (r a) (tag (make-from-mag-ang r a))))
    'polar-package-installed)

  (define (apply-generic op . args)
    (let* ([type-tags (map type-tag args)]
           [proc (get op type-tags)])
      (if proc
          (apply proc (map contents args))
          (error "No method for these types: APPLY-GENERIC"
                 (list op type-tags)))))

  (define (real-part z) (apply-generic 'real-part z))
  (define (imag-part z) (apply-generic 'imag-part z))
  (define (magnitude z) (apply-generic 'magnitude z))
  (define (angle z) (apply-generic 'angle z))

  (define (make-from-real-imag x y)
    ((get 'make-from-real-imag 'rectangular) x y))
  (define (make-from-mag-ang r a)
    ((get 'make-from-mag-ang 'polar) r a))

  (define (add-complex z1 z2)
    (make-from-real-imag (+ (real-part z1) (real-part z2))
                         (+ (imag-part z1) (imag-part z2))))
  (define (sub-complex z1 z2)
    (make-from-real-imag (- (real-part z1) (real-part z2))
                         (- (imag-part z1) (imag-part z2))))
  (define (mul-complex z1 z2)
    (make-from-mag-ang (* (magnitude z1) (magnitude z2))
                       (+ (angle z1) (angle z2))))
  (define (div-complex z1 z2)
    (make-from-mag-ang (/ (magnitude z1) (magnitude z2))
                       (- (angle z1) (angle z2))))

  (module+ test
    (#%require rackunit
               (only (submod "sicp1.rkt" common-utils) tolerance))
    (display "--> Section/2.4.3\n")

    (install-rectangular-package)
    (install-polar-package)

    (let ([z (make-from-real-imag 3 4)])
      (check-equal? (real-part z) 3)
      (check-equal? (imag-part z) 4)
      (check-within (magnitude z) 5 tolerance)
      (check-within (angle z) 0.927295 tolerance)
      (check-equal? (add-complex z z) (make-from-real-imag 6 8))
      (check-equal? (sub-complex z z) (make-from-real-imag 0 0))
      (check-equal? (magnitude (mul-complex z z)) 25)
      (check-within (angle (mul-complex z z)) 1.85459 tolerance)
      (check-equal? (div-complex z z) (make-from-mag-ang 1 0.0)))

    (let ([z (make-from-mag-ang 5 0.927295)])
      (check-within (real-part z) 3 tolerance)
      (check-within (imag-part z) 4 tolerance)
      (check-within (magnitude z) 5 tolerance)
      (check-within (angle z) 0.927295 tolerance))))

(module Exercise/2.73 sicp
  (#%require (only racket/base module+ λ for)
             (only (submod "sicp2_part2.rkt" Example/symbolic-differentiation)
                   variable?
                   same-variable?
                   addend
                   augend
                   multiplier
                   multiplicand
                   make-sum
                   make-product)
             (only (submod "sicp2_part2.rkt" Exercise/2.56)
                   base
                   exponent
                   make-exponentiation)
             (rename (submod "sicp2_part2.rkt" Exercise/2.56) deriv-original deriv)
             (only (submod ".." Section/2.4.3)
                   get
                   put
                   get-op-type-table
                   clear-op-type-table))

  #|
  Task A:
  There are two cases:
  1. Handle expressions with an operator (e.g., sum, product)
  2. Handle expressions without an operator: number?, variable?
  To include the latter case in the former we would need for the `operator` procedure to
  be able to return a 'no-operator token so that under the ('deriv . 'no-operator) key
  we could store:
  (λ (expr var)
     (cond [(number? exp) 0]
           [(variable? exp) (if (same-variable? exp var) 1 0)]))
  While this seems possible, it doesn't really help as the point is to make a dispatch
  on new operators (while the 'no-operator case could be handled once regardless where).
  |#
  (define (deriv exp var)
    (cond [(number? exp) 0]
          [(variable? exp) (if (same-variable? exp var) 1 0)]
          [else
           ((get 'deriv (operator exp))
            (operands exp) var)]))

  (define (operator exp) (car exp))
  #|
  Since I want to reuse the original code as is (which is the whole point of the
  Data-Directed Programming section), I changed the `operands` procedure to return the
  original expression instead of using (define (operands exp) (cdr exp)) which is given
  in the exercise. Note that in the case of e.g., the '+ operation we have defined
  (define (addend s) (cadr s))
  (define (augend s) (caddr s))
  i.e., addend and augend jump over the operator (so it has to be present).
  Of course, if we are ready to change addend and augend, we could return just the
  operands with (cdr exp) and then use
  (define (addend s) (car s))
  (define (augend s) (cadr s))
  Similarly for base and exponent.
  |#
  (define (operands exp) exp)

  ;; Task B
  (define (install-deriv-sum)
    (put 'deriv '+ (λ (expr var) (make-sum (deriv (addend expr) var)
                                           (deriv (augend expr) var))))
    'deriv-sum-installed)

  (define (install-deriv-product)
    (put 'deriv '* (λ (expr var) (make-sum
                                  (make-product (multiplier expr)
                                                (deriv (multiplicand expr) var))
                                  (make-product (deriv (multiplier expr) var)
                                                (multiplicand expr)))))
    'deriv-product-installed)

  ;; Task C
  (define (install-deriv-exponentiate)
    (put 'deriv '** (λ (expr var)
                      (let ([b (base expr)]
                            [n (exponent expr)])
                        (make-product
                         (make-product n (make-exponentiation b (make-sum n -1)))
                         (deriv b var)))))
    'deriv-exponentiate-installed)

  #|
  Task D:
  If we use (get (operator exp) 'deriv) instead of (get 'deriv (operator exp)) we would
  have to use:
  (put '+ 'deriv ...)
  (put '* 'deriv ...)
  (put '** 'deriv ...)
  instead of
  (put 'deriv '+ ...)
  (put 'deriv '* ...)
  (put 'deriv '** ...)
  |#

  (module+ test
    (#%require rackunit)
    (display "--> Exercise/2.73\n")

    (clear-op-type-table) ; because of the way tests are run
    (install-deriv-sum)
    (get-op-type-table)

    (install-deriv-product)
    (get-op-type-table)

    (install-deriv-exponentiate)
    (get-op-type-table)

    (let ([var 'x])
      (for ([expr '(x
                    y
                    (+ x 3)
                    (* x y)
                    (* (* x y) (+ x 3))
                    (** (+ (* 4 x) 3) 2))])
        (check-equal? (deriv expr var)
                      (deriv-original expr var))))))

(module Exercise/2.74 sicp
  (#%require (only racket/base module+ module)
             (only racket string<? string>? string=?))

  #|
  My specification:
  1. I will consider three divisions modeling sets in three different ways:
     - Paris: sets are modeled as unordered lists
     - Stockholm: sets are modeled as ordered lists
     - Tokyo: sets are modeled as binary trees
  2. The names of an employee are stored as a string. I assume that no two employees
     of a division have the same name (but there could be duplicates across divisions).
  3. The records of each division could contain different fields and could be structured
     differently (e.g., the order of fields need not be the same). Some fields could
     be stored e.g., as (cons "address" "some address").
  4. A division tag is specified per personal records file (rather than per record).
  5. The code of each division is self-contained, but to reduce the number of procedures
     in the interface I assume that the divisions use a common library that has get,
     put, procedures for handling named fields (this is just for convenience), etc.
  |#
  (module common-library sicp
    (#%provide make-field
               field->name
               field->value
               attach-tag
               put
               clear-op-type-table
               get-record
               get-record-key
               get-record-salary
               find-employee-record)
    (#%require (only racket/base λ)
               (only (submod "sicp2_part1.rkt" Section/2.2.3) filter)
               (only (submod ".." ".." Section/2.4.2) attach-tag)
               (only (submod ".." ".." Section/2.4.3)
                     get
                     put
                     clear-op-type-table))

    (define (make-field field-name field-value)
      (cons field-name field-value))
    (define (field->name field) (car field))
    (define (field->value field) (cdr field))

    (define (get-division tagged-object) (car tagged-object))
    (define (get-content tagged-object) (cdr tagged-object))

    (define (get-record employee-name personal-records-file)
      (let* ([division (get-division personal-records-file)]
             [records (get-content personal-records-file)]
             [proc (get 'get-record division)])
        (attach-tag division (proc employee-name records))))

    (define (get-record-key tagged-record)
      (let* ([division (get-division tagged-record)]
             [record (get-content tagged-record)]
             [proc (get 'get-record-key division)])
        (proc record)))

    (define (get-record-salary tagged-record)
      (let* ([division (get-division tagged-record)]
             [record (get-content tagged-record)]
             [proc (get 'get-record-salary division)])
        (proc record)))

    (define (find-employee-record employee-name list-of-division-files)
      (define (iter files acc)
        (cond [(null? files) acc]
              [else (iter (cdr files)
                          (cons (get-record employee-name (car files))
                                acc))]))
      (filter (λ (x) (cdr x)) (iter list-of-division-files '()))))

  (module division-paris sicp
    (#%provide install-interface personal-records-file)
    (#%require (only racket/base module+ λ)
               (only (submod ".." common-library)
                     make-field
                     field->name
                     field->value
                     attach-tag
                     put)
               (only (submod "sicp2_part2.rkt" Example/sets-as-unordered-lists)
                     adjoin-set))

    (define division-name 'paris)

    ;; ---------------------------------------------------------------------------------
    ;; division library
    ;; ---------------------------------------------------------------------------------
    (define (make-record name address salary)
      (list name
            (make-field "address" address)
            (make-field "salary" salary)))

    (define (get-record-key record) (car record))
    #|
    Clearly if the salary is always specified in a named field, we could have one
    procedure that finds it for any division, but the point of the exercise is for each
    division to have a different procedure (this is the same as with get-record-key).
    |#
    (define (get-record-salary record) (field->value (caddr record)))
    (define (get-record key records)
      (cond [(null? records) #f]
            [(string=? (get-record-key (car records)) key) (car records)]
            [else (get-record key (cdr records))]))

    (define (install-interface)
      (put 'get-record division-name get-record)
      (put 'get-record-key division-name get-record-key)
      (put 'get-record-salary division-name get-record-salary))

    ;; ---------------------------------------------------------------------------------

    (define personal-records
      (list (make-record "Gabriel Granger" "1 Rue de Rivoli" 1)
            (make-record "Noah Monet" "2 Avenue Montaigne" 2)
            (make-record "Jules Bernard" "3 Boulevard Saint-Germain" 3)
            (make-record "Louis Dumas" "4 Avenue Victor Hugo" 4)
            (make-record "Liam Petit" "5 Rue des Francs-Bourgeois" 5)
            (make-record "Ghost Employee" "Everywhere" 6)))
    (define personal-records-file (attach-tag division-name personal-records))

    (module+ test
      (#%require rackunit)
      (display "--> Exercise/2.74 (paris division)\n")

      (let ([name "Louis Dumas"]
            [record (make-record "Louis Dumas" "4 Avenue Victor Hugo" 4)])
        (check-equal? (get-record name personal-records) record)
        (check-false (get-record "Nonexistent Name" personal-records)))))

  (module division-stockholm sicp
    (#%provide install-interface personal-records-file)
    (#%require (only racket/base module+)
               (only (submod "sicp2_part1.rkt" Section/2.2.3) accumulate)
               (only (submod ".." common-library)
                     make-field
                     field->name
                     field->value
                     attach-tag
                     put))

    (define division-name 'stockholm)

    ;; ---------------------------------------------------------------------------------
    ;; division library
    ;; ---------------------------------------------------------------------------------
    (define (make-record name address salary)
      (list (make-field "name" name)
            (make-field "salary" salary)
            (make-field "address" address)))

    (define (get-record-key record) (field->value (car record)))
    (define (get-record-salary record) (field->value (cadr record)))
    ;; Similar to element-of-set? in Example/sets-as-ordered-lists
    (define (get-record x set)
      (cond [(null? set) #f]
            [(string=? x (get-record-key (car set))) (car set)]
            [(string<? x (get-record-key (car set))) #f]
            [else (get-record x (cdr set))]))

    ;; Unfortunately I cannot reuse adjoin-set from Exercise/2.61
    (define (adjoin-set x set)
      (if (null? set)
          (list x)
          (let ([head (car set)]
                [tail (cdr set)])
            (cond [(string=? (get-record-key x)
                             (get-record-key head)) set]
                  [(string<? (get-record-key x)
                             (get-record-key head)) (cons x set)]
                  [else (cons head (adjoin-set x tail))]))))

    (define (install-interface)
      (put 'get-record division-name get-record)
      (put 'get-record-key division-name get-record-key)
      (put 'get-record-salary division-name get-record-salary))

    ;; ---------------------------------------------------------------------------------

    (define personal-records
      (accumulate adjoin-set '()
                  (list (make-record "Acke Larsson" "1 Drottninggatan" 1)
                        (make-record "Astrid Nilsson" "2 Stockholm Stadsmission" 2)
                        (make-record "Gustav Olsson" "3 Biblioteksgatan" 3)
                        (make-record "Alexander Svensson" "4 Gamla Stan" 4)
                        (make-record "Ebba Berg" "5 Kungsgatan" 5)
                        (make-record "Ghost Employee" "Everywhere" 6))))
    (define personal-records-file (attach-tag division-name personal-records))

    (module+ test
      (#%require rackunit)
      (display "--> Exercise/2.74 (stockholm division)\n")

      (let ([name "Gustav Olsson"]
            [record (make-record "Gustav Olsson" "3 Biblioteksgatan" 3)])
        (check-equal? (get-record name personal-records) record)
        (check-false (get-record "Nonexistent Name" personal-records)))))

  (module division-tokyo sicp
    (#%provide install-interface personal-records-file)
    (#%require (only racket/base module+ parameterize)
               pict
               (only (submod "sicp1.rkt" conversion-utils) pict->file)
               (only (submod "sicp2_part1.rkt" Section/2.2.3) accumulate)
               (only (submod "sicp2_part2.rkt" Example/sets-as-binary-trees)
                     make-tree
                     entry
                     left-branch
                     right-branch
                     binary-tree->diagram
                     node->label)
               (only (submod ".." common-library)
                     make-field
                     field->name
                     field->value
                     attach-tag
                     put))

    (define division-name 'tokyo)

    ;; ---------------------------------------------------------------------------------
    ;; division library
    ;; ---------------------------------------------------------------------------------
    #|
    This record doesn't make much sense (I just want to have a different structure). But
    if someone insists for a motivation, well: in the japanese language the verb comes
    last, so why not put the key at the end :)
    |#
    (define (make-record name address salary)
      (list (make-field "salary" salary)
            (make-field "address" address)
            name))

    (define (get-record-key record)
      (define (list-last-element lst)
        (cond [(null? lst) '()]
              [(null? (cdr lst)) (car lst)]
              [else (list-last-element (cdr lst))]))
      (list-last-element record))
    (define (get-record-salary record) (field->value (car record)))

    ;; Similar to lookup-set-as-binary-tree in Exercise/2.66
    (define (get-record given-key set-of-records)
      (if (null? set-of-records)
          false
          (let* ([record (entry set-of-records)]
                 [record-key (get-record-key record)])
            (cond [(string=? given-key record-key) record]
                  [(string<? given-key record-key)
                   (get-record given-key
                               (left-branch set-of-records))]
                  [(string>? given-key record-key)
                   (get-record given-key
                               (right-branch set-of-records))]))))

    ;; Unfortunately I cannot reuse adjoin-set from Example/sets-as-binary-trees
    (define (adjoin-set x set)
      (cond ((null? set) (make-tree x '() '()))
            ((string=? (get-record-key x) (get-record-key (entry set))) set)
            ((string<? (get-record-key x) (get-record-key (entry set)))
             (make-tree (entry set)
                        (adjoin-set x (left-branch set))
                        (right-branch set)))
            ((string>? (get-record-key x) (get-record-key (entry set)))
             (make-tree (entry set)
                        (left-branch set)
                        (adjoin-set x (right-branch set))))))

    (define (tokyo-division-records->diagram tree)
      (define (tokyo-division-record->label tree)
        (let ([employee-name (get-record-key (entry tree))])
          (vc-append
           (text employee-name)
           (disk 30 #:color "white"))))

      (parameterize ([node->label tokyo-division-record->label])
        (binary-tree->diagram tree)))

    (define (install-interface)
      (put 'get-record division-name get-record)
      (put 'get-record-key division-name get-record-key)
      (put 'get-record-salary division-name get-record-salary))

    ;; ---------------------------------------------------------------------------------

    (define personal-records
      (accumulate adjoin-set '()
                  (list (make-record "Asahi Nakamura" "1 Yanaka Ginza" 1)
                        (make-record "Keiko Sizuki" "2 Takeshita-dori" 2)
                        (make-record "Haruki Ito" "3 Ameya Yokocho" 3)
                        (make-record "Akari Kobayashi" "4 Nakano" 4)
                        (make-record "Chika Ishita" "5 Omotesando" 5)
                        (make-record "Ghost Employee" "Everywhere" 6))))
    (define personal-records-file (attach-tag division-name personal-records))

    (module+ test
      (#%require rackunit)
      (display "--> Exercise/2.74 (tokyo division)\n")

      (pict->file (tokyo-division-records->diagram personal-records)
                  #:file "out/tokyo-personal-records-2.74.svg")

      (let ([name "Haruki Ito"]
            [record (make-record "Haruki Ito" "3 Ameya Yokocho" 3)])
        (check-equal? (get-record name personal-records) record)
        (check-false (get-record "Nonexistent Name" personal-records)))))

  (module+ test
    (#%require rackunit
               (only (submod ".." common-library)
                     clear-op-type-table
                     get-record
                     get-record-key
                     get-record-salary
                     find-employee-record)
               (rename (submod ".." division-paris)
                       install-interface-paris install-interface)
               (rename (submod ".." division-paris)
                       personal-records-file-paris personal-records-file)
               (rename (submod ".." division-stockholm)
                       install-interface-stockholm install-interface)
               (rename (submod ".." division-stockholm)
                       personal-records-file-stockholm personal-records-file)
               (rename (submod ".." division-tokyo)
                       install-interface-tokyo install-interface)
               (rename (submod ".." division-tokyo)
                       personal-records-file-tokyo personal-records-file))
    (display "--> Exercise/2.74\n")

    (clear-op-type-table) ; because of the way tests are run
    (install-interface-paris)
    (install-interface-stockholm)
    (install-interface-tokyo)

    #|
    Task A:
    Each division file should contains the division tag.

    Task B:
    Each record should contain the division tag but this need not be the case in the
    original records - we add it upon record retrieval. Note that I have renamed
    get-salary to get-record-salary.

    Task C:
    Since I assume that employees with the same name could exist, I return a list of
    employees found (at most one per division).

    Task D:
    Each new division should simply provide the procedure install-interface and
    personal-records-file (we store it in a variable but in reality it would be a file).
    No changes in get-record, get-record-key or get-record-salary are required.
    |#
    (let* ([employee-name "Gabriel Granger"]
           [record (get-record employee-name personal-records-file-paris)])
      (check-equal? (get-record-key record) employee-name)
      (check-equal? (get-record-salary record) 1))

    (let* ([employee-name "Alexander Svensson"]
           [record (get-record employee-name personal-records-file-stockholm)])
      (check-equal? (get-record-key record) employee-name)
      (check-equal? (get-record-salary record) 4))

    (let* ([employee-name "Keiko Sizuki"]
           [record (get-record employee-name personal-records-file-tokyo)])
      (check-equal? (get-record-key record) employee-name)
      (check-equal? (get-record-salary record) 2))

    (define personal-records-files (list personal-records-file-paris
                                         personal-records-file-stockholm
                                         personal-records-file-tokyo))

    (check-equal?
     (length (find-employee-record "Keiko Sizuki" personal-records-files))
     1)

    (check-equal?
     (length (find-employee-record "Ghost Employee" personal-records-files))
     3)))

(module Exercise/2.75 sicp
  (#%require (only racket/base module+)
             (only (submod "sicp1.rkt" common-utils) square tolerance))

  (define (make-from-real-imag x y)
    (define (dispatch op)
      (cond ((eq? op 'real-part) x)
            ((eq? op 'imag-part) y)
            ((eq? op 'magnitude) (sqrt (+ (square x) (square y))))
            ((eq? op 'angle) (atan y x))
            (else (error "Unknown op: MAKE-FROM-REAL-IMAG" op))))
    dispatch)

  (define (make-from-mag-ang r a)
    (define (dispatch op)
      (cond ((eq? op 'real-part) (* r (cos a)))
            ((eq? op 'imag-part) (* r (sin a)))
            ((eq? op 'magnitude) r)
            ((eq? op 'angle) a)
            (else (error "Unknown op: MAKE-FROM-MAG-ANG" op))))
    dispatch)

  (define (apply-generic op arg) (arg op))

  (module+ test
    (#%require rackunit)
    (display "--> Exercise/2.75\n")

    (let ([z (make-from-real-imag 3 4)])
      (check-equal? (apply-generic 'real-part z) 3)
      (check-equal? (apply-generic 'imag-part z) 4)
      (check-within (apply-generic 'magnitude z) 5 tolerance)
      (check-within (apply-generic 'angle z) 0.927295 tolerance))

    (let ([z (make-from-mag-ang 5 0.927295)])
      (check-within (apply-generic 'real-part z) 3 tolerance)
      (check-within (apply-generic 'imag-part z) 4 tolerance)
      (check-equal? (apply-generic 'magnitude z) 5)
      (check-equal? (apply-generic 'angle z) 0.927295))))

(module Exercise/2.76 sicp
  #|
  1. generic operations with explicit dispatch
     - adding new types: each new type requires adding a case under each operation
     - adding new operations: simply add the operation (handle all types inside)

  2. data-directed style
     Adding a new type/operation requires updating the interface. That is, adding a
     new key (operation . type). for all new operation/type. In addition installing the
     interface is required.

  3. objects with dispatch (message-passing)
     - adding new types: simply add the type (handle all operations inside)
     - adding new operations: each new operation requires adding a case under each type

  When only new types must often be added, "message-passing" seems appropriate.
  When only new operations must often be added, "generic operations with explicit
  dispatch" seems appropriate. In both cases using "data-directed style" could be
  reasonable (assuming that we have already committed to maintaining a global table).
  |#)

(module Section/2.5.1 sicp
  #|
  In this module I organize the code in racket's units. This helps a lot with code reuse
  in subsequent exercises. For example in Exercise/2.78 we have to update
  attach-tag, type-tag, contents and apply-generic and in order for the new versions of
  these procedures to take effect, we need to re-evaluate much of the code here. Using
  units (https://docs.racket-lang.org/guide/units.html) helps avoiding the copy/paste.
  See racket-examples/example-units.rkt for a simple example of using units.
  |#
  (#%provide generic-arithmetic-package@
             generic-arithmetic-package-imports^
             generic-arithmetic-package-exports^)
  (#%require (only racket/base module+ module λ)
             (only racket/unit
                   define-signature
                   define-unit
                   import
                   export
                   define-values/invoke-unit/infer)
             ;; used in racket-numbers-package@
             (only (submod "sicp2_part1.rkt" Exercise/2.1)
                   make-rat
                   add-rat
                   sub-rat
                   mul-rat
                   div-rat)
             (rename (submod "sicp2_part1.rkt" Exercise/2.1) numer-rat numer)
             (rename (submod "sicp2_part1.rkt" Exercise/2.1) denom-rat denom)
             ;; used in complex-numbers-package@
             (only (submod ".." Section/2.4.3)
                   real-part
                   imag-part
                   magnitude
                   angle
                   make-from-real-imag
                   make-from-mag-ang
                   add-complex
                   sub-complex
                   mul-complex
                   div-complex
                   install-rectangular-package
                   install-polar-package)
             (rename (submod ".." Section/2.4.3) real-part-complex real-part)
             (rename (submod ".." Section/2.4.3) imag-part-complex imag-part)
             (rename (submod ".." Section/2.4.3) magnitude-complex magnitude)
             (rename (submod ".." Section/2.4.3) angle-complex angle)
             ;; to satisfy the generic-arithmetic-package-imports^
             (only (submod ".." Section/2.4.3) apply-generic get put)
             (only (submod ".." Section/2.4.2) attach-tag))

  (define-signature generic-arithmetic-package-imports^
    (apply-generic
     get
     attach-tag
     put))

  (define-signature generic-arithmetic-package-exports^
    (add
     sub
     mul
     div
     make-racket-number
     make-rational
     make-complex-from-real-imag
     make-complex-from-mag-ang
     real-part
     imag-part
     magnitude
     angle
     numer
     denom
     install-generic-arithmetic-package))

  (define-unit generic-arithmetic-package@
    (import generic-arithmetic-package-imports^)
    (export generic-arithmetic-package-exports^)

    (define (install-racket-numbers-package)
      (define (tag x) (attach-tag 'racket-number x))
      (put 'add '(racket-number racket-number) (λ (x y) (tag (+ x y))))
      (put 'sub '(racket-number racket-number) (λ (x y) (tag (- x y))))
      (put 'mul '(racket-number racket-number) (λ (x y) (tag (* x y))))
      (put 'div '(racket-number racket-number) (λ (x y) (tag (/ x y))))
      (put 'make 'racket-number (λ (x) (tag x)))
      'racket-numbers-package-installed)

    (define (install-rational-numbers-package)
      (define (tag x) (attach-tag 'rational x))
      (put 'add '(rational rational) (λ (x y) (tag (add-rat x y))))
      (put 'sub '(rational rational) (λ (x y) (tag (sub-rat x y))))
      (put 'mul '(rational rational) (λ (x y) (tag (mul-rat x y))))
      (put 'div '(rational rational) (λ (x y) (tag (div-rat x y))))
      (put 'make 'rational (λ (n d) (tag (make-rat n d))))

      (put 'numer '(rational) numer-rat)
      (put 'denom '(rational) denom-rat)
      'rational-numbers-package-installed)

    (define (install-complex-numbers-package)
      ;; install dependencies
      (install-rectangular-package)
      (install-polar-package)

      (define (tag z) (attach-tag 'complex z))
      (put 'add '(complex complex) (λ (z1 z2) (tag (add-complex z1 z2))))
      (put 'sub '(complex complex) (λ (z1 z2) (tag (sub-complex z1 z2))))
      (put 'mul '(complex complex) (λ (z1 z2) (tag (mul-complex z1 z2))))
      (put 'div '(complex complex) (λ (z1 z2) (tag (div-complex z1 z2))))
      (put 'make-from-real-imag 'complex (λ (x y) (tag (make-from-real-imag x y))))
      (put 'make-from-mag-ang 'complex (λ (r a) (tag (make-from-mag-ang r a))))

      (put 'real-part '(complex) real-part-complex)
      (put 'imag-part '(complex) imag-part-complex)
      (put 'magnitude '(complex) magnitude-complex)
      (put 'angle '(complex) angle-complex)
      'complex-numbers-package-installed)

    (define (install-generic-arithmetic-package)
      (install-racket-numbers-package)
      (install-rational-numbers-package)
      (install-complex-numbers-package)
      'generic-arithmetic-package-installed)

    ;; operations
    (define (add x y) (apply-generic 'add x y))
    (define (sub x y) (apply-generic 'sub x y))
    (define (mul x y) (apply-generic 'mul x y))
    (define (div x y) (apply-generic 'div x y))

    ;; constructors
    (define (make-racket-number n) ((get 'make 'racket-number) n))
    (define (make-rational n d)
      ; note that (integer? 1.0) gives #t
      (if (and (integer? n)
               (exact? n)
               (integer? d)
               (exact? d))
          ((get 'make 'rational) n d)
          (error "Integer input expected:" n d)))
    (define (make-complex-from-real-imag x y) ((get 'make-from-real-imag 'complex) x y))
    (define (make-complex-from-mag-ang r a) ((get 'make-from-mag-ang 'complex) r a))

    ;; selectors
    (define (real-part n) (apply-generic 'real-part n))
    (define (imag-part n) (apply-generic 'imag-part n))
    (define (magnitude n) (apply-generic 'magnitude n))
    (define (angle n) (apply-generic 'angle n))

    (define (numer n) (apply-generic 'numer n))
    (define (denom n) (apply-generic 'denom n)))

  (define-values/invoke-unit/infer generic-arithmetic-package@)

  (module+ test
    (#%require rackunit
               (only (submod "sicp1.rkt" common-utils) tolerance)
               (only (submod ".." ".." Section/2.4.3) clear-op-type-table))
    (display "--> Section/2.5.1\n")

    (clear-op-type-table)
    (install-generic-arithmetic-package)

    (let ([constructor make-racket-number])
      (let ([x (constructor 10)]
            [y (constructor 5)])
        (check-equal? (add x y) (constructor 15))
        (check-equal? (sub x y) (constructor 5))
        (check-equal? (mul x y) (constructor 50))
        (check-equal? (div x y) (constructor 2))))

    (let ([constructor make-rational])
      (let ([x (constructor 10 3)]
            [y (constructor 6 4)])
        (check-equal? (add x y) (constructor 29 6))
        (check-equal? (sub x y) (constructor 11 6))
        (check-equal? (mul x y) (constructor 5 1))
        (check-equal? (div x y) (constructor 20 9))
        (check-equal? (numer x) 10)
        (check-equal? (denom x) 3)))

    (let ([z (make-complex-from-real-imag 3 4)])
      (check-equal? (add z z) (make-complex-from-real-imag 6 8))
      (check-equal? (sub z z) (make-complex-from-real-imag 0 0))
      (check-equal? (real-part z) 3)
      (check-equal? (imag-part z) 4)
      (check-equal? (magnitude (mul z z)) 25)
      (check-within (angle (mul z z)) 1.85459 tolerance)
      (check-equal? (div z z) (make-complex-from-mag-ang 1 0.0)))))

(module Exercise/2.77 sicp
  (#%require (only racket/base module+)
             (only racket/unit define-values/invoke-unit/infer)
             (only (submod ".." Section/2.5.1) generic-arithmetic-package@)
             ;; to satisfy the generic-arithmetic-package-imports^
             (only (submod ".." Section/2.4.3) apply-generic get put)
             (only (submod ".." Section/2.4.2) attach-tag))

  (define-values/invoke-unit/infer generic-arithmetic-package@)

  (module+ test
    (#%require rackunit
               (only (submod "sicp1.rkt" common-utils) tolerance))
    (display "--> Exercise/2.77\n")

    (install-generic-arithmetic-package)
    (check-equal? (magnitude (make-complex-from-mag-ang 5 1)) 5)
    #|
    0. (magnitude (complex polar 5 . 1))
    1. (apply-generic 'magnitude (complex polar 5 . 1))
       This executes the procedure stored at (cons 'magnitude '(complex)) with
       (polar 5 . 1), as an argument i.e., (cdr (complex polar 5 . 1)).
    2. (define magnitude-of-complex-number (get 'magnitude '(complex)))
    3. (magnitude-of-complex-number (polar 5 . 1))
    4. (apply-generic 'magnitude (polar 5 . 1))
       This executes the procedure stored at (cons 'magnitude '(polar)). Note that in
       install-complex-numbers-package we have installed the polar-package (from
       Section/2.4.1) as dependency and this is what provides at the
       (cons 'magnitude '(polar)) key the procedure used to find the magnitude of
       complex numbers in polar form.
    5. (define magnitude-of-complex-number-in-polar-form (get 'magnitude '(polar)))
    6. (magnitude-of-complex-number-in-polar-form (cdr (polar 5 . 1)))
       This applies the procedure (define (magnitude z) (car z)) from the polar-package
       resulting in a 5

    Hence, apply-generic is invoked twice (in steps 1 and 4 above).
    |#))

(module Exercise/2.78 sicp
  #|
  This exercise requires a very simple change but doing it naively leads to copy/pasting
  a large part of the code in Section/2.5.1. To avoid this I use racket's units.
  |#
  (#%provide type-tag
             contents
             attach-tag
             apply-generic)
  (#%require (only racket/base module+ module)
             (only racket/unit define-values/invoke-unit/infer)
             (only (submod ".." Section/2.5.1) generic-arithmetic-package@)
             ;; to partially satisfy the generic-arithmetic-package-imports^
             (only (submod ".." Section/2.4.3) get)
             (only (submod ".." Section/2.4.3) put))

  ;; this module contains all the necessary changes
  (module necessary-changes sicp
    (#%provide type-tag
               contents
               attach-tag)

    (define (attach-tag type-tag contents)
      (if (number? contents)
          contents
          (cons type-tag contents)))

    (define (type-tag datum)
      (cond [(number? datum) 'racket-number]
            [(pair? datum) (car datum)]
            [else (error "Bad tagged datum: TYPE-TAG" datum)]))

    (define (contents datum)
      (cond [(number? datum) datum]
            [(pair? datum) (cdr datum)]
            [else (error "Bad tagged datum: CONTENTS" datum)])))

  (#%require (only (submod "." necessary-changes) attach-tag type-tag contents))

  (define (apply-generic op . args)
    (let* ([type-tags (map type-tag args)]
           [proc (get op type-tags)])
      (if proc
          (apply proc (map contents args))
          (error "No method for these types: APPLY-GENERIC"
                 (list op type-tags)))))

  ;; evaluate packages with updated: attach-tag, type-tag, contents and apply-generic
  (define-values/invoke-unit/infer generic-arithmetic-package@)

  (module+ test
    (#%require rackunit
               (only (submod "sicp1.rkt" common-utils) tolerance)
               (only (submod ".." ".." Section/2.4.3) clear-op-type-table))
    (display "--> Exercise/2.78\n")

    (clear-op-type-table)
    (install-generic-arithmetic-package)

    ;; verify that racket numbers can be used directly
    (let ([x 10]
          [y 5])
      (check-equal? (add x y) 15)
      (check-equal? (sub x y) 5)
      (check-equal? (mul x y) 50)
      (check-equal? (div x y) 2))

    ;; we can still use make-racket-number
    (let ([constructor make-racket-number])
      (let ([x (constructor 10)]
            [y (constructor 5)])
        (check-equal? (add x y) (constructor 15))
        (check-equal? (sub x y) (constructor 5))
        (check-equal? (mul x y) (constructor 50))
        (check-equal? (div x y) (constructor 2))))

    (let ([constructor make-rational])
      (let ([x (constructor 10 3)]
            [y (constructor 6 4)])
        (check-equal? (add x y) (constructor 29 6))
        (check-equal? (sub x y) (constructor 11 6))
        (check-equal? (mul x y) (constructor 5 1))
        (check-equal? (div x y) (constructor 20 9))
        (check-equal? (numer x) 10)
        (check-equal? (denom x) 3)))

    (let ([z (make-complex-from-real-imag 3 4)])
      (check-equal? (add z z) (make-complex-from-real-imag 6 8))
      (check-equal? (sub z z) (make-complex-from-real-imag 0 0))
      (check-equal? (real-part z) 3)
      (check-equal? (imag-part z) 4)
      (check-equal? (magnitude (mul z z)) 25)
      (check-within (angle (mul z z)) 1.85459 tolerance)
      (check-equal? (div z z) (make-complex-from-mag-ang 1 0.0)))))

(module Exercise/2.79 sicp
  (#%provide equ?
             approx-equ?
             install-generic-arithmetic-package-equality)
  (#%require (only racket/base module+ λ)
             (only racket/unit define-values/invoke-unit/infer)
             (only (submod "sicp2_part1.rkt" Exercise/2.1) equal-rat?)
             (only (submod ".." Section/2.5.1) generic-arithmetic-package@)
             (only (submod ".." Section/2.4.3) get put)
             (only (submod ".." Exercise/2.78) apply-generic attach-tag))

  (define-values/invoke-unit/infer generic-arithmetic-package@)

  (define (equ? x y) (apply-generic 'equ? x y))
  (define (approx-equ? x y tol) (apply-generic 'approx-equ? x y tol))

  (define (install-generic-arithmetic-package-equality)
    (put 'equ? '(racket-number racket-number) (λ (x y) (= x y)))
    (put 'equ? '(rational rational) (λ (x y) (equal-rat? x y)))
    (put 'equ? '(complex complex) (λ (x y) (and (= (real-part x) (real-part y))
                                                (= (imag-part x) (imag-part y)))))
    (put 'approx-equ? '(racket-number racket-number racket-number)
         (λ (x y tol)
           (< (abs (- x y)) tol)))
    (put 'approx-equ? '(rational rational racket-number)
         (λ (x y tol)
           (and (< (abs (- (numer x) (numer y))) tol)
                (< (abs (- (denom x) (denom y))) tol))))
    (put 'approx-equ? '(complex complex racket-number)
         (λ (x y tol)
           (and (< (abs (- (real-part x) (real-part y))) tol)
                (< (abs (- (imag-part x) (imag-part y))) tol))))
    'generic-arithmetic-package-equality-installed)

  (module+ test
    (#%require rackunit
               (only racket/math pi)
               (only (submod "sicp1.rkt" common-utils) tolerance)
               (only (submod ".." ".." Section/2.4.3) clear-op-type-table))
    (display "--> Exercise/2.79\n")

    (clear-op-type-table)
    (install-generic-arithmetic-package)
    (install-generic-arithmetic-package-equality)

    (check-true (equ? 4 4))
    (check-false (equ? 4 5))
    (check-true (equ? (make-rational 3 5) (make-rational 6 10)))
    (check-false (equ? (make-rational 3 5) (make-rational 6 11)))
    (let* ([z1 (make-complex-from-real-imag 3 5)]
           [z2 (make-complex-from-real-imag 3 6)]
           [m1 (magnitude z1)]
           [a1 (angle z1)]
           [z3 (make-complex-from-mag-ang m1 a1)]
           [z4 (make-complex-from-mag-ang m1 (+ a1 (* 2 pi)))])
      (check-true (equ? z1 z1))
      (check-false (equ? z1 z2))
      (check-true (equ? z3 z3))
      (check-false (equ? z3 z4)) ; false due to double precision
      (check-true (approx-equ? z3 z4 tolerance))
      (check-true (approx-equ? z1 z3 tolerance)))))

(module Exercise/2.80 sicp
  (#%provide =zero?
             install-generic-arithmetic-package-zero)
  (#%require (only racket/base module+ λ)
             (only racket/unit define-values/invoke-unit/infer)
             (only (submod ".." Exercise/2.79)
                   equ?
                   install-generic-arithmetic-package-equality)
             ;; (only (submod "sicp2_part1.rkt" Exercise/2.1) numer)
             (only (submod ".." Section/2.5.1) generic-arithmetic-package@)
             (only (submod ".." Section/2.4.3) get put)
             (only (submod ".." Exercise/2.78) apply-generic attach-tag))

  (define-values/invoke-unit/infer generic-arithmetic-package@)

  (define (=zero? x) (apply-generic '=zero? x))

  (define (install-generic-arithmetic-package-zero)
    (put '=zero? '(racket-number) (λ (x) (equ? x 0)))
    (put '=zero? '(rational) (λ (x) (equ? (numer (attach-tag 'rational x)) 0)))
    (put '=zero? '(complex) (λ (x) (equ? (attach-tag 'complex x)
                                         (make-complex-from-real-imag 0 0))))
    'generic-arithmetic-package-=zero-installed)

  (module+ test
    (#%require rackunit
               (only (submod ".." ".." Section/2.4.3) clear-op-type-table))
    (display "--> Exercise/2.80\n")

    (clear-op-type-table)
    (install-generic-arithmetic-package)
    (install-generic-arithmetic-package-equality)
    (install-generic-arithmetic-package-zero)

    (check-true (=zero? 0))
    (check-false (=zero? 1))
    (check-true (=zero? (make-rational 0 1)))
    (check-false (=zero? (make-rational 1 1)))
    (check-true (=zero? (make-complex-from-real-imag 0 0)))
    (check-false (=zero? (make-complex-from-real-imag 1 1)))))

(module Exercise/2.81 sicp
  (#%provide put-coercion
             get-coercion
             clear-coercion-table
             get-coercion-table
             install-coercion-racket-number->complex)
  (#%require (only racket/base module+
                   λ
                   format
                   hash-set!
                   hash-ref
                   make-hash
                   hash-clear!
                   exn:fail?)
             (only racket/unit define-values/invoke-unit/infer)
             (only (submod ".." Section/2.5.1) generic-arithmetic-package@)
             (only (submod ".." Exercise/2.78) type-tag contents)
             (only (submod ".." Section/2.4.3) get put clear-op-type-table)
             (only (submod ".." Exercise/2.78) attach-tag))

  (define COERCION-TABLE (make-hash))
  (define (put-coercion type1 type2 item)
    (hash-set! COERCION-TABLE (cons type1 type2) item))
  (define (get-coercion type1 type2)
    (hash-ref COERCION-TABLE (cons type1 type2) #f))

  (define (clear-coercion-table)
    (hash-clear! COERCION-TABLE))
  (define (get-coercion-table)
    COERCION-TABLE)

  #|
  NOTE:
  This is the modified version of apply-generic as per Task C. It completes the
  implementation of generic-arithmetic-package-imports^
  |#
  (define (apply-generic op . args)
    (display "apply-generic\n")
    (let ([type-tags (map type-tag args)])
      (let ([proc (get op type-tags)])
        (if proc
            (apply proc (map contents args))
            (let ([type1 (car type-tags)]
                  [type2 (cadr type-tags)])
              (if (and (= (length args) 2)
                       (not (eq? type1 type2)))
                  (let ([a1 (car args)]
                        [a2 (cadr args)])
                    (let ([t1->t2 (get-coercion type1 type2)]
                          [t2->t1 (get-coercion type2 type1)])
                      (cond [t1->t2 (apply-generic op (t1->t2 a1) a2)]
                            [t2->t1 (apply-generic op a1 (t2->t1 a2))]
                            [else (error "No method for these types"
                                         (list op type-tags))])))
                  (error "No method for these types"
                         (list op type-tags))))))))

  (define-values/invoke-unit/infer generic-arithmetic-package@)

  (define (exp x y)
    (display "-------------------- exp --------------------\n")
    (apply-generic 'exp x y))

  #|
  This is an addition to the racket-numbers-package. Note that in the book they add a
  tag (cons 'racket-number (expt x y)) but I use the code from Exercise/2.78 where we
  don't need tags for racket numbers.
  |#
  (define (install-racket-number-exp)
    (put 'exp '(racket-number racket-number) (λ (x y) (expt x y))))

  (define (install-coercion-racket-number->complex)
    (put-coercion 'racket-number
                  'complex
                  (λ (n) (make-complex-from-real-imag (contents n) 0))))

  (define (install-self-type-coercion)
    (put-coercion 'racket-number 'racket-number (λ (x) x))
    (put-coercion 'complex 'complex (λ (x) x)))

  #|
  Task A:
  With the suggestion of Louis Reasoner, evaluating exp with two complex numbers results
  in an infinite loop - we keep calling apply-generic with the same arguments. Calling
  exp with a complex number and a racket number results in the same infinite loop after
  one 'racket-number to 'complex coercion.

  Task B:
  The original apply-generic (from page 265) works correctly - it raises an error when
  an operation is not defined for two arguments with the same type.

  Task C:
  The modified apply-generic is given above.
  |#

  (module+ test
    (#%require rackunit)
    (display "--> Exercise/2.81\n")

    (clear-op-type-table)
    (clear-coercion-table)
    (install-generic-arithmetic-package)
    (install-racket-number-exp)
    (install-coercion-racket-number->complex)
    ; install Louis' coercion as it is never used with the modified apply-generic
    (install-self-type-coercion)

    (check-equal? (exp 2 3) 8)
    (check-exn exn:fail? (λ () (exp (make-complex-from-real-imag 1 1)
                                    (make-complex-from-real-imag 1 1))))
    (check-exn exn:fail? (λ () (exp 1
                                    (make-complex-from-real-imag 1 1))))))

(module Exercise/2.82 sicp
  (#%require (only racket/base module+ λ format let-values exn:fail?)
             (only (submod "sicp2_part1.rkt" Section/2.2.3) accumulate)
             (only (submod "sicp2_part2.rkt" Example/sets-as-unordered-lists)
                   adjoin-set)
             (only racket/unit define-values/invoke-unit/infer)
             (only (submod ".." Section/2.5.1) generic-arithmetic-package@)
             (only (submod ".." Exercise/2.79)
                   equ?
                   install-generic-arithmetic-package-equality)
             (only (submod ".." Exercise/2.81)
                   get-coercion
                   clear-coercion-table
                   install-coercion-racket-number->complex)
             (only (submod ".." Exercise/2.78) type-tag contents)
             (only (submod ".." Section/2.4.3)
                   get
                   put
                   clear-op-type-table)
             (only (submod ".." Exercise/2.78) attach-tag))

  #|
  Below I limit myself to generalizing apply-generic according to the strategy given
  in the exercise, i.e., attempt to coerce all arguments to the type of the first
  argument and if this is not possble attempt to coerce all arguments to the type of the
  second argument etc. A better strategy would be developed in the next exercises.

  As an example demonstrating that our strategy is not general enough consider
  (add3 1 2 3), where add3 is defined only for complex numbers. Clearly, there is no
  way for our strategy to identify 'complex as a valid target type for coercion even
  though a racket-number->complex conversion is defined. See end of test section for
  verification.
  |#

  ;; Return coercion procedures for a given target type.
  (define (get-coercion-procedures target-type types)
    (cond [(null? types) '()]
          [else (cons (if (eq? (car types) target-type)
                          (λ (x) x)
                          (get-coercion (car types) target-type))
                      (get-coercion-procedures target-type (cdr types)))]))

  (define (all-defined lst)
    (accumulate (lambda (x y) (and x y)) #t lst))

  ;; Return a valid coercion target type and the associated coercion procedures.
  (define (get-vaild-coercion-procedures all-types)
    (define (helper candidate-types)
      (if (null? candidate-types)
          (values #f #f)
          (let ([procedures (get-coercion-procedures (car candidate-types) all-types)])
            (if (all-defined procedures)
                (values (car candidate-types) procedures)
                (helper (cdr candidate-types))))))
    (helper all-types))

  (define (apply-generic op . args)
    (let ([type-tags (map type-tag args)])
      (let ([proc (get op type-tags)])
        (display (format "[~a] ~a\n" op args))
        (display (format "type-tags: ~a\n" type-tags))
        (display (format "proc: ~a\n" proc))
        (display "---------------------------------------------\n")
        (if proc
            (apply proc (map contents args))
            (if (= 1 (length (accumulate adjoin-set '() type-tags)))
                (error "Same types: no progress is possible with current strategy.")
                (let-values ([(target-type coercion-procedures)
                              (get-vaild-coercion-procedures type-tags)])
                  (display (format "[~a] ~a\n" target-type coercion-procedures))
                  (display "=============================================\n")
                  (if coercion-procedures
                      (apply apply-generic
                             op
                             (map (λ (f x) (f x)) coercion-procedures args))
                      (error "No method for these types"
                             (list op type-tags)))))))))

  (define-values/invoke-unit/infer generic-arithmetic-package@)

  (define (add3 x y z)
    (apply-generic 'add3 x y z))

  ;; this could be seen as an addition to the complex-numbers-package
  (define (install-complex-numbers-add3)
    (define (tag z) (attach-tag 'complex z))
    (put 'add3 '(complex complex complex) (λ (x y z)
                                            (add (add (tag x) (tag y)) (tag z)))))

  (module+ test
    (#%require rackunit)
    (display "--> Exercise/2.82\n")

    (clear-op-type-table)
    (clear-coercion-table)
    (install-generic-arithmetic-package)
    (install-complex-numbers-add3)
    (install-coercion-racket-number->complex)
    (install-generic-arithmetic-package-equality)

    (check-true
     (all-defined
      (get-coercion-procedures 'complex '(racket-number racket-number))))

    (check-false
     (all-defined
      (get-coercion-procedures 'racket-number '(racket-number complex racket-number))))

    (check-false
     (all-defined
      (get-coercion-procedures 'missing '(racket-number complex racket-number))))

    (check-false
     (all-defined
      (get-coercion-procedures 'racket-number '(missing complex racket-number))))

    (let-values ([(target-type coercion-procedures)
                  (get-vaild-coercion-procedures '(racket-number complex))])
      (check-eq? target-type 'complex))

    (let ([x (make-complex-from-real-imag 1 2)]
          [y (make-complex-from-real-imag 3 4)]
          [z (make-complex-from-real-imag 5 6)])
      (check-true (equ? (add3 x y z)
                        (make-complex-from-real-imag 9 12)))
      (check-true (equ? (add3 x y 3)
                        (make-complex-from-real-imag 7 6)))
      (check-true (equ? (add3 x 2 3)
                        (make-complex-from-real-imag 6 2)))
      ;; Demonstrate a limitation of our strategy - complex cannot be identified here
      (check-exn exn:fail? (λ () (add3 1 2 3))))))

(module Exercise/2.83 sicp
  (#%require (only racket/base module+ λ exn:fail?)
             (only racket/unit define-values/invoke-unit/infer)
             (only (submod ".." Section/2.5.1) generic-arithmetic-package@)
             (only (submod ".." Exercise/2.78) contents attach-tag)
             (only (submod ".." Exercise/2.81)
                   get-coercion
                   put-coercion
                   clear-coercion-table)
             (only (submod ".." Section/2.4.3) get put clear-op-type-table))

  #|
  In addition to the types we have already defined in our generic arithmetic package,
  the tower (integer -> rational -> real -> complex) in Figure 2.25 contains an integer
  type. I introduce a racket-integer type below and assume that a racket-number is a
  floating-point number. I focus only on the raise operation and don't implement add,
  sub, mul, div for the integer type.

  The tower can be modelled in many ways. Below I test two approaches:
  version 1: define as a coercion in the coercion-table
  version 2: define as an operation in the op-type-table
  |#

  (define (type-tag datum)
    (cond [(and (integer? datum) (exact? datum)) 'racket-integer] ; order matters
          [(number? datum) 'racket-number]
          [(pair? datum) (car datum)]
          [else (error "Bad tagged datum: TYPE-TAG" datum)]))

  ;; this is apply-generic from Exercise/2.78 but it has to take type-tag into account
  (define (apply-generic op . args)
    (let* ([type-tags (map type-tag args)]
           [proc (get op type-tags)])
      (if proc
          (apply proc (map contents args))
          (error "No method for these types: APPLY-GENERIC"
                 (list op type-tags)))))

  (define-values/invoke-unit/infer generic-arithmetic-package@)

  (define (make-racket-integer x)
    (if (and (integer? x)
             (exact? x))
        x
        (error "Input is not integer:" x)))

  ;; -----------------------------------------------------------------------------------
  ;; coercion routines
  ;; -----------------------------------------------------------------------------------
  (define (racket-integer->rational n)
    (make-rational n 1))

  #|
  I distinguish between racket-integer and racket-number using integer?/exact? and
  number? (i.e., I don't use a tag starting from Exercise/2.78) so I have to use
  exact->inexact when I convert rational to racket-number, otherwise e.g., (/ 4 2)
  would give me a racket-integer while I want it to be a racket-number.

  NOTE:
  When one wants to use raise-v1 with racket-integer and racket-number that have tags
  one has to use `(contents n)` instead of `n` in racket-integer->rational and
  racket-number->complex. I could use it even now (as it would have no effect).
  |#
  (define (rational->racket-number n)
    (exact->inexact (/ (numer n)
                       (denom n))))

  (define (racket-number->complex n)
    (make-complex-from-real-imag n 0))
  ;; -----------------------------------------------------------------------------------

  (define (install-tower-of-types-v1)
    (put-coercion 'racket-integer 'next-tower-level racket-integer->rational)
    (put-coercion 'rational 'next-tower-level rational->racket-number)
    (put-coercion 'racket-number 'next-tower-level racket-number->complex)
    (put-coercion 'complex 'next-tower-level
                  (λ (n)
                    (display "WARNING: complex is the top of the hierarchy\n")
                    n)))

  #|
  Here I don't use complex->complex because I would have to define a raise operation
  for both the rectangular and polar representation of a complex number (and anyway, it
  is not required in the exercise).
  |#
  (define (install-tower-of-types-v2)
    (put 'raise '(racket-integer) racket-integer->rational)
    (put 'raise '(rational)
         ;; I need to add a tag because I use numer/denom in rational->racket-number
         (λ (n) (rational->racket-number (attach-tag 'rational n))))
    (put 'raise '(racket-number) racket-number->complex))

  (define (raise-v1 n)
    ((get-coercion (type-tag n) 'next-tower-level) n))

  (define (raise-v2 n)
    (apply-generic 'raise n))

  (module+ test
    (#%require rackunit)
    (display "--> Exercise/2.83\n")

    (clear-op-type-table)
    (clear-coercion-table)
    (install-generic-arithmetic-package)
    (install-tower-of-types-v1)
    (install-tower-of-types-v2)

    (check-exn exn:fail? (λ () (make-racket-integer 1.0)))
    (let* ([i 2]
           [r (raise-v1 i)]
           [n (raise-v1 r)]
           [c (raise-v1 n)]
           [cc (raise-v1 c)])
      (check-eq? (type-tag i) 'racket-integer)
      (check-eq? (type-tag r) 'rational)
      (check-eq? (type-tag n) 'racket-number)
      (check-eq? (type-tag c) 'complex)
      (check-eq? (type-tag cc) 'complex)
      (check-equal? i (make-racket-integer 2))
      (check-equal? r (make-rational 2 1))
      (check-equal? n (make-racket-number 2.0))
      (check-equal? c (make-complex-from-real-imag 2.0 0))
      (check-equal? cc (make-complex-from-real-imag 2.0 0)))

    (let* ([i 2]
           [r (raise-v2 i)]
           [n (raise-v2 r)]
           [c (raise-v2 n)])
      (check-eq? (type-tag i) 'racket-integer)
      (check-eq? (type-tag r) 'rational)
      (check-eq? (type-tag n) 'racket-number)
      (check-eq? (type-tag c) 'complex)
      (check-equal? i (make-racket-integer 2))
      (check-equal? r (make-rational 2 1))
      (check-equal? n (make-racket-number 2.0))
      (check-equal? c (make-complex-from-real-imag 2.0 0))
      (check-exn exn:fail? (λ () (raise-v2 c))))

    ;; add, sub, mul, div are not implemented for racket-integer
    (check-exn exn:fail? (λ () (add 1 2)))
    (check-exn exn:fail? (λ () (add 1 2.0)))
    (check-equal? (add 1.0 2.0) 3.0)))

(module+ test
  (require (submod ".." Section/2.4.1 rectangular-package test)
           (submod ".." Section/2.4.1 polar-package test)
           (submod ".." Section/2.4.2 test)
           (submod ".." Section/2.4.3 test)
           (submod ".." Exercise/2.73 test)
           (submod ".." Exercise/2.74 division-paris test)
           (submod ".." Exercise/2.74 division-stockholm test)
           (submod ".." Exercise/2.74 division-tokyo test)
           (submod ".." Exercise/2.74 test)
           (submod ".." Exercise/2.75 test)
           ;; 2.76: no tests
           (submod ".." Section/2.5.1 test)
           (submod ".." Exercise/2.77 test)
           (submod ".." Exercise/2.78 test)
           (submod ".." Exercise/2.79 test)
           (submod ".." Exercise/2.80 test)
           (submod ".." Exercise/2.81 test)
           (submod ".." Exercise/2.82 test)
           (submod ".." Exercise/2.83 test)))
