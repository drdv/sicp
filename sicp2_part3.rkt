;; =====================================================================================
;; Exercises in Chapter 2 (part 3)
;; =====================================================================================
#lang racket/base

(module Section/2.4.1 sicp
  (#%require (only racket/base module*))

  (module* rectangular-package sicp ; by Ben Bitdiddle
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

  (module* polar-package sicp ; by Alyssa P. Hacker
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
  #|
  This section demonstrates an antipattern (see Section/2.4.3) so I don't provide
  real-part, imag-part, magnitude, angle, make-from-real-imag and make-from-mag-ang.
  |#
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
             get
             put
             get-op-type-table
             clear-op-type-table)
  (#%require (only racket/base module+ λ hash-set! hash-ref make-hash hash-clear!)
             (only racket/base local-require submod only-in)
             (only (submod "sicp1.rkt" common-utils) square tolerance)
             (only (submod ".." Section/2.4.2) attach-tag type-tag contents))

  #|
  Implementations of put and get are not given so in order to test the code I use a
  built-in hash-table to implement double displatch with (op . type) as a key. Actually,
  we model multiple dispatch because, in the book, the second element in the cons (i.e.,
  the type) is (sometimes) a list.
  |#
  (define OP-TYPE-TABLE (make-hash))
  ;; see page 224
  (define (put op type item)
    (hash-set! OP-TYPE-TABLE (cons op type) item))
  (define (get op type)
    (hash-ref OP-TYPE-TABLE (cons op type)))

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

  (module+ test
    (#%require rackunit)
    (display "--> Section/2.4.3\n")

    (install-rectangular-package)
    (install-polar-package)

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

  (clear-op-type-table)
  (install-deriv-sum)
  (get-op-type-table)

  (install-deriv-product)
  (get-op-type-table)

  ;; Task C
  (define (install-deriv-exponentiate)
    (put 'deriv '** (λ (expr var)
                      (let ([b (base expr)]
                            [n (exponent expr)])
                        (make-product
                         (make-product n (make-exponentiation b (make-sum n -1)))
                         (deriv b var)))))
    'deriv-exponentiate-installed)

  (install-deriv-exponentiate)
  (get-op-type-table)

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
  1. I will consider three divisions modelling sets in three different ways:
     - Paris: sets are modelled as unordered lists
     - Stockholm: sets are modelled as ordered lists
     - Tokyo: sets are modelled as binary trees
  2. The names of an employee are stored as a string. I assume that no two employees
     of a division have the same name (but there could be duplicates accross divisions).
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
               tag-object
               get-division
               get-content
               get
               put
               get-op-type-table
               get-record
               get-record-key
               get-record-salary
               find-employee-record)
    (#%require (only racket/base λ)
               (only (submod "sicp2_part1.rkt" Section/2.2.3) filter)
               (only (submod ".." ".." Section/2.4.3) get put get-op-type-table))

    (define (make-field field-name field-value)
      (cons field-name field-value))
    (define (field->name field) (car field))
    (define (field->value field) (cdr field))

    (define (tag-object division object) (cons division object))
    (define (get-division tagged-object) (car tagged-object))
    (define (get-content tagged-object) (cdr tagged-object))

    (define (get-record employee-name personal-records-file)
      (let* ([division (get-division personal-records-file)]
             [records (get-content personal-records-file)]
             [proc (get 'get-record division)])
        (tag-object division (proc employee-name records))))

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
    (#%provide personal-records-file)
    (#%require (only racket/base module+ λ)
               (only (submod ".." common-library)
                     make-field
                     field->name
                     field->value
                     tag-object
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

    (define (register-interface)
      (put 'get-record division-name get-record)
      (put 'get-record-key division-name get-record-key)
      (put 'get-record-salary division-name get-record-salary))

    (register-interface)
    ;; ---------------------------------------------------------------------------------

    (define personal-records
      (list (make-record "Gabriel Granger" "1 Rue de Rivoli" 1)
            (make-record "Noah Monet" "2 Avenue Montaigne" 2)
            (make-record "Jules Bernard" "3 Boulevard Saint-Germain" 3)
            (make-record "Louis Dumas" "4 Avenue Victor Hugo" 4)
            (make-record "Liam Petit" "5 Rue des Francs-Bourgeois" 5)
            (make-record "Ghost Employee" "Everywhere" 6)))
    (define personal-records-file (tag-object division-name personal-records))

    (module+ test
      (#%require rackunit)
      (display "--> Exercise/2.74 (paris division)\n")

      (let ([name "Louis Dumas"]
            [record (make-record "Louis Dumas" "4 Avenue Victor Hugo" 4)])
        (check-equal? (get-record name personal-records) record)
        (check-false (get-record "Nonexistent Name" personal-records)))))

  (module division-stockholm sicp
    (#%provide personal-records-file)
    (#%require (only racket/base module+)
               (only (submod "sicp2_part1.rkt" Section/2.2.3) accumulate)
               (only (submod ".." common-library)
                     make-field
                     field->name
                     field->value
                     tag-object
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

    (define (register-interface)
      (put 'get-record division-name get-record)
      (put 'get-record-key division-name get-record-key)
      (put 'get-record-salary division-name get-record-salary))

    (register-interface)
    ;; ---------------------------------------------------------------------------------

    (define personal-records
      (accumulate adjoin-set '()
                  (list (make-record "Acke Larsson" "1 Drottninggatan" 1)
                        (make-record "Astrid Nilsson" "2 Stockholm Stadsmission" 2)
                        (make-record "Gustav Olsson" "3 Biblioteksgatan" 3)
                        (make-record "Alexander Svensson" "4 Gamla Stan" 4)
                        (make-record "Ebba Berg" "5 Kungsgatan" 5)
                        (make-record "Ghost Employee" "Everywhere" 6))))
    (define personal-records-file (tag-object division-name personal-records))

    (module+ test
      (#%require rackunit)
      (display "--> Exercise/2.74 (stockholm division)\n")

      (let ([name "Gustav Olsson"]
            [record (make-record "Gustav Olsson" "3 Biblioteksgatan" 3)])
        (check-equal? (get-record name personal-records) record)
        (check-false (get-record "Nonexistent Name" personal-records)))))

  (module division-tokyo sicp
    (#%provide personal-records-file)
    (#%require (only racket/base module+)
               (only (submod "sicp2_part1.rkt" Section/2.2.3) accumulate)
               (only (submod "sicp2_part2.rkt" Example/sets-as-binary-trees)
                     make-tree
                     entry
                     left-branch
                     right-branch)
               (only (submod ".." common-library)
                     make-field
                     field->name
                     field->value
                     tag-object
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

    (define (register-interface)
      (put 'get-record division-name get-record)
      (put 'get-record-key division-name get-record-key)
      (put 'get-record-salary division-name get-record-salary))

    (register-interface)
    ;; ---------------------------------------------------------------------------------

    (define personal-records
      (accumulate adjoin-set '()
                  (list (make-record "Asahi Nakamura" "1 Yanaka Ginza" 1)
                        (make-record "Keiko Sizuki" "2 Takeshita-dori" 2)
                        (make-record "Haruki Ito" "3 Ameya Yokocho" 3)
                        (make-record "Akari Kobayashi" "4 Nakano" 4)
                        (make-record "Chika Ishita" "5 Omotesando" 5)
                        (make-record "Ghost Employee" "Everywhere" 6))))
    (define personal-records-file (tag-object division-name personal-records))

    (module+ test
      (#%require rackunit)
      (display "--> Exercise/2.74 (tokyo division)\n")

      (let ([name "Haruki Ito"]
            [record (make-record "Haruki Ito" "3 Ameya Yokocho" 3)])
        (check-equal? (get-record name personal-records) record)
        (check-false (get-record "Nonexistent Name" personal-records)))))

  (module+ test
    (#%require rackunit
               (only (submod ".." common-library)
                     get-record
                     get-record-key
                     get-record-salary
                     find-employee-record)
               (rename (submod ".." division-paris)
                       personal-records-file-paris personal-records-file)
               (rename (submod ".." division-stockholm)
                       personal-records-file-stockholm personal-records-file)
               (rename (submod ".." division-tokyo)
                       personal-records-file-tokyo personal-records-file))
    (display "--> Exercise/2.74\n")

    #|
    Task A:
    Each division file should contains the division tag.

    Task B:
    Each record should contains the division tag but this need not be the case in the
    original records - we add it upon record retrieval. Note that I have renamed
    get-salary to get-record-salary.

    Task C:
    Since I assume that employees with the same name could exist, I return a list of
    employees found (at most one per division).

    Task D:
    Each new division should simply implement the interface (see register-interface) and
    provide the personal-records-file. No changes in get-record, get-record-key or
    get-record-salary are required.
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

(module+ test
  (require (submod ".." Section/2.4.1 rectangular-package test)
           (submod ".." Section/2.4.1 polar-package test)
           (submod ".." Section/2.4.2 test)
           (submod ".." Section/2.4.3 test)
           (submod ".." Exercise/2.73 test)
           (submod ".." Exercise/2.74 division-paris test)
           (submod ".." Exercise/2.74 division-stockholm test)
           (submod ".." Exercise/2.74 division-tokyo test)
           (submod ".." Exercise/2.74 test)))
