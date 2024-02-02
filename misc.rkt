#lang racket/base

(for/fold ([counter 0])
          ([element '(3 5 3 4 3 2 6 3)])
  (if (= element 3)
      (add1 counter)
      counter))
