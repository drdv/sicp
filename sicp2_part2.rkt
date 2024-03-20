;; =====================================================================================
;; Exercises in Chapter 2 (part 2)
;; =====================================================================================
#lang racket/base

(module Section/2.2.4 sicp
  (#%provide right-split
             up-split
             corner-split
             square-limit)
  (#%require (only racket/base module+ λ)
             (only (submod "sicp1.rkt" Exercise/1.42) compose)
             (only (submod "sicp1.rkt" conversion-utils) painter->png)
             sicp-pict)

  (define (flipped-pairs painter)
    (let ((painter2 (beside painter (flip-vert painter))))
      (below painter2 painter2)))

  (define (right-split painter n)
    (if (= n 0)
        painter
        (let ((smaller (right-split painter (- n 1))))
          (beside painter (below smaller smaller)))))

  #|
  up-split is supposed to be defined in Exercise/2.44 but the code organization is
  simpler if I define it here
  |#
  (define (up-split painter n)
    (if (= n 0)
        painter
        (let ((smaller (up-split painter (- n 1))))
          (below painter (beside smaller smaller)))))

  (define (corner-split painter n)
    (if (= n 0)
        painter
        (let* ([up (up-split painter (- n 1))]
               [right (right-split painter (- n 1))]
               [top-left (beside up up)]
               [bottom-right (below right right)]
               [corner (corner-split painter (- n 1))])
          (beside (below painter top-left)
                  (below bottom-right corner)))))

  (define (square-limit painter n)
    (let* ([quarter (corner-split painter n)]
           [half (beside (flip-horiz quarter) quarter)])
      (below (flip-vert half) half)))

  (module+ test
    (#%require rackunit)
    (display "--> Section/2.2.4\n")

    (painter->png einstein #:file "out/einstein.png")
    (painter->png (flip-horiz einstein) #:file "out/flip-horiz-einstein.png")
    (painter->png (beside einstein einstein) #:file "out/beside-einstein-einstein.png")
    (painter->png (below einstein einstein) #:file "out/below-einstein-einstein.png")
    (painter->png (flipped-pairs einstein) #:file "out/flipped-pairs-einstein.png")
    (painter->png (right-split einstein 4) #:file "out/right-split-4-einstein.png")
    (painter->png (corner-split einstein 4) #:file "out/corner-split-4-einstein.png")
    (painter->png (square-limit einstein 4) #:file "out/square-limit-4-einstein.png"))

  ;; =========================================================
  ;; Higher-order operations
  ;; =========================================================
  (define (square-of-four tl tr bl br)
    (λ (painter)
      (let ((top (beside (tl painter) (tr painter)))
            (bottom (beside (bl painter) (br painter))))
        (below bottom top))))

  (define (flipped-pairs-v2 painter)
    (let ((combine4 (square-of-four identity flip-vert
                                    identity flip-vert)))
      (combine4 painter)))

  (define flipped-pairs-v3
    (square-of-four identity flip-vert
                    identity flip-vert))

  (define (square-limit-v2 painter n)
    (let ((combine4 (square-of-four flip-horiz identity
                                    rotate180 flip-vert)))
      (combine4 (corner-split painter n))))

  (define (square-limit-v3 painter n)
    (let ((combine4 (square-of-four flip-horiz identity
                                    (compose flip-vert flip-horiz) flip-vert)))
      (combine4 (corner-split painter n))))

  (module+ test
    (check-equal? (paint (flipped-pairs einstein))
                  (paint (flipped-pairs-v2 einstein)))

    (check-equal? (paint (flipped-pairs einstein))
                  (paint (flipped-pairs-v3 einstein)))

    (let ([n 4])
      (check-equal? (paint (square-limit einstein n))
                    (paint (square-limit-v2 einstein n)))

      (check-equal? (paint (square-limit einstein n))
                    (paint (square-limit-v3 einstein n))))))

(module Exercise/2.44 sicp
  (#%require (only racket/base module+)
             sicp-pict
             (only (submod "sicp1.rkt" conversion-utils) painter->png)
             (only (submod ".." Section/2.2.4) up-split))

  (module+ test
    (display "--> Exercise/2.44\n")

    (painter->png (up-split einstein 4) #:file "out/up-split-4-einstein.png")))

(module Exercise/2.45 sicp
  (#%require (only racket/base module+ λ)
             sicp-pict
             (only (submod ".." Section/2.2.4) up-split right-split))

  (define (split transform-1 transform-2)
    (λ (painter n)
      (if (= n 0)
          painter
          (let* ([split-transformation (split transform-1 transform-2)]
                 [smaller (split-transformation painter (- n 1))])
            (transform-1 painter (transform-2 smaller smaller))))))

  (define right-split-v2 (split beside below))
  (define up-split-v2 (split below beside))

  (module+ test
    (#%require rackunit)
    (display "--> Exercise/2.45\n")

    (let ([n 4])
      (check-equal? (paint (right-split einstein n))
                    (paint (right-split-v2 einstein n)))

      (check-equal? (paint (up-split einstein n))
                    (paint (up-split-v2 einstein n))))))

(module Section/2.2.4/frames sicp
  (#%provide make-vect
             xcor-vect
             ycor-vect
             make-frame
             origin-frame
             edge1-frame
             edge2-frame)
  (#%require (only racket/base module+))

  #|
  Two versions of make-frame, origin-frame, edge1-frame and edge2-frame are supposed to
  be defined in Exercise/2.47 but the code organization is simpler if I define one of
  them here. Same for make-vect, xcor-vect and ycor-vect, which are supposed to be
  defined in Exercise/2.46.
  |#
  (define (make-vect x y)
    (cons x y))

  (define (xcor-vect vec)
    (car vec))

  (define (ycor-vect vec)
    (cdr vec))

  (define (make-frame origin edge1 edge2)
    (list origin edge1 edge2))

  (define (origin-frame frame)
    (car frame))

  (define (edge1-frame frame)
    (cadr frame))

  (define (edge2-frame frame)
    (caddr frame))

  (module+ test
    (#%require rackunit)
    (display "--> Section/2.2.4/frames\n")

    (let* ([origin (make-vect 1 2)]
           [frame (make-frame origin
                              (make-vect 3 4)
                              (make-vect 5 6))])
      (check-equal? (origin-frame frame) origin)
      (check-equal? (edge1-frame frame) (make-vect 3 4))
      (check-equal? (edge2-frame frame) (make-vect 5 6))
      (check-equal? (xcor-vect origin) 1)
      (check-equal? (ycor-vect origin) 2))))

(module Exercise/2.46 sicp
  (#%provide add-vect
             sub-vect
             scale-vect
             frame-coord-map)
  (#%require (only racket/base module+ λ)
             (only (submod ".." Section/2.2.4/frames)
                   make-vect
                   xcor-vect
                   ycor-vect
                   make-frame
                   origin-frame
                   edge1-frame
                   edge2-frame))

  (define (add-vect vec1 vec2)
    (make-vect (+ (xcor-vect vec1)
                  (xcor-vect vec2))
               (+ (ycor-vect vec1)
                  (ycor-vect vec2))))

  (define (sub-vect vec1 vec2)
    (make-vect (- (xcor-vect vec1)
                  (xcor-vect vec2))
               (- (ycor-vect vec1)
                  (ycor-vect vec2))))

  (define (scale-vect s vec)
    (make-vect (* s (xcor-vect vec))
               (* s (ycor-vect vec))))

  (define (frame-coord-map frame)
    (λ (vec)
      (add-vect
       (origin-frame frame)
       (add-vect (scale-vect (xcor-vect vec) (edge1-frame frame))
                 (scale-vect (ycor-vect vec) (edge2-frame frame))))))

  (module+ test
    (#%require rackunit)
    (display "--> Exercise/2.46\n")

    (let ([vec1 (make-vect 1 2)]
          [vec2 (make-vect 3 1)])
      (check-equal? (add-vect vec1 vec2) (make-vect 4 3))
      (check-equal? (sub-vect vec1 vec2) (make-vect -2 1))
      (check-equal? (scale-vect 2 vec1) (make-vect 2 4)))

    (let* ([frame (make-frame (make-vect 1 2)
                              (make-vect 3 4)
                              (make-vect 5 6))]
           [x (make-vect 3 2)]
           [m (frame-coord-map frame)]
           [mx (m x)])
      (check-equal? (xcor-vect mx) (+ 1 (* 3 3) (* 2 5)))
      (check-equal? (ycor-vect mx) (+ 2 (* 3 4) (* 2 6))))))

(module Exercise/2.47 sicp
  (#%require (only racket/base module+)
             (only (submod ".." Section/2.2.4/frames) make-vect))

  (define (make-frame origin edge1 edge2)
    (cons origin (cons edge1 edge2)))

  (define (origin-frame frame)
    (car frame))

  (define (edge1-frame frame)
    (cadr frame))

  (define (edge2-frame frame)
    (cddr frame))

  (module+ test
    (#%require rackunit)
    (display "--> Exercise/2.47\n")

    ;; same test as in Section/2.2.4/frames
    (let ([frame (make-frame (make-vect 1 2)
                             (make-vect 3 4)
                             (make-vect 5 6))])
      (check-equal? (origin-frame frame) (make-vect 1 2))
      (check-equal? (edge1-frame frame) (make-vect 3 4))
      (check-equal? (edge2-frame frame) (make-vect 5 6)))))

(module Exercise/2.48 sicp
  (#%provide make-segment
             start-segment
             end-segment)
  (#%require (only racket/base module+)
             (only (submod ".." Section/2.2.4/frames) make-vect))

  (define (make-segment origin->start origin->end)
    (cons origin->start origin->end))

  (define (start-segment vec)
    (car vec))

  (define (end-segment vec)
    (cdr vec))

  (module+ test
    (#%require rackunit)
    (display "--> Exercise/2.48\n")

    (let* ([origin->start (make-vect 1 2)]
           [origin->end (make-vect 2 3)]
           [segment (make-segment origin->start origin->end)])
      (check-equal? (start-segment segment) origin->start)
      (check-equal? (end-segment segment) origin->end))))

(module Exercise/2.49 sicp
  (#%provide segments->painter
             splines->painter
             task-d
             wave
             make-spline)
  (#%require (only racket/base module+ λ format)
             racket/class
             (only (submod "sicp1.rkt" conversion-utils) get-drawing-context dc->png)
             (only (submod ".." Section/2.2.4/frames)
                   make-vect
                   xcor-vect
                   ycor-vect
                   make-frame)
             (only (submod ".." Exercise/2.46) frame-coord-map)
             (only (submod ".." Exercise/2.48)
                   make-segment
                   start-segment
                   end-segment))

  (define (segments->painter dc segment-list)
    (define (draw-line start-point end-point)
      (send dc draw-line
            (xcor-vect start-point)
            (ycor-vect start-point)
            (xcor-vect end-point)
            (ycor-vect end-point)))
    (λ (frame)
      (for-each
       (λ (segment)
         (let ([m (frame-coord-map frame)])
           (draw-line
            (m (start-segment segment))
            (m (end-segment segment)))))
       segment-list)))

  (define task-a
    (list (make-segment (make-vect 0 0)
                        (make-vect 1 0))
          (make-segment (make-vect 0 0)
                        (make-vect 0 1))
          (make-segment (make-vect 0 1)
                        (make-vect 1 1))
          (make-segment (make-vect 1 0)
                        (make-vect 1 1))))

  (define task-b
    (list (make-segment (make-vect 0 0)
                        (make-vect 1 1))
          (make-segment (make-vect 1 0)
                        (make-vect 0 1))))

  (define task-c
    (list (make-segment (make-vect 0.5 0)
                        (make-vect 1 0.5))
          (make-segment (make-vect 1 0.5)
                        (make-vect 0.5 1))
          (make-segment (make-vect 0.5 1)
                        (make-vect 0 0.5))
          (make-segment (make-vect 0 0.5)
                        (make-vect 0.5 0))))

  (define (task->png painter task frames size filename open-file)
    (let ([dc (get-drawing-context size)]
          [frame-border (list (make-segment (make-vect 0 0)
                                            (make-vect 1 0))
                              (make-segment (make-vect 0 0)
                                            (make-vect 0 1))
                              (make-segment (make-vect 0 1)
                                            (make-vect 1 1))
                              (make-segment (make-vect 1 0)
                                            (make-vect 1 1)))])
      (for-each
       (λ (frame)
         ((painter dc task) frame))
       frames)
      (send dc set-pen "red" 5 'short-dash)
      (for-each
       (λ (frame)
         ((segments->painter dc frame-border) frame))
       frames)
      (dc->png dc #:file filename #:open open-file)))

  ;; ===================================================================================
  ;; all frames in Figure. 2.10
  ;; ===================================================================================
  (define canvas-size 500)
  (define offset (* canvas-size 0.05))
  (define half-size (/ canvas-size 2))
  (define half-size- (- half-size offset))
  (define half-size+ (+ half-size offset))

  (define frame-top-left (make-frame (make-vect 0 0)
                                     (make-vect half-size- 0)
                                     (make-vect 0 half-size-)))

  ;; the origin is the top left corner
  (define frame-top-right (make-frame (make-vect (+ half-size+ (* 0.2 half-size-))
                                                 (* 0.2 half-size-))
                                      (make-vect (* 0.8 half-size-)
                                                 (* -0.2 half-size-))
                                      (make-vect (* -0.2 half-size-)
                                                 (* 0.8 half-size-))))

  (define frame-bottom-left (make-frame (make-vect 0 half-size+)
                                        (make-vect (/ half-size 2) 0)
                                        (make-vect 0 half-size-)))

  (define frame-bottom-right (make-frame (make-vect half-size+ (* 1.5 half-size))
                                         (make-vect half-size- 0)
                                         (make-vect 0 (/ half-size 2))))

  (define frames-fig-2.10 (list frame-top-left
                                frame-top-right
                                frame-bottom-left
                                frame-bottom-right))
  ;; ===================================================================================

  (module+ test
    (display "--> Exercise/2.49\n")

    (task->png segments->painter task-a frames-fig-2.10 canvas-size "out/task-a.png" #f)
    (task->png segments->painter task-b frames-fig-2.10 canvas-size "out/task-b.png" #f)
    (task->png segments->painter task-c frames-fig-2.10 canvas-size "out/task-c.png" #f)
    )

  (define (make-spline start-point control-point end-point)
    (list start-point control-point end-point))

  (define (spline-start spline)
    (car spline))

  (define (spline-control spline)
    (cadr spline))

  (define (spline-end spline)
    (caddr spline))

  ;; I changed the function signature as I don't want to have dc as a global variable
  (define (splines->painter dc spline-list)
    (define (draw-spline start-point control-point end-point)
      (send dc draw-spline
            (xcor-vect start-point)
            (ycor-vect start-point)
            (xcor-vect control-point)
            (ycor-vect control-point)
            (xcor-vect end-point)
            (ycor-vect end-point)))
    (λ (frame)
      (for-each
       (λ (spline)
         (let ([m (frame-coord-map frame)])
           (draw-spline
            (m (spline-start spline))
            (m (spline-control spline))
            (m (spline-end spline)))))
       spline-list)))

  ;; the original task is to use segments->painter but I prefer using a splines->painter
  ;; afterall, this is supposed to be a wave
  (define task-d
    (list
     ;; legs inner
     (make-spline (make-vect 0.4 1) (make-vect 0.5 0.6) (make-vect 0.6 1))

     ;; outer left leg
     (make-spline (make-vect 0.4 0.5) (make-vect 0.38 0.7) (make-vect 0.3 1))
     ;; armpit left
     (make-spline (make-vect 0.4 0.5) (make-vect 0.4 0.35) (make-vect 0.25 0.45))
     ;; left arm bottom
     (make-spline (make-vect 0.25 0.45) (make-vect 0.15 0.5) (make-vect 0 0.35))
     ;; left arm top
     (make-spline (make-vect 0.35 0.29) (make-vect 0.17 0.43) (make-vect 0 0.26))
     ;; left shoulder
     (make-spline (make-vect 0.35 0.29) (make-vect 0.39 0.28) (make-vect 0.41 0.29))
     ;; left neck
     (make-spline (make-vect 0.41 0.29) (make-vect 0.47 0.27) (make-vect 0.45 0.22))

     ;; outer right leg
     (make-spline (make-vect 0.6 0.5) (make-vect 0.62 0.7) (make-vect 0.7 1))
     ;; armpit right
     (make-spline (make-vect 0.6 0.5) (make-vect 0.6 0.35) (make-vect 0.75 0.45))
     ;; right arm bottom
     (make-spline (make-vect 0.75 0.45) (make-vect 0.84 0.53) (make-vect 1 0.7))
     ;; right arm top
     (make-spline (make-vect 0.59 0.29) (make-vect 0.7 0.29) (make-vect 1 0.6))
     ;; right neck
     (make-spline (make-vect 0.59 0.29) (make-vect 0.53 0.27) (make-vect 0.55 0.22))

     ;; left head
     (make-spline (make-vect 0.45 0) (make-vect 0.35 0.1) (make-vect 0.45 0.22))
     ;; right head
     (make-spline (make-vect 0.55 0) (make-vect 0.65 0.1) (make-vect 0.55 0.22))))

  (define (wave dc)
    (splines->painter dc task-d))

  (module+ test
    (task->png splines->painter task-d frames-fig-2.10 canvas-size "out/task-d.png" #f)
    ))

(module Section/2.2.4/transforming-painters sicp
  (#%provide transform-painter
             flip-vert
             rotate-90
             rotate+90
             beside)
  (#%require (only racket/base module+ λ)
             racket/class
             net/sendurl
             (only (submod ".." Section/2.2.4/frames) make-vect make-frame)
             (only (submod ".." Exercise/2.46) sub-vect frame-coord-map)
             (only (submod ".." Exercise/2.49) wave)
             (only (submod "sicp1.rkt" conversion-utils) get-drawing-context dc->png))

  (define (transform-painter painter origin corner1 corner2)
    (λ (frame)
      (let* ([m (frame-coord-map frame)]
             [new-origin (m origin)])
        (painter (make-frame
                  new-origin
                  (sub-vect (m corner1) new-origin)
                  (sub-vect (m corner2) new-origin))))))

  (define (flip-vert painter)
    (transform-painter painter
                       (make-vect 0.0 1.0) ; new origin
                       (make-vect 1.0 1.0) ; new end of edge1
                       (make-vect 0.0 0.0))) ; new end of edge2

  ;; "upper" because the positive y axis is down
  (define (shrink-to-upper-right painter)
    (transform-painter painter
                       (make-vect 0.5 0.5)
                       (make-vect 1.0 0.5)
                       (make-vect 0.5 1.0)))

  (define (shrink-to-lower-right painter)
    (transform-painter painter
                       (make-vect 0.5 0)
                       (make-vect 1 0)
                       (make-vect 0.5 0.5)))

  ;; NOTE: clockwise direction is negative (so I change the name accordingly)
  (define (rotate-90 painter)
    (transform-painter painter
                       (make-vect 1.0 0.0)
                       (make-vect 1.0 1.0)
                       (make-vect 0.0 0.0)))

  (define (rotate+90 painter)
    (transform-painter painter
                       (make-vect 0.0 1.0)
                       (make-vect 0.0 0.0)
                       (make-vect 1.0 1.0)))

  (define (squash-inwards painter)
    (transform-painter painter
                       (make-vect 0.0 0.0)
                       (make-vect 0.65 0.35)
                       (make-vect 0.35 0.65)))

  (define (beside painter1 painter2)
    (let* ([split-point (make-vect 0.5 0.0)]
           [paint-left (transform-painter painter1
                                          (make-vect 0.0 0.0)
                                          split-point
                                          (make-vect 0.0 1.0))]
           [paint-right (transform-painter painter2
                                           split-point
                                           (make-vect 1.0 0.0)
                                           (make-vect 0.5 1.0))])
      (λ (frame)
        (paint-left frame)
        (paint-right frame))))

  (define size 500)
  (define frame (make-frame (make-vect 0 0)
                            (make-vect size 0)
                            (make-vect 0 size)))

  (module+ test
    (display "--> Section/2.2.4/transforming-painters\n")

    (let ([dc (get-drawing-context size)])
      ((flip-vert (wave dc)) frame)
      (dc->png dc #:file "out/flip-vert-wave.png"))

    (let ([dc (get-drawing-context size)])
      ((shrink-to-lower-right (wave dc)) frame)
      (send dc set-pen "red" 5 'solid)
      ((shrink-to-upper-right (wave dc)) frame)
      (dc->png dc #:file "out/shrink-wave.png"))

    (let ([dc (get-drawing-context size)])
      ((wave dc) frame)
      (send dc set-pen "red" 5 'solid)
      ((rotate-90 (wave dc)) frame)
      (dc->png dc #:file "out/rotate-90-wave.png"))

    (let ([dc (get-drawing-context size)])
      ((wave dc) frame)
      (send dc set-pen "red" 5 'solid)
      ((rotate+90 (wave dc)) frame)
      (dc->png dc #:file "out/rotate+90-wave.png"))

    ;; same result as flip-vert
    (let ([dc (get-drawing-context size)])
      ((wave dc) frame)
      (send dc set-pen "red" 5 'solid)
      ((rotate-90 (rotate-90 (wave dc))) frame)
      (dc->png dc #:file "out/rotate-90-rotate-90-wave.png"))

    (let ([dc (get-drawing-context size)])
      ((rotate+90 (rotate-90 (wave dc))) frame)
      (dc->png dc #:file "out/rotate+90-rotate-90-wave.png"))

    (let ([dc (get-drawing-context size)])
      ((squash-inwards (wave dc)) frame)
      (dc->png dc #:file "out/squash-inwards-wave.png"))

    (let ([dc (get-drawing-context size)])
      ((beside (wave dc) (flip-vert (wave dc))) frame)
      (dc->png dc #:file "out/beside-wave.png"))))

(module Exercise/2.50 sicp
  (#%provide flip-horiz)
  (#%require (only racket/base module+)
             (only (submod "sicp1.rkt" Exercise/1.43) repeated)
             (only (submod "sicp1.rkt" conversion-utils) get-drawing-context dc->png)
             (only (submod ".." Exercise/2.49) wave)
             (only (submod ".." Section/2.2.4/frames) make-vect make-frame)
             (only (submod ".." Section/2.2.4/transforming-painters)
                   transform-painter
                   flip-vert
                   rotate-90
                   rotate+90
                   beside))

  (define (flip-horiz painter)
    (transform-painter painter
                       (make-vect 1.0 0.0)
                       (make-vect 0.0 0.0)
                       (make-vect 1.0 1.0)))

  (define size 500)
  (define frame (make-frame (make-vect 0 0)
                            (make-vect size 0)
                            (make-vect 0 size)))

  (module+ test
    (display "--> Exercise/2.50\n")

    (let ([dc (get-drawing-context size)])
      ((beside (wave dc) (flip-horiz (wave dc))) frame)
      (dc->png dc #:file "out/flip-horiz-wave.png"))

    ;; rotating 180 degrees is equivalent to flip-vert
    (let ([dc (get-drawing-context size)])
      ((beside ((repeated rotate+90 2) (wave dc)) ; (rotate+90 (rotate+90 (wave dc)))
               (flip-vert (wave dc))) frame)
      (dc->png dc #:file "out/rotate-180-wave.png"))

    ;; rotating 270 degrees counterclockwise is equivalent to rotate-90
    (let ([dc (get-drawing-context size)])
      ((beside ((repeated rotate+90 3) (wave dc))
               (rotate-90 (wave dc))) frame)
      (dc->png dc #:file "out/rotate-270-wave.png"))))

(module Exercise/2.51 sicp
  (#%provide below)
  (#%require (only racket/base module+ λ)
             (only (submod "sicp1.rkt" conversion-utils) get-drawing-context dc->png)
             (only (submod ".." Exercise/2.49) wave)
             (only (submod ".." Section/2.2.4/frames) make-vect make-frame)
             (only (submod ".." Section/2.2.4/transforming-painters)
                   transform-painter
                   rotate-90
                   rotate+90
                   beside))

  (define (below painter1 painter2)
    (let* ([split-point (make-vect 0.0 0.5)]
           [paint-bottom (transform-painter painter1
                                            (make-vect 0.0 0.0)
                                            (make-vect 1.0 0.0)
                                            split-point)]
           [paint-top (transform-painter painter2
                                         split-point
                                         (make-vect 1.0 0.5)
                                         (make-vect 0.0 1.0))])
      (λ (frame)
        (paint-bottom frame)
        (paint-top frame))))

  (define (below-rot painter1 painter2)
    (rotate+90 (beside (rotate-90 painter1) (rotate-90 painter2))))

  (define size 500)
  (define frame (make-frame (make-vect 0 0)
                            (make-vect size 0)
                            (make-vect 0 size)))

  (module+ test
    (display "--> Exercise/2.51\n")

    (let ([dc (get-drawing-context size)])
      ((below (wave dc) (wave dc)) frame)
      (dc->png dc #:file "out/below-wave.png"))

    (let ([dc (get-drawing-context size)])
      ((below-rot (wave dc) (wave dc)) frame)
      (dc->png dc #:file "out/below-rot-wave.png"))))

(module Exercise/2.52 sicp
  #|
  Here we cannot reuse right-split, up-split, corner-split, square-limit etc. defined in
  Section/2.2.4 because they are based on utilities in sicp-pict. So I simply redefine
  (and modify) them.
  |#
  (#%require (only racket/base module+ λ)
             (only (submod "sicp1.rkt" Exercise/1.42) compose)
             (only (submod "sicp1.rkt" conversion-utils) get-drawing-context dc->png)
             (only (submod ".." Exercise/2.49) splines->painter make-spline)
             (rename (submod ".." Exercise/2.49) wave-splines task-d)
             (only (submod ".." Section/2.2.4/frames) make-vect make-frame)
             (only (submod ".." Section/2.2.4/transforming-painters)
                   rotate-90
                   rotate+90
                   flip-vert
                   beside)
             (only (submod ".." Exercise/2.50) flip-horiz)
             (only (submod ".." Exercise/2.51) below))

  (define (right-split painter n)
    (if (= n 0)
        painter
        (let ((smaller (right-split painter (- n 1))))
          (beside painter (below smaller smaller)))))

  (define (up-split painter n)
    (if (= n 0)
        painter
        (let ((smaller (up-split painter (- n 1))))
          (below painter (beside smaller smaller)))))

  (define (corner-split painter n)
    (if (= n 0)
        painter
        (let* ([up (up-split (rotate+90 painter) (- n 1))]
               [right (right-split (rotate-90 painter) (- n 1))]
               [top-left up]
               [bottom-right right]
               [corner (corner-split painter (- n 1))])
          (beside (below painter top-left)
                  (below bottom-right corner)))))

  (define (square-of-four tl tr bl br)
    (λ (painter)
      (let ((top (beside (tl painter) (tr painter)))
            (bottom (beside (bl painter) (br painter))))
        (below bottom top))))

  (define (square-limit painter n)
    (let ((combine4 (square-of-four flip-vert rotate+90
                                    flip-vert (compose flip-vert flip-horiz))))
      (combine4 (corner-split painter n))))

  (define size 500)
  (define frame (make-frame (make-vect 0 0)
                            (make-vect size 0)
                            (make-vect 0 size)))

  (define (wave-heart dc)
    (splines->painter
     dc
     (let ([heart-left (make-spline (make-vect 0.54 0.42)
                                    (make-vect 0.49 0.4)
                                    (make-vect 0.54 0.5))]
           [heart-right (make-spline (make-vect 0.54 0.42)
                                     (make-vect 0.59 0.4)
                                     (make-vect 0.54 0.5))])
       (cons heart-left (cons heart-right wave-splines)))))

  (module+ test
    (display "--> Exercise/2.52\n")

    ;; task a
    (let ([dc (get-drawing-context size)])
      ((wave-heart dc) frame)
      (dc->png dc #:file "out/wave-heart.png"))

    ;; task b
    (let ([dc (get-drawing-context size)])
      ((corner-split (wave-heart dc) 2) frame)
      (dc->png dc #:file "out/corner-split-wave-heart.png"))

    ;; task c
    (let ([dc (get-drawing-context size)])
      ((square-limit (wave-heart dc) 1) frame)
      (dc->png dc #:file "out/square-limit-wave-heart.png"))))

(module Lecture/3A sicp
  #|
  This module includes the push example from Lecture 3A. I found both the lecture and
  the "picture language" exercises to be very enlightening.
  |#
  (#%require (only racket/base module+ λ)
             (only (submod "sicp1.rkt" Exercise/1.43) repeated)
             (only (submod "sicp1.rkt" conversion-utils) get-drawing-context dc->png)
             (only (submod ".." Exercise/2.49) splines->painter wave)
             (only (submod ".." Section/2.2.4/frames) make-vect make-frame)
             (only (submod ".." Section/2.2.4/transforming-painters) beside)
             (only (submod ".." Exercise/2.51) below))

  (define (push comb)
    (λ (pict n)
      (let ([f (repeated (λ (p) (comb pict p)) n)])
        (f pict))))

  (define right-push (push beside))
  (define down-push (push below)) ; I call it "down" even though positive y is downwards

  (define size 500)
  (define frame (make-frame (make-vect 0 0)
                            (make-vect size 0)
                            (make-vect 0 size)))

  (module+ test
    (display "--> Lecture/3A\n")

    (let ([dc (get-drawing-context size)])
      ((right-push (wave dc) 3) frame)
      (dc->png dc #:file "out/right-push-wave.png"))

    (let ([dc (get-drawing-context size)])
      ((down-push (wave dc) 3) frame)
      (dc->png dc #:file "out/down-push-wave.png"))))

(module Exercise/2.53 sicp
  (#%require (only racket/base module+))

  (define (memq item x)
    (cond ((null? x) false)
          ((eq? item (car x)) x)
          (else (memq item (cdr x)))))

  (module+ test
    (#%require rackunit)
    (display "--> Exercise/2.53\n")

    (check-false (memq 'apple '(pear banana prune)))
    (check-equal? (memq 'apple '(x (apple sauce) y apple pear)) '(apple pear))

    (list 'a 'b 'c)                         ; (a b c)
    (list (list 'george))                   ; ((george))
    (cdr '((x1 x2) (y1 y2)))                ; ((y1 y2))
    (cadr '((x1 x2) (y1 y2)))               ; (y1 y2)
    (pair? (car '(a short list)))           ; #f
    (memq 'red '((red shoes) (blue socks))) ; #f
    (memq 'red '(red shoes blue socks))     ; (red shoes blue socks)
    ))

(module Exercise/2.54 sicp
  (#%require (only racket/base module+))

  ;; handles only symbols and lists of symbols but not numerical values
  (define (my-equal? a b)
    (cond [(and (symbol? a) (symbol? b)) (eq? a b)]
          [(and (pair? a) (pair? b)) (and (my-equal? (car a) (car b))
                                          (my-equal? (cdr a) (cdr b)))]
          [else (and (null? a) (null? b))]))

  (module+ test
    (#%require rackunit)
    (display "--> Exercise/2.54\n")

    (check-true (my-equal? 'a 'a))
    (check-true (my-equal? '(a b c) '(a b c)))
    (check-false (my-equal? 'a 'b))
    (check-false (my-equal? '(a b c) '((a) b c)))
    (check-false (my-equal? '(a b c) '(b b c)))
    (check-false (my-equal? '(a b c d) '(a b c)))
    (check-false (my-equal? '(a b c) '(a b c d)))

    ;; false by design
    (check-false (my-equal? '(a 1 c) '(a 1 c)))
    (check-false (my-equal? 1 1))

    ;; the builtin equal? gives the expected result
    (check-true (equal? '(a 1 c) '(a 1 c)))
    (check-true (equal? 1 1))))

(module Exercise/2.55 sicp
  (#%require (only racket/base module+))

  (module+ test
    (#%require rackunit)
    (display "--> Exercise/2.55\n")

    ;; see footnote 34 on page 194
    (let ([s1 ''abracadabra]
          [s2 (list 'quote 'abracadabra)])
      (check-equal? s1 s2)
      (check-equal? (car s1) (car s2)))))

(module Example/symbolic-differentiation sicp
  (#%provide variable?
             same-variable?
             sum?
             addend
             augend
             product?
             multiplier
             multiplicand
             =number?
             make-sum
             make-product)
  (#%require (only racket/base module+))

  (define (variable? x) (symbol? x))
  (define (same-variable? v1 v2) (and (variable? v1) (variable? v2) (eq? v1 v2)))
  (define (sum? x) (and (pair? x) (eq? (car x) '+)))
  (define (addend s) (cadr s))
  (define (augend s) (caddr s))
  (define (product? x) (and (pair? x) (eq? (car x) '*)))
  (define (multiplier p) (cadr p))
  (define (multiplicand p) (caddr p))

  (define (=number? exp num) (and (number? exp) (= exp num)))
  (define (make-sum a1 a2)
    (cond [(=number? a1 0) a2]
          [(=number? a2 0) a1]
          [(and (number? a1) (number? a2)) (+ a1 a2)]
          [else (list '+ a1 a2)]))

  (define (make-product m1 m2)
    (cond [(or (=number? m1 0) (=number? m2 0)) 0]
          [(=number? m1 1) m2]
          [(=number? m2 1) m1]
          [(and (number? m1) (number? m2)) (* m1 m2)]
          [else (list '* m1 m2)]))

  (define (deriv expr var)
    (cond [(number? expr) 0]
          [(variable? expr) (if (same-variable? expr var) 1 0)]
          [(sum? expr) (make-sum (deriv (addend expr) var)
                                 (deriv (augend expr) var))]
          [(product? expr)
           (make-sum
            (make-product (multiplier expr)
                          (deriv (multiplicand expr) var))
            (make-product (deriv (multiplier expr) var)
                          (multiplicand expr)))]
          [else
           (error "unknown expression type: DERIV" expr)]))

  (module+ test
    (#%require rackunit)
    (display "--> Example/symbolic-differentiation\n")

    (check-equal? (deriv 'x 'x) 1)
    (check-equal? (deriv 'y 'x) 0)
    (check-equal? (deriv '(+ x 3) 'x) 1)
    (check-equal? (deriv '(* x y) 'x) 'y)
    (check-equal? (deriv '(* (* x y) (+ x 3)) 'x) '(+ (* x y) (* y (+ x 3))))))

(module Exercise/2.56 sicp
  (#%provide exponentiation?
             base
             exponent
             make-exponentiation
             deriv)
  (#%require (only racket/base module+)
             (only (submod ".." Example/symbolic-differentiation)
                   variable?
                   same-variable?
                   sum?
                   addend
                   augend
                   product?
                   multiplier
                   multiplicand
                   =number?
                   make-sum
                   make-product))

  (define (exponentiation? x) (and (pair? x) (eq? (car x) '**)))
  (define (base x) (cadr x))
  (define (exponent x) (caddr x))
  (define (make-exponentiation b n)
    (cond [(=number? b 0) 0]
          [(=number? n 0) 1]
          [(=number? n 1) b]
          [(and (number? b) (number? n)) (expt b n)]
          [else (list '** b n)]))

  (define (deriv expr var)
    (cond [(number? expr) 0]
          [(variable? expr) (if (same-variable? expr var) 1 0)]
          [(sum? expr) (make-sum (deriv (addend expr) var)
                                 (deriv (augend expr) var))]
          [(product? expr)
           (make-sum
            (make-product (multiplier expr)
                          (deriv (multiplicand expr) var))
            (make-product (deriv (multiplier expr) var)
                          (multiplicand expr)))]
          [(exponentiation? expr)
           (let ([b (base expr)]
                 [n (exponent expr)])
             (make-product
              (make-product n (make-exponentiation b (make-sum n -1)))
              (deriv b var)))]
          [else
           (error "unknown expression type: DERIV" expr)]))

  (module+ test
    (#%require rackunit)
    (display "--> Exercise/2.56\n")

    (let ([expr '(** (+ x 1) (+ y 2))])
      (check-true (exponentiation? expr))
      (check-equal? (base expr) '(+ x 1))
      (check-equal? (exponent expr) '(+ y 2)))

    ;; the simplified expression is 32*x + 24
    (check-equal? (deriv '(** (+ (* 4 x) 3) 2) 'x) '(* (* 2 (+ (* 4 x) 3)) 4))))

(module Exercise/2.57 sicp
  (#%require (only racket/base module+)
             (only (submod ".." Example/symbolic-differentiation)
                   variable?
                   same-variable?
                   sum?
                   addend
                   product?
                   multiplier
                   make-sum
                   make-product)
             (only (submod ".." Exercise/2.56)
                   exponentiation?
                   base
                   exponent
                   make-exponentiation))

  #|
  We need to change only augend and multiplicand. make-sum and make-product could still
  take two arguments - all we need is to handle expressions defined as a quoted list,
  e.g., '(+ x y z).
  |#
  (define (augend s) (if (= (length s) 3) ; e.g., '(+ a b)
                         (caddr s)
                         (cons '+ (cddr s))))
  (define (multiplicand p) (if (= (length p) 3) ; e.g., '(* a b)
                               (caddr p)
                               (cons '* (cddr p))))

  #|
  This is the same deriv procedure as in Exercise/2.56. I redefine it here to take into
  account the new augend and multiplicand. Alternatively I could have used Dynamic
  Binding (https://docs.racket-lang.org/guide/parameterize.html).
  |#
  (define (deriv expr var)
    (cond [(number? expr) 0]
          [(variable? expr) (if (same-variable? expr var) 1 0)]
          [(sum? expr) (make-sum (deriv (addend expr) var)
                                 (deriv (augend expr) var))]
          [(product? expr)
           (make-sum
            (make-product (multiplier expr)
                          (deriv (multiplicand expr) var))
            (make-product (deriv (multiplier expr) var)
                          (multiplicand expr)))]
          [(exponentiation? expr)
           (let ([b (base expr)]
                 [n (exponent expr)])
             (make-product
              (make-product n (make-exponentiation b (make-sum n -1)))
              (deriv b var)))]
          [else
           (error "unknown expression type: DERIV" expr)]))

  (module+ test
    (#%require rackunit)
    (display "--> Exercise/2.57\n")

    (check-equal? (deriv '(* (* x y) (+ x 3)) 'x)
                  (deriv '(* x y (+ x 3)) 'x))

    (check-equal? (deriv '(+ (* x y) (+ x 3) (* 2 x)) 'x)
                  '(+ y 3))

    ;; it is not guaranteed to obtain the same simplification
    (deriv '(+ (+ (* x y) (* x 5)) (+ x y)) 'x)
    (deriv '(+ (* x y) (* x 5) (+ x y)) 'x)))

(module Exercise/2.58 sicp
  #|
  NOTE: we could have an expression like '((x * x)+(x * x)), note that there are no
  spaces around the + operator, but we cannot have an expression like '(x+(x * x))
  |#
  (#%require (only racket/base module+)
             (only (submod ".." Example/symbolic-differentiation)
                   variable?
                   same-variable?
                   =number?))

  (define (multiplier p) (car p))

  (define (make-sum a1 a2)
    (cond [(=number? a1 0) a2]
          [(=number? a2 0) a1]
          [(and (number? a1) (number? a2)) (+ a1 a2)]
          [else (list a1 '+ a2)]))

  (define (make-product m1 m2)
    (cond [(or (=number? m1 0) (=number? m2 0)) 0]
          [(=number? m1 1) m2]
          [(=number? m2 1) m1]
          [(and (number? m1) (number? m2)) (* m1 m2)]
          [else (list m1 '* m2)]))

  (module+ test-fully-parenthesized
    (#%require rackunit)
    (display "--> Exercise/2.58/fully-parenthesized\n")

    (define (sum? x) (and (pair? x) (eq? (cadr x) '+)))
    (define (addend s) (car s))
    (define (augend s) (caddr s))
    (define (product? x) (and (pair? x) (eq? (cadr x) '*)))
    (define (multiplicand p) (caddr p))

    ;; this is deriv from Example/symbolic-differentiation (I don't need exponentiation)
    (define (deriv expr var)
      (cond [(number? expr) 0]
            [(variable? expr) (if (same-variable? expr var) 1 0)]
            [(sum? expr) (make-sum (deriv (addend expr) var)
                                   (deriv (augend expr) var))]
            [(product? expr)
             (make-sum
              (make-product (multiplier expr)
                            (deriv (multiplicand expr) var))
              (make-product (deriv (multiplier expr) var)
                            (multiplicand expr)))]
            [else
             (error "unknown expression type: DERIV" expr)]))

    (let ([expr '(x + (3 * (x + (y + 2))))])
      (check-true (sum? expr))
      (check-equal? (addend expr) 'x)
      (check-equal? (augend expr) '(3 * (x + (y + 2))))
      (check-true (product? (augend expr)))
      (check-equal? (multiplier (augend expr)) 3)
      (check-equal? (multiplicand (augend expr)) '(x + (y + 2)))
      (check-equal? (deriv expr 'x) 4))
    (check-equal? (deriv '((x * x) + (3 * x)) 'x) '((x + x) + 3))
    (check-equal? (deriv '((x * (x * x)) + (3 * x)) 'x)
                  ;; 3*x**2 + 3
                  '(((x * (x + x)) + (x * x)) + 3)))

  #|
  My strategy here is as follows:
  1. Use (find-op '+ expr) to get the first top level + operation and recursively handle
     the expressions on the left and right. For example 3 * x + 2 * y + 5 * x is split
     into (3 * x) + (2 * y + 5 * x), and later on 2 * y + 5 * x is further split into
     (2 * y) + (5 * x).
     Note that we cannot handle directly the first multiplication in the original
     expression because, depending on how the selectors are implemented, we would either
     find the derivative of 3 * (x + 2 * y + 5 * x) or of 3 * x and both are wrong.
  2. Some of the leaves would have only product operations and they are handled using
     (find-op '* expr).
  |#
  (module+ test-operator-precedence
    (#%require rackunit)
    (display "--> Exercise/2.58/operator-precedence\n")

    #|
    Find the first occurrence of the given operation op at the top level and return the
    left and right expressions around it.

    Note on efficiency: since addend needs only the car of the result and augend needs
    only the cdr of the result, we could split find-op into two procedures, one
    computing the left expression and the other one computing the right expression. The
    latter could be implemented based on memq from Exercise/2.53.
    |#
    (define (find-op op expr)
      (define (extract expr)
        (if (= (length expr) 1) (car expr) expr))
      (define (iter expr left-expr)
        (cond [(null? expr) '()]
              [(eq? (car expr) op) (cons (extract left-expr) (extract (cdr expr)))]
              [else (iter (cdr expr) (append left-expr (list (car expr))))]))
      (iter expr '()))

    (define (sum? x) (and (pair? x) (not (null? (find-op '+ x)))))
    (define (addend s) (car (find-op '+ s)))
    (define (augend s) (cdr (find-op '+ s)))

    (define (product? x) (and (pair? x) (not (null? (find-op '* x)))))
    #|
    using (define (multiplicand p) (car (find-op '* p))) is the same as using
    (define (multiplier p) (car p)) because '* is processed after '+ (see above note on
    strategy)
    |#
    (define (multiplicand p) (cdr (find-op '* p)))

    ;; this is deriv from Example/symbolic-differentiation (I don't need exponentiation)
    (define (deriv expr var)
      (cond [(number? expr) 0]
            [(variable? expr) (if (same-variable? expr var) 1 0)]
            [(sum? expr) (make-sum (deriv (addend expr) var)
                                   (deriv (augend expr) var))]
            [(product? expr)
             (make-sum
              (make-product (multiplier expr)
                            (deriv (multiplicand expr) var))
              (make-product (deriv (multiplier expr) var)
                            (multiplicand expr)))]
            [else
             (error "unknown expression type: DERIV" expr)]))

    (let ([expr '(2 * x + x * (3 * y + 4) + 3 * x)])
      (check-true (sum? expr))
      (check-equal? (addend expr) '(2 * x))
      (check-equal? (augend expr) '(x * (3 * y + 4) + 3 * x)))

    (let ([expr '(2 * x * x * (3 * y + 4) * 3 * x)])
      (check-true (null? (find-op '+ expr)))
      (check-true (product? expr))
      (check-equal? (multiplier expr) 2)
      (check-equal? (multiplicand expr) '(x * x * (3 * y + 4) * 3 * x)))

    (check-equal? (deriv '(x + 3 * (x + y + 2)) 'x) 4)
    (check-equal? (deriv '(x * x + 3 * x) 'x) '((x + x) + 3))
    (check-equal? (deriv '(x * x * x + 3 * x) 'x)
                  (deriv '((x * (x * x)) + (3 * x)) 'x))))

(module Example/sets-as-unordered-lists sicp
  (#%provide adjoin-set
             intersection-set)
  (#%require (only racket/base module+))

  (define (element-of-set? x set)
    (cond [(null? set) false]
          [(equal? x (car set)) true]
          [else (element-of-set? x (cdr set))]))

  (define (adjoin-set x set)
    (if (element-of-set? x set)
        set
        (cons x set)))

  (define (intersection-set set1 set2)
    (cond [(or (null? set1) (null? set2)) '()]
          [(element-of-set? (car set1) set2)
           (cons (car set1) (intersection-set (cdr set1) set2))]
          [else (intersection-set (cdr set1) set2)]))

  (define (intersection-set-v2 set1 set2)
    (cond [(or (null? set1) (null? set2)) '()]
          [else (let ([intersection (intersection-set-v2 (cdr set1) set2)]
                      [head-set1 (car set1)])
                  (if (element-of-set? head-set1 set2)
                      (cons head-set1 intersection)
                      intersection))]))

  (module+ test
    (#%require rackunit)
    (display "--> Example/sets-as-unordered-lists\n")

    (let ([S1 '(1 2 3)]
          [S2 '(2 3 4)]
          [intersection '(2 3)])
      (check-true (element-of-set? 3 S1))
      (check-false (element-of-set? 4 S1))
      (check-equal? (adjoin-set 3 S1) S1)
      (check-equal? (adjoin-set 4 S1) (cons 4 S1))
      (check-equal? (intersection-set S1 S2) intersection)
      (check-equal? (intersection-set-v2 S1 S2) intersection))))

(module Exercise/2.59 sicp
  (#%require (only racket/base module+)
             (only (submod "sicp2_part1.rkt" Section/2.2.3) accumulate)
             (only (submod ".." Example/sets-as-unordered-lists) adjoin-set))

  (define (union-set set1 set2)
    (if (null? set1)
        set2
        (adjoin-set (car set1) (union-set (cdr set1) set2))))

  (define (union-set-v2 set1 set2)
    (accumulate adjoin-set set2 set1))

  (module+ test
    (#%require rackunit)
    (display "--> Exercise/2.59\n")

    (let ([S1 '(1 2 3)]
          [S2 '(2 3 4)]
          [union '(1 2 3 4)])
      (check-equal? (union-set S1 S2) union)
      (check-equal? (union-set '() S2) S2)
      (check-equal? (union-set S1 '()) S1)
      (check-equal? (union-set-v2 S1 S2) union)
      (check-equal? (union-set-v2 '() S2) S2)
      (check-equal? (union-set-v2 S1 '()) S1))))

(module Exercise/2.60 sicp
  (#%require (only racket/base module+ λ)
             (only (submod "sicp1.rkt" Exercise/1.43) repeated)
             (only (submod ".." Example/sets-as-unordered-lists) intersection-set))

  ;; element-of-set? and intersection-set are the same as in the non-duplicates version

  ;; O(1) compared to O(n) for the non-duplicates version
  (define (adjoin-set x set)
    (cons x set))

  ;; O(n) compared to O(n^2) for the non-duplicates version (for two sets of size n)
  (define (union-set set1 set2)
    (append set1 set2))

  #|
  The version with duplicates would be preferrable when we don't have to perform
  intersections (and even in this case it would be a good idea to de-duplicate the sets
  from time to time). See "worst case" example below.
  |#

  (module+ test
    (#%require rackunit)
    (display "--> Exercise/2.60\n")

    (let ([S1 '(1 2 3)]
          [S2 '(2 3 4)]
          [union '(1 2 3 2 3 4)])
      (check-equal? (union-set S1 S2) union)
      (check-equal? (adjoin-set 2 S1) (cons 2 S1))
      (check-equal? (adjoin-set 4 S1) (cons 4 S1)))

    ;; A worst case example of why intersections would be bad
    (let* ([S '(1 2 3)]
           [n (length S)]
           [union-set-curry (λ (set1)
                              (λ (set2)
                                (append set1 set2)))])
      ;; the intersection-set here is O(n^3)
      (intersection-set ((repeated (union-set-curry S) n) S) S))))

(module Example/sets-as-ordered-lists sicp
  (#%provide intersection-set)
  (#%require (only racket/base module+))

  (define (element-of-set? x set)
    (cond [(null? set) false]
          [(= x (car set)) true]
          [(< x (car set)) false]
          [else (element-of-set? x (cdr set))]))

  (define (intersection-set set1 set2)
    (if (or (null? set1) (null? set2))
        '()
        (let ([h1 (car set1)]
              [h2 (car set2)]
              [t1 (cdr set1)]
              [t2 (cdr set2)])
          (cond [(= h1 h2) (cons h1 (intersection-set t1 t2))]
                [(< h1 h2) (intersection-set t1 set2)]
                [(> h1 h2) (intersection-set set1 t2)]))))

  (module+ test
    (#%require rackunit)
    (display "--> Example/sets-as-ordered-lists\n")

    (let ([S '(2 4 6 8)])
      (check-true (element-of-set? 6 S))
      (check-false (element-of-set? 3 S)))
    (check-equal? (intersection-set '(1 2 3) '(2 3 4)) '(2 3))))

(module Exercise/2.61 sicp
  (#%require (only racket/base module+))

  (define (adjoin-set x set)
    (if (null? set)
        (list x)
        (let ([head (car set)]
              [tail (cdr set)])
          (cond [(= x head) set]
                [(< x head) (cons x set)]
                [else (cons head (adjoin-set x tail))]))))

  (module+ test
    (#%require rackunit)
    (display "--> Exercise/2.61\n")

    (check-equal? (adjoin-set 1 '()) '(1))
    (let ([S '(2 4 6 8)])
      (check-equal? (adjoin-set 1 S) '(1 2 4 6 8))
      (check-equal? (adjoin-set 2 S) '(2 4 6 8))
      (check-equal? (adjoin-set 3 S) '(2 3 4 6 8))
      (check-equal? (adjoin-set 4 S) '(2 4 6 8))
      (check-equal? (adjoin-set 5 S) '(2 4 5 6 8))
      (check-equal? (adjoin-set 6 S) '(2 4 6 8))
      (check-equal? (adjoin-set 7 S) '(2 4 6 7 8))
      (check-equal? (adjoin-set 8 S) '(2 4 6 8))
      (check-equal? (adjoin-set 9 S) '(2 4 6 8 9)))))

(module Exercise/2.62 sicp
  (#%provide union-set)
  (#%require (only racket/base module+))

  (define (union-set set1 set2)
    (cond [(null? set1) set2]
          [(null? set2) set1]
          [else
           (let ([h1 (car set1)]
                 [h2 (car set2)]
                 [t1 (cdr set1)]
                 [t2 (cdr set2)])
             (cond [(= h1 h2) (cons h1 (union-set t1 t2))]
                   [(< h1 h2) (cons h1 (union-set t1 set2))]
                   [(> h1 h2) (cons h2 (union-set set1 t2))]))]))

  (module+ test
    (#%require rackunit)
    (display "--> Exercise/2.62\n")

    (let ([S1 '(0 1 4 6 7 8)]
          [S2 '(1 2 3 4 9)])
      (check-equal? (union-set S1 '()) S1)
      (check-equal? (union-set '() S2) S2)
      (check-equal? (union-set S1 S2) '(0 1 2 3 4 6 7 8 9)))))

(module Example/sets-as-binary-trees sicp
  (#%provide make-tree
             entry
             left-branch
             right-branch
             adjoin-set
             tree->diagram
             binary-tree-1-to-7
             binary-tree-7-to-1)
  (#%require (only racket/base module+ module*)
             pict
             pict/tree-layout
             (only (submod "sicp2_part1.rkt" Section/2.2.3) accumulate)
             (only (submod "sicp1.rkt" conversion-utils) pict->file))

  (define (entry tree) (car tree))
  (define (left-branch tree) (cadr tree))
  (define (right-branch tree) (caddr tree))
  (define (make-tree entry left right)
    (list entry left right))

  (define (element-of-set? x set)
    (cond ((null? set) false)
          ((= x (entry set)) true)
          ((< x (entry set))
           (element-of-set? x (left-branch set)))
          ((> x (entry set))
           (element-of-set? x (right-branch set)))))

  (define (adjoin-set x set)
    (cond ((null? set) (make-tree x '() '()))
          ((= x (entry set)) set)
          ((< x (entry set))
           (make-tree (entry set)
                      (adjoin-set x (left-branch set))
                      (right-branch set)))
          ((> x (entry set))
           (make-tree (entry set)
                      (left-branch set)
                      (adjoin-set x (right-branch set))))))

  (define (tree->diagram tree)
    (define (construct-tree-diagram tree)
      (define (node-label label)
        (cc-superimpose
         (disk 30 #:color "white")
         (text
          (cond [(number? label) (number->string label)]
                [(symbol? label) (symbol->string label)]
                [else label]))))

      (cond [(pair? tree)
             (tree-layout
              #:pict (node-label (entry tree))
              (construct-tree-diagram (left-branch tree))
              (construct-tree-diagram (right-branch tree)))]
            [else #f]))
    (naive-layered (construct-tree-diagram tree)))

  (define binary-tree-1-to-7 (accumulate adjoin-set '() '(7 6 5 4 3 2 1)))
  (define binary-tree-7-to-1 (accumulate adjoin-set '() '(1 2 3 4 5 6 7)))

  (module+ test
    (#%require rackunit)
    (display "--> Example/sets-as-binary-trees\n")

    (check-true (element-of-set? 3 binary-tree-1-to-7))
    (check-false (element-of-set? 8 binary-tree-1-to-7))

    (define as adjoin-set) ; define for convenience
    (check-equal? binary-tree-1-to-7
                  (as 7 (as 6 (as 5 (as 4 (as 3 (as 2 (as 1 '()))))))))
    (check-equal? binary-tree-7-to-1
                  (as 1 (as 2 (as 3 (as 4 (as 5 (as 6 (as 7 '()))))))))

    (pict->file (ht-append 50
                           (tree->diagram binary-tree-1-to-7)
                           (tree->diagram binary-tree-7-to-1))
                #:file "out/binary-tree-1-to-7-and-7-to-1.svg"
                #:open #f))

  (module* test-box-and-pointer racket/base
    (#%require (only (submod "sicp1.rkt" conversion-utils) mcons->cons pict->file)
               (only (submod "..") binary-tree-1-to-7 binary-tree-7-to-1)
               sdraw)
    (display "--> Example/sets-as-binary-trees (test-box-and-pointer)\n")

    (pict->file (sdraw (mcons->cons binary-tree-1-to-7)
                       #:null-style '/)
                #:file "out/binary-tree-1-to-7-box-and-pointer.svg"
                #:open #f)

    (pict->file (sdraw (mcons->cons binary-tree-7-to-1)
                       #:null-style '/)
                #:file "out/binary-tree-7-to-1-box-and-pointer.svg"
                #:open #f)))

(module Exercise/2.63 sicp
  (#%provide (rename tree->list-2 tree->list))
  (#%require (only racket/base module+ format for)
             pict
             (only (submod ".." Example/sets-as-binary-trees)
                   entry
                   left-branch
                   right-branch
                   adjoin-set
                   tree->diagram
                   binary-tree-1-to-7
                   binary-tree-7-to-1)
             (only (submod "sicp1.rkt" conversion-utils) pict->file))

  (define (tree->list-1 tree)
    (if (null? tree)
        '()
        (append (tree->list-1 (left-branch tree))
                (cons (entry tree)
                      (tree->list-1 (right-branch tree))))))

  (define (tree->list-2 tree)
    (define (copy-to-list tree result-list)
      (if (null? tree)
          result-list
          (copy-to-list (left-branch tree)
                        (cons (entry tree)
                              (copy-to-list (right-branch tree)
                                            result-list)))))
    (copy-to-list tree '()))

  (define as adjoin-set)
  (define binary-tree-figure-2.16-a (as 11 (as 5 (as 1 (as 9 (as 3 (as 7 '())))))))
  (define binary-tree-figure-2.16-b (as 11 (as 9 (as 5 (as 7 (as 1 (as 3 '())))))))
  (define binary-tree-figure-2.16-c (as 11 (as 7 (as 1 (as 9 (as 3 (as 5 '())))))))

  (pict->file (ht-append 50
                         (tree->diagram binary-tree-figure-2.16-a)
                         (tree->diagram binary-tree-figure-2.16-b)
                         (tree->diagram binary-tree-figure-2.16-c))
              #:file "out/binary-tree-figure-2.16.svg"
              #:open #f)

  #|
  Task a:
  Both procedures give the same result for every tree. For all the trees in
  Figure 2.16, they return the same ordered list (1 3 5 7 9 11).

  Task b:
  Both procedures will take the same number of iterations but due to the call to
  append, each iteration of tree->list-1 would be more expensive in general.
  Given two lists l1 and l2, (append l1 l2) would apply cons for each element of l1 -
  see append-custom in (submod "sicp2_part1.rkt" Section/2.2.1), hence it is O(n) in
  the worst case. If T(n) denotes the total time for the algorithm on an input of size
  n, then

  tree->list-1: T(n) = 2*T(n/2) + O(n), hence O(n log n)
  tree->list-2: T(n) = 2*T(n/2) + O(1), hence O(n)

  see en.wikipedia.org/wiki/Master_theorem_(analysis_of_algorithms)#Application_to_common_algorithms

  In the test-count-cons below I compare tree->list-1 and tree->list-2 on the two
  limiting cases: binary-tree-1-to-7 and binary-tree-7-to-1. tree->list-1 uses cons
  7 times for binary-tree-1-to-7 and 28 times for binary-tree-7-to-1, while only 7 cons
  operations are performed for both trees when using tree->list-2.
  |#

  (module+ test
    (#%require rackunit)
    (display "--> Exercise/2.63\n")

    (for ([tree (list binary-tree-1-to-7
                      binary-tree-7-to-1
                      binary-tree-figure-2.16-a
                      binary-tree-figure-2.16-b
                      binary-tree-figure-2.16-c)])
      (let ([res-1 (tree->list-1 tree)]
            [res-2 (tree->list-2 tree)])
        (display (format "~a\n" tree))
        (display (format "~a\n" res-1))
        (display (format "~a\n" res-2))
        (check-equal? res-1 res-2))))

  (module+ test-count-cons
    (display "--> Exercise/2.63 (count cons)\n")

    (define (cons-custom x lst)
      (display (format "[cons] x: ~a, lst: ~a\n" x lst))
      (cons x lst))

    (define (append-custom l1 l2)
      (cond [(null? l1) l2]
            [else (cons-custom (car l1)
                               (append-custom (cdr l1) l2))]))

    (define (tree->list-1-show-cons tree)
      (if (null? tree)
          '()
          (append-custom (tree->list-1-show-cons (left-branch tree))
                         (cons-custom (entry tree)
                                      (tree->list-1-show-cons (right-branch tree))))))

    (define (tree->list-2-show-cons tree)
      (define (copy-to-list tree result-list)
        (if (null? tree)
            result-list
            (copy-to-list (left-branch tree)
                          (cons-custom (entry tree)
                                       (copy-to-list (right-branch tree)
                                                     result-list)))))
      (copy-to-list tree '()))

    (begin
      (display "----> binary-tree-1-to-7 with tree->list-1:\n")
      (tree->list-1-show-cons binary-tree-1-to-7))

    (begin
      (display "----> binary-tree-1-to-7 with tree->list-2:\n")
      (tree->list-2-show-cons binary-tree-1-to-7))

    (begin
      (display "----> binary-tree-7-to-1 with tree->list-1:\n")
      (tree->list-1-show-cons binary-tree-7-to-1))

    (begin
      (display "----> binary-tree-7-to-1 with tree->list-2:\n")
      (tree->list-2-show-cons binary-tree-7-to-1))))

(module Exercise/2.64 sicp
  (#%provide list->tree)
  (#%require (only racket/base module+)
             (only (submod "sicp1.rkt" conversion-utils) pict->file)
             (only (submod ".." Example/sets-as-binary-trees)
                   make-tree
                   tree->diagram
                   binary-tree-1-to-7
                   binary-tree-7-to-1))

  ;; simply returns the binary tree of the result of partial-tree
  (define (list->tree elements)
    (car (partial-tree elements (length elements))))

  #|
  0. I have rewritten partial-tree using let* (I find it more clear)
  1. The input is a list ELTS of ordered integers and N such that (>= (length ELTS) N)
  2. The output is a cons of the tree corresponding to the first N elements of ELTS and
     the remaining (- (length ELTS) N) elements in ELTS (to be denoted by RELTS)
  3. If ELTS is empty, return a cons of an empty tree and the empty ELTS
  4. Form the size of the left tree to construct as NL = (floor (/ N 2))
  5. Recursively form the LEFT tree (using NL elements of ELTS)
  6. Form the size NR of the right tree such that NL + NR = N - 1 (where one element is
     left for the node itself)
  7. Get the current NODE (the parent of the left and right trees) as (car RELTS)
  8. Recursively form the RIGHT tree (using the first NR elements of (cdr RELTS))
  9. Keep track of the elements that remain to be handled
  10. Return the tree (make-tree NODE LEFT RIGHT) and the elements to be handled

  SUMMARY:
  Recursively construct a balanced tree (NODE LEFT RIGHT) such that the number of
  nodes in the LEFT tree and the number of nodes in the RIGHT tree differ at most by
  one (in the case N is even). The algorithm relies on the fact that the input list of
  elements is sorted in increasing order.

  ORDER OF GROWTH:
  During each iteration we form two trees of size n/2 and the number of other
  operations doesn't depend on n, so we have: T(n) = 2*T(n/2) + O(1), which is O(n).
  |#

  (define (partial-tree elts n)
    (if (= n 0)
        (cons '() elts)
        (let* ([left-size (quotient (- n 1) 2)]
               [left-result (partial-tree elts left-size)]
               [left-tree (car left-result)]
               [non-left-elts (cdr left-result)]
               [right-size (- n (+ left-size 1))]
               [this-entry (car non-left-elts)]
               [right-result (partial-tree (cdr non-left-elts) right-size)]
               [right-tree (car right-result)]
               [remaining-elts (cdr right-result)])
          (cons (make-tree this-entry left-tree right-tree) remaining-elts))))

  (module+ test
    (display "--> Exercise/2.64\n")

    (pict->file (tree->diagram (list->tree '(1 3 5 7 9 11)))
                #:file "out/binary-tree-1-to-11-odd.svg"
                #:open #f)

    ;; test with a larger tree
    (define (n->1 n) (if (= n 1) (list 1) (cons n (n->1 (- n 1)))))
    (pict->file (tree->diagram (list->tree (reverse (n->1 50))))
                #:file "out/binary-tree-1-to-50.svg"
                #:open #f)))

(module Exercise/2.65 sicp
  (#%require (only racket/base module+)
             pict
             (only (submod "sicp2_part1.rkt" Section/2.2.3) accumulate)
             (only (submod "sicp1.rkt" conversion-utils) pict->file)
             (rename (submod ".." Example/sets-as-ordered-lists)
                     intersection-ordered-lists
                     intersection-set)
             (rename (submod ".." Exercise/2.62)
                     union-ordered-lists
                     union-set)
             (only (submod ".." Example/sets-as-binary-trees)
                   adjoin-set
                   tree->diagram
                   binary-tree-1-to-7)
             (only (submod ".." Exercise/2.63) tree->list)
             (only (submod ".." Exercise/2.64) list->tree))

  ;; O(n)
  (define (union-set set1 set2)
    (list->tree
     (union-ordered-lists
      (tree->list set1)
      (tree->list set2))))

  ;; O(n)
  (define (intersection-set set1 set2)
    (list->tree
     (intersection-ordered-lists
      (tree->list set1)
      (tree->list set2))))

  (module+ test
    (#%require rackunit)
    (display "--> Exercise/2.65\n")

    (define l1 '(1 3 5 7 9 10 11 13))
    (define l2 '(2 4 6 8 10 11 14))
    (define bt1 (accumulate adjoin-set '() l1))
    (define bt2 (accumulate adjoin-set '() l2))

    (let ([res-sorted-union '(1 2 3 4 5 6 7 8 9 10 11 13 14)]
          [res-sorted-intersection '(10 11)])
      (check-equal? (tree->list (union-set bt1 bt2)) res-sorted-union)
      (check-equal? (tree->list (intersection-set bt1 bt2)) res-sorted-intersection))

    (pict->file (ht-append 50
                           (tree->diagram binary-tree-1-to-7)
                           (tree->diagram (list->tree (tree->list binary-tree-1-to-7))))
                #:file "out/binary-tree-1-to-7-before-after.svg"
                #:open #f)

    (pict->file (ht-append 50
                           (tree->diagram (union-set bt1 bt2))
                           (tree->diagram (intersection-set bt1 bt2)))
                #:file "out/union-intersection-binary-tree.svg"
                #:open #f)))

(module+ test
  (require (submod ".." Section/2.2.4 test)
           (submod ".." Exercise/2.44 test)
           (submod ".." Exercise/2.45 test)
           (submod ".." Exercise/2.46 test)
           (submod ".." Exercise/2.47 test)
           (submod ".." Exercise/2.48 test)
           (submod ".." Exercise/2.49 test)
           (submod ".." Section/2.2.4/transforming-painters test)
           (submod ".." Exercise/2.50 test)
           (submod ".." Exercise/2.51 test)
           (submod ".." Exercise/2.52 test)
           (submod ".." Lecture/3A test)
           (submod ".." Exercise/2.53 test)
           (submod ".." Exercise/2.54 test)
           (submod ".." Exercise/2.55 test)
           (submod ".." Example/symbolic-differentiation test)
           (submod ".." Exercise/2.56 test)
           (submod ".." Exercise/2.57 test)
           (submod ".." Exercise/2.58 test-fully-parenthesized)
           (submod ".." Exercise/2.58 test-operator-precedence)
           (submod ".." Example/sets-as-unordered-lists test)
           (submod ".." Exercise/2.59 test)
           (submod ".." Exercise/2.60 test)
           (submod ".." Example/sets-as-ordered-lists test)
           (submod ".." Exercise/2.61 test)
           (submod ".." Exercise/2.62 test)
           (submod ".." Example/sets-as-binary-trees test)
           (submod ".." Example/sets-as-binary-trees test-box-and-pointer)
           (submod ".." Exercise/2.63 test)
           (submod ".." Exercise/2.63 test-count-cons)
           (submod ".." Exercise/2.64 test)
           (submod ".." Exercise/2.65 test)))
