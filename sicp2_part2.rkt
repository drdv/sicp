;; =====================================================================================
;; Exercises in Chapter 2 (part 2)
;; =====================================================================================
#lang racket/base

#|
I define some functions in this separate module because sicp doesn't support defaut
arguments (or at least I don't know how to implement them).
|#
(module pict-utils racket/base
  (#%provide save-painter)
  (#%require sicp-pict
             net/sendurl
             racket/class)

  #|
  Towards the end of the "picture language" related exercises I use my own paiters and
  paiter transformers/combiners but in the initial exercises it is convenient to use the
  sicp-pict package to verify results. Visualizing painters from sicp-pict requires
  using the paint procedure (see with-frame at
  https://github.com/sicp-lang/sicp/blob/master/sicp-pict/main.rkt) and we cannot
  directly pass a frame - which I find "unfortunate". The save-painter procedure saves
  to a png the image associated with a painter.
  |#
  (define (save-painter painter
                    #:file [filename "/var/tmp/_racket_tmp.png"]
                    #:size [size 500]
                    #:open [open-file #f])
    (send (send (paint painter #:width size #:height size) get-bitmap)
          save-file filename 'png)
    (if open-file
        (send-url/file filename)
        (display (format "file ~a created\n" filename)))))

(module Section/2.2.4 sicp
  (#%provide right-split
             up-split
             corner-split
             square-limit)
  (#%require (only racket/base module+)
             (only (submod "sicp1.rkt" Exercise/1.42) compose)
             (only (submod ".." pict-utils) save-painter)
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

    (save-painter einstein #:file "out/einstein.png")
    (save-painter (flip-horiz einstein) #:file "out/flip-horiz-einstein.png")
    (save-painter (beside einstein einstein) #:file "out/beside-einstein-einstein.png")
    (save-painter (below einstein einstein) #:file "out/below-einstein-einstein.png")
    (save-painter (flipped-pairs einstein) #:file "out/flipped-pairs-einstein.png")
    (save-painter (right-split einstein 4) #:file "out/right-split-4-einstein.png")
    (save-painter (corner-split einstein 4) #:file "out/corner-split-4-einstein.png")
    (save-painter (square-limit einstein 4) #:file "out/square-limit-4-einstein.png"))

  ;; =========================================================
  ;; Higher-order operations
  ;; =========================================================
  (define (square-of-four tl tr bl br)
    (lambda (painter)
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
             (only (submod ".." pict-utils) save-painter)
             (only (submod ".." Section/2.2.4) up-split))

  (module+ test
    (display "--> Exercise/2.44\n")

    (save-painter (up-split einstein 4) #:file "out/up-split-4-einstein.png")))

(module Exercise/2.45 sicp
  (#%require (only racket/base module+)
             sicp-pict
             (only (submod ".." pict-utils) save-painter)
             (only (submod ".." Section/2.2.4) up-split right-split))

  (define (split transform-1 transform-2)
    (lambda (painter n)
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
  (#%require (only racket/base module+)
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
    (lambda (vec)
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
             get-drawing-context
             save-dc
             task-d
             wave
             make-spline)
  (#%require (only racket/base module+ format)
             racket/class
             racket/draw
             net/sendurl
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
    (lambda (frame)
      (for-each
       (lambda (segment)
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

  (define (get-drawing-context size)
    (let ([dc (new bitmap-dc% [bitmap (make-bitmap size size)])])
      (send dc set-pen "black" 5 'solid)
      (send dc set-brush "white" 'solid)
      (send dc draw-rectangle 0 0 size size)
      dc))

  (define (save-dc dc filename open-png)
    (send (send dc get-bitmap) save-file filename 'png)
    (if open-png
        (send-url/file filename)
        (display (format "file ~a created\n" filename))))

  (define (task->png painter task frames size filename open-png)
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
       (lambda (frame)
         ((painter dc task) frame))
       frames)
      (send dc set-pen "red" 5 'short-dash)
      (for-each
       (lambda (frame)
         ((segments->painter dc frame-border) frame))
       frames)
      (save-dc dc filename open-png)))

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
    (task->png segments->painter task-c frames-fig-2.10 canvas-size "out/task-c.png" #f))

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
    (lambda (frame)
      (for-each
       (lambda (spline)
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
    (task->png splines->painter task-d frames-fig-2.10 canvas-size "out/task-d.png" #f)))

(module Section/2.2.4/transforming-painters sicp
  (#%provide transform-painter
             flip-vert
             rotate-90
             rotate+90
             beside)
  (#%require (only racket/base module+)
             racket/class
             net/sendurl
             (only (submod ".." Section/2.2.4/frames) make-vect make-frame)
             (only (submod ".." Exercise/2.46) sub-vect frame-coord-map)
             (only (submod ".." Exercise/2.49) get-drawing-context wave save-dc))

  (define (transform-painter painter origin corner1 corner2)
    (lambda (frame)
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
      (lambda (frame)
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
      (save-dc dc "out/flip-vert-wave.png" #f))

    (let ([dc (get-drawing-context size)])
      ((shrink-to-lower-right (wave dc)) frame)
      (send dc set-pen "red" 5 'solid)
      ((shrink-to-upper-right (wave dc)) frame)
      (save-dc dc "out/shrink-wave.png" #f))

    (let ([dc (get-drawing-context size)])
      ((wave dc) frame)
      (send dc set-pen "red" 5 'solid)
      ((rotate-90 (wave dc)) frame)
      (save-dc dc "out/rotate-90-wave.png" #f))

    (let ([dc (get-drawing-context size)])
      ((wave dc) frame)
      (send dc set-pen "red" 5 'solid)
      ((rotate+90 (wave dc)) frame)
      (save-dc dc "out/rotate+90-wave.png" #f))

    ;; same result as flip-vert
    (let ([dc (get-drawing-context size)])
      ((wave dc) frame)
      (send dc set-pen "red" 5 'solid)
      ((rotate-90 (rotate-90 (wave dc))) frame)
      (save-dc dc "out/rotate-90-rotate-90-wave.png" #f))

    (let ([dc (get-drawing-context size)])
      ((rotate+90 (rotate-90 (wave dc))) frame)
      (save-dc dc "out/rotate+90-rotate-90-wave.png" #f))

    (let ([dc (get-drawing-context size)])
      ((squash-inwards (wave dc)) frame)
      (save-dc dc "out/squash-inwards-wave.png" #f))

    (let ([dc (get-drawing-context size)])
      ((beside (wave dc) (flip-vert (wave dc))) frame)
      (save-dc dc "out/beside-wave.png" #f))))

(module Exercise/2.50 sicp
  (#%provide flip-horiz)
  (#%require (only racket/base module+)
             (only (submod "sicp1.rkt" Exercise/1.43) repeated)
             (only (submod ".." Exercise/2.49) get-drawing-context wave save-dc)
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
      (save-dc dc "out/flip-horiz-wave.png" #f))

    ;; rotating 180 degrees is equivalent to flip-vert
    (let ([dc (get-drawing-context size)])
      ((beside ((repeated rotate+90 2) (wave dc)) ; (rotate+90 (rotate+90 (wave dc)))
               (flip-vert (wave dc))) frame)
      (save-dc dc "out/rotate-180-wave.png" #f))

    ;; rotating 270 degrees counterclockwise is equivalent to rotate-90
    (let ([dc (get-drawing-context size)])
      ((beside ((repeated rotate+90 3) (wave dc))
               (rotate-90 (wave dc))) frame)
      (save-dc dc "out/rotate-270-wave.png" #f))))

(module Exercise/2.51 sicp
  (#%provide below)
  (#%require (only racket/base module+)
             (only (submod ".." Exercise/2.49) get-drawing-context wave save-dc)
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
      (lambda (frame)
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
      (save-dc dc "out/below-wave.png" #f))

    (let ([dc (get-drawing-context size)])
      ((below-rot (wave dc) (wave dc)) frame)
      (save-dc dc "out/below-rot-wave.png" #f))))

#|
Here we cannot reuse right-split, up-split, corner-split, square-limit etc. defined in
Section/2.2.4 because they are based on utilities in sicp-pict. So I simply redefine (and
modify) them.
|#
(module Exercise/2.52 sicp
  (#%require (only racket/base module+)
             (only (submod "sicp1.rkt" Exercise/1.42) compose)
             (only (submod ".." Exercise/2.49)
                   splines->painter
                   get-drawing-context
                   save-dc
                   make-spline)
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
    (lambda (painter)
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
      (save-dc dc "out/wave-heart.png" #f))

    ;; task b
    (let ([dc (get-drawing-context size)])
      ((corner-split (wave-heart dc) 2) frame)
      (save-dc dc "out/corner-split-wave-heart.png" #f))

    ;; task c
    (let ([dc (get-drawing-context size)])
      ((square-limit (wave-heart dc) 1) frame)
      (save-dc dc "out/square-limit-wave-heart.png" #f))))

#|
This module includes the push example from Lecture 3A. I found both the lecture and the
"picture language" exercises to be very enlightening.
|#
(module Lecture/3A sicp
  (#%require (only racket/base module+)
             (only (submod "sicp1.rkt" Exercise/1.43) repeated)
             (only (submod ".." Exercise/2.49)
                   splines->painter
                   get-drawing-context
                   save-dc
                   wave)
             (only (submod ".." Section/2.2.4/frames) make-vect make-frame)
             (only (submod ".." Section/2.2.4/transforming-painters) beside)
             (only (submod ".." Exercise/2.51) below))

  (define (push comb)
    (lambda (pict n)
      (let ([f (repeated (lambda (p) (comb pict p)) n)])
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
      (save-dc dc "out/right-push-wave.png" #f))

    (let ([dc (get-drawing-context size)])
      ((down-push (wave dc) 3) frame)
      (save-dc dc "out/down-push-wave.png" #f))))

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
    (memq 'red '(red shoes blue socks))))   ; (red shoes blue socks)

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

#|
NOTE: we could have an expression like '((x * x)+(x * x)), note that there are no
spaces around the + operator, but we cannot have an expression like '(x+(x * x))
|#
(module Exercise/2.58 sicp
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
  (#%require (only racket/base module+)
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
           [union-set-curry (lambda (set1)
                              (lambda (set2)
                                (append set1 set2)))])
      ;; the intersection-set here is O(n^3)
      (intersection-set ((repeated (union-set-curry S) n) S) S))))

(module Example/sets-as-ordered-lists sicp
  (#%require (only racket/base module+))

  (define (element-of-set? x set)
    (cond [(null? set) false]
          [(= x (car set)) true]
          [(< x (car set)) false]
          [else (element-of-set? x (cdr set))]))

  (define (intersection-set set1 set2)
    (if (or (null? set1) (null? set2))
        '()
        (let ([x1 (car set1)]
              [x2 (car set2)])
          (cond [(= x1 x2)
                 (cons x1 (intersection-set (cdr set1)
                                            (cdr set2)))]
                [(< x1 x2)
                 (intersection-set (cdr set1) set2)]
                [(< x2 x1)
                 (intersection-set set1 (cdr set2))]))))

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
           (submod ".." Exercise/2.61 test)))
