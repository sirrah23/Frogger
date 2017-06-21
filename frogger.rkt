#lang racket

(require racket/gui)

(define WIDTH 800)
(define HEIGHT 800)
(define STEP 50)
(define green-brush (make-object brush% "GREEN" 'solid))
(define gray-brush (make-object brush% "GRAY" 'solid))

(define frame (new frame%
                   [label "Frogger"]
                   [width WIDTH]
                   [height HEIGHT]))

(define f-canvas
  (class canvas%
    (define/override (on-char event)
      (cond [(eq? (send event get-key-code) 'left)(set! game-frog (move-rect game-frog (* -1 STEP) 0))]
            [(eq? (send event get-key-code) 'right)(set! game-frog (move-rect game-frog STEP 0))]
            [(eq? (send event get-key-code) 'up)(set! game-frog (move-rect game-frog 0 (* -1 STEP)))]
            [(eq? (send event get-key-code) 'down)(set! game-frog (move-rect game-frog 0 STEP))]))
  (super-new)))

(define canvas (new f-canvas
                    [parent frame]
                    [paint-callback (lambda (canvas dc)
                                      (send dc clear)
                                      (set! lane-A (lane (lane-height lane-A) (lane-dir lane-A) (move-lane-wrap lane-A 10)))
                                      (set! lane-B (lane (lane-height lane-B) (lane-dir lane-B) (move-lane-wrap lane-B 5)))
                                      (set! lane-C (lane (lane-height lane-C) (lane-dir lane-C) (move-lane-wrap lane-C 10)))
                                      (show-rects (lane-cars lane-A) gray-brush)
                                      (show-rects (lane-cars lane-B) gray-brush)
                                      (show-rects (lane-cars lane-C) gray-brush)
                                      (show-rect game-frog green-brush))]))

(define dc (send canvas get-dc))

(send frame show #t)

(struct pos (x y))
(struct rect (pos width height))
(struct lane (height dir cars))

(define (make-lane lane-height dir numcars car-width car-height)
  (define (inner-make-lane cars lane-height dir numcarsleft)
    (if (eq? 0 numcarsleft) (lane lane-height dir cars)
        (inner-make-lane
         (cons (rect (pos (+ (* 3 STEP (- numcars numcarsleft)) (* car-width (- numcars numcarsleft))) lane-height) car-width car-height) cars)
                         lane-height dir (- numcarsleft 1))))
  (inner-make-lane '() lane-height dir numcars))
 
(define lane-A (make-lane (- HEIGHT 100) -1 3 150 50))
(define lane-B (make-lane (- HEIGHT 200) 1 3 150 50))
(define lane-C (make-lane (- HEIGHT 300) -1 3 150 50))

(define (show-rect r brush)
  (send dc set-brush brush)
  (send dc draw-rectangle (pos-x (rect-pos r))
        (pos-y (rect-pos r)) (rect-width r) (rect-height r)))

(define (show-rects rlist brush)
  (map (lambda (r) (show-rect r brush)) rlist))

(define game-frog (rect (pos (- (/ WIDTH 2) 25) (- HEIGHT 50)) 50 50))

(define (move-rect r dispx dispy)
  (rect (pos (+ (pos-x (rect-pos r)) dispx)
             (+ (pos-y (rect-pos r)) dispy))
        (rect-width r)
        (rect-height r)))

(define (move-rect-wrap r dispx dispy)
  (let ([new-pos-x (+ (pos-x (rect-pos r)) dispx)]
        [new-pos-y(+ (pos-y (rect-pos r)) dispy)])
    (cond [(> new-pos-x WIDTH) (rect (pos (- 0 (rect-width r)) new-pos-y) (rect-width r) (rect-height r))]
          [(< (+ new-pos-x (rect-width r)) 0) (rect (pos WIDTH new-pos-y) (rect-width r) (rect-height r))]
          [else (rect (pos new-pos-x new-pos-y) (rect-width r) (rect-height r))])))

(define (move-lane-wrap lane speed)
  (map (lambda (x) (move-rect-wrap x (* speed (lane-dir lane)) 0)) (lane-cars lane)))
  

(define (collission-rects rectA rectB)
  (if (or (>= (pos-x (rect-pos rectA)) (+ (pos-x (rect-pos rectB)) (rect-width rectB)))   ; Right
          (<= (+ (pos-x (rect-pos rectA)) (rect-width rectA)) (pos-x (rect-pos rectB)))   ; Left
          (>= (pos-y (rect-pos rectA)) (+ (pos-y (rect-pos rectB)) (rect-height rectB)))  ; Below
          (<= (+ (pos-y (rect-pos rectA)) (rect-height rectA)) (pos-y (rect-pos rectB)))) ; Above
      #f
      #t))

(define (collission-lane rect lane)
  (ormap (lambda (b) b) (map (lambda (x) (collission-rects rect x)) (lane-cars lane))))

(define (loop)
  (if (collission-lane game-frog lane-A) (write "HIT") #f) ; TODO - Collission Logic and World Update function
  (send canvas  on-paint)
  (sleep/yield 0.02)
  (loop))

(loop)
 