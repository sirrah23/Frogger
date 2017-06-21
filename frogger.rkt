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
                                      (set! car-A (move-rect-wrap car-A 15 0))
                                      (set! car-B (move-rect-wrap car-B 15 0))
                                      (set! car-C (move-rect-wrap car-C 15 0))
                                      (show-rect car-A gray-brush)
                                      (show-rect car-B gray-brush)
                                      (show-rect car-C gray-brush)
                                      (show-rect game-frog green-brush))]))

(define dc (send canvas get-dc))

(send frame show #t)

(struct pos (x y))
(struct rect (pos width height))

;TODO: Make the frog green
(define (show-rect r brush)
  (send dc set-brush brush)
  (send dc draw-rectangle (pos-x (rect-pos r))
        (pos-y (rect-pos r)) (rect-width r) (rect-height r)))

(define game-frog (rect (pos (- (/ WIDTH 2) 25) (- HEIGHT 50)) 50 50))
(define car-A (rect (pos 100 (- HEIGHT 100)) 150 50 ))
(define car-B (rect (pos 275 (- HEIGHT 100)) 150 50))
(define car-C (rect (pos 450 (- HEIGHT 100)) 150 50))

(define (move-rect r dispx dispy)
  (rect (pos (+ (pos-x (rect-pos r)) dispx)
             (+ (pos-y (rect-pos r)) dispy))
        (rect-width r)
        (rect-height r)))

(define (move-rect-wrap r dispx dispy)
  (let ([new-pos-x (+ (pos-x (rect-pos r)) dispx)]
        [new-pos-y(+ (pos-y (rect-pos r)) dispy)])
    (if (> new-pos-x WIDTH) (rect (pos (- 0 (rect-width r)) new-pos-y) (rect-width r) (rect-height r))
        (rect (pos new-pos-x new-pos-y) (rect-width r) (rect-height r)))))

(define (collission-rects rectA rectB)
  (if (or (>= (pos-x (rect-pos rectA)) (+ (pos-x (rect-pos rectB)) (rect-width rectB)))  ; Right
          (<= (+ (pos-x (rect-pos rectA)) (rect-width rectA)) (pos-x (rect-pos rectB)))  ; Left
          (>= (pos-y (rect-pos rectA)) (+ (pos-y (rect-pos rectB)) (rect-height rectB))) ; Below
          (<= (+ (pos-y (rect-pos rectA)) (rect-height rectA)) (pos-y (rect-pos rectB)))) ; Above
      #f
      #t))


(define (loop)
  (if (collission-rects game-frog car-A) (write "HIT YO") #f)
  (send canvas  on-paint)
  (sleep/yield 0.02)
  (loop))

(loop)
 