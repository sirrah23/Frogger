#lang racket

(require racket/gui)

(define WIDTH 800)
(define HEIGHT 800)
(define STEP 50)
(define green-brush (make-object brush% "GREEN" 'solid))

(define frame (new frame%
                   [label "Frogger"]
                   [width WIDTH]
                   [height HEIGHT]))

(define f-canvas
  (class canvas%
    (define/override (on-char event)
      (cond [(eq? (send event get-key-code) 'left)(set! game-frog (move-frog game-frog (* -1 STEP) 0))]
            [(eq? (send event get-key-code) 'right)(set! game-frog (move-frog game-frog STEP 0))]
            [(eq? (send event get-key-code) 'up)(set! game-frog (move-frog game-frog 0 (* -1 STEP)))]
            [(eq? (send event get-key-code) 'down)(set! game-frog (move-frog game-frog 0 STEP))]))
  (super-new)))

(define canvas (new f-canvas
                    [parent frame]
                    [paint-callback (lambda (canvas dc)
                                      (send dc erase)
                                      (show-frog game-frog))]))

(define dc (send canvas get-dc))

(send frame show #t)

(struct pos (x y))

(struct frog (width height pos))

;TODO: Make the frog green
(define (show-frog f)
  (send dc clear)
  (send dc set-brush green-brush)
  (send dc draw-rectangle
        (pos-x (frog-pos f))
        (pos-y (frog-pos f))
        (frog-width f)
        (frog-height f)))

(define game-frog (frog 50 50 (pos (- (/ WIDTH 2) 25) (- HEIGHT 50))))

(define (move-frog f dispx dispy)
  (frog (frog-width f)
        (frog-height f)
        (pos (+ (pos-x (frog-pos f)) dispx)
             (+ (pos-y (frog-pos f)) dispy))))


game-frog

(define (loop)
  (send canvas  on-paint)
  (sleep/yield 0.02)
  (loop))

(loop)
  
