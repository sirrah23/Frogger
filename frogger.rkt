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
      (cond [(eq? (send event get-key-code) 'left)(set! game-frog (move-frog game-frog (* -1 STEP) 0))]
            [(eq? (send event get-key-code) 'right)(set! game-frog (move-frog game-frog STEP 0))]
            [(eq? (send event get-key-code) 'up)(set! game-frog (move-frog game-frog 0 (* -1 STEP)))]
            [(eq? (send event get-key-code) 'down)(set! game-frog (move-frog game-frog 0 STEP))]))
  (super-new)))

(define canvas (new f-canvas
                    [parent frame]
                    [paint-callback (lambda (canvas dc)
                                      (send dc clear)
                                      (set! car-A (move-car car-A 15 0))
                                      (set! car-B (move-car car-B 15 0))
                                      (set! car-C (move-car car-C 15 0))
                                      (show-car car-A)
                                      (show-car car-B)
                                      (show-car car-C)
                                      (show-frog game-frog)
                                      )]))

(define dc (send canvas get-dc))

(send frame show #t)

(struct pos (x y))

(struct frog (width height pos))
(struct car (width height pos))

;TODO: Make the frog green
(define (show-frog f)
  (send dc set-brush green-brush)
  (send dc draw-rectangle
        (pos-x (frog-pos f))
        (pos-y (frog-pos f))
        (frog-width f)
        (frog-height f)))

(define (show-car c)
  (send dc set-brush gray-brush)
  (send dc draw-rectangle
        (pos-x (car-pos c))
        (pos-y (car-pos c))
        (car-width c)
        (car-height c)))

(define game-frog (frog 50 50 (pos (- (/ WIDTH 2) 25) (- HEIGHT 50))))
(define car-A (car 150 50 (pos 100 (- HEIGHT 100))))
(define car-B (car 150 50 (pos 275 (- HEIGHT 100))))
(define car-C (car 150 50 (pos 450 (- HEIGHT 100))))

(define (move-frog f dispx dispy)
  (frog (frog-width f)
        (frog-height f)
        (pos (+ (pos-x (frog-pos f)) dispx)
             (+ (pos-y (frog-pos f)) dispy))))

(define (move-car c dispx dispy)
  (let ([new-pos-x (+ (pos-x (car-pos c)) dispx)]
        [new-pos-y(+ (pos-y (car-pos c)) dispy)])
    (if (> new-pos-x WIDTH) (car (car-width c) (car-height c) (pos (- 0 (car-width c)) new-pos-y))
        (car (car-width c) (car-height c) (pos new-pos-x new-pos-y)))))    


(define (loop)
  (send canvas  on-paint)
  (sleep/yield 0.02)
  (loop))

(loop)
 