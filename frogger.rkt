#lang racket

(require racket/gui)

(define WIDTH 800)
(define HEIGHT 800)
(define STEP 50)
(define green-brush (make-object brush% "GREEN" 'solid))
(define gray-brush (make-object brush% "GRAY" 'solid))
(define brown-brush (make-object brush% "BROWN" 'solid))
(define blue-brush (make-object brush% "BLUE" 'solid))

(define frame (new frame%
                   [label "Frogger"]
                   [width WIDTH]
                   [height HEIGHT]))

(define f-canvas
  (class canvas%
    (define/override (on-char event)
      (cond [(eq? (send event get-key-code) 'left)
             (set! game-frog (move-frog game-frog (* -1 STEP) 0))]
            [(eq? (send event get-key-code) 'right)
             (set! game-frog (move-frog game-frog STEP 0))]
            [(eq? (send event get-key-code) 'up)
             (set! game-frog (move-frog game-frog 0 (* -1 STEP)))]
            [(eq? (send event get-key-code) 'down)
             (set! game-frog (move-frog game-frog 0 STEP))]))
  (super-new)))

(define canvas (new f-canvas
                    [parent frame]
                    [paint-callback (lambda (canvas dc)
                                      (send dc clear)
                                      (show-lane lane-A)
                                      (show-lane lane-B)
                                      (show-lane lane-C)
                                      (show-lane lane-D)
                                      (show-lane lane-E)
                                      (show-lane lane-F)
                                      (show-lane lane-G)
                                      (show-lane lane-H)
                                      (show-rect (frog-rect game-frog) green-brush))]))

(define dc (send canvas get-dc))

(send frame show #t)

(struct pos (x y))
(struct rect (pos width height))
(struct lane (height dir cars speed type))
(struct frog (rect vel))
(define frog-stuck 0)

(define (make-lane lane-height dir numcars car-width car-height speed type)
  (define (inner-make-lane cars lane-height dir numcarsleft)
    (if (eq? 0 numcarsleft) (lane lane-height dir cars speed type)
        (inner-make-lane
         (cons (rect (pos (+ (* 3 STEP (- numcars numcarsleft)) (* car-width (- numcars numcarsleft))) lane-height) car-width car-height) cars)
                         lane-height dir (- numcarsleft 1))))
  (inner-make-lane '() lane-height dir numcars))
 
(define lane-A (make-lane (- HEIGHT 100) -1 3 150 50 8 'car))
(define lane-B (make-lane (- HEIGHT 150) 1 3 150 50 3 'car))
(define lane-C (make-lane (- HEIGHT 300) -1 3 150 50 8 'car))
(define lane-D (make-lane (- HEIGHT 350) 1 3 150 50 7 'car))
(define lane-E (make-lane (- HEIGHT 500) 1 3 150 50 7 'car))
(define lane-F (make-lane (- HEIGHT 600) -1 3 150 50 8 'log))
(define lane-G (make-lane (- HEIGHT 650) 1 3 150 50 8 'log))
(define lane-H (make-lane (- HEIGHT 700) -1 3 150 50 8 'log))

(define (show-rect r brush)
  (send dc set-brush brush)
  (send dc draw-rectangle (pos-x (rect-pos r))
        (pos-y (rect-pos r)) (rect-width r) (rect-height r)))

(define (show-lane-bkgrnd height brush)
  (show-rect (rect (pos 0 height) WIDTH 50) brush))

(define (show-lane lane)
  (if (eq? (lane-type lane) 'car)
      (show-rects (lane-cars lane) gray-brush)
      (begin
        (show-lane-bkgrnd (lane-height lane) blue-brush)
        (show-rects (lane-cars lane) brown-brush))))
       

(define (show-rects rlist brush)
  (map (lambda (r) (show-rect r brush)) rlist))

(define game-frog (frog (rect (pos (- (/ WIDTH 2) 25) (- HEIGHT 50)) 50 50) (pos 0 0)))

(define (reset-frog)
  (set! game-frog (frog (rect (pos (- (/ WIDTH 2) 25) (- HEIGHT 50)) 50 50) (pos 0 0))))

(define (move-frog f dispx dispy)
    (frog (move-rect (frog-rect f) dispx dispy) (pos 0 0)))

(define (move-frog-log f)
  (move-frog f (pos-x (frog-vel f)) 0))

(define (stick-frog f flane)
  (define (inner-stick-frog cars)
    (if (eq? cars '()) (reset-frog)
      (if (not (collission-rects (frog-rect f) (car cars))) (inner-stick-frog (cdr cars))
          (begin
            (set! game-frog (frog (frog-rect f) (pos (* (lane-dir flane)(lane-speed flane)) 0)))
            (set! frog-stuck 1)))))
  (inner-stick-frog (lane-cars flane)))
          
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

(define (move-lane-wrap lane)
  (map (lambda (x) (move-rect-wrap x (* (lane-speed lane) (lane-dir lane)) 0)) (lane-cars lane)))

(define (frog-in-lane lane)
  (eq? (lane-height lane) (pos-y (rect-pos (frog-rect game-frog)))))
  
(define (collission-rects rectA rectB)
  (if (or (>= (pos-x (rect-pos rectA)) (+ (pos-x (rect-pos rectB)) (rect-width rectB)))   ; Right
          (<= (+ (pos-x (rect-pos rectA)) (rect-width rectA)) (pos-x (rect-pos rectB)))   ; Left
          (>= (pos-y (rect-pos rectA)) (+ (pos-y (rect-pos rectB)) (rect-height rectB)))  ; Below
          (<= (+ (pos-y (rect-pos rectA)) (rect-height rectA)) (pos-y (rect-pos rectB)))) ; Above
      #f
      #t))

(define (collission-lane rect lane)
  (ormap (lambda (b) b) (map (lambda (x) (collission-rects rect x)) (lane-cars lane))))

; TODO - Collission Logic and World Update function
; TODO - Win condition check
(define (loop)
  (when (and (frog-in-lane lane-A)
      (collission-lane (frog-rect game-frog) lane-A)) (reset-frog))
  (when (and (frog-in-lane lane-B)
      (collission-lane (frog-rect game-frog) lane-B)) (reset-frog))
  (when (and (frog-in-lane lane-C)
      (collission-lane (frog-rect game-frog) lane-C)) (reset-frog))
  (when (and (frog-in-lane lane-D)
    (collission-lane (frog-rect game-frog) lane-D)) (reset-frog))
  (when (and (frog-in-lane lane-E)
      (collission-lane (frog-rect game-frog) lane-E)) (reset-frog))
  (when (frog-in-lane lane-F)
    (if (not (collission-lane (frog-rect game-frog) lane-F)) (reset-frog) (stick-frog game-frog lane-F)))
  (when (frog-in-lane lane-G)
    (if (not (collission-lane (frog-rect game-frog) lane-G)) (reset-frog) (stick-frog game-frog lane-G)))
  (when (frog-in-lane lane-H)
    (if (not (collission-lane (frog-rect game-frog) lane-H)) (reset-frog) (stick-frog game-frog lane-H)))
  (set! lane-A (lane (lane-height lane-A) (lane-dir lane-A) (move-lane-wrap lane-A) (lane-speed lane-A) (lane-type lane-A)))
  (set! lane-B (lane (lane-height lane-B) (lane-dir lane-B) (move-lane-wrap lane-B) (lane-speed lane-B) (lane-type lane-B)))
  (set! lane-C (lane (lane-height lane-C) (lane-dir lane-C) (move-lane-wrap lane-C) (lane-speed lane-C) (lane-type lane-C)))
  (set! lane-D (lane (lane-height lane-D) (lane-dir lane-D) (move-lane-wrap lane-D) (lane-speed lane-D) (lane-type lane-D)))
  (set! lane-E (lane (lane-height lane-E) (lane-dir lane-E) (move-lane-wrap lane-E) (lane-speed lane-E) (lane-type lane-E)))
  (set! lane-F (lane (lane-height lane-F) (lane-dir lane-F) (move-lane-wrap lane-F) (lane-speed lane-F) (lane-type lane-F)))
  (set! lane-G (lane (lane-height lane-G) (lane-dir lane-G) (move-lane-wrap lane-G) (lane-speed lane-G) (lane-type lane-G)))
  (set! lane-H (lane (lane-height lane-H) (lane-dir lane-H) (move-lane-wrap lane-H) (lane-speed lane-H) (lane-type lane-H)))
  (when (eq? frog-stuck 1) (set! game-frog (move-frog-log game-frog)))
  (send canvas  on-paint)
  (sleep/yield 0.02)
  (loop))

(loop)
 