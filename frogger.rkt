#lang racket

(require racket/gui)

; Global State
(define WIDTH 800)
(define HEIGHT 800)
(define STEP 50)

; Some brushes for drawing stuff
(define green-brush (make-object brush% "GREEN" 'solid))
(define gray-brush (make-object brush% "GRAY" 'solid))
(define brown-brush (make-object brush% "BROWN" 'solid))
(define blue-brush (make-object brush% "BLUE" 'solid))

; Window/Canvas Objects
(define frogger-frame (new frame%
                           [label "Frogger"]
                           [width WIDTH]
                           [height HEIGHT]))

(define frogger-canvas-class
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

(define frogger-canvas (new frogger-canvas-class
                            [parent frogger-frame]
                            [paint-callback (lambda (canvas dc)
                                              (send dc clear)
                                              (show-lanes)
                                              (show-frog))]))

(define frogger-dc (send frogger-canvas get-dc))

(send frogger-frame show #t)

(struct pos (x y))
(struct rect (pos width height))
(struct lane (height dir vehicles speed type))
(struct frog (rect vel))
(define frog-stuck 0)

(define (make-lane lane-height dir numvehicles vehicle-width vehicle-height speed type)
  (define (inner-make-lane vehicles lane-height dir numvehiclesleft)
    (if (eq? 0 numvehiclesleft) (lane lane-height dir vehicles speed type)
        (inner-make-lane
         (cons
          (rect (pos (+ (* 3 STEP (- numvehicles numvehiclesleft)) (* vehicle-width (- numvehicles numvehiclesleft))) lane-height)
                vehicle-width vehicle-height) vehicles)
         lane-height dir (- numvehiclesleft 1))))
  (inner-make-lane '() lane-height dir numvehicles))
 
(define lane-A (make-lane (- HEIGHT 100) -1 3 150 50 8 'car))
(define lane-B (make-lane (- HEIGHT 150) 1 3 150 50 3 'car))
(define lane-C (make-lane (- HEIGHT 300) -1 3 150 50 8 'car))
(define lane-D (make-lane (- HEIGHT 350) 1 3 150 50 7 'car))
(define lane-E (make-lane (- HEIGHT 500) 1 3 150 50 7 'car))
(define lane-F (make-lane (- HEIGHT 600) -1 3 150 50 8 'log))
(define lane-G (make-lane (- HEIGHT 700) 1 3 150 50 8 'log))
(define lane-H (make-lane (- HEIGHT 750) -1 3 150 50 8 'log))
(define all-lanes (list lane-A lane-B lane-C lane-D lane-E lane-F lane-G lane-H))

(define (show-rect r brush)
  (send frogger-dc set-brush brush)
  (send frogger-dc draw-rectangle (pos-x (rect-pos r))
        (pos-y (rect-pos r)) (rect-width r) (rect-height r)))

(define (show-rects rlist brush)
  (map (lambda (r) (show-rect r brush)) rlist))

(define (show-lane lane)
  (if (eq? (lane-type lane) 'car)
      (show-rects (lane-vehicles lane) gray-brush)
      (begin
        (show-lane-bkgrnd (lane-height lane) blue-brush)
        (show-rects (lane-vehicles lane) brown-brush))))

(define (show-lane-bkgrnd height brush)
  (show-rect (rect (pos 0 height) WIDTH 50) brush))

(define (show-lanes)
  (map show-lane all-lanes))
       
(define game-frog (frog (rect (pos (- (/ WIDTH 2) 25) (- HEIGHT 50)) 50 50) (pos 0 0)))

(define (reset-frog)
  (set! game-frog (frog (rect (pos (- (/ WIDTH 2) 25) (- HEIGHT 50)) 50 50) (pos 0 0))))

(define (move-frog f dispx dispy)
  (frog (move-rect (frog-rect f) dispx dispy) (pos 0 0)))

(define (move-frog-log f)
  (move-frog f (pos-x (frog-vel f)) 0))

(define (stick-frog-to-log f flane)
  (define (inner-stick-frog-to-log vehicles)
    (if (eq? vehicles '()) (reset-frog)
        (if (not (collission-rects (frog-rect f) (car vehicles))) (inner-stick-frog-to-log (cdr vehicles))
            (begin
              (set! game-frog (frog (frog-rect f) (pos (* (lane-dir flane)(lane-speed flane)) 0)))
              (set! frog-stuck 1)))))
  (inner-stick-frog-to-log (lane-vehicles flane)))

(define (show-frog)
  (show-rect (frog-rect game-frog) green-brush))
  
          
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
  (map (lambda (x) (move-rect-wrap x (* (lane-speed lane) (lane-dir lane)) 0)) (lane-vehicles lane)))

(define (move-lane-tick l)
  (lane (lane-height l) (lane-dir l) (move-lane-wrap l) (lane-speed l) (lane-type l)))

(define (move-lanes-tick)
  (set! all-lanes (map move-lane-tick all-lanes)))

(define (frog-in-lane lane)
  (eq? (lane-height lane) (pos-y (rect-pos (frog-rect game-frog)))))

(define (frog-lane-interaction l)
  (if (eq? (lane-type l) 'car)
      (when (and (frog-in-lane l)
                 (collission-lane (frog-rect game-frog) l)) (reset-frog))
      (when (frog-in-lane l)
        (if (not (collission-lane (frog-rect game-frog) l)) (reset-frog) (stick-frog-to-log game-frog l)))))

(define (frog-lanes-interaction)
  (map frog-lane-interaction all-lanes)) 
        
(define (collission-rects rectA rectB)
  (if (or (>= (pos-x (rect-pos rectA)) (+ (pos-x (rect-pos rectB)) (rect-width rectB)))   ; Right
          (<= (+ (pos-x (rect-pos rectA)) (rect-width rectA)) (pos-x (rect-pos rectB)))   ; Left
          (>= (pos-y (rect-pos rectA)) (+ (pos-y (rect-pos rectB)) (rect-height rectB)))  ; Below
          (<= (+ (pos-y (rect-pos rectA)) (rect-height rectA)) (pos-y (rect-pos rectB)))) ; Above
      #f
      #t))

(define (collission-lane rect lane)
  (ormap (lambda (b) b) (map (lambda (x) (collission-rects rect x)) (lane-vehicles lane))))

; TODO - Collission Logic and World Update function
; TODO - Win condition check
(define (game-loop)
  (frog-lanes-interaction)
  (move-lanes-tick)
  (when (eq? frog-stuck 1) (set! game-frog (move-frog-log game-frog)))
  (send frogger-canvas on-paint)
  (sleep/yield 0.02)
  (game-loop))

(game-loop)
