;; The first three lines of this file were inserted by DrRacket. They record metadata
;; about the language level of this file in a form that our tools can easily process.
#reader(lib "htdp-intermediate-reader.ss" "lang")((modname Snake) (read-case-sensitive #t) (teachpacks ()) (htdp-settings #(#t constructor repeating-decimal #f #t none #f () #f)))
(require 2htdp/image)
(require 2htdp/universe)



;snake is one of:
; - '()
; - (cons snakeP Snake)
(define snake (list '()))


;Structure SnakePart represents the individual part of the snake
;SnakePart consists of coordinates(x y), direction
(define-struct SnakePart [x y])
(make-SnakePart  20 20)


;Structure Apple represents the apple, if snake touches apple it grows by one Snake-part
;Apple consists of coordinates(x y),
(define-struct Apple [x y])
(define AApple (make-Apple  20 20))

;The size of one game block
(define block-size 20)

;Structure WorldState represents the current state of the game
;It consists of score, state

;Score is the amount of points player has gained
;Score is a Number >= 0

;dir represents the direction of individual snake part
;dir is one of:
; - Up
; - Right
; - Down
; - Left

;status represents the current state of the game
;status is one of:
; - Start-screen
; - Game
; - Game-over
(define-struct WorldState [snake dir score apple status])
(make-WorldState (cons (make-SnakePart 5 5) (cons (make-SnakePart 30 4) '())) "Right" 900 (make-Apple 60 60) "Game")



;WS -> Image
;Will draw everything for the game
(define (ws-draw WS)
  (cond
    [(string=? (WorldState-status WS) "Start-screen")
     (place-image (text "Press Space Bar to START" 42 "orange") 400 400 (rectangle 800 600 "solid" "white"))]
    [(string=? (WorldState-status WS) "Game")
     (place-image (draw-score WS) 400 20                                               ;Score
                  (place-image (rectangle (- block-size 2) (- block-size 2) "solid" "red") (Apple-x (WorldState-apple WS)) (Apple-y (WorldState-apple WS))   ;Apple
                  (draw-snake (WorldState-snake WS))))]))                               ;Snake



;Snake -> Image
;Will draw every element of the snake
(define (draw-snake snake)
  (cond [(empty? snake) (rectangle 800 600 "solid" "white")]
        [(cons? snake)
          (place-image (rectangle (- block-size 2) (- block-size 2) "solid" "green") (SnakePart-x (first snake)) (SnakePart-y (first snake))
                 (draw-snake (rest snake)))]))


;WorldState -> Image
;Will draw the player's score
(define (draw-score WS)
  (text (number->string (WorldState-score WS)) 42 "orange"))


;WorldState -> WorldState
;Will change the WorldState every tick
(define (tick-handler WS)
  (cond
   [(string=? (WorldState-status WS) "Start-screen")
    (make-WorldState (WorldState-snake WS) (WorldState-dir WS) (WorldState-score WS) (WorldState-apple WS) (WorldState-status WS))]
   [(string=? (WorldState-status WS) "Game")
    (cond
      [(string=? (WorldState-dir WS) "Right")(make-WorldState (moveRight (WorldState-snake WS)) (WorldState-dir WS) (WorldState-score WS) (WorldState-apple WS) (WorldState-status WS))]
      [(string=? (WorldState-dir WS) "Left")(make-WorldState (moveLeft (WorldState-snake WS)) (WorldState-dir WS) (WorldState-score WS) (WorldState-apple WS) (WorldState-status WS))]
      [(string=? (WorldState-dir WS) "Down")(make-WorldState (moveDown (WorldState-snake WS)) (WorldState-dir WS) (WorldState-score WS) (WorldState-apple WS) (WorldState-status WS))]
      [(string=? (WorldState-dir WS) "Up")(make-WorldState (moveUp (WorldState-snake WS)) (WorldState-dir WS) (WorldState-score WS) (WorldState-apple WS) (WorldState-status WS))])
    ]))


;snake -> WorldState
(define (moveRight snake)
     (move snake (cons (make-SnakePart (+ (SnakePart-x (first snake)) block-size) (SnakePart-y (first snake))) '())))
;snake -> WorldState
(define (moveLeft snake)
     (move snake (cons (make-SnakePart (- (SnakePart-x (first snake)) block-size) (SnakePart-y (first snake))) '())))
;snake -> WorldState
(define (moveDown snake)
     (move snake (cons (make-SnakePart (SnakePart-x (first snake)) (+ (SnakePart-y (first snake)) block-size)) '())))
;snake -> WorldState
(define (moveUp snake)
     (move snake (cons (make-SnakePart (SnakePart-x (first snake)) (- (SnakePart-y (first snake)) block-size)) '())))

;snake -> WorldState
(define (move snake snake-copy)
  (cond
    [(= (length snake) 1) (reverse snake-copy)]
    [(cons? snake)
     (move (rest snake) (append (cons (make-SnakePart (SnakePart-x (first snake)) (SnakePart-y (first snake))) '()) snake-copy))]))

;WorldState -> WorldState
;Will check what keys are being pressed
(define (key-handler WS key)
  (cond
    [(string=? (WorldState-status WS) "Start-screen")
     (if (key=? key " ") (make-WorldState (WorldState-snake WS) (WorldState-dir WS) (WorldState-score WS) (WorldState-apple WS) "Game")
                         (make-WorldState (WorldState-snake WS) (WorldState-dir WS) (WorldState-score WS) (WorldState-apple WS) (WorldState-status WS)))]
    [(string=? (WorldState-status WS) "Game")
         (cond [(or (key=? key "up") (key=? key "w"))(cond
                                                       [(= (- (SnakePart-y (first (WorldState-snake WS))) block-size) (SnakePart-y (second (WorldState-snake WS)))) WS]
                                                       [else (make-WorldState (WorldState-snake WS) "Up" (WorldState-score WS) (WorldState-apple WS) (WorldState-status WS))])]
               [(or (key=? key "down") (key=? key "s")) (cond
                                                          [(= (+ (SnakePart-y (first (WorldState-snake WS))) block-size) (SnakePart-y (second (WorldState-snake WS)))) WS]
                                                          [else (make-WorldState (WorldState-snake WS) "Down" (WorldState-score WS) (WorldState-apple WS) (WorldState-status WS))])]
               [(or (key=? key "left") (key=? key "a")) (cond
                                                          [(= (- (SnakePart-x (first (WorldState-snake WS))) block-size) (SnakePart-x (second (WorldState-snake WS)))) WS]
                                                          [else (make-WorldState (WorldState-snake WS) "Left" (WorldState-score WS) (WorldState-apple WS) (WorldState-status WS))])]
               [(or (key=? key "right") (key=? key "d")) (cond
                                                           [(= (+ (SnakePart-x (first (WorldState-snake WS))) block-size) (SnakePart-x (second (WorldState-snake WS)))) WS]
                                                           [else (make-WorldState (WorldState-snake WS) "Right" (WorldState-score WS) (WorldState-apple WS) (WorldState-status WS))])]
               [else WS])]))



;Generates the new Apple
;snake -> Apple
(define (generate-new-apple snake)
  (check-if-apple-in-snake snake ( - (* 20 (random 39)) (/ block-size 2)) ( - (* 20 (random 29)) (/ block-size 2)))
 )
;Makes sure Apple doesn't generate on the snake itself
;snake, Number, Number -> Apple                                        
(define (check-if-apple-in-snake snake x y)
  (cond
    [(empty? snake) (make-Apple x y)]
    [(cons?  snake) (if (and (= x (SnakePart-x (first snake)))
                                            (= y (SnakePart-y (first snake))))
                                       (generate-new-apple snake)
                                       (check-if-apple-in-snake (rest snake) x y))]))



;The snake on the beggining of the game
(define starting-snake (cons (make-SnakePart 510 510)(cons (make-SnakePart 490 510)(cons (make-SnakePart 470 510) '()))))
;The main Handler
(big-bang (make-WorldState starting-snake  "Right" 0 (generate-new-apple starting-snake) "Start-screen")
  [on-tick tick-handler 0.1]
  [on-key key-handler]
  [to-draw ws-draw])

























