;; The first three lines of this file were inserted by DrRacket. They record metadata
;; about the language level of this file in a form that our tools can easily process.
#reader(lib "htdp-intermediate-lambda-reader.ss" "lang")((modname Snake) (read-case-sensitive #t) (teachpacks ((lib "image.rkt" "teachpack" "2htdp") (lib "universe.rkt" "teachpack" "2htdp"))) (htdp-settings #(#t constructor repeating-decimal #f #t none #f ((lib "image.rkt" "teachpack" "2htdp") (lib "universe.rkt" "teachpack" "2htdp")) #f)))
(require 2htdp/image)
(require 2htdp/universe)



;snake is one of:
; - '()
; - (cons snakeP Snake)
(define snake (list '()))

(define regime 1)

;Structure SnakePart represents the individual part of the snake
;SnakePart consists of coordinates(x y), direction
(define-struct SnakePart [x y])
;(make-SnakePart  20 20)


;Structure Apple represents the apple, if snake touches apple it grows by one Snake-part
;Apple consists of coordinates(x y),
(define-struct Apple [x y])
;(define AApple (make-Apple  20 20))

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
(define-struct WorldState [snake dir score apple status tick LKUT])
;(make-WorldState (cons (make-SnakePart 5 5) (cons (make-SnakePart 30 4) '())) "Right" 900 (make-Apple 60 60) "Game" 0 -1)



;WS -> Image
;Will draw everything for the game
(define (ws-draw WS)
  (cond
    [(string=? (WorldState-status WS) "Start-screen")
     (place-image (text "Press Space Bar to START" 42 "orange") 400 200 (rectangle 800 600 "solid" "white"))]
    [(string=? (WorldState-status WS) "Game-over")
     (place-image (text "Game over Press R to RESTART" 42 "orange") 400 200 (rectangle 800 600 "solid" "white"))]
    [(string=? (WorldState-status WS) "Game")
     (place-image (draw-score WS) 400 20                                               ;Score
                  (place-image (rectangle (- block-size 2) (- block-size 2) "solid" "red") (Apple-x (WorldState-apple WS)) (Apple-y (WorldState-apple WS))   ;Apple
                  (draw-snake (WorldState-snake WS))))]))                               ;Snake



;Snake -> Image
;Will draw every element of the snake
(define (draw-snake snake)
  (cond [(empty? snake) (place-image (rectangle 681 501 "outline" "black") 400 330 (rectangle 800 600 "solid" "white"))]
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
    (make-WorldState (WorldState-snake WS) (WorldState-dir WS) (WorldState-score WS) (WorldState-apple WS) (WorldState-status WS) (+ (WorldState-tick WS) 1) (WorldState-LKUT WS))]
   [(string=? (WorldState-status WS) "Game-over")
    (make-WorldState (WorldState-snake WS) (WorldState-dir WS) (WorldState-score WS) (WorldState-apple WS) (WorldState-status WS) (+ (WorldState-tick WS) 1) (WorldState-LKUT WS))]
   [(string=? (WorldState-status WS) "Game")(move (make-WorldState (WorldState-snake WS) (WorldState-dir WS) (WorldState-score WS) (WorldState-apple WS) (WorldState-status WS) (+ (WorldState-tick WS) 1) (WorldState-LKUT WS)) '() False)
    ]))

;(define-struct SnakeCopy [copy apple-state])


;SnakePart Apple -> SnakeCopy
(define (is-apple-eaten? snake-head apple)
  (and (= (Apple-x apple) (SnakePart-x snake-head)) (= (Apple-y apple) (SnakePart-y snake-head))))

(define (status? WS snake-head apple-state)
  (cond
    [(= (length (WorldState-snake WS)) (cond [apple-state 0] [else 1])) (WorldState-status WS)]
    [(or (or (< (SnakePart-x snake-head) 70) (> (SnakePart-x snake-head) 730)) (or (< (SnakePart-y snake-head) 90) (> (SnakePart-y snake-head) 570))) "Game-over"]
    [(and (= (SnakePart-x (first (WorldState-snake WS))) (SnakePart-x snake-head)) (= (SnakePart-y (first (WorldState-snake WS))) (SnakePart-y snake-head))) "Game-over"]
    [(cons? (WorldState-snake WS))(status? (make-WorldState (rest (WorldState-snake WS)) (WorldState-dir WS) (WorldState-score WS) (WorldState-apple WS) (WorldState-status WS) (WorldState-tick WS) (WorldState-LKUT WS)) snake-head apple-state)]))

(define (score-update score apple-state)
  (cond
    [apple-state (+ score 100)]
    [else score]))

(define (move WS snake-copy apple-state)
  (cond
    [(empty? snake-copy) (local(
                                (define new-snake-head (cond [(= regime 1)(cond
                                                                      [(string=? (WorldState-dir WS) "Right")
                                                                       (make-SnakePart (+ (SnakePart-x (first (WorldState-snake WS))) block-size) (SnakePart-y (first (WorldState-snake WS))))]
                                                                      [(string=? (WorldState-dir WS) "Left")
                                                                       (make-SnakePart (- (SnakePart-x (first (WorldState-snake WS))) block-size) (SnakePart-y (first (WorldState-snake WS))))]
                                                                      [(string=? (WorldState-dir WS) "Down")
                                                                       (make-SnakePart (SnakePart-x (first (WorldState-snake WS))) (+ (SnakePart-y (first (WorldState-snake WS))) block-size))]
                                                                      [(string=? (WorldState-dir WS) "Up")
                                                                       (make-SnakePart (SnakePart-x (first (WorldState-snake WS))) (- (SnakePart-y (first (WorldState-snake WS))) block-size))])]
                                                             [(= regime 2)(cond
                                                                      [(string=? (WorldState-dir WS) "Right")
                                                                       (make-SnakePart (if (<= (+ (SnakePart-x (first (WorldState-snake WS))) block-size) 730) (+ (SnakePart-x (first (WorldState-snake WS))) block-size) 70) (SnakePart-y (first (WorldState-snake WS))))]
                                                                      [(string=? (WorldState-dir WS) "Left")
                                                                       (make-SnakePart (if (>= (- (SnakePart-x (first (WorldState-snake WS))) block-size) 70) (- (SnakePart-x (first (WorldState-snake WS))) block-size) 730) (SnakePart-y (first (WorldState-snake WS))))]
                                                                      [(string=? (WorldState-dir WS) "Down")
                                                                       (make-SnakePart (SnakePart-x (first (WorldState-snake WS))) (if (<= (+ (SnakePart-y (first (WorldState-snake WS))) block-size) 570) (+ (SnakePart-y (first (WorldState-snake WS))) block-size) 90))]
                                                                      [(string=? (WorldState-dir WS) "Up")
                                                                       (make-SnakePart (SnakePart-x (first (WorldState-snake WS))) (if (>= (- (SnakePart-y (first (WorldState-snake WS))) block-size) 90) (- (SnakePart-y (first (WorldState-snake WS))) block-size) 570))])]))
                                                        
                                (define snake (WorldState-snake WS))
                                (define apple (WorldState-apple WS))
                                (define apple-state (is-apple-eaten? new-snake-head apple)))
                           (move (make-WorldState snake (WorldState-dir WS) (score-update (WorldState-score WS) apple-state) (generate-apple snake apple apple-state) (status? WS new-snake-head apple-state) (WorldState-tick WS) (WorldState-LKUT WS)) (cons new-snake-head '()) apple-state)
                               
                          )]
    [(= (length (WorldState-snake WS)) ((lambda (apple-state) (cond [apple-state 0] [else 1])) apple-state)) (make-WorldState (reverse snake-copy) (WorldState-dir WS) (WorldState-score WS) (WorldState-apple WS) (WorldState-status WS) (WorldState-tick WS) (WorldState-LKUT WS))]
    [(cons? (WorldState-snake WS))
     (move (make-WorldState (rest (WorldState-snake WS)) (WorldState-dir WS) (WorldState-score WS) (WorldState-apple WS) (WorldState-status WS) (WorldState-tick WS) (WorldState-LKUT WS)) (append (cons (make-SnakePart (SnakePart-x (first (WorldState-snake WS))) (SnakePart-y (first (WorldState-snake WS)))) '()) snake-copy) apple-state)])
  )

;WorldState -> Boolian
(define (horizontal? WS)
  (or (string=? (WorldState-dir WS) "Right") (string=? (WorldState-dir WS) "Left")))

;WorldState -> Boolian
(define (vertical? WS)
  (or (string=? (WorldState-dir WS) "Up") (string=? (WorldState-dir WS) "Down")))


;WorldState -> WorldState
;Will check what keys are being pressed

(define (key-handler WS key)
  (if (not (= (WorldState-tick WS) (WorldState-LKUT WS)))
      (cond
        [(string=? (WorldState-status WS) "Start-screen")
         (if (key=? key " ") (make-WorldState (WorldState-snake WS) (WorldState-dir WS) (WorldState-score WS) (WorldState-apple WS) "Game" (WorldState-tick WS) (WorldState-tick WS))
             WS)]
        [(string=? (WorldState-status WS) "Game-over")
         (if (key=? key "r") (make-WorldState starting-snake "Right" 0 (generate-apple starting-snake (make-Apple 0 0) true) "Game" (WorldState-tick WS) (WorldState-tick WS))
             WS)]
        [(string=? (WorldState-status WS) "Game")
         (cond [(or (key=? key "up") (key=? key "w"))(if (horizontal? WS)
                                                         (make-WorldState (WorldState-snake WS) "Up" (WorldState-score WS) (WorldState-apple WS) (WorldState-status WS) (WorldState-tick WS) (WorldState-tick WS))
                                                         WS)]
               [(or (key=? key "down") (key=? key "s"))(if (horizontal? WS)
                                                           (make-WorldState (WorldState-snake WS) "Down" (WorldState-score WS) (WorldState-apple WS) (WorldState-status WS) (WorldState-tick WS) (WorldState-tick WS))
                                                           WS)]
               [(or (key=? key "left") (key=? key "a")) (if (vertical? WS)
                                                            (make-WorldState (WorldState-snake WS) "Left" (WorldState-score WS) (WorldState-apple WS) (WorldState-status WS) (WorldState-tick WS) (WorldState-tick WS))
                                                            WS)]
               [(or (key=? key "right") (key=? key "d")) (if (vertical? WS)
                                                             (make-WorldState (WorldState-snake WS) "Right" (WorldState-score WS) (WorldState-apple WS) (WorldState-status WS) (WorldState-tick WS) (WorldState-tick WS))
                                                             WS)]
               [else WS])])
      WS))


;Generates the new Apple
;snake -> Apple
(define (generate-apple snake apple apple-status)
  (cond [apple-status (check-if-apple-in-snake snake ( + (* block-size (random 33)) (/ block-size 2) 60) ( + (* block-size (random 24)) (/ block-size 2) 80))]
        [else apple])
 )
;Makes sure Apple doesn't generate on the snake itself
;snake, Number, Number -> Apple                                        
(define (check-if-apple-in-snake snake x y)
  (cond
    [(empty? snake) (make-Apple x y)]
    [(cons?  snake) (if (and (= x (SnakePart-x (first snake)))
                                            (= y (SnakePart-y (first snake))))
                                       (generate-apple snake (make-Apple x y) true)
                                       (check-if-apple-in-snake (rest snake) x y))]))



;The snake on the beggining of the game
(define starting-snake (cons (make-SnakePart 510 510)(cons (make-SnakePart 490 510)(cons (make-SnakePart 470 510) '()))))
;The main Handler
(big-bang (make-WorldState starting-snake  "Right" 0 (generate-apple starting-snake (make-Apple 730 90) true) "Start-screen" 0 -1)
  [on-tick tick-handler 0.15]
  [on-key key-handler]
  [to-draw ws-draw])

























