;; The first three lines of this file were inserted by DrRacket. They record metadata
;; about the language level of this file in a form that our tools can easily process.
#reader(lib "htdp-intermediate-reader.ss" "lang")((modname Snake) (read-case-sensitive #t) (teachpacks ()) (htdp-settings #(#t constructor repeating-decimal #f #t none #f () #f)))
(require 2htdp/image)
(require 2htdp/universe)



;snake is one of:
; - '()
; - (cons snakeP Snake)
(define snake (list '()))


;Structure snakeP represents the individual part of the snake
;snakeP consists of coordinates(x y), direction

;dir represents the direction of individual snake part
;dir is one of:
; - Up
; - Right
; - Down
; - Left
(define-struct snakeP [x y dir])
(make-snakeP  20 20 "Right")


;Structure Apple represents the apple, if snake touches apple it grows by one Snake-part
;Apple consists of coordinates(x y),
(define-struct Apple [x y])
(make-Apple  20 20)



;Structure WorldState represents the current state of the game
;It consists of score, state

;Score is the amount of points player has gained
;Score is a Number >= 0

;GameState represents the current state of the game
;State is one of:
; - Before-game
; - Game
; - Game-over
(define-struct WorldState [snake score])
(make-WorldState (cons (make-snakeP 5 5 "Right") (cons (make-snakeP 30 4 "Left") '())) 900)



;WS -> Image
;Will draw everything for the game
(define (ws-draw WS)
  (draw-snake (WorldState-snake WS))) 



;Snake -> Image
;Will draw every element of the snake
(define (draw-snake snake)
  (cond [(empty? snake) (rectangle 1000 1000 "solid" "white")]
        [(cons? snake)
          (place-image (rectangle 20 20 "solid" "green") (snakeP-x (first snake)) (snakeP-y (first snake))
                 (draw-snake (rest snake)))]))



;WorldState -> WorldState
;Will change the WorldState every tick
(define (change-ws WS)
  (cond
    [(string=? (snakeP-dir (first (WorldState-snake WS))) "Right")(make-WorldState (cons (make-snakeP (+ 20 (snakeP-x (first (WorldState-snake WS)))) (snakeP-y (first (WorldState-snake WS))) (snakeP-dir (first (WorldState-snake WS)))) (cons (make-snakeP (+ 20 (snakeP-x (second (WorldState-snake WS)))) (snakeP-y (second (WorldState-snake WS))) (snakeP-dir (second (WorldState-snake WS)))) '())) (WorldState-score WS))]))




;The main Handler
(big-bang (make-WorldState (cons (make-snakeP 500 500 "Right")(cons (make-snakeP 440 500 "Right") '())) 0)
          [on-tick change-ws 0.25]
          [to-draw ws-draw])
