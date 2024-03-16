;; The first three lines of this file were inserted by DrRacket. They record metadata
;; about the language level of this file in a form that our tools can easily process.
#reader(lib "htdp-intermediate-reader.ss" "lang")((modname g) (read-case-sensitive #t) (teachpacks ()) (htdp-settings #(#t constructor repeating-decimal #f #t none #f () #f)))
(require 2htdp/image)
(require 2htdp/universe)



(define (create-UFO-scene WS)
  (draw-snake (WorldState-snake WS))) 
 
;(define UFO
;  (underlay/align "center"
;                  "top"
  ;                (circle 10 "solid" "green"))
                  ;(rectangle 40 4 "solid" "green"));)
 (define-struct Apple [x y])
(make-Apple  20 20)


(define (draw-snake snake)
  (cond [(empty? snake) (rectangle 1000 1000 "solid" "white")]
        [(cons? snake)
          (place-image (rectangle 20 20 "solid" "green") (snakeP-x (first snake)) (snakeP-y (first snake))
                 (draw-snake (rest snake)))]))
  ;(rectangle (Apple-x Apple) (Apple-y Apple) "solid" "green"))


(define-struct snakeP [x y dir])
(define-struct WorldState [snake score])





(define (change-ws WS)
  (cond
    [(string=? (snakeP-dir (first (WorldState-snake WS))) "Right")(make-WorldState (cons (make-snakeP (+ 20 (snakeP-x (first (WorldState-snake WS)))) (snakeP-y (first (WorldState-snake WS))) (snakeP-dir (first (WorldState-snake WS)))) (cons (make-snakeP (+ 20 (snakeP-x (second (WorldState-snake WS)))) (snakeP-y (second (WorldState-snake WS))) (snakeP-dir (second (WorldState-snake WS)))) '())) (WorldState-score WS))]))
  
 
(big-bang (make-WorldState (cons (make-snakeP 500 500 "Right")(cons (make-snakeP 478 500 "Right") '())) 0)
          [on-tick change-ws 0.25]
          [to-draw create-UFO-scene])