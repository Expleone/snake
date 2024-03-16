;; The first three lines of this file were inserted by DrRacket. They record metadata
;; about the language level of this file in a form that our tools can easily process.
#reader(lib "htdp-intermediate-reader.ss" "lang")((modname Snake) (read-case-sensitive #t) (teachpacks ()) (htdp-settings #(#t constructor repeating-decimal #f #t none #f () #f)))
(require 2htdp/image)
(require 2htdp/universe)



;Snake is one of:
; - '()
; - (cons Snake-part Snake)


;Structure Snake-part represents the individual part of the snake
;Snake-part consists of coordinates, direction
(define-struct Snake-part [x y direction])



;Structure Apple represents the apple, if snake touches apple it grows by one Snake-part
;Apple consists of coordinates,
(define-struct Apple [x y])



;Structure Game-State represents the current state of the game
;It consists of score, state
(define Game-State [score state])