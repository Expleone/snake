;; The first three lines of this file were inserted by DrRacket. They record metadata
;; about the language level of this file in a form that our tools can easily process.
#reader(lib "htdp-beginner-abbr-reader.ss" "lang")((modname g) (read-case-sensitive #t) (teachpacks ()) (htdp-settings #(#t constructor repeating-decimal #f #t none #f () #f)))
(require 2htdp/image)
(require 2htdp/universe)


(define (update ws)
  (not ws))


(define (render ws)
  (if ws
      (triangle 135 "solid" "red")
      (square 135 "solid" "blue")))


(big-bang #false
  [on-tick update]
  [on-draw render])