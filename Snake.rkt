;; The first three lines of this file were inserted by DrRacket. They record metadata
;; about the language level of this file in a form that our tools can easily process.
#reader(lib "htdp-advanced-reader.ss" "lang")((modname Snake) (read-case-sensitive #t) (teachpacks ()) (htdp-settings #(#t constructor repeating-decimal #t #t none #f () #f)))
(require 2htdp/image)
(require 2htdp/universe)

(define close-game #false)

;snake is one of:
; - '()
; - (cons SnakePart snake)
#;(define snake (cons (make-SnakePart 510 510)(cons (make-SnakePart 490 510)(cons (make-SnakePart 470 510) '()))))

;buttons is one of:
; - '()
; - (cons Button buttons)

(define regime 1)

;Structure SnakePart represents the individual part of the snake
;SnakePart consists of it's coordinates(x y)
(define-struct SnakePart [x y])
#;(make-SnakePart  20 20)


;Structure Apple represents the apple, if snake touches apple it grows by one Snake-part
;Apple consists of  it's coordinates(x y)
(define-struct Apple [x y])
#;(define AApple (make-Apple  20 20))

;Structure Button represents the buttons
;Button consists of it's coordinates(x y) 
;text(String) - what this button does
;is-pressed(boolean) - if this button is about to be pressed
(define-struct Button [x y text is-pressed])
#;(define button 60 50 "Start the game" false)

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

;tick is the tick counter
;tick is natural number

;Regime represents the game mode
; 0 - Normal
; 1 - Easy
(define-struct WorldState [snake dir score apple status tick LKUT buttons regime])
;(make-WorldState (cons (make-SnakePart 5 5) (cons (make-SnakePart 30 4) '())) "Right" 900 (make-Apple 60 60) "Game" 0 -1)


;WS -> Image
;Will draw everything for the game
(define (ws-draw WS)
  (cond
    [(string=? (WorldState-status WS) "Start-screen")
     (draw-buttons (WorldState-buttons WS))]
    [(string=? (WorldState-status WS) "Game-over")
     (place-image (text "Game over Press R to RESTART" 42 "blue") 400 300 ;Message
     (place-image (text "You have gained" 42 "orange") 400 100 ;Message 
     (place-image (draw-score WS) 400 170      ;Score
     (rectangle 800 600 "solid" "white"))))]   ;Arena
    [(string=? (WorldState-status WS) "Game")
     (place-image (rectangle 681 501 "outline" "black") 400 330              ;Scene borders
     (place-image (draw-score WS) 400 20                                     ;Score
     (place-image (rectangle (- block-size 2) (- block-size 2) "solid" "red") (Apple-x (WorldState-apple WS)) (Apple-y (WorldState-apple WS))   ;Apple
     (draw-snake-head (WorldState-snake WS)))))]))                               ;Snake


;snake -> Image
;Will draw snake head
(define (draw-snake-head snake)
  (place-image (rectangle (- block-size 2) (- block-size 2) "solid" "forest green") (SnakePart-x (first snake)) (SnakePart-y (first snake))
               (draw-snake-body (rest snake))))
;snake -> Image
;Will draw snake body
(define (draw-snake-body snake)
  (cond [(empty? snake) (rectangle 800 600 "solid" "white")]
        [(cons? snake)
          (place-image (rectangle (- block-size 2) (- block-size 2) "solid" "green") (SnakePart-x (first snake)) (SnakePart-y (first snake))
                 (draw-snake-body (rest snake)))]))

;buttons -> Image
;Will draw every Button of the button's list
(define (draw-buttons buttons)
  (cond [(empty? buttons) (rectangle 800 600 "solid" "white")]
        [(cons? buttons)
          (place-image (text  (Button-text (first buttons)) 32 (determine-button-color (first buttons))) (Button-x (first buttons)) (Button-y (first buttons))
                 (draw-buttons (rest buttons)))]))

;Determines the color of the button
;Button -> Color
(define (determine-button-color button)
  (if (Button-is-pressed button) "yellow"
                                 "green"))
;WorldState -> Image
;Will draw the player's score
(define (draw-score WS)
  (text (number->string (WorldState-score WS)) 42 "orange"))


(define (tick-counter WS)
  (make-WorldState (WorldState-snake WS) (WorldState-dir WS) (WorldState-score WS) (WorldState-apple WS) (WorldState-status WS) (+ (WorldState-tick WS) 1) (WorldState-LKUT WS) (WorldState-buttons WS) (WorldState-regime WS)))
(define (rest-snake WS)
  (make-WorldState (rest (WorldState-snake WS)) (WorldState-dir WS) (WorldState-score WS) (WorldState-apple WS) (WorldState-status WS) (WorldState-tick WS) (WorldState-LKUT WS) (WorldState-buttons WS) (WorldState-regime WS)))
(define (reverse-snake WS snake)
  (make-WorldState (reverse snake) (WorldState-dir WS) (WorldState-score WS) (WorldState-apple WS) (WorldState-status WS) (WorldState-tick WS) (WorldState-LKUT WS) (WorldState-buttons WS) (WorldState-regime WS)))
(define (make-dir WS new-dir)
  ((make-WorldState (WorldState-snake WS) new-dir (WorldState-score WS) (WorldState-apple WS) (WorldState-status WS) (WorldState-tick WS) (WorldState-tick WS) (WorldState-buttons WS) (WorldState-regime WS))))



;WorldState -> WorldState
;Will change the WorldState every tick
(define (tick-handler WS)
  (cond
   [(string=? (WorldState-status WS) "Start-screen")(tick-counter WS)]
   [(string=? (WorldState-status WS) "Game-over")(tick-counter WS)]
   [(string=? (WorldState-status WS) "Game")
    (move (tick-counter WS) '() False)
    ]))

;(define-struct SnakeCopy [copy apple-state])

;Determines if the Apple was eaten
;SnakePart, Apple -> Boolean
(define (is-apple-eaten? snake-head apple)
  (and (= (Apple-x apple) (SnakePart-x snake-head)) (= (Apple-y apple) (SnakePart-y snake-head))))

;Checks the current status of the game
;WorldState, SnakePart, Boolean -> String
(define (status? WS snake-head apple-state)
  (cond
    [(= (length (WorldState-snake WS)) (cond [apple-state 0] [else 1])) (WorldState-status WS)]
    [(or (or (< (SnakePart-x snake-head) 70) (> (SnakePart-x snake-head) 730)) (or (< (SnakePart-y snake-head) 90) (> (SnakePart-y snake-head) 570))) "Game-over"]
    [(and (= (SnakePart-x (first (WorldState-snake WS))) (SnakePart-x snake-head)) (= (SnakePart-y (first (WorldState-snake WS))) (SnakePart-y snake-head))) "Game-over"]
    [(cons? (WorldState-snake WS))(status? (rest-snake WS) snake-head apple-state)]))

;Determines the player's score
;Number, Boolean -> Number
(define (score-update score apple-state)
  (cond
    [apple-state (+ score 100)]
    [else score]))


;Will move the snake every tick
;WorldState, snake, Boolean -> WorldState
(define (move WS snake-copy apple-state)
  (cond
    [(empty? snake-copy) (local(
                                (define new-snake-head (cond [(= (WorldState-regime WS) 0)(cond
                                                                      [(string=? (WorldState-dir WS) "Right")
                                                                       (make-SnakePart (+ (SnakePart-x (first (WorldState-snake WS))) block-size) (SnakePart-y (first (WorldState-snake WS))))]
                                                                      [(string=? (WorldState-dir WS) "Left")
                                                                       (make-SnakePart (- (SnakePart-x (first (WorldState-snake WS))) block-size) (SnakePart-y (first (WorldState-snake WS))))]
                                                                      [(string=? (WorldState-dir WS) "Down")
                                                                       (make-SnakePart (SnakePart-x (first (WorldState-snake WS))) (+ (SnakePart-y (first (WorldState-snake WS))) block-size))]
                                                                      [(string=? (WorldState-dir WS) "Up")
                                                                       (make-SnakePart (SnakePart-x (first (WorldState-snake WS))) (- (SnakePart-y (first (WorldState-snake WS))) block-size))])]
                                                             [(= (WorldState-regime WS) 1)(cond
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
                                (define apple-state (is-apple-eaten? new-snake-head apple))
                                (define new-apple (generate-apple snake apple apple-state))
                                (define new-status (status? WS new-snake-head apple-state))
                                (define new-score (score-update (WorldState-score WS) apple-state))
                                (define new-snake (cons new-snake-head '())))
                           (move (make-WorldState snake (WorldState-dir WS) new-score new-apple new-status (WorldState-tick WS) (WorldState-LKUT WS) (WorldState-buttons WS) (WorldState-regime WS)) new-snake apple-state)
                               
                          )]
    [(= (length (WorldState-snake WS)) (cond [apple-state 0] [else 1])) (reverse-snake WS snake-copy)]
    [(cons? (WorldState-snake WS))
     (move (rest-snake WS) (append (cons (make-SnakePart (SnakePart-x (first (WorldState-snake WS))) (SnakePart-y (first (WorldState-snake WS)))) '()) snake-copy) apple-state)])
  )

;WorldState -> Boolean
(define (horizontal? WS)
  (or (string=? (WorldState-dir WS) "Right") (string=? (WorldState-dir WS) "Left")))

;WorldState -> Boolean
(define (vertical? WS)
  (or (string=? (WorldState-dir WS) "Up") (string=? (WorldState-dir WS) "Down")))


;Will check what keys are being pressed
;WorldState -> WorldState
(define (key-handler WS key)
  (if (not (= (WorldState-tick WS) (WorldState-LKUT WS)))
      (cond
        [(string=? (WorldState-status WS) "Start-screen")
         (cond [(key=? key " ") (check-what-button-is-pressed  (WorldState-buttons WS)  WS)]
               [(or (key=? key "down") (key=? key "s")) (make-WorldState (WorldState-snake WS) (WorldState-dir WS) (WorldState-score WS) (WorldState-apple WS) "Start-screen" (WorldState-tick WS) (WorldState-tick WS) (change-button (WorldState-buttons WS)) (WorldState-regime WS))]
               [(or (key=? key "up") (key=? key "w")) (make-WorldState (WorldState-snake WS) (WorldState-dir WS) (WorldState-score WS) (WorldState-apple WS) "Start-screen" (WorldState-tick WS) (WorldState-tick WS) (change-button (reverse (WorldState-buttons WS))) (WorldState-regime WS))]
               [else WS])]
        [(string=? (WorldState-status WS) "Game-over")
         (if (key=? key "r") (make-WorldState starting-snake "Right" 0 (generate-apple starting-snake (make-Apple 0 0) true) "Game" (WorldState-tick WS) (WorldState-tick WS) (WorldState-buttons WS) (WorldState-regime WS))
             WS)]
        [(string=? (WorldState-status WS) "Game")
         (cond [(or (key=? key "up") (key=? key "w"))(if (horizontal? WS)
                                                         (make-WorldState (WorldState-snake WS) "Up" (WorldState-score WS) (WorldState-apple WS) (WorldState-status WS) (WorldState-tick WS) (WorldState-tick WS) (WorldState-buttons WS) (WorldState-regime WS))
                                                         WS)]
               [(or (key=? key "down") (key=? key "s"))(if (horizontal? WS)
                                                           (make-WorldState (WorldState-snake WS) "Down" (WorldState-score WS) (WorldState-apple WS) (WorldState-status WS) (WorldState-tick WS) (WorldState-tick WS) (WorldState-buttons WS) (WorldState-regime WS))
                                                           WS)]
               [(or (key=? key "left") (key=? key "a")) (if (vertical? WS)
                                                            (make-WorldState (WorldState-snake WS) "Left" (WorldState-score WS) (WorldState-apple WS) (WorldState-status WS) (WorldState-tick WS) (WorldState-tick WS) (WorldState-buttons WS) (WorldState-regime WS))
                                                            WS)]
               [(or (key=? key "right") (key=? key "d")) (if (vertical? WS)
                                                             (make-WorldState (WorldState-snake WS) "Right" (WorldState-score WS) (WorldState-apple WS) (WorldState-status WS) (WorldState-tick WS) (WorldState-tick WS) (WorldState-buttons WS) (WorldState-regime WS))
                                                             WS)]
               [else WS])])
      WS))

;Will check what button is being pressed
;buttons, WorldState -> WorldState
(define (check-what-button-is-pressed buttons WS)
  (cond
    [(empty? buttons) WS]
    [(cons? buttons)(if (Button-is-pressed (first buttons)) (what-this-button-does (first buttons) WS)
                                           (check-what-button-is-pressed (rest buttons) WS))]))

;Will check what this button does
;button, WorldState -> WorldState
(define (what-this-button-does button WS)
  (cond
    [(string=? (Button-text button) "Start the game") (make-WorldState (WorldState-snake WS) (WorldState-dir WS) (WorldState-score WS) (WorldState-apple WS) "Start-screen" (WorldState-tick WS) (WorldState-tick WS) Choose-mode-buttons (WorldState-regime WS))]
    [(string=? (Button-text button) "Leave") (make-WorldState (WorldState-snake WS) (WorldState-dir WS) (WorldState-score WS) (WorldState-apple WS) "Start-screen" (WorldState-tick WS) (WorldState-tick WS) (WorldState-buttons WS) (WorldState-regime WS))]
    [(string=? (Button-text button) "Easy") (make-WorldState (WorldState-snake WS) (WorldState-dir WS) (WorldState-score WS) (WorldState-apple WS) "Game" (WorldState-tick WS) (WorldState-tick WS) (WorldState-buttons WS) 1)]
    [(string=? (Button-text button) "Normal") (make-WorldState (WorldState-snake WS) (WorldState-dir WS) (WorldState-score WS) (WorldState-apple WS) "Game" (WorldState-tick WS) (WorldState-tick WS) (WorldState-buttons WS) 0)]
    [else WS]
    ))

;Will change what button is being pressed
;buttons -> buttons
(define (change-button buttons)
  (cond
    [(empty? (rest buttons)) '()]
    [(cons? buttons) (cond [(Button-is-pressed (first (reverse buttons)))
                           (cons (make-Button (Button-x (first buttons)) (Button-y (first buttons)) (Button-text (first buttons)) true) (finisher (rest buttons)))]
                           [(Button-is-pressed (first buttons))
                           (cons (make-Button (Button-x (first buttons)) (Button-y (first buttons)) (Button-text (first buttons)) false)
                                 (if (empty? (rest (rest buttons))) (cons (make-Button (Button-x (second buttons)) (Button-y (second buttons)) (Button-text (second buttons)) true) '())
                                 (cons (make-Button (Button-x (second buttons)) (Button-y (second buttons)) (Button-text (second buttons)) true) (finisher2 (rest buttons)))))]
                           [else (cons (first buttons) (change-button (rest buttons)))])]))
;buttons -> buttons
(define (finisher buttons)
  (cond
    [(empty? (rest buttons)) (cons (make-Button (Button-x (first buttons)) (Button-y (first buttons)) (Button-text (first buttons)) false) '())]
    [(cons? buttons) (cons (first buttons) (finisher (rest buttons)))]))

;buttons -> buttons
(define (finisher2 buttons)
  (cond
    [(empty? buttons) '()]
    [(empty? (rest buttons)) (cons (make-Button (Button-x (first buttons)) (Button-y (first buttons)) (Button-text (first buttons)) false) '())]
    [(cons? buttons) (cons (first buttons) (finisher2 (rest buttons)))]))
;Generates the new Apple
;snake, Apple, Boolean -> Apple
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


;The Start screen buttons
(define Start-screen-buttons (cons (make-Button 400 100 "Start the game" true)(cons (make-Button 400 200 "Leave" false) '())))
;The Choose mode buttons
(define Choose-mode-buttons (cons (make-Button 400 100 "Easy" true)(cons (make-Button 400 200 "Normal" false) '())))
;The snake on the beggining of the game
(define starting-snake (cons (make-SnakePart 510 510)(cons (make-SnakePart 490 510)(cons (make-SnakePart 470 510) '()))))
;The main Handler
(big-bang (make-WorldState starting-snake  "Right" 0 (generate-apple starting-snake (make-Apple 730 90) true) "Start-screen" 0 -1 Start-screen-buttons 1)
  [on-tick tick-handler 0.15]
  [on-key key-handler]
  [to-draw ws-draw])

























