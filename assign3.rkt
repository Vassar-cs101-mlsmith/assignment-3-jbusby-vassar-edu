;; The first three lines of this file were inserted by DrRacket. They record metadata
;; about the language level of this file in a form that our tools can easily process.
#reader(lib "htdp-beginner-abbr-reader.ss" "lang")((modname assign3) (read-case-sensitive #t) (teachpacks ()) (htdp-settings #(#t constructor repeating-decimal #f #t none #f () #f)))
; CMPU-101 
; Fall 2018
; Assign 3
; Julia Busby
;
; Description: Uses a list of bouncing balls to animate many balls
; of different sizes and colors, all moving in the same scene at 
; different speeds.

(require 2htdp/image) 
(require 2htdp/universe)

(define RADIUS 25)

; Scene dimensions
(define WIDTH 500)
(define HEIGHT 300)

; Create the background scene image
(define BACKGROUND
  (place-image (rectangle WIDTH HEIGHT "solid" "lightgray")
               (/ WIDTH 2) (/ HEIGHT 2)
               (empty-scene WIDTH HEIGHT)))

; Data Definitions 
(define-struct ball (im x y dx dy))
; A ball is a (make-ball im p dx dy) where
; im is an image (of the ball), 
; x and y are numbers representing the ball's position, and
; dx and dy are numbers representing the ball's horizontal and 
;   vertical velocity

; Data Definition for a list-of-balls:
; A list-of-balls is either:
; 1. '(), or
; 2. (cons b lob), where b is a ball
;    and lob is a list-of-balls

;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;

; Four (4) example ball CONSTANTS:
;   one touching each edge of the scene (top, bottom, left, right)
(define BALL-AT-LEFT 
  (make-ball (circle (+ RADIUS 4) "solid" "teal")
             (+ RADIUS 4) (/ HEIGHT 2) -4 -4))

(define BALL-AT-RIGHT 
  (make-ball (circle (- RADIUS 3) "solid" "sea green")
             (- WIDTH (- RADIUS 3)) (/ HEIGHT 2) 5 5))

(define BALL-AT-TOP
  (make-ball (circle RADIUS "solid" "orchid")
             (/ WIDTH 2) (+ RADIUS 1) -6 6))

(define BALL-AT-BOTTOM
  (make-ball (circle (+ RADIUS 7) "solid" "slate blue")
             (/ WIDTH 2) (- HEIGHT (+ RADIUS 7)) -3 3))

; EXTRA BALL CONSTANTS

; Large crimson ball placed in bottom-left corner
(define BALL-EXTRA-1 
  (make-ball (circle (+ RADIUS 20) "solid" "crimson")
             (+ RADIUS 20) (- HEIGHT (+ RADIUS 20)) -1 1))

; Small dark orange ball placed in top-left corner
(define BALL-EXTRA-2
  (make-ball (circle (- RADIUS 15) "solid" "darkorange")
             (- RADIUS 15) (- RADIUS 15) -9 -8))

; Medium salmon ball placed in bottom-right corner
(define BALL-EXTRA-3
  (make-ball (circle (- RADIUS 2) "solid" "salmon")
             (- WIDTH (- RADIUS 2)) (- HEIGHT (- RADIUS 2)) 3 5))

; Medium gold ball placed in top-right corner
(define BALL-EXTRA-4
  (make-ball (circle (+ RADIUS 2) "solid" "gold")
             (- WIDTH (+ RADIUS 2)) (+ RADIUS 2) 7 -5))

; Medium spring green ball placed in left-center
(define BALL-EXTRA-5
  (make-ball (circle (+ RADIUS 6) "solid" "spring green")
             (/ WIDTH 4) (/ HEIGHT 2) -3 5))

; Large royal blue ball placed in right-center
(define BALL-EXTRA-6
  (make-ball (circle (+ RADIUS 15) "solid" "royalblue")
             (/ (* 3 WIDTH) 4) (/ HEIGHT 2) 6 -4))

; Medium indigo ball placed in center-top
(define BALL-EXTRA-7
  (make-ball (circle (+ RADIUS 3) "solid" "indigo")
             (/ WIDTH 2) (/ HEIGHT 4) 3 -3))

; Medium orange red ball placed in center-bottom
(define BALL-EXTRA-8
  (make-ball (circle (+ RADIUS 5) "solid" "orange red")
             (/ WIDTH 2) (/ (* 3 HEIGHT) 4) 7 -4))

; INIT-LOB is a list-of-balls used for the initial state of the world
; all balls in the scene must be in this list
(define INIT-LOB (list BALL-AT-LEFT
                       BALL-AT-RIGHT
                       BALL-AT-TOP
                       BALL-AT-BOTTOM
                       BALL-EXTRA-1
                       BALL-EXTRA-2
                       BALL-EXTRA-3
                       BALL-EXTRA-4
                       BALL-EXTRA-5
                       BALL-EXTRA-6
                       BALL-EXTRA-7
                       BALL-EXTRA-8)) 


;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;
; Templates for a ball and a list-of-balls.
;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;

; ball -> ???
; Template for a function that consumes a ball
;(define (fun-for-ball b) 
;  (...(ball-im b)...
;   ...(ball-x b)...(ball-y b)...
;   ...(ball-dx b)...(ball-dy b)...))

; list-of-balls -> ???
; Template for a function that consumes a list-of-balls
;(define (fun-for-list-of-balls lob) 
;  (cond
;    [(empty? lob)...] 
;    [cons? (...(fun-for-ball (first lob))...
;           ...(fun-for-lob (rest lob))...)]))


;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;
; Helper Functions
;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;

; ball -> number
; computes the radius of given ball
(define (ball-radius b) 
  (/ (image-height (ball-im b)) 2))

; check-expects for ball-radius
(check-expect (ball-radius
               (make-ball (circle 5 "outline" "purple") 24 52 1 2)) 5)
(check-expect (ball-radius
               (make-ball (circle 13 "solid" "orange") 51 12 2 1)) 13)


; ball -> boolean
; determines whether the ball reached the top edge of scene
(define (top-edge? b)
  (cond
    [(<= (- (ball-y b) (ball-radius b)) 0) #true]
    [else #false]))

; check-expects for top-edge?
(check-expect (top-edge?
               (make-ball (circle 5 "outline" "purple") 24 52 1 2)) #false)
(check-expect (top-edge?
               (make-ball (circle 13 "solid" "orange") 51 0 2 1)) #true)
(check-expect (top-edge?
               (make-ball (circle 10 "solid" "orange") 51 10 2 1)) #true)


; ball -> boolean
; determines whether the ball reached the bottom edge of scene
(define (bottom-edge? b)
  (cond
    [(>= (+ (ball-y b) (ball-radius b)) HEIGHT) #true]
    [else #false]))

; check-expects for bottom-edge?
(check-expect (bottom-edge?
               (make-ball (circle 5 "outline" "purple") 24 52 1 2)) #false)
(check-expect (bottom-edge?
               (make-ball (circle 13 "solid" "orange") 51 300 2 1)) #true)
(check-expect (bottom-edge?
               (make-ball (circle 10 "solid" "orange") 51 290 2 1)) #true)

; ball -> boolean
; determines whether the ball reached the left edge of scene
(define (left-edge? b)
  (cond
    [(<= (- (ball-x b) (ball-radius b)) 0) #true]
    [else #false]))

; check-expects for left-edge?
(check-expect (left-edge?
               (make-ball (circle 5 "outline" "purple") 24 52 1 2)) #false)
(check-expect (left-edge?
               (make-ball (circle 13 "solid" "orange") 0 52 2 1)) #true)
(check-expect (left-edge?
               (make-ball (circle 10 "solid" "orange") 10 75 2 1)) #true)

; ball -> boolean
; determines whether the ball reached the right edge of scene
(define (right-edge? b)
  (cond
    [(>= (+ (ball-x b) (ball-radius b)) WIDTH) #true]
    [else #false]))

; check-expects for right-edge?
(check-expect (right-edge?
               (make-ball (circle 5 "outline" "purple") 24 52 1 2)) #false)
(check-expect (right-edge?
               (make-ball (circle 13 "solid" "orange") 500 50 2 1)) #true)
(check-expect (right-edge?
               (make-ball (circle 10 "solid" "orange") 490 49 2 1)) #true)


; ball -> ball
; reverse ball's up-down direction   
(define (reverse-up-down b)
  (make-ball (ball-im b) (ball-x b) (ball-y b) (ball-dx b)
             (* (ball-dy b) -1)))

; check-expects for reverse-up-down
(check-expect (reverse-up-down
               (make-ball (circle 13 "solid" "orange") 51 17 2 1))
               (make-ball (circle 13 "solid" "orange") 51 17 2 -1))
(check-expect (reverse-up-down
               (make-ball (circle 13 "solid" "orange") 51 17 -2 -1))
               (make-ball (circle 13 "solid" "orange") 51 17 -2 1))

; ball -> ball
; reverse ball's left-right direction   
(define (reverse-left-right b)
  (make-ball (ball-im b) (ball-x b) (ball-y b)
             (* (ball-dx b) -1) (ball-dy b)))

; check-expects for reverse-left-right
(check-expect (reverse-left-right
               (make-ball (circle 13 "solid" "orange") 51 17 2 1))
               (make-ball (circle 13 "solid" "orange") 51 17 -2 1))
(check-expect (reverse-left-right
               (make-ball (circle 13 "solid" "orange") 51 17 -2 -1))
               (make-ball (circle 13 "solid" "orange") 51 17 2 -1))


; ball -> ball
; changes direction of given ball if it hits the top or bottom edge
(define (bounce-up-down b)
  (cond
    [(or (top-edge? b) (bottom-edge? b)) (reverse-up-down b)]
    [else b]))

; check-expects for bounce-up-down
(check-expect (bounce-up-down
               (make-ball (circle 13 "solid" "orange") 51 52 2 1))
               (make-ball (circle 13 "solid" "orange") 51 52 2 1))
(check-expect (bounce-up-down
               (make-ball (circle 10 "solid" "orange") 51 10 2 -1))
               (make-ball (circle 10 "solid" "orange") 51 10 2 1))
(check-expect (bounce-up-down
               (make-ball (circle 10 "solid" "orange") 51 290 2 1))
               (make-ball (circle 10 "solid" "orange") 51 290 2 -1))


; ball -> ball
; changes direction of given ball if it hit the left or right edge
(define (bounce-left-right b)
  (cond
    [(or (left-edge? b) (right-edge? b)) (reverse-left-right b)]
    [else b]))

; check-expects for bounce-left-right
(check-expect (bounce-left-right
               (make-ball (circle 13 "solid" "orange") 51 52 2 1))
               (make-ball (circle 13 "solid" "orange") 51 52 2 1))
(check-expect (bounce-left-right
               (make-ball (circle 10 "solid" "orange") 10 64 -2 1))
               (make-ball (circle 10 "solid" "orange") 10 64 2 1))
(check-expect (bounce-left-right
               (make-ball (circle 10 "solid" "orange") 490 64 2 1))
               (make-ball (circle 10 "solid" "orange") 490 64 -2 1))


;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;
; Event Handler Functions
;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;

; ball -> ball
; moves the given ball by its dx and dy amounts
; and checks to see if the ball bounces or not
(define (move-ball b)
  (make-ball (ball-im b)
             (+ (ball-x b) (ball-dx (bounce-left-right b)))
             (+ (ball-y b) (ball-dy (bounce-up-down b)))
             (ball-dx (bounce-left-right b))
             (ball-dy (bounce-up-down b))))

; check-expects for move-ball
(check-expect (move-ball
               (make-ball (circle 10 "solid" "orange") 56 64 8 8))
               (make-ball (circle 10 "solid" "orange") 64 72 8 8))
(check-expect (move-ball
               (make-ball (circle 10 "solid" "orange") 56 64 -8 8))
               (make-ball (circle 10 "solid" "orange") 48 72 -8 8))
(check-expect (move-ball
               (make-ball (circle 10 "solid" "orange") 56 64 8 -8))
               (make-ball (circle 10 "solid" "orange") 64 56 8 -8))
(check-expect (move-ball
               (make-ball (circle 10 "solid" "orange") 490 64 8 -8))
               (make-ball (circle 10 "solid" "orange") 482 56 -8 -8))
(check-expect (move-ball
               (make-ball (circle 10 "solid" "orange") 56 290 8 8))
               (make-ball (circle 10 "solid" "orange") 64 282 8 -8))


; list-of-balls -> list-of-balls
; moves (and possibly bounces) each ball in given list
(define (move-list-of-balls lob)
  (cond
    [(empty? lob) lob]
    [(cons? lob)
     (cons (move-ball (first lob))
           (move-list-of-balls (rest lob)))]))

; check-expect for move-list-of-balls
(check-expect (move-list-of-balls
              (list (make-ball (circle 10 "solid" "orange") 56 64 8 8)
                    (make-ball (circle 10 "solid" "orange") 56 64 -8 8)
                    (make-ball (circle 10 "solid" "orange") 56 64 8 -8)))
              (list (make-ball (circle 10 "solid" "orange") 64 72 8 8)
                    (make-ball (circle 10 "solid" "orange") 48 72 -8 8)
                    (make-ball (circle 10 "solid" "orange") 64 56 8 -8)))


; ball image -> image
; renders given ball b on given background bg
(define (render-ball b bg)
  (place-image (ball-im b) (ball-x b) (ball-y b) bg))
  
; list-of-balls -> image 
; produces image of each ball at each given current position on
; background.
(define (render-balls lob) 
  (cond [(empty? lob) BACKGROUND]
        [else (render-ball (first lob)
                           (render-balls (rest lob)))]))


;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;
; main and big-bang
;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;

; main function which contains big-bang function
(define (main w)
  (big-bang w
            (on-tick move-list-of-balls 1/28) 
            (to-draw render-balls)))
; Run program automatically, or type (main INIT-LOB) in Interactions Pane
; Use INIT-LOB as the initial state of the world


; Calls main function above
(main INIT-LOB)