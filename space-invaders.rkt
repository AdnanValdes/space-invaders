;; The first three lines of this file were inserted by DrRacket. They record metadata
;; about the language level of this file in a form that our tools can easily process.
#reader(lib "htdp-beginner-abbr-reader.ss" "lang")((modname space-invaders) (read-case-sensitive #t) (teachpacks ()) (htdp-settings #(#t constructor repeating-decimal #f #t none #f () #f)))
(require 2htdp/universe)
(require 2htdp/image)

;; Space Invaders


;; Constants:
;; =================

(define WIDTH  300)
(define HEIGHT 500)

(define INVADER-X-SPEED 1.5)  ;speeds (not velocities) in pixels per tick
(define INVADER-Y-SPEED 1.5)
(define TANK-SPEED 2)
(define MISSILE-SPEED 10)

(define HIT-RANGE 10)

(define INVADE-RATE 2)

(define BACKGROUND (empty-scene WIDTH HEIGHT))

(define INVADER
  (overlay/xy (ellipse 10 15 "outline" "blue")              ;cockpit cover
              -5 6
              (ellipse 20 10 "solid"   "blue")))            ;saucer

(define TANK
  (overlay/xy (overlay (ellipse 28 8 "solid" "black")       ;tread center
                       (ellipse 30 10 "solid" "green"))     ;tread outline
              5 -14
              (above (rectangle 5 10 "solid" "black")       ;gun
                     (rectangle 20 10 "solid" "black"))))   ;main body

(define TANK-HEIGHT/2 (/ (image-height TANK) 2))
(define TANK-Y (- HEIGHT TANK-HEIGHT/2))

(define MISSILE (ellipse 5 15 "solid" "red"))



;; Data Definitions:
;; =================

(define-struct game (invaders missiles tank))
;; Game is (make-game  (listof Invader) (listof Missile) Tank)
;; interp. the current state of a space invaders game
;;         with the current invaders, missiles and tank position

;; Game constants defined below Missile data definition

#;
(define (fn-for-game s)
  (... (fn-for-loinvader (game-invaders s))
       (fn-for-lom (game-missiles s))
       (fn-for-tank (game-tank s))))



(define-struct tank (x dir))
;; Tank is (make-tank Number Integer[-1, 1])
;; interp. the tank location is x, HEIGHT - TANK-HEIGHT/2 in screen coordinates
;;         the tank moves TANK-SPEED pixels per clock tick left if dir -1, right if dir 1

(define T0 (make-tank (/ WIDTH 2) 1))   ;center going right
(define T1 (make-tank 50 1))            ;going right
(define T2 (make-tank 50 -1))           ;going left

#;
(define (fn-for-tank t)
  (... (tank-x t) (tank-dir t)))



(define-struct invader (x y dir))
;; Invader is (make-invader Number Number Integer[-1,1])
;; interp. the invader is at (x, y) in screen coordinates
;;         the invader moves INVADER-X-SPEED pixels per clock tick left if dir -1, right if dir 1
;;         the invader moves INVADER-X-SPEED pixels per clock tick down

(define I1 (make-invader 150 100 1))           ;not landed, moving right
(define I2 (make-invader 150 HEIGHT -1))       ;exactly landed, moving left
(define I3 (make-invader 150 (+ HEIGHT 10) 1)) ;> landed, moving right


#;
(define (fn-for-invader invader)
  (... (invader-x invader) (invader-y invader) (invader-dir invader)))


(define-struct missile (x y))
;; Missile is (make-missile Number Number)
;; interp. the missile's location is x y in screen coordinates

(define M1 (make-missile 150 300))                       ;not hit U1
(define M2 (make-missile (invader-x I1) (+ (invader-y I1) 10)))  ;exactly hit U1
(define M3 (make-missile (invader-x I1) (+ (invader-y I1)  5)))  ;> hit U1
(define M4 (make-missile 100 TANK-HEIGHT/2))
(define M5 (make-missile 300 HEIGHT))


#;
(define (fn-for-missile m)
  (... (missile-x m) (missile-y m)))



(define G0 (make-game empty empty T0))
(define G1 (make-game empty empty T1))
(define G2 (make-game (list I1) (list M1) T1))
(define G3 (make-game (list I1 I2) (list M1 M2) T1))


;; ListOfInvader is one of:
;; - empty
;; - (cons invader ListOfInvader)
;; interp. a list of all invaders in play
(define LOI0 empty)
(define LOI1 (cons I1 empty))
(define LOI3 (list I1 I2 I3))

#;
(define (fn-for-loi loi)
  (cond [(empty? loi) (...)]
        [else
         (fn-for-invader (first loi)
                         (fn-for-loi (rest loi)))]))

;; Rules used:
;; - one of: 2 cases
;; - atomic distinct: false
;; - self-reference: (rest loi) is ListOfInvader
;; - compound: (list invader ListOfInvader)
;; - reference: (first loi) is invader


;; ListOfMisile is one of:
;; - empty
;; - (cons missile ListOfMissile)
;; interp. a list of all missiles in play

(define LOM0 empty)
(define LOM1 (list M1 M2 M3))

#;
(define (fn-for-lom lom)
  (cond [(empty? lom) (...)]
        [else
         (fn-for-missile (first lom)
                         (fn-for-lom (rest lom)))]))

;; Rules used:
;; - one of: 2 cases
;; - atomic distinct: false
;; - self-reference: (rest loi) is ListOfMissile
;; - compound: (list missile ListOfMissile)
;; - reference: (first loi) is missile


;; Functions:
;; =================

;; ======================================================
;; Main
;; ======================================================

;; Game -> Game
;; start the world with (main G0).

(define (main g)
  (big-bang g                  ; Game
    (on-tick    update-game) ; Game -> Game
    (on-key     handle-key)    ; Game KeyEvent -> Game
    (to-draw    render)        ; Game -> Image
    ))   

;; ======================================================
;; Update Game
;; ======================================================

;; Game -> Game
;; advance alien, missile, and tank positions by changing ListOfInvaders and ListOfMissiles

#;
(define (update-game s) 0)

;; Template from game
(define (update-game s)
  (make-game (kill-invaders (game-missiles s) (advance-invaders (spawn-invaders (game-invaders s))))
             (advance-missiles (game-missiles s))
             (move-tank (game-tank s))))

;; ======================================================
;; Advance Invaders
;; ======================================================

;; ListOfInvaders -> ListOfInvaders
;; advance every invader in ListOfInvaders along x axis by INVADER-X-SPEED and along y axis by INVADER-Y-SPEED
(check-expect (advance-invaders empty) empty)
(check-expect (advance-invaders (list I1)) (list
                                            (make-invader (+ (invader-x I1) INVADER-X-SPEED) (+ (invader-y I1) INVADER-Y-SPEED) 1)))

(check-expect (advance-invaders (list I1 (make-invader 150 200 -1))) (list
                                                                      (make-invader (+ (invader-x I1) INVADER-X-SPEED) (+ (invader-y I1) INVADER-Y-SPEED) 1)
                                                                      (make-invader (- 150 INVADER-X-SPEED) (+ 200 INVADER-Y-SPEED) -1)))
                            
(check-expect (advance-invaders (list I1 I3)) (list
                                                  (make-invader (+ (invader-x I1) INVADER-X-SPEED) (+ (invader-y I1) INVADER-Y-SPEED) (invader-dir I1))
                                                  (make-invader (+ (invader-x I3) INVADER-X-SPEED) (+ (invader-y I3) INVADER-Y-SPEED) (invader-dir I3))))
#;
(define (advance-invaders i) LOI0)

;; Template from ListOfInvaders
(define (advance-invaders loi)
  (cond [(empty? loi) empty]
        [else
         (cons (move-invader (first loi))
                         (advance-invaders (rest loi)))]))

;; ======================================================
;; Spawn Invaders
;; ======================================================
;; ListOfInvaders -> ListOfInvaders
;; add invaders to ListOfInvaders randomly at y coordinate 0 and x position random between [0,WIDTH]

#;
(define (spawn-invaders loi) loi)

(define (spawn-invaders loi)
  (cond [(< (random 200) INVADE-RATE)
         (cons (make-invader (random WIDTH) 0 (spawn-direction 1)) loi)]
        [else loi]))


;; ======================================================
;; Spawn Direction
;; ======================================================
;; Integer -> Integer
;; produce -1 if (random INVADE-RATE) is odd, else 1

#;
(define (spawn-direction i) 1)

(define (spawn-direction i)
  (if (odd? (random INVADE-RATE))
        (- i)
        i))

;; ======================================================
;; Move Invader
;; ======================================================
;; Invader -> Invader
;; move a single invader by (-)INVADER-X-SPEED along x axis and INVADER-Y-SPEED along y axis
(check-expect (move-invader I1) (make-invader (+ (invader-x I1) INVADER-X-SPEED) (+ (invader-y I1) INVADER-Y-SPEED) 1))
(check-expect (move-invader (make-invader 150 200 -1)) (make-invader (- 150 INVADER-X-SPEED) (+ 200 INVADER-Y-SPEED) -1))

#;
(define (move-invader i) i)

(define (move-invader invader)
  (if (> 0 (invader-dir invader))     
      (make-invader
       (+ (invader-x invader) (- INVADER-X-SPEED)) (+ (invader-y invader) INVADER-Y-SPEED)  (* (rotate-invader? invader) (invader-dir invader)))
      (make-invader 
       (+ (invader-x invader)    INVADER-X-SPEED)  (+ (invader-y invader) INVADER-Y-SPEED)  (* (rotate-invader? invader) (invader-dir invader)))))


;; ======================================================
;; Rotate Invader
;; ======================================================
;; Invader -> Bool
;; rotate invader direction if (invader-x)  < 0 or > WIDTH
(check-expect (rotate-invader? (make-invader 150 200 -1)) 1)
(check-expect (rotate-invader? (make-invader 230 345 -1)) 1)
(check-expect (rotate-invader? (make-invader 301 100 1)) -1)
(check-expect (rotate-invader? (make-invader -1 5 -1))   -1)

#;
(define (rotate-invader? i) false)

(define (rotate-invader? invader)
    (if (or (< (invader-x invader) 0)
        (> (invader-x invader) WIDTH))
        -1
        1))

;; ======================================================
;; Advance Missiles
;; ======================================================

;; ListOfMissiles -> ListOfMissiles
;; advance every missile in ListOfMissiles by subtracting MISSILE-SPEED from (missile-y)
(check-expect (advance-missiles empty) empty)
(check-expect (advance-missiles (list M1)) (list (make-missile (missile-x M1) (- (missile-y M1) MISSILE-SPEED))))
(check-expect (advance-missiles (list M1 M4 M5)) (list (make-missile (missile-x M1) (- (missile-y M1) MISSILE-SPEED))
                                                       (make-missile (missile-x M4) (- (missile-y M4) MISSILE-SPEED)) 
                                                       (make-missile (missile-x M5) (- (missile-y M5) MISSILE-SPEED))))

(check-expect (advance-missiles (list (make-missile 100 -1))) empty)
(check-expect (advance-missiles (list (make-missile 3 -1) (make-missile 45 200))) (list (make-missile 45 (- 200 MISSILE-SPEED))))
#;
(define (advance-missiles m) LOM0)


(define (advance-missiles lom)
  (cond [(empty? lom) empty]
        [else
          (if (< (missile-y (first lom)) 0)
            (advance-missiles (rest lom))
         (cons (move-missile (first lom))
                         (advance-missiles (rest lom))))]))

;; Missile -> Missile
;; advance a single missile by MISSILE-SPEED

#;
(define (move-missile m) M1)


(define (move-missile m)
  (make-missile (missile-x m) (- (missile-y m) MISSILE-SPEED)))


;; ======================================================
;; Kill Invaders
;; ======================================================

;; ListOfMissiles ListOfInvaders -> ListOfInvaders
;; given a list of missiles and a list of invaders, remove invaders and missiles that collide
(check-expect (kill-invaders empty empty) empty)
(check-expect (kill-invaders empty LOI3) LOI3)
(check-expect (kill-invaders LOM1 empty) empty)
(check-expect (kill-invaders (list (make-missile 150 100)) (list (make-invader 150 100 1))) empty) ; Exact hit on single invader

#;
(define (kill-invaders loi lom) loi)

;; CROSS PRODUCT OF TYPE COMMENTS TABLE
;;
;;                                     loi
;;                           empty           (cons invader LOI)                
;;                                        |
;; l   empty                              |  LOI
;; o                         --  empty  -----------------
;; m  (cons missile LOM)                  |  (and <invader at (x,y)?>
;;                                        |       <missile at (x,y)?>)

(define (kill-invaders lom loi)
  (cond [(empty? loi) empty]
        [(empty? lom) loi]
        [else
         (if (hit-invader? (first loi) lom)
            (rest loi)
             (cons (first loi) (kill-invaders lom (rest loi))))]))


;; Invader ListOfMissile -> Bool
;; produce true if Invader is within hit box of a missile
(check-expect (hit-invader? (make-invader 150 100 1) (list (make-missile 150 100))) true)
(check-expect (hit-invader? I1 (list M1 M4 M5)) false)

#;
(define (hit-invader? i lom) false)

(define (hit-invader? invader lom)
  (cond [(empty? lom) false] 
        [else
          (if (and (< (- (invader-x invader) HIT-RANGE) (missile-x (first lom)) (+ (invader-x invader) HIT-RANGE))
                   (< (- (invader-y invader) HIT-RANGE) (missile-y (first lom)) (+ (invader-y invader) HIT-RANGE)))
                true
                (hit-invader? invader (rest lom)))]))



;; Tank -> Tank
;; update tank position in box [0,WIDTH] l(eft) or r(ight) by TANK-SPEED based on direction (tank-dir)
(check-expect (move-tank T0) (make-tank (+ (tank-x T0) TANK-SPEED) (tank-dir T0)))
(check-expect (move-tank T2) (make-tank (- (tank-x T2) TANK-SPEED) (tank-dir T2)))

(check-expect (move-tank (make-tank -1 -1)) (make-tank 0 -1))
(check-expect (move-tank (make-tank (+ WIDTH 1) 1)) (make-tank WIDTH 1))

#;
(define (move-tank t) T0)


(define (move-tank t)
  (cond [(> (tank-x t) WIDTH) (make-tank WIDTH 1)]
        [(< (tank-x t) 0) (make-tank 0 -1)]
        [else
          (make-tank (+ (tank-x t) (* (tank-dir t) TANK-SPEED)) (tank-dir t))]))


;; Game KeyEvent -> Game
;; handle key events. Left and right arrow keys move tank in box [0,WIDTH]. Spacebar is used to fire missile.
;; !!!

(define (handle-key s ke)
  (cond [(key=? ke " ") (make-game (game-invaders s) (fire-missile (game-missiles s) (game-tank s)) (game-tank s))]
        [(key=? ke "right") (make-game (game-invaders s) (game-missiles s) (go-right (game-tank s)))]
        [(key=? ke "left") (make-game (game-invaders s) (game-missiles s) (go-left (game-tank s)))]
        [else s]))

;; ListOfMissiles Tank -> ListOfMissiles
;; fire a missile from Tank-x position
(check-expect (fire-missile empty T0) (list (make-missile (tank-x T0) TANK-Y)))
(check-expect (fire-missile LOM1 T1) (cons (make-missile (tank-x T1) TANK-Y) LOM1))

#;
(define (fire-missile lom t) LOM1)

(define (fire-missile lom t)
  (cond [(empty? lom) (list (make-missile (tank-x t) TANK-Y))]
        [else
         (cons (make-missile (tank-x t) TANK-Y) lom)]))

;; Tank -> Tank
;; set tank direction to 1 (moving right)
(check-expect (go-right (make-tank 10  1)) (make-tank 10 1))
(check-expect (go-right (make-tank 50 -1)) (make-tank 50 1))

#;
(define (go-right t) t)

(define (go-right t)
  (make-tank (tank-x t) 1))

;; Tank -> Tank
;; set tank direction to -1 (moving left)
(check-expect (go-left (make-tank 10 -1)) (make-tank 10 -1))
(check-expect (go-left (make-tank 50  1)) (make-tank 50 -1))

#;
(define (go-left t) t)

(define (go-left t)
  (make-tank (tank-x t) -1))

;; Game -> Image
;; render current game state by producing next alien, missile, and tank positions
(check-expect (render G0) (place-image TANK (tank-x (game-tank G0)) TANK-Y BACKGROUND))
(check-expect (render G1) (place-image TANK (tank-x (game-tank G1)) TANK-Y BACKGROUND))
(check-expect (render G2) (place-image TANK (tank-x (game-tank G2)) TANK-Y 
                                       (place-image MISSILE (missile-x (first (game-missiles G2))) (missile-y (first (game-missiles G2))) 
                                                    (place-image INVADER (invader-x (first (game-invaders G2))) (invader-y (first (game-invaders G2))) BACKGROUND))))
(check-expect (render G3) (place-image TANK (tank-x (game-tank G3)) TANK-Y 
                                       (place-image MISSILE (missile-x (first (game-missiles G3))) (missile-y (first (game-missiles G3)))
                                                    (place-image MISSILE (missile-x (first (rest (game-missiles G3)))) (missile-y (first (rest (game-missiles G3))))
                                                                 (place-image INVADER (invader-x (first (game-invaders G3))) (invader-y (first (game-invaders G3))) 
                                                                              (place-image INVADER (invader-x (first (rest (game-invaders G3)))) (invader-y (first (rest (game-invaders G3)))) BACKGROUND))))))
#;
(define (render game) BACKGROUND)

(define (render s)
  (render-missiles (game-missiles s)
                    (render-invaders (game-invaders s)
                                    (render-tank (game-tank s)))))


;; ListOfInvaders -> Image
;; Render every invader in ListOfInvader
#;
(define (render-invaders loi img) INVADER)

(define (render-invaders loi img)
  (cond [(empty? loi) img]
        [else
         (place-image INVADER
                      (invader-x (first loi))
                      (invader-y (first loi))
                        (render-invaders (rest loi) img))]))

;; ListOfMissiles -> Image
;; Render every missile in ListOfMissiles
#;
(define (render-missiles lom img) MISSILE)

(define (render-missiles lom img)
  (cond [(empty? lom) img]
        [else
         (place-image MISSILE
                      (missile-x (first lom))
                      (missile-y (first lom))
                         (render-missiles (rest lom) img))]))
;; Tank -> Image
;; Render tank at position x

#;
(define (render-tank t) TANK)

(define (render-tank t)
  (place-image TANK
               (tank-x t)
               TANK-Y
               BACKGROUND))
