; POINTS & DISTANCE FILE

(define distance-const 10) ; Distance added when player moves
; (define point-const 10) ; NOT USED!!
(define init 0) ; Counter initial value

(define status (make-hash)) ; Create hash table

(define (create-hash) ; Create hash table
  (define points 0) ; Define points as 0
  (define distance-travelled 0) ; Define distance travelled as 0
  
  ; Set points in status to points (0)
  (hash-set! status "Points" points)
  
  ; Set distance in status to distance travelled
  (hash-set! status "Distance Travelled" distance-travelled))

(define (add-points) ; Add points to distance travelled
  (set! init (add1 init))
  ; (distance ((0.9^init) x 100))
  (hash-set! status "Points" (+ (hash-ref status "Distance Travelled") (round (* (expt 0.9 init) 100)))))

(define (add-distance) ; Add fixed value to distance travelled
  (hash-set! status "Distance Travelled" (+ (hash-ref status "Distance Travelled") distance-const)))

(define (display-status) ; Display current status to user
  (printf "[ - - CURRENT STATUS - - ]\n") ; Heading
  (printf "- Points: \t\t~a\n" (hash-ref status "Points"))
  (printf "- Distance Travelled: \t~a\n" (hash-ref status "Distance Travelled")))

(create-hash) ; Create hash (to be called at the beginning of loop)