#lang racket

(define/match (translate move)
  [((or "A" "X")) 'rock]
  [((or "B" "Y")) 'paper]
  [((or "C" "Z")) 'scissors])

(define/match (win-score p1 p2) ; for p2
  [(draw draw) 3]          
  [('rock 'paper) 6]
  [('paper 'rock) 0]
  [('rock 'scissors) 0]
  [('scissors 'rock) 6]
  [('rock 'paper) 0]
  [('paper 'scissors) 6]
  [('scissors 'paper) 0])

(define/match (shape-score p)
  [('rock) 1]
  [('paper) 2]
  [('scissors) 3])

(define (score p1 p2)
  (+ (shape-score p2) (win-score p1 p2)))

(for/sum ([line (file->lines "day02")])
  (apply score (map translate (string-split line))))
;; 10595

(define/match (correct-move p1 p2)
  [(p1        "Y") (list p1 p1)]        ; draw
  [('rock     "X") (list p1 'scissors)] ; lose
  [('paper    "X") (list p1 'rock)]     ; lose
  [('scissors "X") (list p1 'paper)]    ; lose
  [('rock     "Z") (list p1 'paper)]    ; win
  [('paper    "Z") (list p1 'scissors)] ; win
  [('scissors "Z") (list p1 'rock)]     ; win
  )

(for/sum ([line (file->lines "day02")])
  (match-let ([(list p1 p2) (string-split line)])
    (apply score (correct-move (translate p1) p2))))
;; 9541
