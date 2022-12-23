#lang racket

;; https://github.com/progheal/adventofcode/blob/master/2022/22.cpp

(define-values (field path)
  (let-values ([(field path) (splitf-at (file->lines "day22") (lambda (line) (not (equal? line ""))))])
    (let ([max-line (+ 1 (apply max (map string-length field)))])
      (values (for/vector ([line (append field (list ""))])
                (for/vector ([char (~a line #:min-width max-line)])
                  char))
              (for/list ([step (regexp-match* #rx"[0-9]+|." (second path))])
                (or (string->number step) step))))))

;; field
;; path

(define (vector-index-of v e)
  (let loop ([i 0])
    (if (>= i (vector-length v))
      #f
      (if (equal? (vector-ref v i) e)
        i
        (loop (+ i 1))))))

(define dir #(0 1 0 -1 0))

(define (do-step x y d)
  (values (modulo (+ x (vector-ref dir d))
                  (vector-length field))
          (modulo (+ y (vector-ref dir (+ d 1)))
                  (vector-length (vector-ref field 0)))
          d))

(define (turn-left d)
  (modulo (- d 1) 4))

(define (turn-right d)
  (modulo (+ d 1) 4))

(define (walk field path)
  (define d 0)
  (define x 0)
  (define y (vector-index-of (vector-ref field x) #\.))

  (for ([step path])
    (match step
      ["L" (set! d (turn-left d))]
      ["R" (set! d (turn-right d))]
      [n (let loop ([n n])
           (when (> n 0)
             (let ([px x]
                   [py y]
                   [pd d])
               (let loop2 ()
                 (set!-values (x y d) (do-step x y d))
                 (when (equal? (vector-ref (vector-ref field x) y) #\space)
                   (loop2)))
               (if (equal? (vector-ref (vector-ref field x) y) #\#)
                 (set!-values (x y d) (values px py pd))
                 (loop (- n 1))))))]))

  ; (list x y d)
  (+ (* 1000 (+ x 1)) (* 4 (+ y 1)) d)
)


(walk field path)
;; 13566


(define cube-edge (make-hash))

(define d 0)
(define x 0)
(define y (vector-index-of (vector-ref field x) #\.))

(define cube-size (gcd (- (vector-length field) 1)
                       (- (vector-length (vector-ref field 0)) 1)))

(displayln (list "cube size:" cube-size))

(let ([reverse false]
      [unmatched-edge '()]
      [unmatched-turn (list 1)]
      [ex x]
      [ey y]
      [ed d]
      [ignore 42]
      )
  (let loop ()
    (if reverse
      (for ([i (in-range 0 cube-size)])
        (let-values ([(rx ry rd) (apply values (car unmatched-edge))])
          (set! unmatched-edge (cdr unmatched-edge))
          (hash-set! cube-edge
                     (vector rx ry (turn-left rd))
                     (vector ex ey (turn-right ed)))
          (hash-set! cube-edge
                     (vector ex ey (turn-left ed))
                     (vector rx ry (turn-right rd)))
          (set!-values (ex ey ed) (do-step ex ey ed))))
      (for ([i (in-range 0 cube-size)])
        (set! unmatched-edge (cons (list ex ey ed) unmatched-edge))
        (set!-values (ex ey ed) (do-step ex ey ed))))
    (let*-values ([(now) (vector-ref (vector-ref field ex) ey)]
                  [(lx ly ld) (do-step ex ey (turn-left ed))]
                  [(myleft) (vector-ref (vector-ref field lx) ly)]
                  [(edge-turn) -1])
      (cond [(and (equal? myleft #\space) (equal? now #\space))
             (set! edge-turn 1)]
            [(and (equal? myleft #\space) (not (equal? now #\space)))
             (set! edge-turn 0)])
      (if reverse
        (let ([last-unmatched-turn (car unmatched-turn)])
          (if (= 1 (+ last-unmatched-turn edge-turn))
            (begin
              (set! unmatched-turn (cdr unmatched-turn))
              (when (null? unmatched-turn)
                (set! unmatched-turn (cons edge-turn unmatched-turn))
                (set! reverse #f)))
            (begin
              (set! unmatched-turn (cons 0 (cdr unmatched-turn)))
              (set! reverse #f))))
        (if (= edge-turn -1)
          (set! reverse #t)
          (set! unmatched-turn (cons edge-turn unmatched-turn))))

      (cond [(= edge-turn 1)
             (set! ed (turn-right ed))
             (set!-values (ex ey ignore) (do-step ex ey (turn-right ed)))]
            [(= edge-turn -1)
             (set! ed (turn-left ed))
             (set!-values (ex ey ignore) (do-step ex ey ed))]))

    (unless (and (equal? ex x) (equal? ey y) (equal? ed d))
      (loop)))
  )


;; cube-edge

(define (do-cube-step x y d)
  (if (hash-has-key? cube-edge (vector x y d))
    (vector->values (hash-ref cube-edge (vector x y d)))
    (do-step x y d)))

(define (walk2 field path)
  (define d 0)
  (define x 0)
  (define y (vector-index-of (vector-ref field x) #\.))

  (for ([step path])
    (match step
      ["L" (set! d (modulo (- d 1) 4))]
      ["R" (set! d (modulo (+ d 1) 4))]
      [n (let loop ([n n])
           (when (> n 0)
             (let ([px x]
                   [py y]
                   [pd d])
               (set!-values (x y d) (do-cube-step x y d))
               (if (equal? (vector-ref (vector-ref field x) y) #\#)
                 (set!-values (x y d) (values px py pd))
                 (loop (- n 1))))))]))

  ; (list x y d)
  (+ (* 1000 (+ x 1)) (* 4 (+ y 1)) d)
)

(walk2 field path)
;; 11451
