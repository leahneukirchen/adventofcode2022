#lang racket

(define area (build-vector 800 (lambda _ (make-vector 800 0))))

(define (area-get x y)
  (vector-ref (vector-ref area y) x))

(define (area-set! x y v)
  (vector-set! (vector-ref area y) x v))

(define/match (draw from to)
  [((list x y1) (list x y2))
   (for ([y (in-inclusive-range (min y1 y2) (max y1 y2))])
     (area-set! x y 9))]
  [((list x1 y) (list x2 y))
   (for ([x (in-inclusive-range (min x1 x2) (max x1 x2))])
     (area-set! x y 9))])

(for ([line (file->lines "day14")])
  (let ([coords (for/list ([coord (string-split line " -> ")])
                  (map string->number (string-split coord ",")))])
    (for ([from coords]
          [to (cdr coords)])
      (draw from to))))

(define (drop-sand x y)
  (if (or (>= y 799) (>= x 799))
    #f
    (case (area-get x (+ y 1))
      [(0)   (drop-sand x (+ y 1))]
      [(1 9) (case (area-get (- x 1) (+ y 1))
               [(0)   (drop-sand (- x 1) (+ y 1))]
               [(1 9) (case (area-get (+ x 1) (+ y 1))
                        [(0)   (drop-sand (+ x 1) (+ y 1))]
                        [(1 9) (area-set! x y 1)])])])))

(let loop ([i 0])
  (if (drop-sand 500 0)
    (loop (+ 1 i))
    i))
;; 665

;; clear sand
(void
 (vector-map! (lambda (line) (vector-map! (lambda (x) (if (= x 1) 0 x)) line)) area))

(void 
 (for/and ([i (in-inclusive-range 599 0 -1)])
   (when (for/or ([item (vector-ref area i)])
           (= item 9))
     (draw (list 0 (+ i 2))
           (list 799 (+ i 2)))
     #f)))

(let loop ([i 0])
  (if (= (area-get 500 0) 1)
    i
    (begin
      (drop-sand 500 0)
      (loop (+ 1 i)))))
;; 25434

;; (for ([line area]) (displayln line))
