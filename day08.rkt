#lang racket
(require math/array)

(define data (list*->array
              (for/list ([line (file->lines "day08")])
                (for/list ([digit (string->list line)])
                  (- (char->integer digit) (char->integer #\0))))
              number?))

;(pretty-print data)

(define visible (array->mutable-array (array-map (lambda _ 0) data)))

(define height (- (vector-ref (array-shape data) 0) 1))
(define width (- (vector-ref (array-shape data) 1) 1))

(define (visit x0 dx xmax y0 dy ymax)
  (let ((highest -1))
    (for [(x (in-inclusive-range x0 xmax dx))
          (y (in-inclusive-range y0 ymax dy))]
      (let ((v (array-ref data (vector x y))))
        (when (< highest v)
          (set! highest v)
          (array-set! visible (vector x y) 1))))))
                                
(for* [(y (in-inclusive-range 0 height))]
  (visit 0 1 width y 0 (add1 y))
  (visit width -1 0 y 0 (add1 y)))
(for* [(x (in-inclusive-range 0 width))]
  (visit x 0 (add1 x) 0 1 height)
  (visit x 0 (add1 x) height -1 0))

(array-all-sum visible)
; 1849

(define (score index)
  (let ([h (array-ref data index)]
        [x (vector-ref index 1)]
        [y (vector-ref index 0)]
        [right 0]
        [down 0]
        [left 0]
        [up 0])
    (for ([wx (in-inclusive-range (+ x 1) width 1)]
          #:final (>= (array-ref data (vector y wx)) h))
      (set! right (+ 1 right)))
    (for ([wx (in-inclusive-range (- x 1) 0 -1)]
          #:final (>= (array-ref data (vector y wx)) h))
      (set! left (+ 1 left)))
    (for ([wy (in-inclusive-range (+ y 1) height 1)]
          #:final (>= (array-ref data (vector wy x)) h))
      (set! down (+ 1 down)))
    (for ([wy (in-inclusive-range (- y 1) 0 -1)]
          #:final (>= (array-ref data (vector wy x)) h))
      (set! up (+ 1 up)))
    (* up right down left)))
     
(define views (array-map score (indexes-array (array-shape data))))

(array-all-max views)
; 201600
