#lang racket

(require data/heap)

(define field
  (for/list ([line (file->lines "day24")])
    (for/list ([char line])
      char)))

(define width (length (first field)))
(define height (length field))

(define period (lcm (- width 2) (- height 2)))

(define pos (mutable-set))

(for ([(line y) (in-indexed field)])
  (for ([(c x) (in-indexed line)])
    (match c
      [#\# (for ([z (in-range 0 period)])
             (set-add! pos (vector x y z)))]
      [#\< (for ([z (in-range 0 period)])
             (set-add! pos (vector (+ 1 (modulo (- x 1 z) (- width 2))) y z)))]
      [#\> (for ([z (in-range 0 period)])
             (set-add! pos (vector (+ 1 (modulo (- x 1 (- z)) (- width 2))) y z)))]

      [#\^ (for ([z (in-range 0 period)])
             (set-add! pos (vector x (+ 1 (modulo (- y 1 z) (- height 2))) z)))]
      [#\v (for ([z (in-range 0 period)])
             (set-add! pos (vector x (+ 1 (modulo (- y 1 (- z)) (- height 2))) z)))]
      [#\. (void)])))

(define source (index-of (first field) #\.))
(define target (index-of (last field) #\.))

;; close behind source and target
(for ([z (in-range 0 period)])
  (set-add! pos (vector source -1 z))
  (set-add! pos (vector target (length field) z)))

(define (first<=? a b)
  (<= (first a) (first b)))

(define (distance pos source target period)
  (define distances (make-hash))
  (define queue (make-heap first<=?))

  (hash-set! distances source 0)
  (heap-add! queue (list 0 source))

  (let loop ()
    (if (zero? (heap-count queue))
      #f
      (let ([item (heap-min queue)])
        (heap-remove-min! queue)
        (if (equal? (vector-take (second item) 2) target)
          (first item)
          (let ([ndist (+ 1 (first item))]
                [z (modulo (+ (vector-ref (second item) 2) 1) period)])
            (for ([x (list -1 1  0 0 0)]
                  [y (list  0 0 -1 1 0)])
              (let ([neighbor (vector (+ x (vector-ref (second item) 0))
                                      (+ y (vector-ref (second item) 1))
                                      z)])
                (unless (set-member? pos neighbor)
                  (let [(dist (hash-ref distances neighbor 999999))]
                    (when (> dist ndist)
                      (hash-set! distances neighbor ndist)
                      (heap-add! queue (list ndist neighbor)))))))
            (loop)))))))

(let* ([a (distance pos (vector source 0 0) (vector target (- height 1)) period)]
       [b (distance pos (vector target (- height 1) a) (vector source 0) period)]
       [c (distance pos (vector source 0 (+ a b)) (vector target (- height 1)) period)])
  (values a                             ;; 332
          (+ a b c)))                   ;; 942
