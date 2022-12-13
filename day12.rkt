#lang racket

(require data/heap)

(struct item (score value) #:transparent)

(define (item<=? x y)
  (<= (item-score x) (item-score y)))

(define start #f)
(define end #f)
(define data (for/vector ([(line x) (in-indexed (file->lines "day12"))])
               (for/vector ([(char y) (in-indexed line)])
                 (case char
                   [(#\S) (set! start (vector x y)) 0]
                   [(#\E) (set! end (vector x y)) 26]
                   [else (- (char->integer char) (char->integer #\a) 0)]))))

(define (neighbors v)
  (for*/list ([x '(-1 0 1)]
              [y '(-1 0 1)]
              #:unless (= x y 0)
              #:when (zero? (* x y))
              #:when (< -1 (+ x (vector-ref v 0)) (vector-length data))
              #:when (< -1 (+ y (vector-ref v 1)) (vector-length
                                                   (vector-ref data 0))))
    (vector (+ x (vector-ref v 0))
            (+ y (vector-ref v 1)))))

(define (height v)
  (vector-ref (vector-ref data (vector-ref v 0)) (vector-ref v 1)))

(define (solve data start fin? comp)
  (define heap (make-heap item<=?))
  (define seen (mutable-set))

  (heap-add! heap (item 0 start))

  (let loop ()
    (if (zero? (heap-count heap))
      #f
      (let* ([step (heap-min heap)]
             [pos (item-value step)])
        (heap-remove-min! heap)
        (if (set-member? seen pos)
          (loop)
          (if (fin? pos)
              (item-score step)             ; done
              (begin
                (set-add! seen pos)
                (for ([n (neighbors pos)])
                  (when (comp (- (height n) (height pos)))
                    ;(displayln n)
                    (heap-add! heap (item (+ 1 (item-score step)) n))))
                (loop))))))))

(solve data start (curry equal? end) (curry >= 1))
;; 447

(solve data end (lambda (pos) (zero? (height pos))) (curry <= -1))
;; 446
