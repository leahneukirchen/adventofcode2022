#lang racket
(require racket/set)

(define (scan f i l)
  (if (null? l)
    l
    (let ((v (f i (car l))))
      (cons v (scan f v (cdr l))))))

(define/match (move-after head tail)
  [((list hx hy) (list tx ty))
   (if (and (<= (abs (- hx tx)) 1)
            (<= (abs (- hy ty)) 1))
     tail
     (list (+ tx (sgn (- hx tx)))
           (+ ty (sgn (- hy ty)))))])

(for/list ([rope-length '(1 9)])
  (let ([visited (mutable-set)]
        [head '(0 0)]
        [tail (make-list rope-length '(0 0))])

    (set-add! visited (last tail))
    (for ([line (file->lines "day09")])
      (let* ([parts (string-split line " ")]
             [dir (string->symbol (first parts))]
             [n (string->number (second parts))])
        (for ([_ (in-range n)])
          (set! head (map + head (case dir
                                   [(L) '(-1 0)]
                                   [(R) '(1 0)]
                                   [(U) '(0 1)]
                                   [(D) '(0 -1)])))
          (set! tail (scan move-after head tail))
          (set-add! visited (last tail)))))
    (set-count visited)))
; 6494
; 2691
