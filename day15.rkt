#lang racket
;; ala https://github.com/narimiran/AdventOfCode2022/blob/main/clojure/day15.clj

(define data
  (for/list ([line (file->lines "day15")])
    (map string->number (regexp-match* #rx"[0-9]+" line))))

(define (dist x1 y1 x2 y2)
  (+ (abs (- x1 x2))
     (abs (- y1 y2))))

(define (seen-in-row row)
  (append-map (lambda (line)
                (let* ([sx (first line)]
                       [sy (second line)]
                       [r (dist sx sy (third line) (fourth line))]
                       [d (- r (abs (- row sy)))])
                  (if (positive? d)
                    (list (list (- sx d) (+ sx d)))
                    (list))))
                data))

(define (part1)
  (let* ([seen (seen-in-row 2000000)]
         [a (first (first (sort seen < #:key first)))]
         [b (second (first (sort seen > #:key second)))])
    (- b a)))

(part1)
;; 5147333

(define (find-a-hole seen)
  (let loop ([highest 0]
             [seen (sort seen < #:key first)])
    (if (null? seen)
      #f
      (if (<= (caar seen) (+ highest 1))
        (loop (max highest (cadar seen)) (rest seen))
        (- (caar seen) 1)))))

(define (part2)
  (let loop ([row 4000000])
    (let* ([seen (seen-in-row row)]
           [hole (find-a-hole seen)])
      (if hole
        (+ (* 4000000 hole) row)
        (loop (- row 1))))))

(part2)
;; 13734006908372
