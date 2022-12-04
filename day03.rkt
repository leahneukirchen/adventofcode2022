#lang racket
(require srfi/1)

(define (priority c)
  (cond [(char<=? #\a c #\z) (- (char->integer c) (char->integer #\a) -1)]
        [(char<=? #\A c #\Z) (- (char->integer c) (char->integer #\A) -1 -26)]))

(for/sum ([line (file->lines "day03")])
  (let* ([len (string-length line)]
         [mid (/ len 2)]
         [p1 (substring line 0 mid)]
         [p2 (substring line mid)]
         [dup (car (lset-intersection eq?
                                      (string->list p1)
                                      (string->list p2)))])
    (priority dup)))
;; 8243

(let loop ([lines (file->lines "day03")]
           [sum 0])
  (if (null? lines)
    sum
    (let*-values ([(group rest) (split-at lines 3)]
                  [(badge) (car (apply lset-intersection eq?
                                       (map string->list group)))])
      (loop rest (+ sum (priority badge))))))
;; 2631
