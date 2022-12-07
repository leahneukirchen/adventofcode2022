#lang racket

(define data (car (file->lines "day06")))

(for/list ([n '(4 14)])
  (for/or ([i (in-range 0 (- (string-length data) n))])
    (and (= n (length (remove-duplicates
                       (string->list (substring data i (+ i n))))))
         (+ i n))))
; 1623
; 3774
