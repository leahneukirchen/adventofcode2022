#lang racket

(define (scan1 f i l)
  (if (null? l)
    l
    (let ((v (f i (car l))))
      (cons v (scan1 f v (cdr l))))))

(define xs
  (cons 1 (scan1 + 1
            (append-map (lambda (insn)
                          (match (string-split insn)
                            [(list "noop") (list 0)]
                            [(list "addx" sn) (list 0 (string->number sn))]))
                        (file->lines "day10")))))

(for/sum ([i (in-inclusive-range 20 220 40)])
  (* i (list-ref xs (- i 1))))
; 14160

(let loop ([xs (take xs (* 6 40))])
  (if (null? xs)
    (void)
    (let-values ([(line rest) (split-at xs 40)])
      (displayln (list->string
                  (map (lambda (a b)
                         (if (<= (abs (- a b)) 1) #\@ #\space))
                       line
                       (range 0 40))))
      (loop rest))))
; RJERPEFC
