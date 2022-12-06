#lang racket

(let-values ([(stacks steps) (splitf-at (file->lines "day05")
                                        (lambda (s) (not (equal? s ""))))])

  (let* ([state (for/vector ([x (in-range 1 35 4)])
                   (dropf (for/list ([y (in-range 0 (sub1 (length stacks)))])
                            (string-ref (list-ref stacks y) x))
                          (lambda (c) (equal? c #\space))))]
         [steps (for/list ([line (cdr steps)])
                  (remove* '(#f) (map string->number (string-split line " "))))]
         [solve (lambda (state f)
                  (for ([step steps])
                    (let* ([n (first step)]
                           [from (sub1 (second step))]
                           [to (sub1 (third step))]

                           [taken (take (vector-ref state from) n)]
                           [left (drop (vector-ref state from) n)]
                           [placed (append (f taken) (vector-ref state to))])
                      (vector-set! state to placed)
                      (vector-set! state from left)))
                  (list->string (for/list ([s state])
                                  (first s))))])
    (displayln (solve (vector-copy state) reverse))
    (displayln (solve (vector-copy state) identity))
    ))
; MQTPGLLDN
; LVZPSTTCZ
