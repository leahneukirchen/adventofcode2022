#lang racket

(define-values (stacks raw-steps)
  (let ((lines (file->lines "day05")))
    (split-at lines (index-of lines ""))))

(define initial-state
  (for/vector ([x (in-range 1 35 4)])
    (dropf (for/list ([stack (drop-right stacks 1)])
             (string-ref stack x))
           (curry equal? #\space))))

(define steps
  (for/list ([line (cdr raw-steps)])
    (filter number? (map string->number (string-split line " ")))))

(define (solve f)
  (define state (vector-copy initial-state))
  (for ([step steps])
    (let* ([n (first step)]
           [from (sub1 (second step))]
           [to (sub1 (third step))]

           [taken (take (vector-ref state from) n)]
           [left (drop (vector-ref state from) n)]
           [placed (append (f taken) (vector-ref state to))])
      (vector-set! state to placed)
      (vector-set! state from left)))
  (list->string (for/list ([s state]) (first s))))

(solve reverse)
(solve identity)
; MQTPGLLDN
; LVZPSTTCZ
