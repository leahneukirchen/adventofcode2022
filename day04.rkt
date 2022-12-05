#lang racket
(require (for-syntax syntax/for-body)
         syntax/parse/define)

(define-syntax-parse-rule (for/count clauses body ... tail-expr)
  #:with original this-syntax
  #:with ((pre-body ...) (post-body ...))
         (split-for-body this-syntax #'(body ... tail-expr))
  (for/fold/derived original
    ([count 0])
    clauses
    pre-body ...
    (define maybe-count (let () post-body ...))
    (if maybe-count
      (+ count 1)
      count)))


(for/count ([line (file->lines "day04")])
  (match-let ([(list a b c d) (map string->number
                                   (string-split line #px"[^0-9]"))])
    (or (<= a c d b)
        (<= c a b d))))
; 651

(for/count ([line (file->lines "day04")])
  (match-let ([(list a b c d) (map string->number
                                   (string-split line #px"[^0-9]"))])
    (and (<= c b)
         (<= a d))))
; 956                       
