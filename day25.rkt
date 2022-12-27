#lang racket
(require srfi/1)

(define data (for/list ([line (file->lines "day25")])
               (for/list ([c line])
                 (match c
                   [#\2 2]
                   [#\1 1]
                   [#\0 0]
                   [#\- -1]
                   [#\= -2]))))

(define (base5 ns)
  (foldl (lambda (x a) (+ (* 5 a) x)) 0 ns))

(define (unbase5 n)
  (list->string (reverse (unfold zero?
                                 (lambda (n) (match (modulo (+ n 2) 5)
                                               [0 #\=]
                                               [1 #\-]
                                               [2 #\0]
                                               [3 #\1]
                                               [4 #\2]))
                                 (lambda (n) (quotient (+ n 2) 5))
                                 n))))

; (map base5 data)

(unbase5 (for/sum ([n data])
           (base5 n)))

;; "2-0-020-1==1021=--01"
