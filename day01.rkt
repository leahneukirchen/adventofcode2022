#lang racket

(define ordered-food
  (sort (for/list ([para (string-split (file->string "day01") "\n\n")])
          (for/sum ([line (string-split para "\n")])
            (string->number line)))
        >))

(first ordered-food)                    ; 67016
(apply + (take ordered-food 3))         ; 200116
