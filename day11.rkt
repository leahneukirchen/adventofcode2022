#lang racket

(struct monkey
  (items operation divisible if-true if-false inspections)
  #:mutable #:transparent)

(define (parse desc)
  (let* ([lines (string-split desc "\n")]
         [items (map string->number
                     (string-split (second (string-split (second lines) ": "))
                                   ", "))]
         [operation
          (match (string-split (second (string-split (third lines) "old ")) " ")
            [(list "*" "old") (lambda (old) (* old old))]
            [(list "*" n)     (lambda (old) (* old (string->number n)))]
            [(list "+" n)     (lambda (old) (+ old (string->number n)))])]
         [test (string->number (last (string-split (fourth lines) " ")))]
         [if-true (string->number (last (string-split (fifth lines) " ")))]
         [if-false (string->number (last (string-split (sixth lines) " ")))]
         )
    (monkey items operation test if-true if-false 0)))

(define monkeys
  (for/vector ([para (string-split (file->string "day11") "\n\n")])
    (parse para)))

(define q (for/product ([m monkeys]) (monkey-divisible m)))

(define (step monkeys part)
  (for ([m monkeys])
    (for ([item (monkey-items m)])
      (let* ([worry ((monkey-operation m) item)]
             [worry-bored (if (= part 1)
                              (quotient worry 3)
                              (remainder worry q))]
             [to (if (zero? (remainder worry-bored (monkey-divisible m)))
                     (monkey-if-true m)
                     (monkey-if-false m))]
             [mto (vector-ref monkeys to)])
        (set-monkey-inspections! m (+ 1 (monkey-inspections m)))
        (set-monkey-items! mto
                           (append (monkey-items mto) (list worry-bored))))
      (set-monkey-items! m '()))))

(for ([i (in-inclusive-range 1 20)])
  (step monkeys 1))
(apply * (take (sort (for/list ([m monkeys]) (monkey-inspections m)) >) 2))
;; 54253

(set! monkeys
  (for/vector ([para (string-split (file->string "day11") "\n\n")])
    (parse para)))

(for ([i (in-inclusive-range 1 10000)])
  (step monkeys 2))
(apply * (take (sort (for/list ([m monkeys]) (monkey-inspections m)) >) 2))
;; 13119526120
