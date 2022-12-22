#lang racket

(define monkeys (make-hash))

(for ([line (file->lines "day21")])
  (let* ([parts (string-split line ": ")]
         [words (string-split (second parts) " ")])
    (if (= (length words) 1)
      (hash-set! monkeys (first parts) (string->number (first words)))
      (hash-set! monkeys (first parts) (list
                                        (case (second words)
                                          [("+") +]
                                          [("-") -]
                                          [("*") *]
                                          [("/") quotient])
                                        (first words)
                                        (third words))))))

(define (eval-monkey m)
  (match (hash-ref monkeys m)
    [(list op m1 m2) (op (eval-monkey m1) (eval-monkey m2))]
    [n n]))

(eval-monkey "root")
;; 353837700405464

(define (part2 m)
  (case m
    [("root") (match (hash-ref monkeys m)
                [(list _ m1 m2) (list '= (part2 m1) (part2 m2))])]
    [("humn") 'humn]
    [else     (match (hash-ref monkeys m)
                [(list op m1 m2) (list op (part2 m1) (part2 m2))]
                [n n])]))

(define (partial-eval t)
  (match t
    [(list op t1 t2) (match (list (partial-eval t1) (partial-eval t2))
                       [(list (? number? n1) (? number? n2))
                        (op n1 n2)]
                       [(list v1 v2)
                        (list op v1 v2)])]
    [n n]))

(define (solve l r)
  (match l
    [(list (== quotient) l1 (? number? l2)) (solve l1 (* l2 r))]
    [(list (== +) (? number? l1) l2) (solve l2 (- r l1))]
    [(list (== +) l1 (? number? l2)) (solve l1 (- r l2))]
    [(list (== *) (? number? l1) l2) (solve l2 (quotient r l1))]
    [(list (== *) l1 (? number? l2)) (solve l1 (quotient r l2))]
    [(list (== -) l1 (? number? l2)) (solve l1 (+ r l2))]
    [(list (== -) (? number? l1) l2) (solve l2 (- l1 r))]
    [e (list '= l r)]
    ))
    
(apply solve (cdr (partial-eval (part2 "root"))))
;; 3678125408017
