#lang racket

(require json)

(define lists (file->list "day13" read-json))

(define data
  (let loop ([lists lists]
             [pairs '()])
    (if (null? lists)
      (reverse pairs)
      (let-values ([(pair rest) (split-at lists 2)])
        (loop rest (cons pair pairs))))))

(define (compare left right)
  (cond [(and (number? left) (number? right)) (- left right)]
        [(and (list? left) (list? right))
         (let loop ([leaning (compare (length left) (length right))]
                    [left left]
                    [right right])
           (cond [(null? left) leaning]
                 [(null? right) 1]
                 [(compare (car left) (car right)) =>
                  (lambda (c)
                    (if (zero? c)
                      (loop leaning (cdr left) (cdr right))
                      c))]))]
        [(and (number? left) (list? right))
         (compare (list left) right)]
        [(and (list? left) (number? right))
         (compare left (list right))]
        [else (error "can't happen")]))

(for/sum ([(pair i) (in-indexed data)])
  (if (negative? (apply compare pair))
    (+ i 1)
    0))
; 6420

(define part2
  (sort (list* '((2)) '((6)) lists)
        (compose negative? compare)))

(* (+ 1 (index-of part2 '((2))))
   (+ 1 (index-of part2 '((6)))))
;; 22000
