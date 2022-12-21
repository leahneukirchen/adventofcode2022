#lang racket

(require data/queue)

(define data (for/list ([(n i) (in-indexed (file->list "day20-sterni"))])
               (vector n i)))

(define (move zip n)
  (cond [(= n 0) zip]
        [(< n 0) (if (null? (car zip))
                   (move (cons (reverse (cdr zip)) '()) n)
                   (move (cons (cdr (car zip))
                               (cons (car (car zip)) (cdr zip)))
                         (+ n 1)))]
        [(> n 0) (if (null? (cdr zip))
                   (move (cons '() (reverse (car zip))) n)
                   (move (cons (cons (car (cdr zip)) (car zip))
                               (cdr (cdr zip)))
                         (- n 1)))]))

(define (peek zip)
  (if (null? (cdr zip))
    (peek (move (move zip 1) -1))
    (car (cdr zip))))

(define (zip->list zip)
  (append (reverse (car zip)) (cdr zip)))

(define (find ring elt)
  (let ([ring (move (move ring 1) -1)])
    (if (equal? (vector-ref (peek ring) 1) elt)
      ring
      (find (move ring 1) elt))))

(define (find-val ring elt)
  (let ([ring (move (move ring 1) -1)])
    (if (equal? (vector-ref (peek ring) 0) elt)
      ring
      (find-val (move ring 1) elt))))

; (move (cons (list 1 2 3) (list 4 5 6)) 2)
; (move (move (cons (list 1 2 3) (list 4 5 6)) 100) -100)

(define ring (cons '() data))

(define (shift ring n)
  (let* ([n (remainder n (- (length data) 1))]
         [ring (move (move ring 1) -1)]
         [e (peek ring)]
         [ring (cons (car ring) (cdr (cdr ring)))]  ; drop e
         [ring (move ring n)]
         [ring (cons (car ring) (cons e (cdr ring)))]  ; insert e
         [ring (move ring (- n 1))]
         )
    ring))

; (shift (cons (list #(1 1) #(2 2) #(3 3)) (list #(4 4) #(5 5) #(6 6))) 2)

(for ([(_ i) (in-indexed data)])
  (set! ring (find ring i))
  (set! ring (shift ring (vector-ref (peek ring) 0))))

(let* ([_ (set! ring (find-val ring 0))]
       [_ (set! ring (move ring 1000))]
       [a (vector-ref (peek ring) 0)]
       [_ (set! ring (move ring 1000))]
       [b (vector-ref (peek ring) 0)]
       [_ (set! ring (move ring 1000))]
       [c (vector-ref (peek ring) 0)])
  (+ a b c))
;; 16533


(define data2 (for/list ([(n i) (in-indexed (file->list "day20"))])
                (vector (* n 811589153) i)))

(define ring2 (cons '() data2))

(for ([_ (in-inclusive-range 1 10)])
  (for ([(_ i) (in-indexed data2)])
    (set! ring2 (find ring2 i))
    (set! ring2 (shift ring2 (vector-ref (peek ring2) 0)))))

(zip->list ring2)

(let* ([_ (set! ring2 (find-val ring2 0))]
       [_ (set! ring2 (move ring2 1000))]
       [a (vector-ref (peek ring2) 0)]
       [_ (set! ring2 (move ring2 1000))]
       [b (vector-ref (peek ring2) 0)]
       [_ (set! ring2 (move ring2 1000))]
       [c (vector-ref (peek ring2) 0)])
  (+ a b c))
;; 4789999181006
