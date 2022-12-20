#lang racket

(define blueprints
  (for/list ([line (file->lines "day19s")])
    (match (cdr (map string->number (regexp-match* #rx"[0-9]+" line)))
      [(list a b c d e f)
       (list (list (list 0 0 0 a) (list 0 0 0 1))
             (list (list 0 0 0 b) (list 0 0 1 0))
             (list (list 0 0 d c) (list 0 1 0 0))
             (list (list 0 f 0 e) (list 1 0 0 0))
             (list (list 0 0 0 0) (list 0 0 0 0)))])))

(define (list>? x y)
  (let comparing ((x x) (y y))
    (cond ((null? x) #f) ; same
          ((> (car x) (car y)) #t)
          ((> (car y) (car x)) #f)
          (else (comparing (cdr x) (cdr y))))))

(define (key a)
  (map + (list-ref a 0) (list-ref a 1)))

(define (take* lst n)
  (if (< (length lst) n)
      lst
      (take lst n)))

(define (run blueprint t)
  (let ([todo (list (list (list 0 0 0 0) (list 0 0 0 1)))])
    (for ([_ (in-range 0 t)])
      (let ([new-todo '()])
        (for* ([item1 todo]
               [item2 blueprint])
          (let ([have (first item1)]
                [make (second item1)]
                [cost (first item2)]
                [more (second item2)])
            (when (andmap >= have cost)
              (set! new-todo (cons (list (map + have make (map - cost))
                                         (map + make more))
                                   new-todo)))))
        (set! todo (take* (sort new-todo list>? #:key key) 2000))))
    (first (sort todo list>? #:key key))))

; (pretty-print blueprints)

(run (first blueprints) 24)
(run (second blueprints) 24)

(for/sum ([b blueprints]
      [i (in-inclusive-range 1 (length blueprints))])
  (* i (caar (run b 24))))
;; 1294

(for/product ([b blueprints]
              [i (in-inclusive-range 1 3)])
  (caar (run b 32)))
;; 13640
