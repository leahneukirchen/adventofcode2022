#lang racket

(define pieces
  '(#(#(#t #t #t #t))

    #(#(#f #t #f)
      #(#t #t #t)
      #(#f #t #f))

    #(#(#f #f #t)
      #(#f #f #t)
      #(#t #t #t))

    #(#(#t)
      #(#t)
      #(#t)
      #(#t))

    #(#(#t #t)
      #(#t #t))
    ))

(define piece-height vector-length)
(define (piece-width p) (vector-length (vector-ref p 0)))

(define input (list->vector (string->list (car (file->lines "day17")))))

(define stack #())

(define (piece-fit stack piece offset-height offset-width)
  (for/and ([h (in-range 0 (piece-height piece))])
    (for/and ([w (in-range 0 (piece-width piece))])
      (if (and (< -1 (+ offset-height h) (vector-length stack))
               (< -1 (+ offset-width w) 7))
        (if (equal? #t (vector-ref (vector-ref piece h) w))
          (equal? #f (vector-ref (vector-ref stack (+ offset-height h)) (+ offset-width w)))
          #t)
        #f))))

(define (piece-place stack piece offset-height offset-width)
  (for ([h (in-range 0 (piece-height piece))])
    (for ([w (in-range 0 (piece-width piece))])
      (when (and (< -1 (+ offset-height h) (vector-length stack))
                 (< -1 (+ offset-width w) 7)
                 (equal? #t (vector-ref (vector-ref piece h) w)))
        (vector-set! (vector-ref stack (+ offset-height h)) (+ offset-width w) #t)))))

(define (drop-piece piece)
  (set! stack (vector-append (build-vector (+ 3 (piece-height piece))
                                           (lambda (_)
                                             (make-vector 7 #f)))
                             stack))

  (let ([offset-height 0]
        [offset-width 2])
    (let loop ()
      (let ([move (if (equal? (next-move) #\<) -1 1)])
        (when (piece-fit stack piece offset-height (+ offset-width move))
          (set! offset-width (+ offset-width move)))
        (when (piece-fit stack piece (+ 1 offset-height) offset-width)
          (set! offset-height (+ offset-height 1))
          (loop))))
    (piece-place stack piece offset-height offset-width)
    (let loop2 ([empty-height 0])
      (if (vector-member #t (vector-ref stack empty-height))
        (when (> empty-height 0)
          (set! stack (vector-drop stack empty-height)))
        (loop2 (+ 1 empty-height)))))
  )

(define next-move
  (let ([i 0]
        [s (vector-length input)])
    (lambda ()
      (let ([r (vector-ref input (remainder i s))])
        (set! i (+ 1 i))
        r))))

(for ([_ (in-inclusive-range 1 2022)]
      [piece (in-cycle pieces)])
  (drop-piece piece))

(vector-length stack)
;; 3067

; stare at the output
;; 1514369501484
