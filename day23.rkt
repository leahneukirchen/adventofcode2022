#lang racket

(define occupied
  (list->mutable-set
   (for*/list ([(line row) (in-indexed (file->lines "day23"))]
               [(char col) (in-indexed line)]
               #:when (equal? char #\#))
     (vector col row))))

; (set-count occupied)
; occupied

(define adjs (list (list #(0 -1) #(1 -1) #(-1 -1))
                   (list #(0  1) #(1  1) #(-1  1))
                   (list #(-1 0) #(-1 1) #(-1 -1))
                   (list #( 1 0) #( 1 1) #( 1 -1))))
(define dirs (list #(0 -1)
                   #(0  1)
                   #(-1 0)
                   #( 1 0)))

(let loop ((rounds 1))
  (define next (make-hash))
  (for ([elf occupied])
    (unless (for*/and ([dx '(-1 0 1)]
                       [dy '(-1 0 1)]
                       #:unless (= dx dy 0))
              (not (set-member? occupied (vector-map + elf (vector dx dy)))))
      (for/or ([adj adjs]
               [dir dirs])
        (if (for/and ([p adj])
              (not (set-member? occupied (vector-map + elf p))))
          (begin
            (hash-update! next
                          (vector-map + elf dir)
                          (lambda (l) (cons elf l))
                          '())
            #t)                         ;; break
          #f))))

  (set! adjs (append (cdr adjs) (list (car adjs))))
  (set! dirs (append (cdr dirs) (list (car dirs))))

  (define moved #f)
  (for ([(cand elf) next])
    (when (= (length elf) 1)
      (set-remove! occupied (first elf))
      (set-add! occupied cand)
      (set! moved #t)))

  (when (= rounds 10)
    (displayln (for/fold ([min-x  9999]
                          [max-x -9999]
                          [min-y  9999]
                          [max-y -9999]
                          #:result (- (* (- max-x min-x -1)
                                         (- max-y min-y -1))
                                      (set-count occupied))
                          )
                   ([elf occupied])
                 (values (min min-x (vector-ref elf 0))
                         (max max-x (vector-ref elf 0))
                         (min min-y (vector-ref elf 1))
                         (max max-y (vector-ref elf 1))))))
  ;; 3940

  (if moved 
    (loop (+ rounds 1))
    (displayln rounds)))
;; 990
