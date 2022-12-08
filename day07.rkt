#lang racket

(define data (file->lines "day07"))

(define fs (make-hash))
(let loop ([data (cdr data)]
           [cwd '()])
  (let ([cmd (car data)]
        [rest (cdr data)])
    (cond [(string-prefix? cmd "$ cd")
           (let ([dir (third (string-split cmd " "))])
             (if (equal? dir "..")
               (loop rest (cdr cwd))
               (loop rest (cons dir cwd))))]
          [(equal? cmd "$ ls")
           (let dir-loop ([dir-list rest])
             (if (null? dir-list)
               (void)                   ; done
               (let ([line (car dir-list)]
                     [dir-rest (cdr dir-list)])
                 (cond [(string-prefix? line "$")
                        (loop (cons line dir-rest) cwd)] ; push back line
                       [(string-prefix? line "dir")
                        (dir-loop dir-rest)] ; ignore
                       [else
                        (let* ([fields (string-split line " ")]
                               [size (string->number (first fields))])
                          (let segment-loop ([dir cwd])
                            (hash-update! fs
                                          (string-join (reverse dir) "/" #:before-first "/")
                                          (curry + size)
                                          0)
                            (unless (null? dir)
                              (segment-loop (cdr dir))))
                          (dir-loop dir-rest))]))))]
          [else (error "invalid line")])))

(for/sum ([(dir size) fs])
  (if (<= size 100000)
    size
    0))
; 1306611

(let* ([total (hash-ref fs "/")]
       [unused (- 70000000 total)]
       [needed (- 30000000 unused)])
  (for/fold ([smallest total])
            ([(dir size) fs])
    (if (>= size needed)
      (min smallest size)
      smallest)))
; 13210366
