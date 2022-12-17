#lang racket

;; https://www.reddit.com/r/adventofcode/comments/zn6k1l/2022_day_16_solutions/j0g5nlk/

(define flow-rates (make-hash))
(define tunnels (make-hash))

(for ([line (file->lines "day16")])
  (let* ([valves (regexp-match* #rx"[A-Z][A-Z]" line)]
         [valve (first valves)]
         [flow-rate (string->number (first (regexp-match #rx"[0-9]+" line)))])
    (hash-set! flow-rates valve flow-rate)
    (hash-set! tunnels valve (rest valves))))

(define key-rooms (for/set ([(room flow-rate) flow-rates]
                            #:when (or (equal? room "AA") (positive? flow-rate)))
                    room))

(set-count key-rooms)

(define distance (make-hash))
(for ([start-room (hash-keys tunnels)])
  (let ([cur (mutable-set start-room)]
        [next (mutable-set)]
        [dist 0])
    (hash-set! distance (list start-room start-room) 0)
    (let loop ()
      (set! dist (+ 1 dist))
      (for ([pos cur])
        (for ([newpos (hash-ref tunnels pos)])
          (unless (hash-has-key? distance (list start-room newpos))
            (hash-set! distance (list start-room newpos) dist)
            (set-add! next newpos))))
      (set! cur next)
      (set! next (mutable-set))
      (unless (set-empty? cur)
        (loop)))))

;distance

(define (part1 cur time seen targets)
  (let* ([seen (set-add seen cur)]
         [targets (set-subtract targets seen)])
    (for/fold ([best-flow 0])
              ([target targets])
      (let ([time-left (- time (hash-ref distance (list cur target)) 1)])
        (if (positive? time-left)
          (let ([flow (+ (* (hash-ref flow-rates target) time-left)
                         (part1 target time-left seen targets))])
            (max best-flow flow))
          best-flow)))))

(part1 "AA" 30 (set) key-rooms)
;; 1720


(define endpoints (make-hash))

(define (find-and-record cur curflow time seen)
  (let* ([seen (set-add seen cur)]
         [targets (set-subtract key-rooms seen)]
         [torecord (set-remove seen "AA")])
    (hash-update! endpoints torecord (lambda (v) (max v curflow)) 0)
    (for/fold ([best-flow 0])
              ([target targets])
      (let ([time-left (- time (hash-ref distance (list cur target)) 1)])
        (if (positive? time-left)
          (let* ([flow (* (hash-ref flow-rates target) time-left)]
                 [flow (+ flow (find-and-record
                                target
                                (+ curflow flow) time-left seen))])
            (max best-flow flow))
          best-flow)))))

(void (find-and-record "AA" 0 26 (set)))

(define (fill-in-endpoints cur)
  (unless (hash-has-key? endpoints cur)
    (hash-set! endpoints cur (for/fold ([best-flow 0])
                                       ([e cur])
                               (max best-flow (fill-in-endpoints
                                               (set-remove cur e))))))
  (hash-ref endpoints cur))

(void (fill-in-endpoints (set-remove key-rooms "AA")))

(for/fold ([best-flow 0])
          ([human (hash-keys endpoints)])
  (let ([elephant (set-subtract key-rooms (set "AA") human)])
    (max best-flow (+ (hash-ref endpoints human)
                      (hash-ref endpoints elephant)))))
;; 2582
