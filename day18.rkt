#lang racket

(define points (for/set ([line (file->lines "day18")])
                 (list->vector (map string->number (string-split line ",")))))

(define (neighbors p)
  (for/list ([o '(#(-1  0  0)
                  #( 1  0  0)
                  #( 0 -1  0)
                  #( 0  1  0)
                  #( 0  0  1)
                  #( 0  0 -1))])
    (vector-map + p o)))

(for/sum ([p points])
  (for/sum ([n (neighbors p)])
    (if (not (set-member? points n))
        1
        0)))
;; 3550

(define (foldl1 op lst)
  (foldl op (car lst) (cdr lst)))

(let ([minx (foldl1 min (map (lambda (v) (vector-ref v 0)) (set->list points)))]
      [maxx (foldl1 max (map (lambda (v) (vector-ref v 0)) (set->list points)))]
      [miny (foldl1 min (map (lambda (v) (vector-ref v 1)) (set->list points)))]
      [maxy (foldl1 max (map (lambda (v) (vector-ref v 1)) (set->list points)))]
      [minz (foldl1 min (map (lambda (v) (vector-ref v 2)) (set->list points)))]
      [maxz (foldl1 max (map (lambda (v) (vector-ref v 2)) (set->list points)))]
      [exterior (mutable-set)])
  (define (flood-fill p)
    (when (and (<= (- minx 1) (vector-ref p 0) (+ maxx 1))
               (<= (- miny 1) (vector-ref p 1) (+ maxy 1))
               (<= (- minz 1) (vector-ref p 2) (+ maxz 1))
               (not (set-member? points p))
               (not (set-member? exterior p)))
      (set-add! exterior p)
      (for-each flood-fill (neighbors p))))
  (flood-fill (vector (- minx 1) (- miny 1) (- minz 1)))
  
  (for/sum ([p points])
    (for/sum ([n (neighbors p)])
      (if (set-member? exterior n)
          1
          0))))
;; 2028
