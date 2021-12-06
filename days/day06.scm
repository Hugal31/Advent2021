(define-module (days day06)
  #:use-module (advent-utils)
  #:use-module (ice-9 rdelim)
  #:use-module (srfi srfi-1)
  #:use-module (srfi srfi-171))

(define-public (parse-and-solve1 input)
  (solve1 (parse input)))

(define-public (parse-and-solve2 input)
  (solve2 (parse input)))

(define-public (solve1 numbers)
  (predict-population numbers 80))

(define-public (solve2 numbers)
  (predict-population numbers 256))

(define (predict-population fishes days)
  (define fishes-vec
    (list->vector (map
                   (lambda (day)
                     (count (lambda (fish) (eq? day fish)) fishes))
                   (iota 9))))

  (for-each
   (lambda (_)
     (let ((n-ready-fishes (vector-ref fishes-vec 0)))
       (vector-move-left! fishes-vec 1 9 fishes-vec 0)
       (vector-set! fishes-vec 8 n-ready-fishes)
       (vector-set! fishes-vec 6 (+ n-ready-fishes (vector-ref fishes-vec 6)))))
   (iota days))

  (fold + 0 (vector->list fishes-vec)))

(define (parse input)
  (map string->number (string-split (read-line input) #\,)))
