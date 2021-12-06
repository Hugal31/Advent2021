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
  (length
   (fold
    (lambda (_ fishes)
      (advance-ages fishes))
    numbers
    (iota 80))))

(define-public (solve2 numbers)
  (length
   (fold
    (lambda (_ fishes)
      (advance-ages fishes))
    numbers
    (iota 256))))

(define (advance-ages fishes)
  (list-transduce
   (compose (tmap age-and-breed) tflatten)
   rcons fishes))

(define (age-and-breed fish-timer)
  (if (eq? 0 fish-timer)
      (list 6 8)
      (1- fish-timer)))

(define (parse input)
  (map string->number (string-split (read-line input) #\,)))
