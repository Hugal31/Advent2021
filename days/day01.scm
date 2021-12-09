(define-module (days day01)
  #:use-module (advent-utils)
  #:use-module (srfi srfi-1))

(define-public (solve1 numbers)
  (count >
         (cdr numbers) numbers))

(define-public (solve2 numbers)
  (count >
         (cdddr numbers) numbers))

(define-public parse parse-ints-file)
